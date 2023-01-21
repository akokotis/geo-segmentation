create or replace table "AK_global_vars" as (
select
20200201 as start_date,
20210102 as end_date,
'2020-02-01' as start_dt,
'2021-01-02' as end_dt
);

--INCLUDE BRAND
--WEATHER DATA
--PRODUCTS PURCHASED AND ATC NO MATTER HOW THEY GOT THERE
-----USE jON'S GROUPING
---traffic by event

create or replace table "AK_sessions" as (
select
SESSION_SEARCH_DATE_KEY,
a.VISITOR_KEY,
b.FISCAL_MONTH,
a.session_key,
a.SITE_ID,
a.page_name,
a.page_name_key,
a.PAGE_TEMPLATE,
CLICKSTREAM_VISIT_PAGE_NUM,
USER_SEARCH_RESULTS_RETURNED_COUNT,
trim(NAVIGATION_SEARCH_TERM) as nav_search_term,
case when contains(BREADCRUMB, '|') then SPLIT_PART(BREADCRUMB,'|',1)
    when (BREADCRUMB is null or BREADCRUMB='') then trim(NAVIGATION_SEARCH_TERM)
    else BREADCRUMB end as nav_breadcrumb,
SEARCH_REFINEMENT_TYPE,
case when USER_SEARCH_RESULTS_RETURNED_COUNT>0 then 0 else 1 end as null_search_flag
from
"SESSION_SEARCH" a
inner join "CALENDAR" b on a.SESSION_SEARCH_DATE_KEY=b.date_key
left join (select SESSION_KEY from SHARED_ANALYSIS.MARKETING.CLICKSTREAM_SESSION_BOT
			where SESSION_BOT_FLAG = 1
			AND SESSION_START_DATE between (select start_dt from "AK_global_vars") and (select end_dt from "AK_global_vars")
			group by 1) c on a.session_key=c.session_key

where SESSION_SEARCH_DATE_KEY between (select start_date from "AK_global_vars") and (select end_date from "AK_global_vars")
and (NAVIGATION_SEARCH_TERM!='' or USER_SEARCH_TERM!='')
and NAVIGATION_CATEGORY_TYPE in ('cat','query')
and PAGE_TEMPLATE='nav_search'
and page_name!=''
and c.session_key is null
group by 1,2,3,4,5,6,7,8,9,10,11,12,13,14
);

create or replace table "SEARCH_AK_GSS_total_visitors_CBSA" as (
select
--FISCAL_MONTH,
--d.CBSA_NAME,
d.DMA_CODE,
count(distinct b.VISITOR_KEY) as visitor_count_total
from
."AK_sessions" b
left join "SESSION" d on d.visitor_key=b.visitor_key and d.SESSION_START_DATE_KEY=b.SESSION_SEARCH_DATE_KEY
group by 1--,2
);


create or replace table "SEARCH_AK_GSS_nav_theme_counts_CBSA" as (
select
--FISCAL_MONTH,
--d.CBSA_NAME,
d.DMA_CODE,
nav_breadcrumb as NAV_THEME, 
count(distinct b.VISITOR_KEY) as visitor_count
from
"AK_sessions" b
inner join "SESSION" d on d.visitor_key=b.visitor_key and d.SESSION_START_DATE_KEY=b.SESSION_SEARCH_DATE_KEY
group by 1,2--,3
);


create or replace table "DIGITAL_RETAIL_ANALYSIS"."DWADMIN"."SEARCH_AK_GSS_outlet_counts_CBSA" as (
select
--FISCAL_MONTH,
--d.CBSA_NAME,
d.DMA_CODE,
SITE_ID, 
count(distinct b.VISITOR_KEY) as visitor_count
from
"AK_sessions" b
inner join "SESSION" d on d.visitor_key=b.visitor_key and d.SESSION_START_DATE_KEY=b.SESSION_SEARCH_DATE_KEY
where SITE_ID='outlet'
group by 1,2--,3
);


create or replace table "AK_product_purchases" as (
SELECT D.FISCAL_DATE
	,GEO.DMA_CODE
	,GEO.DMA_NAME
	,GEO.DMA_SHORT_DESC
	,GEO.DISTRICT_NAME
	,GEO.STATE_NAME
	,GEO.CBSA_CODE
	,GEO.CBSA_NAME
	,DMA_ZIP_CODE
	,L.ORDER_ID
    , S.PRODUCT		
	, case when DS.DIGITAL_SHOP IS NOT NULL THEN DS.DIGITAL_SHOP ELSE S.SHOP_DESC END AS DIGITAL_SHOP
	, S.SUBCLASS_DESC
	, P.BRAND_DESC
	, S.COLOR_FAMILY 

	, FL.ORDER_FLOW_DESC
	, PS.PRICE_SUB_TYPE_DESC AS PRICE_FLAG
    , att.ACTIVITY_PRODUCT
	,ACT.ACTIVITY_DESC AS JONS_LABEL
	
    , SUM(L.LINE_QTY) AS UNITS
	, SUM(L.NET_AMT + (CASE WHEN L.CURRENT_CORP_PRICE IS NULL THEN 0 ELSE L.CURRENT_CORP_PRICE END) * L.CANCELLED_QTY) AS DEMAND
	,SUM(CASE WHEN COGS_FLAG = 1 THEN L.LINE_QTY * ADJUSTED_MOVING_AVERAGE_PRICE ELSE 0 END) AS COST

FROM ORDER_LINE L 
	JOIN CALENDAR D ON L.ORDER_DATE_KEY = D.DATE_KEY

	JOIN MERCH_PRODUCT P ON P.PRODUCT_KEY = L.PRODUCT_KEY 
	JOIN MERCH_SKU S ON P.PRODUCT_KEY = S.PRODUCT_KEY AND L.SKU_KEY = S.SKU_KEY AND P.DEPARTMENT_KEY = S.DEPARTMENT_KEY 
	JOIN ORDER_FLOW FL ON L.ORDER_FLOW_KEY = FL.ORDER_FLOW_KEY 
	JOIN PRICE_SUB_TYPE PS ON L.PRICE_SUB_TYPE_KEY = PS.PRICE_SUB_TYPE_KEY
	
    LEFT JOIN DIGITAL_SHOP DS ON DS.DEPARTMENT_DESC = P.DEPARTMENT_DESC
   LEFT JOIN (select PRODUCT_KEY,max(case when ATTRIBUTE_NAME='PRIMARY_ACTIVITY_CODE' then ATTRIBUTE_VALUE end) as ACTIVITY_PRODUCT from "MERCH_SKU_ATTRIBUTE" group by 1) att on att.PRODUCT_KEY=s.PRODUCT_KEY
	
	LEFT JOIN MERCH_HIER_ACTIVITY_XREF ACT ON
				ACT.SHOP_DESC=s.SHOP_DESC AND ACT.DEPARTMENT_ID=s.DEPARTMENT_ID AND ACT.CLASS_ID=s.CLASS_ID AND ACT.SUBCLASS_ID=s.SUBCLASS_ID AND ACT.DONA_ID=s.DONA_ID

	LEFT JOIN (SELECT AD.ADDRESS_KEY
					, AD.DMA_GEO_KEY
					,DG.DMA_CODE
					,DG.DMA_NAME
					,DG.STATE_NAME
					,D.DMA_SHORT_DESC
					,D.DISTRICT_NAME
					,DG.CBSA_CODE
					, DG.CBSA_NAME
					,DG.DMA_ZIP_CODE
               FROM ADDRESS AD 
				  	JOIN (SELECT DMA_GEO_KEY
							, CAST(CASE WHEN DMA_CODE = 'N/A' OR DMA_CODE = 'UNK' THEN '1' ELSE DMA_CODE END AS INTEGER) AS DMA_CODE
                           ,DMA_NAME
                          ,CBSA_CODE
                          ,CBSA_NAME
						  ,DMA_ZIP_CODE
						  ,STATE_NAME
						  FROM DMA_GEO_DIM) DG
						ON DG.DMA_GEO_KEY = AD.DMA_GEO_KEY 
					JOIN DMA_DISTRICT D 
						ON D.DMA_CODE = DG.DMA_CODE) 
                        
                        GEO ON GEO.ADDRESS_KEY = L.BILLTO_ADDRESS_KEY
						
WHERE 1=1
AND ORDER_DATE_KEY BETWEEN (select start_date from "AK_global_vars") and (select end_date from "AK_global_vars")
	AND L.SKU NOT IN ('9000000000', '6485920018', '6485930017','9990490006', '9000000208', '9000000006','6486000018')
	AND L.ORDER_FLOW_KEY NOT IN (8,10,11,12) ---filtering out fulfillment types: returns, adventures, opo, used gear
    and ORIGINATION_CHANNEL_KEY in (102,101)
    and ORDER_ENTRY_GROUP_KEY IN (1,-2, 4, 2, 3)
    and ORDER_ENTRY_APPLICATION_KEY = 3
    ---these last 3 should include digital, retail, and call center orders

GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19

);


create or replace table "SEARCH_AK_GSS_product_activity_counts_CBSA" as (
select
--CBSA_NAME,
DMA_CODE,
--DMA_NAME,
--DMA_SHORT_DESC,
SUBCLASS_DESC,
--JONS_LABEL as ACTIVITY, 
count(distinct ORDER_ID) as ORDER_count
from
"AK_product_purchases"
group by 1,2--,3,4,5
);


create or replace table "SEARCH_AK_GSS_product_BRAND_counts_CBSA" as (
select
--CBSA_NAME,
DMA_CODE,
BRAND_DESC, 
count(distinct ORDER_ID) as ORDER_count
from
"AK_product_purchases"
group by 1,2--,3
);

create or replace table "SEARCH_AK_GSS_product_ORDERFLOW_counts_CBSA" as (
select
--CBSA_NAME,
DMA_CODE,
ORDER_FLOW_DESC, 
count(distinct ORDER_ID) as ORDER_count
from
"AK_product_purchases"
group by 1,2--,3
);	
	
create or replace table "SEARCH_AK_GSS_product_PRICE_counts_CBSA" as (
select
--CBSA_NAME,
DMA_CODE,
PRICE_FLAG, 
count(distinct ORDER_ID) as ORDER_count
from
"AK_product_purchases"
group by 1,2--,3
);	

create or replace table "SEARCH_AK_GSS_product_COLOR_counts_CBSA" as (
select
--CBSA_NAME,
DMA_CODE,
COLOR_FAMILY, 
count(distinct ORDER_ID) as ORDER_count
from
"AK_product_purchases"
group by 1,2--,3
);	


create or replace table "SEARCH_AK_GSS_GEO_MAPPING_TABLE" as (
select
DMA_CODE,
DMA_NAME,
DMA_SHORT_DESC,
DMA_ZIP_CODE,
CBSA_CODE,
CBSA_NAME,
DISTRICT_NAME,
STATE_NAME
from
"AK_product_purchases"
group by 1,2,3,4,5,6,7,8
);	





