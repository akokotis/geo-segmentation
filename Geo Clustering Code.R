#####################
## GEO Clustering ###
#####################
## Search, 5/1/20 ###

library(nzr)
nzConnect(machine='', database='', user='', password='xxxx')

#https://cran.r-project.org/web/packages/keyringr/vignettes/Avoiding_plain_text_passwords_in_R_with_keyringr.html
########################
######## Datasets ######
########################
total=nzQuery('SELECT * FROM AK_GEO_PIVOT_total_visitors')
  total$VISITOR_COUNT_TOTAL=as.integer(total$VISITOR_COUNT_TOTAL)
  total$FISCAL_MONTH=as.integer(total$FISCAL_MONTH)
save(total, file="geo_total.RData")

theme=nzQuery('SELECT * FROM AK_GEO_PIVOT_nav_theme_counts')
  theme$FISCAL_MONTH=as.integer(theme$FISCAL_MONTH)
  theme$VISITOR_COUNT=as.integer(theme$VISITOR_COUNT)
save(theme, file="geo_theme.RData")
  
filter=nzQuery('SELECT * FROM AK_GEO_PIVOT_nav_filter_counts')
  filter$FISCAL_MONTH=as.integer(filter$FISCAL_MONTH)
  filter$VISITOR_COUNT=as.integer(filter$VISITOR_COUNT)
save(filter, file="geo_filter.RData")
  
outlet=nzQuery('SELECT * FROM AK_GEO_PIVOT_outlet_counts')
  outlet$FISCAL_MONTH=as.integer(outlet$FISCAL_MONTH)
  outlet$VISITOR_COUNT=as.integer(outlet$VISITOR_COUNT)  
  save(outlet, file="geo_outlet.RData")
  
product=nzQuery('SELECT * FROM AK_GEO_PIVOT_product_counts')
  product$FISCAL_MONTH=as.integer(product$FISCAL_MONTH)
  product$VISITOR_COUNT=as.integer(product$VISITOR_COUNT)  
  save(product, file="geo_product.RData")

# Disconnect
nzDisconnect()

#######################
## Clean & Manipulate #
#######################
require("reshape2")
theme_wide=dcast(theme, FISCAL_MONTH+CBSA_NAME ~ NAV_THEME, value.var="VISITOR_COUNT", fill=0)
filter_wide=dcast(filter[which(filter$SEARCH_REFINEMENT_TYPE!=''),], FISCAL_MONTH+CBSA_NAME ~ SEARCH_REFINEMENT_TYPE, value.var="VISITOR_COUNT", fill=0)
outlet_wide=dcast(outlet, FISCAL_MONTH+CBSA_NAME ~ SITE_ID, value.var="VISITOR_COUNT", fill=0)
product_wide=dcast(product, FISCAL_MONTH+CBSA_NAME ~ CLASS_DESC, value.var="VISITOR_COUNT", fill=0)

start_merge1=merge(total, theme_wide, by=c("FISCAL_MONTH","CBSA_NAME"))
start_merge2=merge(start_merge1, filter_wide, by=c("FISCAL_MONTH","CBSA_NAME"))
start_merge3=merge(start_merge2, outlet_wide, by=c("FISCAL_MONTH","CBSA_NAME"))
start_merge4=merge(start_merge3, product_wide, by=c("FISCAL_MONTH","CBSA_NAME"))

final=start_merge4 ### dataset with all raw counts
save(final, file="geo_final.RData")

# roll up to quarter level
final$quarter=0
final$quarter[which(final$FISCAL_MONTH %in% c(201801,201802,201803))]=201801
final$quarter[which(final$FISCAL_MONTH %in% c(201804,201805,201806))]=201802
final$quarter[which(final$FISCAL_MONTH %in% c(201807,201808,201809))]=201803
final$quarter[which(final$FISCAL_MONTH %in% c(201810,201811,201812))]=201804

final$quarter[which(final$FISCAL_MONTH %in% c(201901,201902,201903))]=201901
final$quarter[which(final$FISCAL_MONTH %in% c(201904,201905,201906))]=201902
final$quarter[which(final$FISCAL_MONTH %in% c(201907,201908,201909))]=201903
final$quarter[which(final$FISCAL_MONTH %in% c(201910,201911,201912))]=201904

final$quarter[which(final$FISCAL_MONTH %in% c(202001,202002,202003))]=202001

require("dplyr")
final_quarter=aggregate(.~quarter+CBSA_NAME, final[,-c("FISCAL_MONTH")], sum)
  
  #final %>% 
  #group_by(quarter, CBSA_NAME) %>% 
  #summarise_each(.funs(sum)) ### dataset with all raw counts at quarter time granularity

save(final_quarter, file="geo_final_quarter.RData")



## create dataset with all percents
final_percent=final
next_column=ncol(final_percent)+1
for(i in 4:ncol(final_percent)){
  name=colnames(final_percent)[i]
  final_percent[,next_column]=final_percent[,i]/final_percent[,3]
  colnames(final_percent)[next_column]=paste0(name,"_p")
  next_column=next_column+1
}

final_p=final_percent[,c(1,2,3,(ncol(final)+1):ncol(final_percent))]
save(final_p,file="geo_final_p.RData")

### for quarter level
final_quarter_percent=final_quarter
next_column=ncol(final_quarter_percent)+1
for(i in 4:ncol(final_quarter_percent)){
  name=colnames(final_quarter_percent)[i]
  final_quarter_percent[,next_column]=final_quarter_percent[,i]/final_quarter_percent[,3]
  colnames(final_quarter_percent)[next_column]=paste0(name,"_p")
  next_column=next_column+1
}

final_quarter_p=final_quarter_percent[,c(1,2,3,(ncol(final_quarter)+1):ncol(final_quarter_percent))]
save(final_quarter_p,file="geo_final_quarter_p.RData")

### for CBSA level
load("~/geo_final_cbsa.RData")
final_cbsa_percent=final_cbsa
next_column=ncol(final_cbsa_percent)+1
for(i in 3:ncol(final_cbsa_percent)){
  name=colnames(final_cbsa_percent)[i]
  final_cbsa_percent[,next_column]=final_cbsa_percent[,i]/final_cbsa_percent[,2]
  colnames(final_cbsa_percent)[next_column]=paste0(name,"_p")
  next_column=next_column+1
}
final_cbsa_p=final_cbsa_percent[,c(1,2,(ncol(final_cbsa)+1):ncol(final_cbsa_percent))]

lets_try_cbsa=final_cbsa_p[,4:ncol(final_cbsa_p)]*100
i0_cbsa=colSums(lets_try_cbsa==0)
nrow(lets_try_cbsa) #938
to_remove_cbsa=names(i0_cbsa[i0_cbsa>=891]) #95% zero values

final_cbsa_p_reduced=final_cbsa_p[,-which(names(final_cbsa_p) %in% to_remove_cbsa)]
save(final_cbsa_p_reduced,file="geo_final_cbsa_p_reduced.RData")



# Removing columns with 97% nulls or all zeros
lets_try=final_quarter_p[,4:ncol(final_quarter_p)]*100
i0=colSums(lets_try==0)
i_na=colSums(is.na(lets_try))
nrow(lets_try) #7870
to_remove=names(i0[i0>=7476]) #95% zero values

final_quarter_p_reduced=final_quarter_p[,-which(names(final_quarter_p) %in% to_remove)]
save(final_quarter_p_reduced,file="geo_final_quarter_p_reduced.RData")

#removed columns which varied little across rows
i_var=apply(lets_try,2,IQR)
to_remove2=names(i_var[i_var<=0.001])


#### CBSA level, split by quarter
# Q1
final_cbsa_Q1=final_quarter[which(final_quarter$quarter %in% c(201801,201901,202001)),2:ncol(final_quarter)]
final_cbsa_Q1=aggregate(.~CBSA_NAME, final_cbsa_Q1, sum)
save(final_cbsa_Q1,file="geo_final_cbsa_Q1.RData")

final_cbsa_Q1_percent=final_cbsa_Q1
next_column=ncol(final_cbsa_Q1_percent)+1
for(i in 3:ncol(final_cbsa_Q1_percent)){
  name=colnames(final_cbsa_Q1_percent)[i]
  final_cbsa_Q1_percent[,next_column]=final_cbsa_Q1_percent[,i]/final_cbsa_Q1_percent[,2]
  colnames(final_cbsa_Q1_percent)[next_column]=paste0(name,"_p")
  next_column=next_column+1
}
final_cbsa_Q1_p=final_cbsa_Q1_percent[,c(1,2,(ncol(final_cbsa_Q1)+1):ncol(final_cbsa_Q1_percent))]

lets_try_cbsa_q1=final_cbsa_Q1_p[,3:ncol(final_cbsa_Q1_p)]
i0_cbsa_q1=colSums(lets_try_cbsa_q1==0)
nrow(lets_try_cbsa_q1) #935
to_remove_cbsa_q1=names(i0_cbsa_q1[i0_cbsa_q1>=916]) #98% zero values

final_cbsa_Q1_p_reduced=final_cbsa_Q1_p[,-which(names(final_cbsa_Q1_p) %in% to_remove_cbsa_q1)]
save(final_cbsa_Q1_p_reduced,file="geo_final_cbsa_Q1_p_reduced.RData")

#Q2
final_cbsa_Q2=final_quarter[which(final_quarter$quarter %in% c(201802,201902,202002)),2:ncol(final_quarter)]
final_cbsa_Q2=aggregate(.~CBSA_NAME, final_cbsa_Q2, sum)
save(final_cbsa_Q2,file="geo_final_cbsa_Q2.RData")

final_cbsa_Q2_percent=final_cbsa_Q2
next_column=ncol(final_cbsa_Q2_percent)+1
for(i in 3:ncol(final_cbsa_Q2_percent)){
  name=colnames(final_cbsa_Q2_percent)[i]
  final_cbsa_Q2_percent[,next_column]=final_cbsa_Q2_percent[,i]/final_cbsa_Q2_percent[,2]
  colnames(final_cbsa_Q2_percent)[next_column]=paste0(name,"_p")
  next_column=next_column+1
}
final_cbsa_Q2_p=final_cbsa_Q2_percent[,c(1,2,(ncol(final_cbsa_Q2)+1):ncol(final_cbsa_Q2_percent))]

lets_try_cbsa_q2=final_cbsa_Q2_p[,3:ncol(final_cbsa_Q2_p)]
i0_cbsa_q2=colSums(lets_try_cbsa_q2==0)
nrow(lets_try_cbsa_q2) #930
to_remove_cbsa_q2=names(i0_cbsa_q2[i0_cbsa_q2>=916]) #98% zero values

final_cbsa_Q2_p_reduced=final_cbsa_Q2_p[,-which(names(final_cbsa_Q2_p) %in% to_remove_cbsa_q2)]
save(final_cbsa_Q2_p_reduced,file="geo_final_cbsa_Q2_p_reduced.RData")

#Q3
final_cbsa_Q3=final_quarter[which(final_quarter$quarter %in% c(201803,201903,202003)),2:ncol(final_quarter)]
final_cbsa_Q3=aggregate(.~CBSA_NAME, final_cbsa_Q3, sum)
save(final_cbsa_Q3,file="geo_final_cbsa_Q3.RData")

final_cbsa_Q3_percent=final_cbsa_Q3
next_column=ncol(final_cbsa_Q3_percent)+1
for(i in 3:ncol(final_cbsa_Q3_percent)){
  name=colnames(final_cbsa_Q3_percent)[i]
  final_cbsa_Q3_percent[,next_column]=final_cbsa_Q3_percent[,i]/final_cbsa_Q3_percent[,2]
  colnames(final_cbsa_Q3_percent)[next_column]=paste0(name,"_p")
  next_column=next_column+1
}
final_cbsa_Q3_p=final_cbsa_Q3_percent[,c(1,2,(ncol(final_cbsa_Q3)+1):ncol(final_cbsa_Q3_percent))]

lets_try_cbsa_q3=final_cbsa_Q3_p[,3:ncol(final_cbsa_Q3_p)]
i0_cbsa_q3=colSums(lets_try_cbsa_q3==0)
nrow(lets_try_cbsa_q3) #934
to_remove_cbsa_q3=names(i0_cbsa_q3[i0_cbsa_q3>=916]) #98% zero values

final_cbsa_Q3_p_reduced=final_cbsa_Q3_p[,-which(names(final_cbsa_Q3_p) %in% to_remove_cbsa_q3)]
save(final_cbsa_Q3_p_reduced,file="geo_final_cbsa_Q3_p_reduced.RData")


#Q4
final_cbsa_Q4=final_quarter[which(final_quarter$quarter %in% c(201804,201904,202004)),2:ncol(final_quarter)]
final_cbsa_Q4=aggregate(.~CBSA_NAME, final_cbsa_Q4, sum)
save(final_cbsa_Q4,file="geo_final_cbsa_Q4.RData")

final_cbsa_Q4_percent=final_cbsa_Q4
next_column=ncol(final_cbsa_Q4_percent)+1
for(i in 3:ncol(final_cbsa_Q4_percent)){
  name=colnames(final_cbsa_Q4_percent)[i]
  final_cbsa_Q4_percent[,next_column]=final_cbsa_Q4_percent[,i]/final_cbsa_Q4_percent[,2]
  colnames(final_cbsa_Q4_percent)[next_column]=paste0(name,"_p")
  next_column=next_column+1
}
final_cbsa_Q4_p=final_cbsa_Q4_percent[,c(1,2,(ncol(final_cbsa_Q4)+1):ncol(final_cbsa_Q4_percent))]

lets_try_cbsa_q4=final_cbsa_Q4_p[,3:ncol(final_cbsa_Q4_p)]
i0_cbsa_q4=colSums(lets_try_cbsa_q4==0)
nrow(lets_try_cbsa_q4) #935
to_remove_cbsa_q4=names(i0_cbsa_q4[i0_cbsa_q4>=916]) #98% zero values

final_cbsa_Q4_p_reduced=final_cbsa_Q4_p[,-which(names(final_cbsa_Q4_p) %in% to_remove_cbsa_q4)]
save(final_cbsa_Q4_p_reduced,file="geo_final_cbsa_Q4_p_reduced.RData")

#https://towardsdatascience.com/how-to-cluster-in-high-dimensions-4ef693bacc6
## Reduce Dimensions
#https://stats.stackexchange.com/questions/199501/user-segmentation-by-clustering-with-sparse-data
#########################################
#### Non-Negative Matrix Factorization ##
#########################################
#install.packages("NMF")
require("NMF")
nmf_data=final_quarter_p_reduced[,4:ncol(final_quarter_p_reduced)]
labels=paste(final_quarter_p_reduced$quarter,final_quarter_p_reduced$CBSA_NAME)
row.names(nmf_data)=labels

#Columns (W): Search Actions
#Rows (H): Geographies
#Questions to answer: group search actions (W) to then use as clustering
# group geos (H) and see what search actions are the top most relevent

nmf_data_transpose=t(nmf_data)
save(nmf_data,file="nmf_data.RData")
save(nmf_data_transpose,file="nmf_data_transpose.RData")

test_notime=nmf(nmf_data_transpose,method = 'brunet',rank=10)
test_time=nmf(nmf_data_transpose,method = 'brunet',rank=50)

saveRDS(test_notime,"nmf_10.rds")
saveRDS(test_time,"nmf_50.rds")

geos_reduced_10=test_notime@fit@W    #dim: 1068,10
row.names(geos_reduced_10)=rownames(nmf_data_transpose)
geos_reduced_10_extra=data.frame(actions=rownames(nmf_data_transpose),geos_reduced_10)
rownames(geos_reduced_10_extra)=c()
save(geos_reduced_10_extra,file="nmf-geos_reduced_10_extra.RData")

actions_reduced_10=test_notime@fit@H    #dim: 10,7870
save(geos_reduced_10,file="nmf-geos_reduced_10.RData")
save(actions_reduced_10,file="nmf-actions_reduced_10.RData")

geos_reduced_50=test_time@fit@W    #dim: 1068,50
row.names(geos_reduced_50)=rownames(nmf_data_transpose)
geos_reduced_50_extra=data.frame(actions=rownames(nmf_data_transpose),geos_reduced_50)
rownames(geos_reduced_50_extra)=c()

actions_reduced_50=test_time@fit@H    #dim: 50,7870
geos_reduced_50=as.data.frame(geos_reduced_50)
actions_reduced_50=as.data.frame(actions_reduced_50)
save(geos_reduced_50,file="nmf-geos_reduced_50.RData")
save(actions_reduced_50,file="nmf-actions_reduced_50.RData")

# transpose and save H matrix for additional k-means clustering experiment
nmf_ddr=t(actions_reduced_50)
nmf_ddr=data.frame(final_quarter_p_reduced[,c("quarter","CBSA_NAME")],nmf_ddr)
rownames(nmf_ddr)=c()
col_labels=paste0("C",1:50)
colnames(nmf_ddr)=c("quarter","CBSA_NAME",col_labels)
save(nmf_ddr,file="nmf_ddr.RData")

# invesitgate nmf groupings-10
actions_inside_geos=extractFeatures(test_notime,20)
actions_score_inside_geos=featureScore(test_notime)
rownames(nmf_data_transpose)[actions_inside_geos[[1]]]

# invesitgate nmf groupings-50
actions_inside_geos_2=extractFeatures(test_time,20)
rownames(nmf_data_transpose)[actions_inside_geos_2[[1]]]

#results
nmf_results=list()

nmf_results$Geo1=head(geos_reduced_10_extra[order(geos_reduced_10_extra$V1,decreasing = T),c("actions","V1")],n=20)
nmf_results$Geo2=head(geos_reduced_10_extra[order(geos_reduced_10_extra$V2,decreasing = T),c("actions","V2")],n=20)
nmf_results$Geo3=head(geos_reduced_10_extra[order(geos_reduced_10_extra$V3,decreasing = T),c("actions","V3")],n=20)
nmf_results$Geo4=head(geos_reduced_10_extra[order(geos_reduced_10_extra$V4,decreasing = T),c("actions","V4")],n=20)
nmf_results$Geo5=head(geos_reduced_10_extra[order(geos_reduced_10_extra$V5,decreasing = T),c("actions","V5")],n=20)
nmf_results$Geo6=head(geos_reduced_10_extra[order(geos_reduced_10_extra$V6,decreasing = T),c("actions","V6")],n=20)
nmf_results$Geo7=head(geos_reduced_10_extra[order(geos_reduced_10_extra$V7,decreasing = T),c("actions","V7")],n=20)
nmf_results$Geo8=head(geos_reduced_10_extra[order(geos_reduced_10_extra$V8,decreasing = T),c("actions","V8")],n=20)
nmf_results$Geo9=head(geos_reduced_10_extra[order(geos_reduced_10_extra$V9,decreasing = T),c("actions","V9")],n=20)
nmf_results$Geo10=head(geos_reduced_10_extra[order(geos_reduced_10_extra$V10,decreasing = T),c("actions","V10")],n=20)
head(nmf_results)

rlist::list.save(nmf_results,file="nmf_results_list.rdata")
saveRDS(nmf_results,file="nmf_results_list.rds")
save(nmf_results,file="nmf_results_list.RData")

#results for 50
nmf_results_50=list()
for(i in 1:50){
  nmf_results_50$new=head(geos_reduced_50_extra[order(geos_reduced_50_extra[,paste0("X",i)],decreasing = T),c("actions",paste0("X",i))],n=20)
  names(nmf_results_50)[i]=paste0("Geo",i)
  }
head(nmf_results_50)
rlist::list.save(nmf_results_50,file="nmf_results_50_list.rdata")
saveRDS(nmf_results_50,file="nmf_results_50_list.rds")

###########################################################
## Repeat NMF for Quarter split into separate datasets ####
###########################################################
nmf_data_Q1=final_cbsa_Q1_p_reduced[,3:ncol(final_cbsa_Q1_p_reduced)]
nmf_data_Q2=final_cbsa_Q2_p_reduced[,3:ncol(final_cbsa_Q2_p_reduced)]
nmf_data_Q3=final_cbsa_Q3_p_reduced[,3:ncol(final_cbsa_Q3_p_reduced)]
nmf_data_Q4=final_cbsa_Q4_p_reduced[,3:ncol(final_cbsa_Q4_p_reduced)]


# a=colSums(nmf_data_Q1==0)
# remove_a=names(a[a>=935]) #0
# 
# b=colSums(nmf_data_Q2==0)
# remove_b=names(b[b>=nrow(nmf_data_Q2)]) #6
# nmf_data_Q2=nmf_data_Q2[,-which(colnames(nmf_data_Q2) %in% remove_b)]
# 
# c=colSums(nmf_data_Q3==0)
# remove_c=names(c[c>=nrow(nmf_data_Q3)])  #5
# nmf_data_Q3=nmf_data_Q3[,-which(colnames(nmf_data_Q3) %in% remove_c)]
# 
# d=colSums(nmf_data_Q4==0)
# remove_d=names(d[d>=nrow(nmf_data_Q4)])  #2
# nmf_data_Q4=nmf_data_Q4[,-which(colnames(nmf_data_Q4) %in% remove_d)]


save(nmf_data_Q1,file="nmf_data_Q1.RData")
save(nmf_data_Q2,file="nmf_data_Q2.RData")
save(nmf_data_Q3,file="nmf_data_Q3.RData")
save(nmf_data_Q4,file="nmf_data_Q4.RData")

nmf_data_Q1_transpose=t(nmf_data_Q1)
save(nmf_data_Q1_transpose,file="nmf_data_Q1_transpose.RData")
nmf_data_Q2_transpose=t(nmf_data_Q2)
save(nmf_data_Q2_transpose,file="nmf_data_Q2_transpose.RData")
nmf_data_Q3_transpose=t(nmf_data_Q3)
save(nmf_data_Q3_transpose,file="nmf_data_Q3_transpose.RData")
nmf_data_Q4_transpose=t(nmf_data_Q4)
save(nmf_data_Q4_transpose,file="nmf_data_Q4_transpose.RData")

## models
nmf_model_Q1_50=nmf(nmf_data_Q1_transpose,method = 'brunet',rank=50)
saveRDS(nmf_model_Q1_50,"nmf_model_Q1_50.rds")
nmf_model_Q1_100=nmf(nmf_data_Q1_transpose,method = 'brunet',rank=100)
saveRDS(nmf_model_Q1_100,"nmf_model_Q1_100.rds")

nmf_model_Q2_50=nmf(nmf_data_Q2_transpose,method = 'brunet',rank=50)
saveRDS(nmf_model_Q2_50,"nmf_model_Q2_50.rds")
nmf_model_Q2_100=nmf(nmf_data_Q2_transpose,method = 'brunet',rank=100)
saveRDS(nmf_model_Q2_100,"nmf_model_Q2_100.rds")

nmf_model_Q3_50=nmf(nmf_data_Q3_transpose,method = 'brunet',rank=50)
saveRDS(nmf_model_Q3_50,"nmf_model_Q3_50.rds")
nmf_model_Q3_100=nmf(nmf_data_Q3_transpose,method = 'brunet',rank=100)
saveRDS(nmf_model_Q3_100,"nmf_model_Q3_100.rds")

nmf_model_Q4_50=nmf(nmf_data_Q4_transpose,method = 'brunet',rank=50)
saveRDS(nmf_model_Q4_50,"nmf_model_Q4_50.rds")
nmf_model_Q4_100=nmf(nmf_data_Q4_transpose,method = 'brunet',rank=100)
saveRDS(nmf_model_Q4_100,"nmf_model_Q4_100.rds")

#output datasets
#Q1
dim(nmf_data_Q1)
nmf_ddr_Q1_50=t(nmf_model_Q1_50@fit@H)
nmf_ddr_Q1_50=data.frame(final_cbsa_Q1_p_reduced$CBSA_NAME,nmf_ddr_Q1_50)
col_labels=paste0("C",1:50)
colnames(nmf_ddr_Q1_50)=c("CBSA_NAME",col_labels)
save(nmf_ddr_Q1_50,file="nmf_ddr_Q1_50.RData")

nmf_ddr_Q1_100=t(nmf_model_Q1_100@fit@H)
nmf_ddr_Q1_100=data.frame(final_cbsa_Q1_p_reduced$CBSA_NAME,nmf_ddr_Q1_100)
col_labels=paste0("C",1:100)
colnames(nmf_ddr_Q1_100)=c("CBSA_NAME",col_labels)
save(nmf_ddr_Q1_100,file="nmf_ddr_Q1_100.RData")
dim(nmf_ddr_Q1_100)
head(nmf_ddr_Q1_100[1:7])

#Q2
dim(nmf_data_Q2)
nmf_ddr_Q2_50=t(nmf_model_Q2_50@fit@H)
nmf_ddr_Q2_50=data.frame(final_cbsa_Q2_p_reduced$CBSA_NAME,nmf_ddr_Q2_50)
col_labels=paste0("C",1:50)
colnames(nmf_ddr_Q2_50)=c("CBSA_NAME",col_labels)
save(nmf_ddr_Q2_50,file="nmf_ddr_Q2_50.RData")
dim(nmf_ddr_Q2_50)
head(nmf_ddr_Q2_50[1:7])

dim(nmf_data_Q2)
nmf_ddr_Q2_100=t(nmf_model_Q2_100@fit@H)
nmf_ddr_Q2_100=data.frame(final_cbsa_Q2_p_reduced$CBSA_NAME,nmf_ddr_Q2_100)
col_labels=paste0("C",1:100)
colnames(nmf_ddr_Q2_100)=c("CBSA_NAME",col_labels)
save(nmf_ddr_Q2_100,file="nmf_ddr_Q2_100.RData")
dim(nmf_ddr_Q2_100)
head(nmf_ddr_Q2_100[1:7])

#Q3
dim(nmf_data_Q3)
nmf_ddr_Q3_50=t(nmf_model_Q3_50@fit@H)
nmf_ddr_Q3_50=data.frame(final_cbsa_Q3_p_reduced$CBSA_NAME,nmf_ddr_Q3_50)
col_labels=paste0("C",1:50)
colnames(nmf_ddr_Q3_50)=c("CBSA_NAME",col_labels)
save(nmf_ddr_Q3_50,file="nmf_ddr_Q3_50.RData")
dim(nmf_ddr_Q3_50)
head(nmf_ddr_Q3_50[1:7])

dim(nmf_data_Q3)
nmf_ddr_Q3_100=t(nmf_model_Q3_100@fit@H)
nmf_ddr_Q3_100=data.frame(final_cbsa_Q3_p_reduced$CBSA_NAME,nmf_ddr_Q3_100)
col_labels=paste0("C",1:100)
colnames(nmf_ddr_Q3_100)=c("CBSA_NAME",col_labels)
save(nmf_ddr_Q3_100,file="nmf_ddr_Q3_100.RData")
dim(nmf_ddr_Q3_100)
head(nmf_ddr_Q3_100[1:7])

#Q4
dim(nmf_data_Q4)
nmf_ddr_Q4_50=t(nmf_model_Q4_50@fit@H)
nmf_ddr_Q4_50=data.frame(final_cbsa_Q4_p_reduced$CBSA_NAME,nmf_ddr_Q4_50)
col_labels=paste0("C",1:50)
colnames(nmf_ddr_Q4_50)=c("CBSA_NAME",col_labels)
save(nmf_ddr_Q4_50,file="nmf_ddr_Q4_50.RData")
dim(nmf_ddr_Q4_50)
head(nmf_ddr_Q4_50[1:7])

dim(nmf_data_Q4)
nmf_ddr_Q4_100=t(nmf_model_Q4_100@fit@H)
nmf_ddr_Q4_100=data.frame(final_cbsa_Q4_p_reduced$CBSA_NAME,nmf_ddr_Q4_100)
col_labels=paste0("C",1:100)
colnames(nmf_ddr_Q4_100)=c("CBSA_NAME",col_labels)
save(nmf_ddr_Q4_100,file="nmf_ddr_Q4_100.RData")
dim(nmf_ddr_Q4_100)
head(nmf_ddr_Q4_100[1:7])

#https://www.centerspace.net/clustering-analysis-part-iv-non-negative-matrix-factorization
 ####################################################
####### Repeat NMF for CBSA level without time ######
#####################################################
load(final_cbsa_p_reduced)
require("NMF")
nmf_data_cbsa=final_cbsa_p_reduced[,3:ncol(final_cbsa_p_reduced)]
labels_cbsa=final_cbsa_p_reduced$CBSA_NAME
row.names(nmf_data_cbsa)=labels_cbsa

#Columns (W): Search Actions
#Rows (H): Geographies
#Questions to answer: group search actions (W) to then use as clustering
# group geos (H) and see what search actions are the top most relevent

#nmf_data_transpose=t(nmf_data)
save(nmf_data_cbsa,file="nmf_data_cbsa.RData")
#save(nmf_data_transpose,file="nmf_data_transpose.RData")

test_cbsa_50=nmf(nmf_data_cbsa,method = 'brunet',rank=50)
saveRDS(test_cbsa_50,"test_cbsa_50.rds")
test_cbsa_50_v2=nmf(nmf_data_cbsa,method = 'nsNMF',rank=50)
saveRDS(test_cbsa_50_v2,"test_cbsa_50_v2.rds")
test_cbsa_50_v3=nmf(nmf_data_cbsa,method = 'lee',rank=50)
saveRDS(test_cbsa_50_v3,"test_cbsa_50_v3.rds")
test_cbsa_100=nmf(nmf_data_cbsa,method = 'brunet',rank=100)
saveRDS(test_cbsa_100,"test_cbsa_100.rds")

test_cbsa_100_v2=nmf(nmf_data_cbsa,method = 'nsNMF',rank=100)
saveRDS(test_cbsa_100_v2,"test_cbsa_100_v2.rds")
test_cbsa_100_v3=nmf(nmf_data_cbsa,method = 'lee',rank=100)
saveRDS(test_cbsa_100_v3,"test_cbsa_100_v3.rds")

test_cbsa_30=nmf(nmf_data_cbsa,method = 'brunet',rank=30)
saveRDS(test_cbsa_30,"test_cbsa_30.rds")



# save W matrix for tsne & k-means clustering
#50, brunet
nmf_ddr_cbsa_50=test_cbsa_50@fit@W   #dim:938  50
nmf_ddr_cbsa_50=data.frame(final_cbsa_p_reduced$CBSA_NAME,nmf_ddr_cbsa_50)
col_labels=paste0("C",1:50)
colnames(nmf_ddr_cbsa_50)=c("CBSA_NAME",col_labels)
save(nmf_ddr_cbsa_50,file="nmf_ddr_cbsa_50.RData")

#50, nsNMF
nmf_ddr_cbsa_50_v2=test_cbsa_50_v2@fit@W   #dim:938  50
nmf_ddr_cbsa_50_v2=data.frame(final_cbsa_p_reduced$CBSA_NAME,nmf_ddr_cbsa_50_v2)
col_labels=paste0("C",1:50)
colnames(nmf_ddr_cbsa_50_v2)=c("CBSA_NAME",col_labels)
save(nmf_ddr_cbsa_50_v2,file="nmf_ddr_cbsa_50_v2.RData")

#50, lee
nmf_ddr_cbsa_50_v3=test_cbsa_50_v3@fit@W   #dim:938  50
nmf_ddr_cbsa_50_v3=data.frame(final_cbsa_p_reduced$CBSA_NAME,nmf_ddr_cbsa_50_v3)
col_labels=paste0("C",1:50)
colnames(nmf_ddr_cbsa_50_v3)=c("CBSA_NAME",col_labels)
save(nmf_ddr_cbsa_50_v3,file="nmf_ddr_cbsa_50_v3.RData")

#100, brunet
nmf_ddr_cbsa_100=test_cbsa_100@fit@W   #dim:938  50
nmf_ddr_cbsa_100=data.frame(final_cbsa_p_reduced$CBSA_NAME,nmf_ddr_cbsa_100)
col_labels=paste0("C",1:100)
colnames(nmf_ddr_cbsa_100)=c("CBSA_NAME",col_labels)
save(nmf_ddr_cbsa_100,file="nmf_ddr_cbsa_100.RData")

#30, brunet
nmf_ddr_cbsa_30=test_cbsa_30@fit@W   #dim:938  50
nmf_ddr_cbsa_30=data.frame(final_cbsa_p_reduced$CBSA_NAME,nmf_ddr_cbsa_30)
col_labels=paste0("C",1:30)
colnames(nmf_ddr_cbsa_30)=c("CBSA_NAME",col_labels)
save(nmf_ddr_cbsa_30,file="nmf_ddr_cbsa_30.RData")

##################
###### T-sne #####
##################
#https://www.r-bloggers.com/playing-with-dimensions-from-clustering-pca-t-sne-to-carl-sagan/
library("Rtsne")
tsne_data=final_cbsa_p_reduced[,3:ncol(final_cbsa_p_reduced)]
#labels_tsne=final_cbsa_p_reduced$CBSA_NAME
#row.names(tsne_data)=labels_tsne


##CBSA level, full columns==No pattern
perplexity=30
max_iter=8000
pca="FALSE"
data="tsne_data"

tsne=Rtsne(tsne_data,PCA=pca, verbose=FALSE,check_duplicates=FALSE,
              perplexity=perplexity,dims=2,max_iter=max_iter)
tsne=tsne$Y
##save(tsne_30_nmf30,file="tsne_30_nmf30.RData")
plot(tsne, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)


##CBSA+Quarter level, NMF reduced columns==distinct clusters, ~9-11
tsne_data=nmf_ddr[,3:ncol(nmf_ddr)]
perplexity=30
max_iter=8000
pca="FALSE"
data="tsne_data"

tsne=Rtsne(tsne_data,PCA=pca, verbose=FALSE,check_duplicates=FALSE,
           perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_30_nmf50_quarter=tsne$Y
save(tsne_30_nmf50_quarter,file="tsne_30_nmf50_quarter.RData")
plot(tsne_30_nmf50_quarter, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)


## CBSA level split into separate quarters, NMF reduced columns ####
## Q1 ## --- do 2,3,4 clusters
perplexity=30
max_iter=8000
pca="FALSE"
data="nmf_ddr_Q1_50"

tsne=Rtsne(nmf_ddr_Q1_50[,3:ncol(nmf_ddr_Q1_50)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
           perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_30_nmf50_Q1=tsne$Y
save(tsne_30_nmf50_Q1,file="tsne_30_nmf50_Q1.RData")
plot(tsne_30_nmf50_Q1, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)
        #### not distinct clusters

perplexity=30
max_iter=8000
pca="FALSE"
data="nmf_ddr_Q1_100"

tsne=Rtsne(nmf_ddr_Q1_100[,3:ncol(nmf_ddr_Q1_100)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
           perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_30_nmf100_Q1=tsne$Y
save(tsne_30_nmf100_Q1,file="tsne_30_nmf100_Q1.RData")
plot(tsne_30_nmf100_Q1, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)
      #### 1 big blob

## Q2 ## --- do 2-3 clusters
perplexity=30
max_iter=8000
pca="FALSE"
data="nmf_ddr_Q2_50"

tsne=Rtsne(nmf_ddr_Q2_50[,3:ncol(nmf_ddr_Q2_50)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
           perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_30_nmf50_Q2=tsne$Y
save(tsne_30_nmf50_Q2,file="tsne_30_nmf50_Q2.RData")
plot(tsne_30_nmf50_Q2, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)
    #### no distinct clusters

perplexity=30
max_iter=8000
pca="FALSE"
data="nmf_ddr_Q2_100"

tsne=Rtsne(nmf_ddr_Q2_100[,3:ncol(nmf_ddr_Q2_100)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
           perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_30_nmf100_Q2=tsne$Y
save(tsne_30_nmf100_Q2,file="tsne_30_nmf100_Q2.RData")
plot(tsne_30_nmf100_Q2, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)
      #### 1 big blob

## Q3 ## ---- do 2 clusters
perplexity=30
max_iter=8000
pca="FALSE"
data="nmf_ddr_Q3_50"

tsne=Rtsne(nmf_ddr_Q3_50[,3:ncol(nmf_ddr_Q3_50)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
           perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_30_nmf50_Q3=tsne$Y
save(tsne_30_nmf50_Q3,file="tsne_30_nmf50_Q3.RData")
plot(tsne_30_nmf50_Q3, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)
        #### not as distinct clusters; 2 blobs

perplexity=30
max_iter=8000
pca="FALSE"
data="nmf_ddr_Q3_100"

tsne=Rtsne(nmf_ddr_Q3_100[,3:ncol(nmf_ddr_Q3_100)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
           perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_30_nmf100_Q3=tsne$Y
save(tsne_30_nmf100_Q3,file="tsne_30_nmf100_Q3.RData")
plot(tsne_30_nmf100_Q3, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)
          #### 1 big blob

## Q4 ## ---try 2 clusters
perplexity=30
max_iter=8000
pca="FALSE"
data="nmf_ddr_Q4_50"

tsne=Rtsne(nmf_ddr_Q4_50[,3:ncol(nmf_ddr_Q4_50)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
           perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_30_nmf50_Q4=tsne$Y
save(tsne_30_nmf50_Q4,file="tsne_30_nmf50_Q4.RData")
plot(tsne_30_nmf50_Q4, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)
          ####not as distinct pattern; 2 blobs

perplexity=30
max_iter=8000
pca="FALSE"
data="nmf_ddr_Q4_100"

tsne=Rtsne(nmf_ddr_Q4_100[,3:ncol(nmf_ddr_Q4_100)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
           perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_30_nmf100_Q4=tsne$Y
save(tsne_30_nmf100_Q4,file="tsne_30_nmf100_Q4.RData")
plot(tsne_30_nmf100_Q4, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)
          ####no distinct pattern; 1 big blob



########### CBSA level, NMF done first #############
##Plots for 30
perplexity=30
max_iter=8000
pca="FALSE"
data="nmf_ddr_cbsa_30"

tsne_30=Rtsne(nmf_ddr_cbsa_30[,2:ncol(nmf_ddr_cbsa_30)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
                     perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_30_nmf30=tsne_30$Y
save(tsne_30_nmf30,file="tsne_30_nmf30.RData")
plot(tsne_30_nmf30, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)

perplexity=50
max_iter=8000
pca="FALSE"
data="nmf_ddr_cbsa_30"
tsne_50=Rtsne(nmf_ddr_cbsa_30[,2:ncol(nmf_ddr_cbsa_30)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
               perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_50_nmf30=tsne_50$Y
save(tsne_50_nmf30,file="tsne_50_nmf30.RData")
plot(tsne_50_nmf30, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)


perplexity=100
max_iter=50000
pca="FALSE"
data="nmf_ddr_cbsa_30"
tsne_100=Rtsne(nmf_ddr_cbsa_30[,2:ncol(nmf_ddr_cbsa_30)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
               perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_100_nmf30=tsne_100$Y
save(tsne_100_nmf30,file="tsne_100_nmf30.RData")
plot(tsne_100_nmf30, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)

##########
##Plots for 100
perplexity=30
max_iter=8000
pca="FALSE"
data="nmf_ddr_cbsa_100"

tsne_30=Rtsne(nmf_ddr_cbsa_100[,2:ncol(nmf_ddr_cbsa_100)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
              perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_30_nmf100=tsne_30$Y
save(tsne_30_nmf100,file="tsne_30_nmf100.RData")
plot(tsne_30_nmf100, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)

perplexity=50
max_iter=8000
pca="FALSE"
data="nmf_ddr_cbsa_100"
tsne_50=Rtsne(nmf_ddr_cbsa_100[,2:ncol(nmf_ddr_cbsa_100)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
              perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_50_nmf100=tsne_50$Y
save(tsne_50_nmf100,file="tsne_50_nmf100.RData")
plot(tsne_50_nmf100, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)


perplexity=100
max_iter=50000
pca="FALSE"
data="nmf_ddr_cbsa_100"
tsne_100=Rtsne(nmf_ddr_cbsa_100[,2:ncol(nmf_ddr_cbsa_100)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
               perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_100_nmf100=tsne_100$Y
save(tsne_100_nmf100,file="tsne_100_nmf100.RData")
plot(tsne_100_nmf100, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)


##########
##Plots for 50
perplexity=30
max_iter=8000
pca="FALSE"
data="nmf_ddr_cbsa_50"

tsne_30=Rtsne(nmf_ddr_cbsa_50[,2:ncol(nmf_ddr_cbsa_50)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
              perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_30_nmf50=tsne_30$Y
save(tsne_30_nmf50,file="tsne_30_nmf50.RData")
plot(tsne_30_nmf50, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)

perplexity=50
max_iter=8000
pca="FALSE"
data="nmf_ddr_cbsa_50"
tsne_50=Rtsne(nmf_ddr_cbsa_50[,2:ncol(nmf_ddr_cbsa_50)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
              perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_50_nmf50=tsne_50$Y
save(tsne_50_nmf50,file="tsne_50_nmf50.RData")
plot(tsne_50_nmf50, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)


perplexity=100
max_iter=50000
pca="FALSE"
data="nmf_ddr_cbsa_50"
tsne_100=Rtsne(nmf_ddr_cbsa_50[,2:ncol(nmf_ddr_cbsa_50)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
               perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_100_nmf50=tsne_100$Y
save(tsne_100_nmf50,file="tsne_100_nmf50.RData")
plot(tsne_100_nmf50, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)


##########
##Plots for 50_v2
perplexity=30
max_iter=8000
pca="FALSE"
data="nmf_ddr_cbsa_50_v2"

tsne_30=Rtsne(nmf_ddr_cbsa_50_v2[,2:ncol(nmf_ddr_cbsa_50_v2)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
              perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_30_nmf50v2=tsne_30$Y
save(tsne_30_nmf50v2,file="tsne_30_nmf50v2.RData")
plot(tsne_30_nmf50v2, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)

perplexity=50
max_iter=8000
pca="FALSE"
data="nmf_ddr_cbsa_50_v2"
tsne_50=Rtsne(nmf_ddr_cbsa_50_v2[,2:ncol(nmf_ddr_cbsa_50_v2)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
              perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_50_nmf50v2=tsne_50$Y
save(tsne_50_nmf50v2,file="tsne_50_nmf50v2.RData")
plot(tsne_50_nmf50v2, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)


perplexity=100
max_iter=50000
pca="FALSE"
data="nmf_ddr_cbsa_50_v2"
tsne_100=Rtsne(nmf_ddr_cbsa_50_v2[,2:ncol(nmf_ddr_cbsa_50_v2)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
               perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_100_nmf50v2=tsne_100$Y
save(tsne_100_nmf50v2,file="tsne_100_nmf50v2.RData")
plot(tsne_100_nmf50v2, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)

##########
##Plots for 50_v3
perplexity=30
max_iter=8000
pca="FALSE"
data="nmf_ddr_cbsa_50_v3"

tsne_30=Rtsne(nmf_ddr_cbsa_50_v3[,2:ncol(nmf_ddr_cbsa_50_v3)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
              perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_30_nmf50v3=tsne_30$Y
save(tsne_30_nmf50v3,file="tsne_30_nmf50v3.RData")
plot(tsne_30_nmf50v3, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)

perplexity=50
max_iter=8000
pca="FALSE"
data="nmf_ddr_cbsa_50_v3"
tsne_50=Rtsne(nmf_ddr_cbsa_50_v3[,2:ncol(nmf_ddr_cbsa_50_v3)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
              perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_50_nmf50v3=tsne_50$Y
save(tsne_50_nmf50v3,file="tsne_50_nmf50v3.RData")
plot(tsne_50_nmf50v3, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)


perplexity=100
max_iter=50000
pca="FALSE"
data="nmf_ddr_cbsa_50_v3"
tsne_100=Rtsne(nmf_ddr_cbsa_50_v3[,2:ncol(nmf_ddr_cbsa_50_v3)],PCA=pca, verbose=FALSE,check_duplicates=FALSE,
               perplexity=perplexity,dims=2,max_iter=max_iter)
tsne_100_nmf50v3=tsne_100$Y
save(tsne_100_nmf50v3,file="tsne_100_nmf50v3.RData")
plot(tsne_100_nmf50v3, main=paste("tSNE",data,perplexity,max_iter,sep = ","), xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)

##########
## Run these different tsne versions, then K-means to get clusters
require("ggplot2")
plot_cluster=function(data, var_cluster, palette)  
{
  ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
    geom_point(size=0.25) +
    guides(colour=guide_legend(override.aes=list(size=6))) +
    xlab("") + ylab("") +
    ggtitle("") +
    theme_light(base_size=20) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.box = "horizontal") + 
    scale_colour_brewer(palette = palette) 
}


###################################
########## K-Means ################
###################################
#CBSA level
#all time
#nmf_ddr_cbsa_100, p=30, iter=8000
load("~/Projects/Research/Search Geo/5_tSNE Output Data/tsne_30_nmf100.RData")
centers=4 #try centers 2 and 4
kmeans_cbsa=kmeans(as.data.frame(tsne_30_nmf100), centers)
save(kmeans_cbsa,file="kmeans_cbsa.RData")
#tsne_30_nmf100=as.data.frame(tsne_30_nmf100)
#tsne_30_nmf100$CBSA_NAME = nmf_ddr_cbsa_100$CBSA_NAME
tsne_30_nmf100$kmeans_c4 = factor(kmeans_cbsa$cluster)
plot_cluster(tsne_30_nmf100, "kmeans", "Set3")
cluster_tsne_30_nmf100=tsne_30_nmf100
cluster_tsne_30_nmf100=merge(cluster_tsne_30_nmf100,Geo_city_to_district_mapping,by="CBSA_NAME",all.x=T)
save(cluster_tsne_30_nmf100,file="cluster_tsne_30_nmf100.RData")


#https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/
##########################################################
#Visualize K-Means outside of t_SNE and calculate metrics
##########################################################
#CBSA level
#all time
#nmf_ddr_cbsa_100, p=30, iter=8000
#load("~/Projects/Research/Search Geo/6_KMeans Output/cluster_tsne_30_nmf100.RData")
require("factoextra")
fviz_cluster(kmeans_cbsa,as.data.frame(tsne_30_nmf100))

# try kmeans on nmf100 data
centers=4 #try centers 2 and 4
kmeans_cbsa_v2=kmeans(nmf_ddr_cbsa_100[,2:ncol(nmf_ddr_cbsa_100)], centers)
fviz_cluster(kmeans_cbsa_v2,nmf_ddr_cbsa_100[,2:ncol(nmf_ddr_cbsa_100)])


#totss: The total sum of squares (TSS), i.e ???(xi???x¯)2. TSS measures the total variance in the data.
#withinss: Vector of within-cluster sum of squares, one component per cluster
#tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss)
#betweenss: The between-cluster sum of squares, i.e. totss???tot.withinss
#size: The number of observations in each cluster




#CBSA+quarter level
#tsne, p=30, iter=8000
centers=11   #try centers 9, 10, 11
#tsne_30_nmf50_quarter=as.data.frame(tsne_30_nmf50_quarter)
kmeans_time=kmeans(tsne_30_nmf50_quarter, centers)  
tsne_30_nmf50_quarter$kmeans_c11 = factor(kmeans_time$cluster)
plot_cluster(tsne_30_nmf50_quarter[,c(1,2,5)], "kmeans_c11", "Set3")
cluster_tsne_30_nmf50_quarter=tsne_30_nmf50_quarter
cluster_tsne_30_nmf50_quarter$CBSA_NAME=nmf_ddr$CBSA_NAME
cluster_tsne_30_nmf50_quarter$quarter=nmf_ddr$quarter
cluster_tsne_30_nmf50_quarter=merge(cluster_tsne_30_nmf50_quarter,Geo_city_to_district_mapping,by="CBSA_NAME",all.x=T)
save(cluster_tsne_30_nmf50_quarter,file="cluster_tsne_30_nmf50_quarter.RData")

## CBSA level, as separate quarter datasets
#Q1
#nmf_ddr_Q1_50, p=30, iter=8000
centers=4 #try centers 2,3,4
#tsne_30_nmf50_Q1=as.data.frame(tsne_30_nmf50_Q1)
kmeans_q1=kmeans(tsne_30_nmf50_Q1, centers)  
tsne_30_nmf50_Q1$kmeans_c4 = factor(kmeans_q1$cluster)
#plot_cluster(tsne_30_nmf50_Q1, "kmeans_c2", "Set3")

cluster_tsne_30_nmf50_Q1=tsne_30_nmf50_Q1
cluster_tsne_30_nmf50_Q1$CBSA_NAME=nmf_ddr_Q1_50$CBSA_NAME
cluster_tsne_30_nmf50_Q1=merge(cluster_tsne_30_nmf50_Q1,Geo_city_to_district_mapping,by="CBSA_NAME",all.x=T)
save(cluster_tsne_30_nmf50_Q1,file="cluster_tsne_30_nmf50_Q1.RData")
write.csv(cluster_tsne_30_nmf50_Q1,file="cluster_tsne_30_nmf50_Q1.csv")

#Q2
#nmf_ddr_Q2_50, p=30, iter=8000
centers=3 #try centers 2 and 3
#tsne_30_nmf50_Q2=as.data.frame(tsne_30_nmf50_Q2)
kmeans_q2=kmeans(tsne_30_nmf50_Q2, centers)  
tsne_30_nmf50_Q2$kmeans_c3 = factor(kmeans_q2$cluster)
#plot_cluster(tsne_30_nmf50_Q2, "kmeans_c6", "Set3")

cluster_tsne_30_nmf50_Q2=tsne_30_nmf50_Q2
cluster_tsne_30_nmf50_Q2$CBSA_NAME=nmf_ddr_Q2_50$CBSA_NAME
cluster_tsne_30_nmf50_Q2=merge(cluster_tsne_30_nmf50_Q2,Geo_city_to_district_mapping,by="CBSA_NAME",all.x=T)
save(cluster_tsne_30_nmf50_Q2,file="cluster_tsne_30_nmf50_Q2.RData")
write.csv(cluster_tsne_30_nmf50_Q2,file="cluster_tsne_30_nmf50_Q2.csv")

#Q3
#nmf_ddr_Q3_50, p=30, iter=8000
centers=2 #try centers 2
tsne_30_nmf50_Q3=as.data.frame(tsne_30_nmf50_Q3)
kmeans_q3=kmeans(tsne_30_nmf50_Q3, centers)  
tsne_30_nmf50_Q3$kmeans_c2 = factor(kmeans_q3$cluster)
#plot_cluster(tsne_30_nmf50_Q3, "kmeans_c7", "Set3")

cluster_tsne_30_nmf50_Q3=tsne_30_nmf50_Q3
cluster_tsne_30_nmf50_Q3$CBSA_NAME=nmf_ddr_Q3_50$CBSA_NAME
cluster_tsne_30_nmf50_Q3=merge(cluster_tsne_30_nmf50_Q3,Geo_city_to_district_mapping,by="CBSA_NAME",all.x=T)
save(cluster_tsne_30_nmf50_Q3,file="cluster_tsne_30_nmf50_Q3.RData")
write.csv(cluster_tsne_30_nmf50_Q3,file="cluster_tsne_30_nmf50_Q3.csv")

#Q4
#nmf_ddr_Q4_50, p=30, iter=8000
centers=3 #try centers 2
#tsne_30_nmf50_Q4=as.data.frame(tsne_30_nmf50_Q4)
kmeans_q4=kmeans(tsne_30_nmf50_Q4,centers)
tsne_30_nmf50_Q4$kmeans_c3 = factor(kmeans_q4$cluster)
#plot_cluster(tsne_30_nmf50_Q4, "kmeans_c4", "Set3")

cluster_tsne_30_nmf50_Q4=tsne_30_nmf50_Q4
cluster_tsne_30_nmf50_Q4$CBSA_NAME=nmf_ddr_Q4_50$CBSA_NAME
cluster_tsne_30_nmf50_Q4=merge(cluster_tsne_30_nmf50_Q4,Geo_city_to_district_mapping,by="CBSA_NAME",all.x=T)
save(cluster_tsne_30_nmf50_Q4,file="cluster_tsne_30_nmf50_Q4.RData")
write.csv(cluster_tsne_30_nmf50_Q4,file="cluster_tsne_30_nmf50_Q4.csv")

#****************************************************************
###############################################
#### Investigate Clusters to determine WHY ###
##############################################

################All Time
#data: cluster_tsne_30_nmf100; 2 or 4 clusters
#nmf data: nmf_ddr_cbsa_100
#nmf model: test_cbsa_100
#orig data: nmf_data_cbsa=final_cbsa_p_reduced

investigate_alltime=merge(cluster_tsne_30_nmf100[,c("CBSA_NAME","kmeans_c4")],final_cbsa_p_reduced, by="CBSA_NAME", all.x=TRUE)
write.csv(investigate_alltime,"investigate_alltime.csv")

alltime_2=nmf(final_cbsa_p_reduced[,3:ncol(final_cbsa_p_reduced)],method = 'brunet',rank=2)
alltime_4=nmf(final_cbsa_p_reduced[,3:ncol(final_cbsa_p_reduced)],method = 'brunet',rank=4)

saveRDS(alltime_2,"nmf_alltime_2.rds")
saveRDS(alltime_4,"nmf_alltime_4.rds")

colsReduced=alltime_2@fit@W    #dim: 938,2
rowsReduced=alltime_2@fit@H    #dim: 2,1138
colsReduced=data.frame(CBSA_NAME=final_cbsa_p_reduced$CBSA_NAME,colsReduced)

#results
## Summary for 2 dim:
nmf_alltime_2_results=list()
# Most representative behaviors in each cluster
nmf_alltime_2_results$representative_behavior1=head(rowsReduced[1,order(rowsReduced[1,],decreasing = T)],n=20)
nmf_alltime_2_results$representative_behavior2=head(rowsReduced[2,order(rowsReduced[2,],decreasing = T)],n=20)

# group1 is basically all Hiking and Camping
# group2 is basically everything else with an emphasis on Apparel
#
# Most representative geos in each cluster
nmf_alltime_2_results$representative_geos1=head(colsReduced[order(colsReduced$X1,decreasing = T),c("CBSA_NAME","X1")],n=20)
nmf_alltime_2_results$representative_geos2=head(colsReduced[order(colsReduced$X2,decreasing = T),c("CBSA_NAME","X2")],n=20)

rlist::list.save(nmf_alltime_2_results,file="nmf_alltime_2_results.rdata")
saveRDS(nmf_alltime_2_results,file="nmf_alltime_2_results.rds")
save(nmf_alltime_2_results,file="nmf_alltime_2_results.RData")

## Summary for 4 dim:
colsReduced=alltime_4@fit@W    #dim: 938,2
rowsReduced=alltime_4@fit@H    #dim: 2,1138
colsReduced=data.frame(CBSA_NAME=final_cbsa_p_reduced$CBSA_NAME,colsReduced)

nmf_alltime_4_results=list()
# Most representative behaviors in each cluster
nmf_alltime_4_results$representative_behavior1=head(rowsReduced[1,order(rowsReduced[1,],decreasing = T)],n=100)
nmf_alltime_4_results$representative_behavior2=head(rowsReduced[2,order(rowsReduced[2,],decreasing = T)],n=100)
nmf_alltime_4_results$representative_behavior3=head(rowsReduced[3,order(rowsReduced[3,],decreasing = T)],n=100)
nmf_alltime_4_results$representative_behavior4=head(rowsReduced[4,order(rowsReduced[4,],decreasing = T)],n=100)


# group1 is Backpacking and Camping, kayaking, and apparel
# group2 is Backpacking and Camping, Hiking, Biking
# group3 is Apparel, some Backpacking and Camping, deals more important
# group4 is Backpacking and Camping, and Snowsports
#
# Most representative geos in each cluster
nmf_alltime_4_results$representative_geos1=head(colsReduced[order(colsReduced$X1,decreasing = T),c("CBSA_NAME","X1")],n=20)
nmf_alltime_4_results$representative_geos2=head(colsReduced[order(colsReduced$X2,decreasing = T),c("CBSA_NAME","X2")],n=20)
nmf_alltime_4_results$representative_geos3=head(colsReduced[order(colsReduced$X3,decreasing = T),c("CBSA_NAME","X3")],n=20)
nmf_alltime_4_results$representative_geos4=head(colsReduced[order(colsReduced$X4,decreasing = T),c("CBSA_NAME","X4")],n=20)

rlist::list.save(nmf_alltime_4_results,file="nmf_alltime_4_results.rdata")
saveRDS(nmf_alltime_4_results,file="nmf_alltime_4_results.rds")
save(nmf_alltime_4_results,file="nmf_alltime_4_results.RData")


############################Q1
#data: cluster_tsne_30_nmf50_Q1; 2,3,4 clusters
#nmf data: nmf_ddr_Q1_50
#nmf model: nmf_model_Q1_50
#orig data: final_cbsa_Q1_p_reduced

Q1time_2=nmf(final_cbsa_Q1_p_reduced[,3:ncol(final_cbsa_Q1_p_reduced)],method = 'brunet',rank=2)
Q1time_3=nmf(final_cbsa_Q1_p_reduced[,3:ncol(final_cbsa_Q1_p_reduced)],method = 'brunet',rank=3)
Q1time_4=nmf(final_cbsa_Q1_p_reduced[,3:ncol(final_cbsa_Q1_p_reduced)],method = 'brunet',rank=4)

saveRDS(Q1time_2,"nmf_Q1time_2.rds")
saveRDS(Q1time_3,"nmf_Q1time_3.rds")
saveRDS(Q1time_4,"nmf_Q1time_4.rds")

#results
## Summary for 2 dim:
colsReduced_q1_2=Q1time_2@fit@W    
rowsReduced_q1_2=Q1time_2@fit@H    
colsReduced_q1_2=data.frame(CBSA_NAME=final_cbsa_Q1_p_reduced$CBSA_NAME,colsReduced_q1_2)

nmf_Q1time_results=list()
# Most representative behaviors in each cluster
nmf_Q1time_results$nmf_Q1time_2_results=list()
nmf_Q1time_results$nmf_Q1time_2_results$representative_behavior1=head(rowsReduced_q1_2[1,order(rowsReduced_q1_2[1,],decreasing = T)],n=20)
nmf_Q1time_results$nmf_Q1time_2_results$representative_behavior2=head(rowsReduced_q1_2[2,order(rowsReduced_q1_2[2,],decreasing = T)],n=20)

# group1 is Backpacking and Camping, and Apparel
# group2 is Women's Clothing, Snowsports
#
# Most representative geos in each cluster
nmf_Q1time_results$nmf_Q1time_2_results$representative_geos1=head(colsReduced_q1_2[order(colsReduced_q1_2$X1,decreasing = T),c("CBSA_NAME","X1")],n=20)
nmf_Q1time_results$nmf_Q1time_2_results$representative_geos2=head(colsReduced_q1_2[order(colsReduced_q1_2$X2,decreasing = T),c("CBSA_NAME","X2")],n=20)


##Summary for 3 dim:
colsReduced_q1_3=Q1time_3@fit@W    
rowsReduced_q1_3=Q1time_3@fit@H    
colsReduced_q1_3=data.frame(CBSA_NAME=final_cbsa_Q1_p_reduced$CBSA_NAME,colsReduced_q1_3)

nmf_Q1time_results$nmf_Q1time_3_results=list()
nmf_Q1time_results$nmf_Q1time_3_results$representative_behavior1=head(rowsReduced_q1_3[1,order(rowsReduced_q1_3[1,],decreasing = T)],n=20)
nmf_Q1time_results$nmf_Q1time_3_results$representative_behavior2=head(rowsReduced_q1_3[2,order(rowsReduced_q1_3[2,],decreasing = T)],n=20)
nmf_Q1time_results$nmf_Q1time_3_results$representative_behavior3=head(rowsReduced_q1_3[3,order(rowsReduced_q1_3[3,],decreasing = T)],n=20)

# group1 is Snowsports
# group2 is mostly Apparel
# group3 is Backpacking and Camping, Hiking
#
# Most representative geos in each cluster
nmf_Q1time_results$nmf_Q1time_3_results$representative_geos1=head(colsReduced_q1_3[order(colsReduced_q1_3$X1,decreasing = T),c("CBSA_NAME","X1")],n=20)
nmf_Q1time_results$nmf_Q1time_3_results$representative_geos2=head(colsReduced_q1_3[order(colsReduced_q1_3$X2,decreasing = T),c("CBSA_NAME","X2")],n=20)
nmf_Q1time_results$nmf_Q1time_3_results$representative_geos3=head(colsReduced_q1_3[order(colsReduced_q1_3$X3,decreasing = T),c("CBSA_NAME","X3")],n=20)


##Summary for 4 dim:
colsReduced_q1_4=Q1time_4@fit@W    
rowsReduced_q1_4=Q1time_4@fit@H    
colsReduced_q1_4=data.frame(CBSA_NAME=final_cbsa_Q1_p_reduced$CBSA_NAME,colsReduced_q1_4)

nmf_Q1time_results$nmf_Q1time_4_results=list()
nmf_Q1time_results$nmf_Q1time_4_results$representative_behavior1=head(rowsReduced_q1_4[1,order(rowsReduced_q1_4[1,],decreasing = T)],n=20)
nmf_Q1time_results$nmf_Q1time_4_results$representative_behavior2=head(rowsReduced_q1_4[2,order(rowsReduced_q1_4[2,],decreasing = T)],n=20)
nmf_Q1time_results$nmf_Q1time_4_results$representative_behavior3=head(rowsReduced_q1_4[3,order(rowsReduced_q1_4[3,],decreasing = T)],n=20)
nmf_Q1time_results$nmf_Q1time_4_results$representative_behavior4=head(rowsReduced_q1_4[4,order(rowsReduced_q1_4[4,],decreasing = T)],n=20)

# group1 is mostly Apparel
# group2 is apparel, deals, some Backpacking and Camping
# group3 is Backpacking and Camping, Hiking
# group4 is Snowsports
#
# Most representative geos in each cluster
nmf_Q1time_results$nmf_Q1time_4_results$representative_geos1=head(colsReduced_q1_4[order(colsReduced_q1_4$X1,decreasing = T),c("CBSA_NAME","X1")],n=20)
nmf_Q1time_results$nmf_Q1time_4_results$representative_geos2=head(colsReduced_q1_4[order(colsReduced_q1_4$X2,decreasing = T),c("CBSA_NAME","X2")],n=20)
nmf_Q1time_results$nmf_Q1time_4_results$representative_geos3=head(colsReduced_q1_4[order(colsReduced_q1_4$X3,decreasing = T),c("CBSA_NAME","X3")],n=20)
nmf_Q1time_results$nmf_Q1time_4_results$representative_geos4=head(colsReduced_q1_4[order(colsReduced_q1_4$X4,decreasing = T),c("CBSA_NAME","X4")],n=20)

rlist::list.save(nmf_Q1time_results,file="nmf_Q1time_results.rdata")
saveRDS(nmf_Q1time_results,file="nmf_Q1time_results.rds")
save(nmf_Q1time_results,file="nmf_Q1time_results.RData")


############################ Q2
#data: cluster_tsne_30_nmf50_Q2; 2,3 clusters
#nmf data: nmf_ddr_Q2_50
#nmf model: nmf_model_Q2_50
#orig data: final_cbsa_Q2_p_reduced

Q2time_2=nmf(final_cbsa_Q2_p_reduced[,3:ncol(final_cbsa_Q2_p_reduced)],method = 'brunet',rank=2)
Q2time_3=nmf(final_cbsa_Q2_p_reduced[,3:ncol(final_cbsa_Q2_p_reduced)],method = 'brunet',rank=3)
Q2time_4=nmf(final_cbsa_Q2_p_reduced[,3:ncol(final_cbsa_Q2_p_reduced)],method = 'brunet',rank=4)

saveRDS(Q2time_2,"nmf_Q2time_2.rds")
saveRDS(Q2time_3,"nmf_Q2time_3.rds")
saveRDS(Q2time_4,"nmf_Q2time_4.rds")

#results
## Summary for 2 dim:
colsReduced_q2_2=Q2time_2@fit@W    
rowsReduced_q2_2=Q2time_2@fit@H    
colsReduced_q2_2=data.frame(CBSA_NAME=final_cbsa_Q2_p_reduced$CBSA_NAME,colsReduced_q2_2)

nmf_Q2time_results=list()
# Most representative behaviors in each cluster
nmf_Q2time_results$nmf_Q2time_2_results=list()
nmf_Q2time_results$nmf_Q2time_2_results$representative_behavior1=head(rowsReduced_q2_2[1,order(rowsReduced_q2_2[1,],decreasing = T)],n=20)
nmf_Q2time_results$nmf_Q2time_2_results$representative_behavior2=head(rowsReduced_q2_2[2,order(rowsReduced_q2_2[2,],decreasing = T)],n=20)

# group1 is Backpacking and Camping, Hiking, Men's Clothes
# group2 is Backpacking and Camping, Hiking, Women's Clothes
#
# Most representative geos in each cluster
nmf_Q2time_results$nmf_Q2time_2_results$representative_geos1=head(colsReduced_q2_2[order(colsReduced_q2_2$X1,decreasing = T),c("CBSA_NAME","X1")],n=20)
nmf_Q2time_results$nmf_Q2time_2_results$representative_geos2=head(colsReduced_q2_2[order(colsReduced_q2_2$X2,decreasing = T),c("CBSA_NAME","X2")],n=20)


##Summary for 3 dim:
colsReduced_q2_3=Q2time_3@fit@W    
rowsReduced_q2_3=Q2time_3@fit@H    
colsReduced_q2_3=data.frame(CBSA_NAME=final_cbsa_Q2_p_reduced$CBSA_NAME,colsReduced_q2_3)

nmf_Q2time_results$nmf_Q2time_3_results=list()
nmf_Q2time_results$nmf_Q2time_3_results$representative_behavior1=head(rowsReduced_q2_3[1,order(rowsReduced_q2_3[1,],decreasing = T)],n=20)
nmf_Q2time_results$nmf_Q2time_3_results$representative_behavior2=head(rowsReduced_q2_3[2,order(rowsReduced_q2_3[2,],decreasing = T)],n=20)
nmf_Q2time_results$nmf_Q2time_3_results$representative_behavior3=head(rowsReduced_q2_3[3,order(rowsReduced_q2_3[3,],decreasing = T)],n=20)

# group1 is Backpacking & Camping & Hiking, Kayaking, some Men's apparel
# group2 is Sportswear, Cycling, Camping & Hiking, some apparel
# group3 is Camping & Hiking, some apparel, biking
#
# Most representative geos in each cluster
nmf_Q2time_results$nmf_Q2time_3_results$representative_geos1=head(colsReduced_q2_3[order(colsReduced_q2_3$X1,decreasing = T),c("CBSA_NAME","X1")],n=20)
nmf_Q2time_results$nmf_Q2time_3_results$representative_geos2=head(colsReduced_q2_3[order(colsReduced_q2_3$X2,decreasing = T),c("CBSA_NAME","X2")],n=20)
nmf_Q2time_results$nmf_Q2time_3_results$representative_geos3=head(colsReduced_q2_3[order(colsReduced_q2_3$X3,decreasing = T),c("CBSA_NAME","X3")],n=20)

##Summary for 4 dim:
colsReduced_q2_4=Q2time_4@fit@W    
rowsReduced_q2_4=Q2time_4@fit@H    
colsReduced_q2_4=data.frame(CBSA_NAME=final_cbsa_Q2_p_reduced$CBSA_NAME,colsReduced_q2_4)

nmf_Q2time_results$nmf_Q2time_4_results=list()
nmf_Q2time_results$nmf_Q2time_4_results$representative_behavior1=head(rowsReduced_q2_4[1,order(rowsReduced_q2_4[1,],decreasing = T)],n=20)
nmf_Q2time_results$nmf_Q2time_4_results$representative_behavior2=head(rowsReduced_q2_4[2,order(rowsReduced_q2_4[2,],decreasing = T)],n=20)
nmf_Q2time_results$nmf_Q2time_4_results$representative_behavior3=head(rowsReduced_q2_4[3,order(rowsReduced_q2_4[3,],decreasing = T)],n=20)
nmf_Q2time_results$nmf_Q2time_4_results$representative_behavior4=head(rowsReduced_q2_4[4,order(rowsReduced_q2_4[4,],decreasing = T)],n=20)

# group1 is sportsear, clothing, Camping & Hiking & Backpacking, cycling
# group2 is Camping & Hiking & Backpacking, kayaking, some apparel
# group3 is Cycling
# group4 is Camping & Hiking & Backpacking, kayaking, some apparel
#
# Most representative geos in each cluster
nmf_Q2time_results$nmf_Q2time_4_results$representative_geos1=head(colsReduced_q2_4[order(colsReduced_q2_4$X1,decreasing = T),c("CBSA_NAME","X1")],n=20)
nmf_Q2time_results$nmf_Q2time_4_results$representative_geos2=head(colsReduced_q2_4[order(colsReduced_q2_4$X2,decreasing = T),c("CBSA_NAME","X2")],n=20)
nmf_Q2time_results$nmf_Q2time_4_results$representative_geos3=head(colsReduced_q2_4[order(colsReduced_q2_4$X3,decreasing = T),c("CBSA_NAME","X3")],n=20)
nmf_Q2time_results$nmf_Q2time_4_results$representative_geos4=head(colsReduced_q2_4[order(colsReduced_q2_4$X4,decreasing = T),c("CBSA_NAME","X4")],n=20)

rlist::list.save(nmf_Q2time_results,file="nmf_Q2time_results.rdata")
saveRDS(nmf_Q2time_results,file="nmf_Q2time_results.rds")
save(nmf_Q2time_results,file="nmf_Q2time_results.RData")

########## Q3
#data: cluster_tsne_30_nmf50_Q3; 2,3 clusters
#nmf data: nmf_ddr_Q3_50
#nmf model: nmf_model_Q3_50
#orig data: final_cbsa_Q3_p_reduced

Q3time_2=nmf(final_cbsa_Q3_p_reduced[,3:ncol(final_cbsa_Q3_p_reduced)],method = 'brunet',rank=2)
Q3time_3=nmf(final_cbsa_Q3_p_reduced[,3:ncol(final_cbsa_Q3_p_reduced)],method = 'brunet',rank=3)

saveRDS(Q3time_2,"nmf_Q3time_2.rds")
saveRDS(Q3time_3,"nmf_Q3time_3.rds")

#results
## Summary for 2 dim:
colsReduced_q3_2=Q3time_2@fit@W    
rowsReduced_q3_2=Q3time_2@fit@H    
colsReduced_q3_2=data.frame(CBSA_NAME=final_cbsa_Q3_p_reduced$CBSA_NAME,colsReduced_q3_2)

nmf_Q3time_results=list()
# Most representative behaviors in each cluster
nmf_Q3time_results$nmf_Q3time_2_results=list()
nmf_Q3time_results$nmf_Q3time_2_results$representative_behavior1=head(rowsReduced_q3_2[1,order(rowsReduced_q3_2[1,],decreasing = T)],n=20)
nmf_Q3time_results$nmf_Q3time_2_results$representative_behavior2=head(rowsReduced_q3_2[2,order(rowsReduced_q3_2[2,],decreasing = T)],n=20)

# group1 is Backpacking and Camping, Hiking, apparel, biking, kayaking
# group2 is Backpacking and Camping, Hiking, apparel, kayaking, deals
#
# Most representative geos in each cluster
nmf_Q3time_results$nmf_Q3time_2_results$representative_geos1=head(colsReduced_q3_2[order(colsReduced_q3_2$X1,decreasing = T),c("CBSA_NAME","X1")],n=20)
nmf_Q3time_results$nmf_Q3time_2_results$representative_geos2=head(colsReduced_q3_2[order(colsReduced_q3_2$X2,decreasing = T),c("CBSA_NAME","X2")],n=20)


##Summary for 3 dim:
colsReduced_q3_3=Q3time_3@fit@W    
rowsReduced_q3_3=Q3time_3@fit@H    
colsReduced_q3_3=data.frame(CBSA_NAME=final_cbsa_Q3_p_reduced$CBSA_NAME,colsReduced_q3_3)

nmf_Q3time_results$nmf_Q3time_3_results=list()
nmf_Q3time_results$nmf_Q3time_3_results$representative_behavior1=head(rowsReduced_q3_3[1,order(rowsReduced_q3_3[1,],decreasing = T)],n=20)
nmf_Q3time_results$nmf_Q3time_3_results$representative_behavior2=head(rowsReduced_q3_3[2,order(rowsReduced_q3_3[2,],decreasing = T)],n=20)
nmf_Q3time_results$nmf_Q3time_3_results$representative_behavior3=head(rowsReduced_q3_3[3,order(rowsReduced_q3_3[3,],decreasing = T)],n=20)

# group1 is deals, Backpacking & Camping & Hiking, Kayaking, some apparel
# group2 is deals, apparel, Camping & Hiking
# group3 is Camping & Hiking, some apparel, biking
#
# Most representative geos in each cluster
nmf_Q3time_results$nmf_Q3time_3_results$representative_geos1=head(colsReduced_q3_3[order(colsReduced_q3_3$X1,decreasing = T),c("CBSA_NAME","X1")],n=20)
nmf_Q3time_results$nmf_Q3time_3_results$representative_geos2=head(colsReduced_q3_3[order(colsReduced_q3_3$X2,decreasing = T),c("CBSA_NAME","X2")],n=20)
nmf_Q3time_results$nmf_Q3time_3_results$representative_geos3=head(colsReduced_q3_3[order(colsReduced_q3_3$X3,decreasing = T),c("CBSA_NAME","X3")],n=20)

# group1 is Backpacking & Camping & Hiking, Cycling
# group2 is Backpacking & Camping & Hiking, Kayaking
# group3 is Apparel
# group4 is deals

rlist::list.save(nmf_Q3time_results,file="nmf_Q3time_results.rdata")
saveRDS(nmf_Q3time_results,file="nmf_Q3time_results.rds")
save(nmf_Q3time_results,file="nmf_Q3time_results.RData")



######### Q4
#data: cluster_tsne_30_nmf50_Q4; 2,3 clusters
#nmf data: nmf_ddr_Q4_50
#nmf model: nmf_model_Q4_50
#orig data: final_cbsa_Q4_p_reduced

Q4time_2=nmf(final_cbsa_Q4_p_reduced[,3:ncol(final_cbsa_Q4_p_reduced)],method = 'brunet',rank=2)
Q4time_3=nmf(final_cbsa_Q4_p_reduced[,3:ncol(final_cbsa_Q4_p_reduced)],method = 'brunet',rank=3)

saveRDS(Q4time_2,"nmf_Q4time_2.rds")
saveRDS(Q4time_3,"nmf_Q4time_3.rds")

#results
## Summary for 2 dim:
colsReduced_q4_2=Q4time_2@fit@W    
rowsReduced_q4_2=Q4time_2@fit@H    
colsReduced_q4_2=data.frame(CBSA_NAME=final_cbsa_Q4_p_reduced$CBSA_NAME,colsReduced_q4_2)

nmf_Q4time_results=list()
# Most representative behaviors in each cluster
nmf_Q4time_results$nmf_Q4time_2_results=list()
nmf_Q4time_results$nmf_Q4time_2_results$representative_behavior1=head(rowsReduced_q4_2[1,order(rowsReduced_q4_2[1,],decreasing = T)],n=20)
nmf_Q4time_results$nmf_Q4time_2_results$representative_behavior2=head(rowsReduced_q4_2[2,order(rowsReduced_q4_2[2,],decreasing = T)],n=20)

# group1 is Apparel
# group2 is Backpacking and Camping, Hiking, some apparel
#
# Most representative geos in each cluster
nmf_Q4time_results$nmf_Q4time_2_results$representative_geos1=head(colsReduced_q4_2[order(colsReduced_q4_2$X1,decreasing = T),c("CBSA_NAME","X1")],n=20)
nmf_Q4time_results$nmf_Q4time_2_results$representative_geos2=head(colsReduced_q4_2[order(colsReduced_q4_2$X2,decreasing = T),c("CBSA_NAME","X2")],n=20)


##Summary for 3 dim:
colsReduced_q4_3=Q4time_3@fit@W    
rowsReduced_q4_3=Q4time_3@fit@H    
colsReduced_q4_3=data.frame(CBSA_NAME=final_cbsa_Q4_p_reduced$CBSA_NAME,colsReduced_q4_3)

nmf_Q4time_results$nmf_Q4time_3_results=list()
nmf_Q4time_results$nmf_Q4time_3_results$representative_behavior1=head(rowsReduced_q4_3[1,order(rowsReduced_q4_3[1,],decreasing = T)],n=20)
nmf_Q4time_results$nmf_Q4time_3_results$representative_behavior2=head(rowsReduced_q4_3[2,order(rowsReduced_q4_3[2,],decreasing = T)],n=20)
nmf_Q4time_results$nmf_Q4time_3_results$representative_behavior3=head(rowsReduced_q4_3[3,order(rowsReduced_q4_3[3,],decreasing = T)],n=20)

# group1 is Apparel, Snowport
# group2 is Camping & Hiking, Cycling, some apparel
# group3 is Apparel, Backpacking & Camping
#
# Most representative geos in each cluster
nmf_Q4time_results$nmf_Q4time_3_results$representative_geos1=head(colsReduced_q4_3[order(colsReduced_q4_3$X1,decreasing = T),c("CBSA_NAME","X1")],n=20)
nmf_Q4time_results$nmf_Q4time_3_results$representative_geos2=head(colsReduced_q4_3[order(colsReduced_q4_3$X2,decreasing = T),c("CBSA_NAME","X2")],n=20)
nmf_Q4time_results$nmf_Q4time_3_results$representative_geos3=head(colsReduced_q4_3[order(colsReduced_q4_3$X3,decreasing = T),c("CBSA_NAME","X3")],n=20)

rlist::list.save(nmf_Q4time_results,file="nmf_Q4time_results.rdata")
saveRDS(nmf_Q4time_results,file="nmf_Q4time_results.rds")
save(nmf_Q4time_results,file="nmf_Q4time_results.RData")



######################################
### Sync up clusters with orig data ##
######################################
load("~/Projects/Research/Search Geo/7_Kmeans to Tableau input/cluster_tsne_30_nmf100.RData")
head(cluster_tsne_30_nmf100)

load("~/Projects/Research/Search Geo/2_Data Post-Processing/geo_final_cbsa.RData")
head(final_cbsa[,1:5])

load("~/Projects/Research/Search Geo/2_Data Post-Processing/geo_themes_counts.RData")
head(themes[,1:5])

to_analyze_clusters=merge(unique(cluster_tsne_30_nmf100[,c("CBSA_NAME","kmeans_c4")]),
                          final_cbsa[,c("CBSA_NAME","VISITOR_COUNT_TOTAL")], by="CBSA_NAME")
to_analyze_clusters=merge(to_analyze_clusters,
                          themes, by="CBSA_NAME")
save(to_analyze_clusters,file="to_analyze_clusters_pregroup.RData")

analyze_clusters=aggregate(.~kmeans_c4, to_analyze_clusters[,-1], sum)
analyze_clusters_percent=analyze_clusters
next_column=ncol(analyze_clusters_percent)+1
for(i in 3:ncol(analyze_clusters_percent)){
  name=colnames(analyze_clusters_percent)[i]
  analyze_clusters_percent[,next_column]=analyze_clusters_percent[,i]/analyze_clusters_percent[,2]
  colnames(analyze_clusters_percent)[next_column]=paste0(name,"_p")
  next_column=next_column+1
}

analyze_clusters_p=analyze_clusters_percent[,c(1,2,(ncol(analyze_clusters)+1):ncol(analyze_clusters_percent))]
save(analyze_clusters_p,file="analyze_clusters_postgroup_p.RData")



#*****************************************************************
#############
## HDBSCAN ##
#############
#clustering N_iter=50 times and average the results

#############
## K-Means ##
#############
# lodad nmf_ddr,file="nmf_ddr.RData"
head(nmf_ddr[1:4,1:6])

#Elbow Method
k.max=15
sample=nmf_ddr[,3:ncol(nmf_ddr)]
wss=sapply(1:k.max, 
           function(k){kmeans(sample, k, iter.max = 15)$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


# try 6 clusters
#eigenvectors checked - no neg values
centers=6
k_model=kmeans(sample, centers=centers)

cluster_data=data.frame(nmf_ddr[,1:2],cluster=k_model$cluster)

head(cluster_data)
save(cluster_data,file="kmeans_quarterCBSA_results.RData")
write.csv(cluster_data,file="kmeans_quarterCBSA_results.csv")
