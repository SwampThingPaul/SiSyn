#random forest
#install.packages("caret", dependencies = TRUE)
#install.packages("randomForest")

require(caret)
require(randomForest)

import_plot <- function(rf_model) {
  
  importance_df<-as.data.frame(rf_model$importance)
  importance_df$driver<-rownames(importance_df)
  
  importance_melt<-melt(importance_df, id.vars=c("MeanDecreaseAccuracy", "MeanDecreaseGini", "driver"))
  
  ggplot(importance_melt, aes(driver, value))+geom_point()+facet_wrap(~variable)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

drivers<-read.csv("AllDrivers_Harmonized.csv")

drivers<-drivers[,-c(1, 20,23, 24, 26, 27,28)]

#read in cluster data for average cluster, modal cluster, number of clusters
si_clust<-read.csv("AllSiteYearClusters.csv")
colnames(si_clust)[9]<-"Stream_Name"

clust_meaning<-as.data.frame(c("1"="ST", "2"="FT", "3"="FP", "4"="STFP","5"="STVS"))
colnames(clust_meaning)[1]<-"abb"
clust_meaning$clust<-seq(1,5,1)

si_clust<-merge(si_clust, clust_meaning, by="clust")

monthly_conc<-read.csv("AllSiteYearsMonthlyConc.csv")
colnames(monthly_conc)[1]<-"siteyear"
si_clust2<-merge(si_clust[,c(1,3,9,11)], monthly_conc, by="siteyear")
colnames(si_clust2)[5:16]<-seq(1,12,1)
si_clust2_melt<-melt(si_clust2, id.vars = c("siteyear","clust", "Stream_Name", "abb"))

si_clust2<-si_clust2[!si_clust2$siteyear %in% c("Imnavait Weir_2003", "ALBION_1984", "Site 10_2017"),]

si_clust2_melt %>%
  filter(value > 6)

ggplot(si_clust2_melt, aes(variable, value))+geom_line(aes(group=siteyear), alpha=0.3)+theme_bw()+facet_wrap(~abb, scales = "free")+
  theme(text = element_text(size=20))+labs(x="Month", y="Normalized Si Concentration")

drivers<-merge(drivers, si_clust, by=c("Stream_Name", "Year"))

#lapply(drivers[,c(2:37)], summary)

drivers<-dplyr::select(drivers, -c("cycle1"))

drivers<-drivers[complete.cases(drivers$npp),]

drivers$abb<-as.factor(drivers$abb)



###test ntree###
test_numtree <- function(ntree_list) {
  
  OOB<-list()
  
  for (i in 1:length(ntree_list)) {
    
    set.seed(123)
    rf_model<-randomForest(abb~med_si+CV_C+med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+npp+cycle0+elevation_mean_m+
                             major_soil+major_rock+major_land+ClimateZ+prop_area, 
                           data=drivers, importance=TRUE, proximity=TRUE, ntree=ntree_list[i], mtry=4, sampsize=c(600, 600, 600, 600, 600))
    
    OOB[[i]]<-rf_model$err.rate[,1]
    
  }
  
  return(OOB)
  
}

set.seed(123)
OOB_list<-test_numtree(c(100,200,300,400,500,600,700,800,900,1000))

tre_list<-c(100,200,300,400,500,600,700,800,900,1000)

OOB_df<-as.data.frame(unlist(OOB_list))

OOB_num<-list()

for (i in 1:length(tre_list)) {
  
  OOB_num[[i]]<-rep(tre_list[i], tre_list[i])
  
}

OOB_df$tree_num<-unlist(OOB_num)

OOB_mean<-OOB_df %>% group_by(tree_num) %>%
  summarise(mean_oob=mean(`unlist(OOB_list)`))

ggplot(OOB_mean, aes(tree_num, mean_oob))+geom_point()+geom_line()+
  theme_classic()+scale_x_continuous(breaks = seq(100,1000,100))+theme(text = element_text(size=20))


# #### test mtry ####
# test_mtry <- function(mtry_list) {
#   
#   OOB<-list()
#   
#   for (i in 1:length(mtry_list)) {
#     
#     set.seed(123)
#     rf_model<-randomForest(abb~med_si+CV_C+med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+npp+cycle0+elevation_mean_m+
#                              major_soil+major_rock+major_land+ClimateZ+prop_area, 
#                            data=drivers, importance=TRUE, proximity=TRUE, ntree=500, mtry=mtry_list[i], sampsize=c(600, 600, 600, 600, 600))
#     
#     OOB[[i]]<-rf_model$err.rate[,1]
#     
#   }
#   
#   return(OOB)
#   
# }
# 
# 
# OOB_list<-test_mtry(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
# 
# OOB_df<-as.data.frame(do.call(cbind, OOB_list))
# colnames(OOB_df)<-seq(1,15,1)
# OOB_df$seq<-seq(1, 500,1)
# 
# OOB_melt<-melt(OOB_df, id.vars = "seq")
# 
# OOB_mean<-OOB_melt %>% group_by(variable) %>%
#   summarise(mean_oob=mean(value))
# 
# ggplot(OOB_mean, aes(variable, mean_oob))+geom_point(size=3)+theme_classic()+labs(x="mtry", y="OOB")+theme(text = element_text(size=20))

#original model - all parameters

set.seed(123)
rf_model1<-randomForest(abb~med_si+CV_C+med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+npp+cycle0+elevation_mean_m+
                         major_soil+major_rock+major_land+ClimateZ+prop_area, 
             data=drivers, importance=TRUE, proximity=TRUE, ntree=600, mtry=8, sampsize=c(600, 600, 600, 600, 600))

rf_model1

set.seed(123)
rf_model2<-randomForest(abb~med_si+CV_C+med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+npp+cycle0+elevation_mean_m+
                          major_soil+major_rock+major_land+ClimateZ+prop_area, 
                        data=drivers, importance=TRUE, proximity=TRUE, ntree=600, mtry=4)

rf_model2

conf1<-as.data.frame(rf_model1$confusion)
conf1$cluster<-rownames(conf1)

conf2<-as.data.frame(rf_model2$confusion)
conf2$cluster<-rownames(conf2)

conf<-merge(conf1, conf2, by="cluster")
conf<-conf[,c(1,7,13)]
colnames(conf)<-c("Cluster", "Even_Sampling", "Normal_Sampling")
conf_melt<-melt(conf, id.vars = "Cluster")

ggplot(conf_melt, aes(x=Cluster, y=value, fill=variable))+geom_bar(stat = "identity", position = position_dodge())+theme_classic()+
  labs(x="", y="Class Error")+theme(axis.text.x = element_text(angle = 45,hjust = 1), text = element_text(size=20))

tab<-as.data.frame(table(drivers$abb))

ggplot(tab, aes(x=Var1, y=Freq))+geom_bar(stat = "identity", fill="black")+theme_classic()+
  labs(x="", y="Count")+theme(axis.text.x = element_text(angle = 45,hjust = 1), text = element_text(size=20))


set.seed(123)
rf_model<-randomForest(abb~med_si+CV_C+med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+npp+cycle0+elevation_mean_m+
                          major_soil+major_rock+major_land+ClimateZ+prop_area, 
                        data=drivers, importance=TRUE, proximity=TRUE, ntree=600, mtry=4, sampsize=c(600, 600, 600, 600, 600))

rf_model

import_factors<-drivers[,c("clust","slope", "med_q", "med_si", "CV_C", "cycle0")]
import_factors_melt<-melt(import_factors, id.vars = "clust")

fact_cropped<-import_factors_melt %>%
  group_by(clust, variable) %>%
  filter(value > quantile(value, probs=.25) & value < quantile(value, probs=.75))


ggplot(fact_cropped, aes(clust, value))+geom_boxplot(outlier.shape = NA)+facet_wrap(~variable, scales = "free")+theme_bw()+
  theme(text = element_text(size = 20))+labs(x="", y="Driver Value")


varImpPlot(rf_model)

import_plot(rf_model)

df<-as.data.frame(rf_model$confusion)

df_new<-t(apply(df[,c(1:5)],1, function(x) x/sum(x)))

par(mar=c(3,3,3,5))

plot(df_new, digits=3, col=c("lightpink", "hotpink", "violetred3", "deeppink4", "darkred", "forestgreen"), breaks=c(0,0.025,0.05,0.1,0.15,0.3,1), border=NA)



####using annual Si and Q####
     

test_numtree_annual <- function(ntree_list) {
  
  OOB<-list()
  
  for (i in 1:length(ntree_list)) {
    
    rf_model<-randomForest(abb~an_med_si+CV_C+an_med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+npp+cycle0+elevation_mean_m+
                             major_soil+major_rock+major_land+ClimateZ+prop_area, 
                           data=drivers, importance=TRUE, proximity=TRUE, ntree=ntree_list[i], mtry=4, sampsize=c(600, 600, 600, 600, 600))
    
    OOB[[i]]<-rf_model$err.rate[,1]
    
  }
  
  return(OOB)
  
}

OOB_list<-test_numtree_annual(c(100,200,300,400,500,600,700,800,900,1000))

tre_list<-c(100,200,300,400,500,600,700,800,900,1000)

OOB_df<-as.data.frame(unlist(OOB_list))

OOB_num<-list()

for (i in 1:length(tre_list)) {
  
  OOB_num[[i]]<-rep(tre_list[i], tre_list[i])
  
}

OOB_df$tree_num<-unlist(OOB_num)

OOB_mean<-OOB_df %>% group_by(tree_num) %>%
  summarise(mean_oob=mean(`unlist(OOB_list)`))

ggplot(OOB_mean, aes(tree_num, mean_oob))+geom_point()+geom_line()+
  theme_classic()+scale_x_continuous(breaks = seq(100,1000,100))+theme(text = element_text(size=20))

set.seed(123)
rf_model1<-randomForest(abb~an_med_si+CV_C+an_med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+npp+cycle0+elevation_mean_m+
                          major_soil+major_rock+major_land+ClimateZ+prop_area, 
                        data=drivers, importance=TRUE, proximity=TRUE, ntree=500, mtry=4, sampsize=c(600, 600, 600, 600, 600))

rf_model1

set.seed(123)
rf_model2<-randomForest(abb~an_med_si+CV_C+an_med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+npp+cycle0+elevation_mean_m+
                         major_soil+major_rock+major_land+ClimateZ+prop_area, 
                       data=drivers, importance=TRUE, proximity=TRUE, ntree=500, mtry=4)

rf_model2

conf1<-as.data.frame(rf_model1$confusion)
conf1$cluster<-rownames(conf1)

conf2<-as.data.frame(rf_model2$confusion)
conf2$cluster<-rownames(conf2)

conf<-merge(conf1, conf2, by="cluster")
conf<-conf[,c(1,7,13)]
colnames(conf)<-c("Cluster", "Even_Sampling", "Normal_Sampling")
conf_melt<-melt(conf, id.vars = "Cluster")

ggplot(conf_melt, aes(x=Cluster, y=value, fill=variable))+geom_bar(stat = "identity", position = position_dodge())+theme_classic()+
  labs(x="", y="Class Error")+theme(axis.text.x = element_text(angle = 45,hjust = 1), text = element_text(size=20))


set.seed(123)
rf_model<-randomForest(abb~an_med_si+CV_C+an_med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+npp+cycle0+elevation_mean_m+
                          major_soil+major_rock+major_land+ClimateZ+prop_area, 
                        data=drivers, importance=TRUE, proximity=TRUE, ntree=500, mtry=4, sampsize=c(600, 600, 600, 600, 600))

rf_model

varImpPlot(rf_model)

import_factors<-drivers[,c("abb","an_med_si", "an_med_q","slope", "CV_C", "cycle0")]
import_factors_melt<-melt(import_factors, id.vars = "abb")

fact_cropped<-import_factors_melt %>%
  group_by(abb, variable) %>%
  filter(value > quantile(value, probs=.25) & value < quantile(value, probs=.75))


ggplot(fact_cropped, aes(abb, value))+geom_boxplot(outlier.shape = NA)+facet_wrap(~variable, scales = "free")+theme_bw()+
  theme(text = element_text(size = 20))+labs(x="", y="Driver Value")


varImpPlot(rf_model)

import_plot(rf_model)

df<-as.data.frame(rf_model$confusion)

df_new<-t(apply(df[,c(1:5)],1, function(x) x/sum(x)))

par(mar=c(3,3,3,5))

plot(df_new, digits=3, col=c("lightpink", "hotpink", "violetred3", "deeppink4", "darkred", "forestgreen"), breaks=c(0,0.025,0.05,0.1,0.15,0.3,1), border=NA)



