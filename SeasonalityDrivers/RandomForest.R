require(caret)
require(randomForest)
#install.packages(c("DAAG", "party", "rpart", "rpart.plot", "mlbench", "pROC", "tree"))
#install.packages("tree")
library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(caret)
library(pROC)
library(tree)
require(dplyr)
require(plot.matrix)
require(reshape2)

import_plot <- function(rf_model) {
  
  importance_df<-as.data.frame(rf_model$importance)
  importance_df$driver<-rownames(importance_df)
  
  importance_melt<-melt(importance_df, id.vars=c("MeanDecreaseAccuracy", "MeanDecreaseGini", "driver"))
  
  ggplot(importance_melt, aes(driver, value))+geom_point()+facet_wrap(~variable)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

drivers<-read.csv("AllDrivers_Harmonized.csv")

drivers<-drivers[,-c(1, 3,20, 24, 26, 27,28)]
drivers<-drivers[!duplicated(drivers$Stream_ID),]

#read in cluster data for average cluster, modal cluster, number of clusters
si_clust<-read.csv("ClusterAllMetadata.csv")

#si_clust<-read.csv("AllSiteYearClusters.csv")
#colnames(si_clust)[9]<-"Stream_Name"

si_clust$Stream_ID<-paste0(si_clust$LTER, "__", si_clust$Site)

si_clust<-si_clust[,c("Centroid_Name", "Stream_ID")]

drivers<-merge(drivers, si_clust, by=c("Stream_ID"))

lapply(drivers[,c(2:36)], summary)

drivers<-dplyr::select(drivers, -c("cycle1"))

drivers<-drivers[complete.cases(drivers$npp),]
drivers<-drivers[complete.cases(drivers$drainSqKm),]

drivers$Centroid_Name<-as.factor(drivers$Centroid_Name)

#clust<-drivers %>% dplyr::filter(Centroid_Name=="Fall Peak")

# #check multicolinearity
# d2 <- drivers[,c(3,5,7,9,10,11,14:20,25)] %>% 
#   as.matrix %>%
#   cor %>%
#   as.data.frame %>%
#   rownames_to_column(var = 'var1') %>%
#   gather(var2, value, -var1)
# 
# d3<-highcor %>% 
#   count(var1)
# 
# colnames(d3)[1]<-"vars"
# 
# d4<-highcor %>%
#   count(var2)
# colnames(d4)[1]<-"vars"
# 
# vars_count<-merge(d3, d4, by="vars")
# 
# vars_count$sum<-vars_count$n.x+vars_count$n.y
# 
# highcor<-d2[which(abs(d2$value)>0.49),]
# 
# highcor<-subset(highcor, highcor$value < 1)
# highcor<-highcor[!duplicated(highcor$value),]
# 
# nrow(subset(highcor, highcor$var1=="prop_area"|highcor$var2=="prop_area"))

# ####set weights####
# w<-1/table(drivers$Centroid_Name)
# w<-w/sum(w)
# 
# weights <- rep(0, length(drivers$Centroid_Name))
# weights[drivers$Centroid_Name == 1] <- w['1']
# weights[drivers$Centroid_Name == 2] <- w['2']
# weights[drivers$Centroid_Name == 3] <- w['3']
# weights[drivers$Centroid_Name == 4] <- w['4']
# weights[drivers$Centroid_Name == 5] <- w['5']
# weights_table<-table(weights, drivers$clust)
# 
# w
# 
# weights_vect<-c("Fall Peak"=0.18768912,"Fall Trough"=0.08775076, "Spring Trough"=0.23299339, "Spring Trough, Fall Peak"=0.25025216, 
#                 "Spring Trough, Variable Summer"=0.24131458)
# 

# #### test mtry ####
# set.seed(123)
# mtry <- tuneRF(drivers[c(5,7,9,11,12,13,20,23:32,33)],drivers$Centroid_Name, ntreeTry=100,
#                stepFactor=1,improve=0.01, plot = FALSE)
# best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
# print(mtry)
# print(best.m)

####test ntree ####
test_numtree_average <- function(ntree_list) {
  
  OOB<-list()
  
  for (i in 1:length(ntree_list)) {
    
    rf_model<-randomForest(Centroid_Name~med_si+CV_C+med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+npp+cycle0+elevation_mean_m+
                             major_soil+major_rock+major_land+ClimateZ+prop_area+drainSqKm+Latitude, 
                           data=drivers, importance=TRUE, proximity=TRUE, ntree=ntree_list[i], mtry=4, sampsize=c(26,26,26,26,26))
    
    OOB[[i]]<-rf_model$err.rate[,1]
    
  }
  
  return(OOB)
  
}

set.seed(123)
OOB_list<-test_numtree_average(c(100,200,300,400,500,600,700,800,900,1000))

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


#original model - all parameters
set.seed(123)
rf_model1<-randomForest(Centroid_Name~med_si+CV_C+med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+npp+cycle0+elevation_mean_m+
                         major_soil+major_rock+major_land+ClimateZ+prop_area, 
                       data=drivers, importance=TRUE, proximity=TRUE, ntree=700, mtry=4, sampsize=c(26,26,26,26,26))

rf_model1

set.seed(123)
rf_model2<-randomForest(Centroid_Name~med_si+CV_C+med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+npp+cycle0+elevation_mean_m+
                         major_soil+major_rock+major_land+ClimateZ+prop_area, 
                       data=drivers, importance=TRUE, proximity=TRUE, ntree=700, mtry=4)

rf_model2

conf1<-as.data.frame(rf_model1$confusion)
conf1$cluster<-rownames(conf1)

conf2<-as.data.frame(rf_model2$confusion)
conf2$cluster<-rownames(conf2)

conf<-merge(conf1, conf2, by="cluster")
conf<-conf[,c(1,7,13)]
colnames(conf)<-c("Centroid_Name", "Even_Sampling", "Normal_Sampling")
conf_melt<-melt(conf, id.vars = "Centroid_Name")

centroid_abb<-as.data.frame(c("Fall Peak"="FP", "Fall Trough"="FT", "Spring Trough"="ST", "Spring Trough, Fall Peak"="STFP", 
                              "Spring Trough, Variable Summer"="STVS"))
colnames(centroid_abb)<-"abb"
centroid_abb$Centroid_Name<-rownames(centroid_abb)

conf_melt<-merge(conf_melt, centroid_abb, by="Centroid_Name")

ggplot(conf_melt, aes(x=abb, y=value, fill=variable))+geom_bar(stat = "identity", position = position_dodge())+theme_classic()+
  labs(x="", y="Class Error")+theme(axis.text.x = element_text(angle = 45,hjust = 1), text = element_text(size=20))

tab<-as.data.frame(table(drivers$Centroid_Name))

ggplot(tab, aes(x=Var1, y=Freq))+geom_bar(stat = "identity", fill="black")+theme_classic()+
  labs(x="", y="Count")+theme(axis.text.x = element_text(angle = 45,hjust = 1), text = element_text(size=20))

set.seed(123)
rf_model<-randomForest(Centroid_Name~med_si+CV_C+med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+npp+cycle0+elevation_mean_m+
                          major_soil+major_rock+major_land+ClimateZ+prop_area+drainSqKm+Latitude, 
                        data=drivers, importance=TRUE, proximity=TRUE, ntree=700, mtry=4, sampsize=c(26,26,26,26,26))

rf_model
varImpPlot(rf_model)

colnames(drivers)<-c("Stream_ID", "mean_si", "Median_Si_Concentration", "sd_si", "CV_Si", "mean_q", "Median_Discharge", "sd_q",
                     "CV_Discharge", "CVc_CVq", "CQ_Slope", "an_mean_si", "an_med_si", "an_sd_si", "an_mean_q","an_med_q", "an_sd_q",
                     "Koppen_Geiger_Climate", "Stream_Name", "Latitude", "LTER", "Drainage_Area", "Max_Snow_Extent", "Watershed_Snow_Days",
                     "Annual_Precipitation", "ET", "Annual_Temperature", "NPP", "major_rock", "Land_Cover", "major_soil",
                     "elevation_median_m", "Mean_Elevation", "elevation_min_m", "elevation_max_m", "Green_Up_Day", "Centroid_Name")

#altered model
set.seed(123)
rf_model<-randomForest(Centroid_Name~med_si+CV_C+med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+npp+cycle0+elevation_mean_m+
                         major_land+ClimateZ+prop_area+Latitude, 
                       data=drivers, importance=TRUE, proximity=TRUE, ntree=700, mtry=4, sampsize=c(26,26,26,26,26))

set.seed(123)
rf_model<-randomForest(Centroid_Name~Median_Si_Concentration+CV_Si+Median_Discharge+CV_Discharge+
                         CVc_CVq+CQ_Slope+Watershed_Snow_Days+ET+Annual_Temperature+
                         Green_Up_Day+Mean_Elevation+Koppen_Geiger_Climate+Max_Snow_Extent+
                         Latitude+Drainage_Area, 
                       data=drivers, importance=TRUE, proximity=TRUE, ntree=600, mtry=4, sampsize=c(26,26,26,26,26))

rf_model

pdf("VarImp.pdf", width = 13, height = 10, family = "Times", bg="white")

varImpPlot(rf_model)

dev.off()

rf_model$votes

set.seed(123)
rf_model<-party::ctree(Centroid_Name~med_si+CV_C+med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+npp+cycle0+elevation_mean_m+
                         major_soil+major_rock+major_land+ClimateZ+prop_area, 
                       data=drivers)

tree:::plot.tree(rf_model)

rf_model$frame

getTree(rf_model, k=2, labelVar = TRUE)

varImpPlot(rf_model)

import_plot(rf_model)

df<-as.data.frame(rf_model$confusion)

df_new<-t(apply(df[,c(1:5)],1, function(x) x/sum(x)))
colnames(df_new)<-c("FP", "FT", "ST", "STFP", "STVS")
rownames(df_new)<-c("FP", "FT", "ST", "STFP", "STVS")

par(mar=c(3,3,3,5))

pdf("RFMatrix.pdf", width = 10, height = 8, family = "Times")

par(mar=c(4,4,3,6))

plot(df_new, digits=2, col=c("lightpink", "hotpink", "violetred3", "deeppink4", "darkred", "forestgreen"), 
     breaks=c(0,0.025,0.05,0.1,0.15,0.3,1), border=NA, cex=1.3, xlab="", ylab="", main="", cex.axis=1.3)


dev.off()

centroid_abb<-as.data.frame(c("Fall Peak"="FP", "Fall Trough"="FT", "Spring Trough"="ST", "Spring Trough, Fall Peak"="STFP", 
                "Spring Trough, Variable Summer"="STVS"))
colnames(centroid_abb)<-"abb"
centroid_abb$Centroid_Name<-rownames(centroid_abb)

import_factors<-drivers[,c("Centroid_Name","Watershed_Snow_Days", "Green_Up_Day", "Latitude",
                           "Annual_Temperature", "CV_Si", "CV_Discharge")]
#colnames(import_factors)<-c("Centroid_Name", "Max Snow Extent (proportion of WS)", "Watershed Snow Days (days)", "Latitude (degrees)",
 #                           "Temperature (C)", "Green Up Day (DOY)","CV Si")
import_factors_melt<-melt(import_factors, id.vars = "Centroid_Name")
import_factors_melt<-merge(import_factors_melt, centroid_abb, by="Centroid_Name")

import_factors_melt<-import_factors_melt[-which(import_factors_melt$variable=='CV_Si'&import_factors_melt$value > 1),]


pdf("MostImportVars.pdf", width = 14, height = 9, family="Times")

ggplot(import_factors_melt, aes(abb, value))+geom_boxplot(aes(fill=abb), alpha=0.5, outlier.shape = NA)+facet_wrap(~variable, scales = "free")+theme_bw()+
  theme(text = element_text(size = 20))+labs(x="", y="Driver Value")+
  geom_jitter(aes(col=abb))+
  scale_fill_manual(values=carto_pal(n=5, "Bold"))+scale_color_manual(values=carto_pal(n=5, "Bold"))+
  theme(axis.text.x = element_blank())+labs(fill="Cluster", color="Cluster")

dev.off()

drivers<-merge(centroid_abb, drivers, by="Centroid_Name")



