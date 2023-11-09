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
si_clust<-read.csv("ClusterAllMetadata.csv")

si_clust<-read.csv("AllSiteYearClusters.csv")
colnames(si_clust)[9]<-"Stream_Name"

si_clust$Stream_ID<-paste0(si_clust$LTER, "__", si_clust$Site)

si_clust<-si_clust[,c("Centroid_Name", "Stream_ID")]

drivers<-merge(drivers, si_clust, by=c("Stream_Name", "Year"))

lapply(drivers[,c(2:37)], summary)

drivers<-dplyr::select(drivers, -c("cycle1"))

drivers<-drivers[complete.cases(drivers$npp),]

drivers$clust<-as.factor(drivers$clust)

#check multicolinearity
d2 <- drivers[,c(3,5,7,9,10,11,14:20,25)] %>% 
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1)

d3<-highcor %>% 
  count(var1)

colnames(d3)[1]<-"vars"

d4<-highcor %>%
  count(var2)
colnames(d4)[1]<-"vars"

vars_count<-merge(d3, d4, by="vars")

vars_count$sum<-vars_count$n.x+vars_count$n.y

highcor<-d2[which(abs(d2$value)>0.49),]

highcor<-subset(highcor, highcor$value < 1)
highcor<-highcor[!duplicated(highcor$value),]

nrow(subset(highcor, highcor$var1=="prop_area"|highcor$var2=="prop_area"))

####set weights####
w<-1/table(drivers$clust)
w<-w/sum(w)

weights <- rep(0, length(drivers$clust))
weights[drivers$clust == 1] <- w['1']
weights[drivers$clust == 2] <- w['2']
weights[drivers$clust == 3] <- w['3']
weights[drivers$clust == 4] <- w['4']
weights[drivers$clust == 5] <- w['5']
weights_table<-table(weights, drivers$clust)

weights_vect<-c("1"=0.257618533638319,"2"=0.0953155247355229, "3"=0.227755488780708, "4"=0.206672933016996, "5"=0.212637519828454)


#### test mtry ####
set.seed(123)
mtry <- tuneRF(drivers[c(5,7,9,11,12,13,20,23:32,33)],drivers$Centroid_Name, ntreeTry=100,
               stepFactor=1,improve=0.01, plot = FALSE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

#original model - all parameters

set.seed(123)
rf_model<-randomForest(clust~med_si+CV_C+med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+npp+cycle0+elevation_mean_m+
                         major_soil+major_rock+major_land+ClimateZ+prop_area+, 
             data=drivers, importance=TRUE, proximity=TRUE, ntree=100, mtry=4)

rf_model


##with annual si and q

set.seed(123)
rf_model<-randomForest(clust~an_med_si+CV_C+an_med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+cycle0+elevation_mean_m+
                         major_rock+major_land+ClimateZ+prop_area+major_soil+npp, 
                       data=drivers, importance=TRUE, proximity=TRUE, ntree=100, mtry=4)

rf_model

##with annual si and q with weights, no climate
set.seed(123)
rf_model<-randomForest(clust~an_med_si+CV_C+an_med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+cycle0+elevation_mean_m+
                         major_rock+major_land+prop_area+major_soil+npp, 
                       data=drivers, importance=TRUE, proximity=TRUE, ntree=100, mtry=4, classwt=weights_vect)

rf_model

set.seed(123)
rf_model<-randomForest(clust~an_med_si+CV_C+an_med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+cycle0+elevation_mean_m+
                         major_rock+major_land+prop_area+major_soil+npp, 
                       data=drivers, importance=TRUE, proximity=TRUE, ntree=100, mtry=4, sampsize=c(100,100,100,100,100))

rf_model

#best model - annual si and q, no weights, no climate
set.seed(123)
rf_model<-randomForest(clust~an_med_si+CV_C+an_med_q+CV_Q+cvc_cvq+slope+num_days+precip+evapotrans+temp+cycle0+elevation_mean_m+
                         major_rock+major_land+prop_area+major_soil+npp, 
                       data=drivers, importance=TRUE, proximity=TRUE, ntree=100, mtry=4)

rf_model

#### current best model - cvc_cvq and prop_area removed, tested ntree on this model - ntree 1000 is ideal (lowest OOB)

set.seed(123)
rf_model<-randomForest(Centroid_Name~med_si+CV_C+mean_q+CV_Q+slope+num_days+precip+evapotrans+temp+npp+cycle0+elevation_mean_m+
                         major_soil+major_rock+major_land+ClimateZ, 
                       data=drivers, importance=TRUE, proximity=TRUE, ntree=1000, mtry=4)

rf_model

varImpPlot(rf_model)

import_plot(rf_model)

df<-as.data.frame(rf_model$confusion)

df_new<-t(apply(df[,c(1:5)],1, function(x) x/sum(x)))

par(mar=c(3,3,3,5))

plot(df_new, digits=3, col=c("lightpink", "hotpink", "violetred3", "deeppink4", "darkred", "forestgreen"), breaks=c(0,0.025,0.05,0.1,0.15,0.3,1), border=NA)

     