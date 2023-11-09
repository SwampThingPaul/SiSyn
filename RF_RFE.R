###recursive feature elimination

library(caret)
library(mlbench)
library(Hmisc)
library(randomForest)
library(ranger)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

drivers<-read.csv("AllDrivers_Harmonized.csv")

drivers<-drivers[,-c(1, 3,20, 22, 25:28)]
drivers<-drivers[!duplicated(drivers$Stream_ID),]

#convert NA in %land cover, lithology, and soils to 0
drivers[, c(31:62)][is.na(drivers[,c(31:62)])] <- 0

#read in cluster data for average cluster, modal cluster, number of clusters
si_clust<-read.csv("ClusterAllMetadata.csv")

#si_clust<-read.csv("AllSiteYearClusters.csv")
#colnames(si_clust)[9]<-"Stream_Name"

si_clust$Stream_ID<-paste0(si_clust$LTER, "__", si_clust$Site)

si_clust<-si_clust[,c("Centroid_Name", "Stream_ID")]

drivers<-merge(drivers, si_clust, by=c("Stream_ID"))

drivers<-dplyr::select(drivers, -c("cycle1"))

drivers<-drivers[complete.cases(drivers$npp),]
drivers<-drivers[complete.cases(drivers$drainSqKm),]

drivers$Centroid_Name<-as.factor(drivers$Centroid_Name)

drivers_cropped<-drivers[,c(3,5,7,9,10,11,19,21:28,30:34, 36:44, 46:57,59,62)]

drivers_x<-as.data.frame(drivers_cropped[,c(1:42)], stringsAsFactors=TRUE)

subsets<-seq(1,42,1)

set.seed(123)

lmFuncs

rfRFE <-  list(summary = defaultSummary,
               fit = function(x, y, first, last, ...){
                 library(randomForest)
                 randomForest(x, y, importance = first, ...)
               },
               pred = function(object, x)  predict(object, x),
               rank = function(object, x, y) {
                 vimp <- varImp(object)
                 vimp <- vimp[order(vimp$Overall,decreasing = TRUE),,drop = FALSE]
                 vimp$var <- rownames(vimp)                  
                 vimp
               },
               selectSize = pickSizeBest,
               selectVar = pickVars)

ctrl <- rfeControl(functions = rfRFE,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = TRUE)

lmProfile <- rfe(x=drivers_x, y=drivers_cropped$Centroid_Name,
                 sizes = subsets,
                 rfeControl = ctrl,
                 metric = "Accuracy")

lmProfile$results

plot(lmProfile, type = c("g", "o"))

pickSizeBest(lmProfile$results[,c(1,2)], metric = "Accuracy", maximize = TRUE)

ranger::ranger(drivers_cropped$Centroid_Name ~ drivers_x)

rfFIT<-rfRFE$fit(drivers_x, drivers_cropped$Centroid_Name, first = TRUE)

importance<-varImp(rfFIT)

importance$mean<-rowMeans(importance)

importance<-setorder(importance, -mean)

importance_top20<-importance[1:20,]

drivers_ideal<-drivers_x[,colnames(drivers_x) %in% rownames(importance_top20)]

rfFIT<-rfRFE$fit(drivers_ideal, drivers_cropped$Centroid_Name, first = TRUE)

rfFIT

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

