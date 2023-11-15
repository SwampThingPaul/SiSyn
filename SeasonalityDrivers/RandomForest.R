#install.packages(c("DAAG", "party", "rpart", "rpart.plot", "mlbench", "pROC", "tree"))
#install.packages("tree")
install.packages("RRF")
require(RRF)
require(caret)
require(randomForest)
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

#function to see variable importance by regime
import_plot <- function(rf_model) {
  
  importance_df<-as.data.frame(rf_model$importance)
  importance_df$driver<-rownames(importance_df)
  
  importance_melt<-melt(importance_df, id.vars=c("MeanDecreaseAccuracy", "MeanDecreaseGini", "driver"))
  
  ggplot(importance_melt, aes(driver, value))+geom_point()+facet_wrap(~variable)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

####function to test ntree - change the internal function to reflect the RF model that you are using
test_numtree_average <- function(ntree_list) {
  
  OOB<-list()
  
  for (i in 1:length(ntree_list)) {
    
    rf_model<-randomForest(Centroid_Name~.,
                           data=drivers_df, importance=TRUE, proximity=TRUE, ntree=ntree_list[[i]], mtry=4,sampsize=c(30,30,30,30,30))
    OOB[[i]]<-rf_model$err.rate[,1]
    
  }
  
  return(OOB)
  
}


#read in drivers data
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

drivers_url<-"https://drive.google.com/file/d/1ZmyKMiMNlHCxgrHWOa_B_6nSgOSgog0D/view?usp=drive_link"
  
file_get<-drive_get(as_id(drivers_url))

drive_download(file_get$drive_resource, overwrite = T)

drivers<-read.csv("AllDrivers_Harmonized_20231114.csv")

#remove any duplicated rows
drivers<-drivers[!duplicated(drivers$Stream_ID),]

#read in cluster data for average cluster, modal cluster, number of clusters
si_clust<-read.csv("ClusterAllMetadata.csv")

#merge cluster and drivers data
si_clust$Stream_ID<-paste0(si_clust$LTER, "__", si_clust$Site)

si_clust<-si_clust[,c("Centroid_Name", "Stream_ID")]

drivers<-merge(drivers, si_clust, by=c("Stream_ID"))

#remove variables not interested in ever including
drivers<-dplyr::select(drivers, -c("cycle1","X","X.1","ClimateZ","Latitude","Longitude","LTER","rndCoord.lat",
                                   "rndCoord.lon","major_rock","major_land"))

#look at distribution of NA across columns
#sapply(drivers, function(x) sum(is.na(x)))

#remove sites w NA
drivers<-drivers[complete.cases(drivers$npp),]

#turn centroid name to factor
drivers$Centroid_Name<-as.factor(drivers$Centroid_Name)

#select only features to be included in model
drivers_df<-drivers[,c("Centroid_Name","CV_Q","precip","evapotrans","temp","npp","cycle0","q_95","q_5",
                        "prop_area","N","P","Max_Daylength")]

keep_these_too<-drivers[,colnames(drivers) %like% c("rock|land")]

drivers_df<-bind_cols(drivers_df, keep_these_too)

drivers_df[,c(14:27)]<-replace(drivers_df[,c(14:27)], is.na(drivers_df[,c(14:27)]), 0)

#look at correlation between variables
driver_cor<-cor(drivers_df[,c(2:9,12:15)])
corrplot(driver_cor, type="lower", pch.col = "black", tl.col = "black", diag = F)

#original model, all parameters
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

#visualize and select number of trees that gives the minimum OOB error
ggplot(OOB_mean, aes(tree_num, mean_oob))+geom_point()+geom_line()+
  theme_classic()+scale_x_continuous(breaks = seq(100,1000,100))+theme(text = element_text(size=20))

set.seed(123)
rf_model1<-randomForest(Centroid_Name~.,
                        data=drivers_df, importance=TRUE, proximity=TRUE, ntree=900, mtry=4,sampsize=c(30,30,30,30,30))

rf_model1

randomForest::varImpPlot(rf_model1)

#set control functions for recursive feature elimination on RF
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

#divide data into predictor variables (y) and response variables (x)
x<-drivers_df[,!(colnames(drivers_df)=="Centroid_Name")]

y<-drivers_df$Centroid

#split into testing and training data
inTrain <- createDataPartition(y, p = .80, list = FALSE)[,1]

x_train <- x[ inTrain, ]
x_test  <- x[-inTrain, ]

y_train <- y[ inTrain]
y_test  <- y[-inTrain]

#run RFE, this will take a bit
#we are allowing the number of variables retained to range from 1 to all of them here
#to change that changes input into the "sizes" variable
set.seed(123)
result_rfe <- rfe(x = x_train, 
                  y = y_train, 
                  sizes = c(1:(ncol(drivers_df)-1)),
                  rfeControl = control)


#print rfe results
result_rfe

#Put selected features into variable
new_rf_input<-paste(predictors(result_rfe), collapse = "+")

#Format those features into a formula to put in the optimized random forest model
rf_formula<-formula(paste("Centroid_Name~", new_rf_input))

#run optimized random forest model, with same number of trees
set.seed(123)
rf_model2<-randomForest(rf_formula,
                        data=drivers_df, importance=TRUE, proximity=TRUE, ntree=300, sampsize=c(30,30,30,30,30))


rf_model2

randomForest::varImpPlot(rf_model2)

#plot confusion matrix
df<-as.data.frame(rf_model2$confusion)

df_new<-data.frame(t(apply(df[,c(1:5)],1, function(x) x/sum(x))))
colnames(df_new)<-c("FP", "FT", "ST", "STFP", "STVS")
rownames(df_new)<-c("FP", "FT", "ST", "STFP", "STVS")
df_new$cluster<-rownames(df_new)

df_new_melt<-melt(df_new, id.vars = "cluster")
df_new_melt$same<-ifelse(df_new_melt$cluster==df_new_melt$variable, "yes","no")

#visualize matrix
ggplot(df_new_melt, aes(cluster, variable))+geom_raster(aes(fill=same))+
  scale_fill_manual(values=c("yes"="forestgreen", "no"="salmon"))+
  geom_text(aes(label=round(value, 2)))+theme_bw()+labs(x="",y="",fill="")+
  theme(legend.position = "null", text = element_text(size = 15))

###plot most important variables across clusters
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


