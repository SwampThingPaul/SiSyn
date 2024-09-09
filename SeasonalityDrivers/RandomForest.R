#install.packages(c("DAAG", "party", "rpart", "rpart.plot", "mlbench", "pROC", "tree"))
#install.packages("tree")
#install.packages("RRF")
#install.packages("arsenal")
require(remotes)
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
require(rcartocolor)
require(arsenal)
require(googledrive)
require(data.table)
require(corrplot)

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
    
    set.seed(123)
    rf_model<-randomForest(Centroid_Name~.,
                           data=drivers_df, importance=TRUE, proximity=TRUE, ntree=ntree_list[[i]],sampsize=c(29,29,29,29,29))
    OOB[[i]]<-rf_model$err.rate[,1]
    
  }
  
  return(OOB)
  
}


#read in drivers data
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")
# 
# drivers_url<-"https://drive.google.com/file/d/102LAmZFHOg64kMvorybxMiy9vCrFF1Cd/view?usp=drive_link"
#   
# file_get<-drive_get(as_id(drivers_url))
# 
# drive_download(file_get$drive_resource, overwrite = T)

drivers<-read.csv("AllDrivers_Harmonized_20240621.csv")

#remove any duplicated rows
drivers<-drivers[!duplicated(drivers$Stream_ID),]

#read in cluster data for average cluster, modal cluster, number of clusters
si_clust<-read.csv("ClusterAllMetadata.csv")

#merge cluster and drivers data
si_clust$Stream_ID<-paste0(si_clust$LTER, "__", si_clust$Site)

# plot_si_clust<-merge(si_clust, drivers, by="Stream_ID")
# 
# plot_si_clust_melt<-melt(plot_si_clust[,c(4:16, 24)], id.vars=c("Centroid_Name.x", "Site"))

# pdf("Sites_Clusters_190.pdf", width = 12, height = 8)
# 
# ggplot(plot_si_clust_melt, aes(variable, value))+geom_line(aes(group=Site))+facet_wrap(~Centroid_Name.x)+
#   theme(text = element_text(size = 20))+scale_x_discrete(labels=seq(1,12,1))+labs(x="Month", y="Normalized DSi Concentration")+
#   theme_classic()
# 
# dev.off()

si_clust<-si_clust[,c("Centroid_Name", "Stream_ID")]

drivers<-merge(drivers, si_clust, by=c("Stream_ID"))

#remove variables not interested in ever including
drivers<-dplyr::select(drivers, -c("cycle1","X","X.1","ClimateZ","Latitude","Longitude","LTER","rndCoord.lat",
                                   "rndCoord.lon","major_rock","major_land"))

# ggplot(drivers, aes(log(drainSqKm), log(med_q)))+geom_point()+theme_classic()+labs(x="Log(Drainage Area)", y="Log(Median Q)")+
#   theme(text = element_text(size = 20))

#look at distribution of NA across columns
#sapply(drivers, function(x) sum(is.na(x)))

#remove sites w NA
drivers<-drivers[complete.cases(drivers$npp),]
#setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/USGS_DataReview_SeasonalityDrivers")

#write.csv(drivers[,c(3,26,24,25)], "Site_Information.csv")

#turn centroid name to factor
drivers$Centroid_Name<-as.factor(drivers$Centroid_Name)

#select only features to be included in model
drivers_df<-drivers[,c("Centroid_Name","CV_Q","precip","evapotrans","temp","npp","cycle0","q_95","q_5",
                        "prop_area","N","P","Max_Daylength","q_max_day","q_min_day")]

keep_these_too<-drivers[,colnames(drivers) %like% c("rock|land")]

drivers_df<-bind_cols(drivers_df, keep_these_too)

drivers_df[,c(16:31)]<-replace(drivers_df[,c(16:31)], is.na(drivers_df[,c(16:31)]), 0)
# 
# setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/USGS_DataReview_SeasonalityDrivers")
# 
# write.csv(drivers_df, "SeasonalityDrivers_USGS_AvgDrivers_06232024.csv")

#look at correlation between variables
pdf("CorPlot_20240621.pdf", width = 10, height = 10)

driver_cor<-cor(drivers_df[,c(2:31)])
corrplot(driver_cor, type="lower", pch.col = "black", tl.col = "black", diag = F)

dev.off()

#original model, all parameters
#test number of trees 100-1000
OOB_list<-test_numtree_average(c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000))

tre_list<-c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000)

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
  theme_classic()+scale_x_continuous(breaks = seq(100,2000,100))+theme(text = element_text(size=20))

#tune mtry based on optimized ntree
set.seed(123)
tuneRF(drivers_df[,c(2:31)], drivers_df[,1], ntreeTry = 500, stepFactor = 1, improve = 0.5, plot = FALSE)

#run intial RF using tuned parameters
set.seed(123)
rf_model1<-randomForest(Centroid_Name~.,
                        data=drivers_df, importance=TRUE, proximity=TRUE, ntree=500,mtry=5,sampsize=c(29,29,29,29,29))

#visualize output
rf_model1

#visualize variable importance
randomForest::varImpPlot(rf_model1)

#set seeds for RFE
size=ncol(drivers_df)-1
#this is number of cross validation repeats and folds
cv_repeats = 5
cv_number = 5

total_repeats<-(cv_repeats*cv_number)+1

seeds <- vector(mode = "list", length = total_repeats)
for (i in 1:25) {
  
  seeds[[i]]<-rep(123, size)
  
}

seeds[[total_repeats]]<-123

#set control functions for recursive feature elimination on RF
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = cv_repeats, # number of repeats
                      number = cv_number,
                      seeds = seeds,
                      verbose = TRUE) # number of folds

#divide data into predictor variables (y) and response variables (x)
x<-drivers_df[,!(colnames(drivers_df)=="Centroid_Name")]

y<-drivers_df$Centroid

#run RFE, this will take a bit
#we are allowing the number of variables retained to range from 1 to all of them here
#to change that changes input into the "sizes" variable
set.seed(123)
result_rfe <- rfe(x = x, 
                  y = y, 
                  sizes = c(1:size),
                  rfeControl = control)

#print rfe results
result_rfe

#Put selected features into variable
new_rf_input<-paste(predictors(result_rfe), collapse = "+")

#Format those features into a formula to put in the optimized random forest model
rf_formula<-formula(paste("Centroid_Name~", new_rf_input))

#retune RF after RFE optimization
test_numtree_optimized <- function(ntree_list) {
  
  OOB<-list()
  
  for (i in 1:length(ntree_list)) {
    
    set.seed(123)
    rf_model<-randomForest(rf_formula,
                           data=drivers_df, importance=TRUE, proximity=TRUE, ntree=ntree_list[[i]],sampsize=c(29,29,29,29,29))
    OOB[[i]]<-rf_model$err.rate[,1]
    
  }
  
  return(OOB)
  
}

OOB_list<-test_numtree_optimized(c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000))

tre_list<-c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000)

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
  theme_classic()+scale_x_continuous(breaks = seq(100,2000,100))+theme(text = element_text(size=20))

#retune mtry
kept_drivers<-drivers_df[,c(colnames(drivers_df) %in% predictors(result_rfe))]

set.seed(123)
tuneRF(kept_drivers, drivers_df[,1], ntreeTry = 200, stepFactor = 1, improve = 0.5, plot = FALSE)

#run optimized random forest model, with retuned ntree and mtry parameters
set.seed(123)
rf_model2<-randomForest(rf_formula,
                        data=drivers_df, importance=TRUE, proximity=TRUE, ntree=200, mtry=5, sampsize=c(29,29,29,29,29))


rf_model2

randomForest::varImpPlot(rf_model2)

setdiff(colnames(drivers_df), predictors(result_rfe))

#plot confusion matrix
df<-as.data.frame(rf_model2$confusion)

df_new<-data.frame(t(apply(df[,c(1:5)],1, function(x) x/sum(x))))
colnames(df_new)<-c("FP", "FT", "ST", "STFP", "STVS")
rownames(df_new)<-c("FP", "FT", "ST", "STFP", "STVS")
df_new$cluster<-rownames(df_new)

df_new_melt<-melt(df_new, id.vars = "cluster")
df_new_melt$same<-ifelse(df_new_melt$cluster==df_new_melt$variable, "yes","no")

tiff("Average_Regime_Confusion_Matrix_06212024.tiff", width = 8, height = 7, units = "in", res = 300)

#visualize matrix
ggplot(df_new_melt, aes(variable, cluster))+geom_raster(aes(fill=same))+
  scale_fill_manual(values=c("yes"="forestgreen", "no"="salmon"))+
  geom_text(aes(label=round(value, 2)), size=8, family="Times")+theme_bw()+labs(x="",y="",fill="")+
  theme(legend.position = "null", text = element_text(size = 22, family = "Times"))+
  ggtitle("Average Regime Prediction Confusion Matrix")

dev.off()

importance_df<-data.frame(rf_model2$importance)
importance_df$driver<-rownames(importance_df)

vars_order<-importance_df %>%
  dplyr::arrange(desc(MeanDecreaseAccuracy), driver) %>%
  dplyr::select(driver)

importance_melt<-melt(importance_df[,-7], id.vars = "driver")

importance_melt$driver<-factor(importance_melt$driver, levels = vars_order$driver)

vars_order

driver_variable_list<-c("Maximum Snow Covered Area", "Maximum Daylength", "Green Up Day", "Temperature", "CV(Q)", "Evapotranspiration",
                        "q(5)", "q(95)", "Metamorphic", "Evergreen Needleaf Forest", "NPP", "Day of Minimum Q",
                        "Mixed Forest", "N Concentration", "P Concentration", "Cropland", "Precipitation", "Deciduous Needleleaf Forest",
                        "Urban", "Day of Maximum Q", "Shrubland/Grassland", "Deciduous Broadleaf Forest", "Sedimentary",
                        "Tundra", "Plutonic", "Carbonate/Evaporite", "Volcanic", "Barren", "Wetland", "Evergreen Broadleaf Forest")

tiff("Average_Regime_Variable_Importance_06212024.tiff", width = 11, height = 10, units = "in", res = 300)

ggplot(importance_melt, aes(variable, driver))+geom_raster(aes(fill=value))+
  scale_fill_gradient(low="grey90", high="red")+labs(x="", y="Variable",fill="Mean Decrease Accuracy")+
  theme_classic()+
  theme(text = element_text(size=15, family = "Times"))+
  scale_y_discrete(labels=rev(driver_variable_list), limits=rev)+
  scale_x_discrete(labels=c("FP","FT","ST","STFP","STVS","Overall Model"))+
  ggtitle("Average Regime Prediction Variable Importance")

dev.off()

###plot most important variables across clusters
centroid_abb<-as.data.frame(c("Fall Peak"="FP", "Fall Trough"="FT", "Spring Trough"="ST", "Spring Trough, Fall Peak"="STFP", 
                "Spring Trough, Variable Summer"="STVS"))
colnames(centroid_abb)<-"abb"
centroid_abb$Centroid_Name<-rownames(centroid_abb)

import_factors<-drivers_df[,c("Centroid_Name","prop_area", "Max_Daylength", "cycle0", "temp",
                              "CV_Q", "evapotrans")]
#import_factors$q_95<-log(import_factors$q_95)
#colnames(import_factors)<-c("Centroid_Name", "Max Snow Extent (proportion of WS)", "Watershed Snow Days (days)", "Latitude (degrees)",
 #                           "Temperature (C)", "Green Up Day (DOY)","CV Si")
import_factors_melt<-melt(import_factors, id.vars = "Centroid_Name")
import_factors_melt<-merge(import_factors_melt, centroid_abb, by="Centroid_Name")

colnames(import_factors_melt)[4]<-"Cluster"

vars<-unique(import_factors_melt$variable)

vars_ordered<-c(vars[1], vars[4], vars[5], vars[2], vars[6], vars[3])

y_axis_labs<-c("proportion of watershed", "hours", "day of year", "deg C","unitless","kg/m2")

title<-c("Maximum Snow Covered Area", "Maximum Daylength", "Green Up Day", "Temperature", "CV(Q)", "Evapotranspiration")

tag_val<-c("a","b","c","d","e","f")

variable_plot_list<-list()

for (i in 1:length(vars)) {
  
  one_var<-import_factors_melt[import_factors_melt$variable==vars_ordered[i],]
  
  if(i > 3){
    
    variable_plot_list[[i]]<-
      ggplot(one_var, aes(Cluster, value))+
      geom_boxplot(aes(fill=Cluster), alpha=0.5, outlier.shape = NA)+
      theme_classic()+
      theme(text = element_text(size = 20), legend.position = "null", plot.title = element_text(size=18))+
      geom_jitter(aes(col=Cluster))+
      scale_fill_manual(values=carto_pal(n=5, "Bold"))+scale_color_manual(values=carto_pal(n=5, "Bold"))+
      labs(x="", y=y_axis_labs[i], tag = tag_val[i])+ggtitle(title[i])
    
  } else{
  
  variable_plot_list[[i]]<-
    ggplot(one_var, aes(Cluster, value))+
    geom_boxplot(aes(fill=Cluster), alpha=0.5, outlier.shape = NA)+
    theme_classic()+
    theme(text = element_text(size = 20), legend.position = "null", plot.title = element_text(size=18))+
    geom_jitter(aes(col=Cluster))+
    theme(axis.text.x = element_blank())+
    scale_fill_manual(values=carto_pal(n=5, "Bold"))+scale_color_manual(values=carto_pal(n=5, "Bold"))+
    labs(x="", y=y_axis_labs[i], tag = tag_val[i])+ggtitle(title[i])
  
  }
  
}

pdf("Average_Prediction_MostImportVars_06212024.pdf", width = 14, height = 9, family="Times")

ggarrange(plotlist = variable_plot_list, common.legend = TRUE, legend = "right")

dev.off()
