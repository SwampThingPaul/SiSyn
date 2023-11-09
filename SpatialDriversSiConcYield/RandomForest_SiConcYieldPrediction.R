#install.packages(c("DAAG", "party", "rpart", "rpart.plot", "mlbench", "pROC", "tree", "pdp))
#require(caret)
require(randomForest)
library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(caret)
library(pROC)
library(tree)
library(pdp)
require(data.table)

#this code made importance plots
import_plot <- function(rf_model) {
  
  importance_df<-as.data.frame(rf_model$importance)
  importance_df$driver<-rownames(importance_df)
  
  importance_melt<-melt(importance_df, id.vars=c("MeanDecreaseAccuracy", "MeanDecreaseGini", "driver"))
  
  ggplot(importance_melt, aes(driver, value))+geom_point()+facet_wrap(~variable)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

drivers<-read.csv("AllDrivers_Harmonized.csv")

drivers<-drivers[,!colnames(drivers) %in% c("X.1", "X", "LTER", "Stream_Name", "rndCoord.lon", "rndCoord.lat", "Name", "cycle1")]
drivers<-drivers[!duplicated(drivers$Stream_ID),]

#crop to only relevant drivers
drivers_cropped<-drivers[,c("med_si","CV_C","med_q","CV_Q", "cvc_cvq","slope","ClimateZ","Latitude","Longitude",
                            "drainSqKm", "num_days","prop_area","precip", "evapotrans", "temp", "npp",
                            "cycle0", "elevation_mean_m", colnames(drivers[30:58]))]

#replace NA with 0 in proportion of land/lithology/soils
drivers_cropped[,c(19:47)]<-replace(drivers_cropped[,c(19:47)], is.na(drivers_cropped[,c(19:47)]), 0)

drivers_cropped<-drivers_cropped[complete.cases(drivers_cropped$num_days),]
drivers_cropped<-drivers_cropped[complete.cases(drivers_cropped$drainSqKm),]

#remove "major" category for rock, land, soils
drivers_cropped<-drivers_cropped[,!c(colnames(drivers_cropped) %like% "major")]

####test ntree ####
#this only tests on the base model, which includes all parameters. we might expect things to change once we start 
#removing variables

test_numtree_average <- function(ntree_list) {
  
  OOB<-list()
  
  for (i in 1:length(ntree_list)) {
    
    print(ntree_list[i])
    
    set.seed(123)
    
    rf_model<-randomForest(med_si~.,data=drivers_cropped, importance=TRUE, proximity=TRUE, ntree=ntree_list[i], mtry=6)
    
    OOB[[i]]<-mean(rf_model$rsq)
    
  }
  
  return(OOB)
  
}

#test
set.seed(123)

#test 100 to 5000 trees, can change 5000 to smaller value
ntree_test<-seq(100,5000,by=100)

#this will take a while to run, depending on how many trees you test
OOB_list<-test_numtree_average(ntree_list = ntree_test)

OOB_df<-as.data.frame(unlist(OOB_list))

OOB_df$tree_num<-ntree_test
colnames(OOB_df)[1]<-"max_r2"

#visualize ntree test results
ggplot(OOB_df, aes(tree_num, max_r2))+geom_point()+geom_line()+
  theme_classic()+scale_x_continuous(breaks = ntree_test)+theme(text = element_text(size=20))

#### model ####
#default model - includes all parameters
set.seed(123)
rf_model<-randomForest(med_si~., 
                        data=drivers_cropped, importance=TRUE, proximity=TRUE, ntree=1600, mtry=6)

mean(rf_model$rsq)
varImpPlot(rf_model)

#visualize observed vs predicted
plot(rf_model$predicted, drivers_cropped$med_si, xlab="Predicted", ylab="Observed")
abline(a=0, b=1, col="red")

#playing around w partial dependence plots
par.Long<-partial(rf_model, pred.var = "drainSqKm")
autoplot(par.Long, contour = T)

#some next steps (ideas)
# 1. implement recursive feature elimanation to only keep variables that are important
# 2. tune mtry (currently set to 6, default is the square root of number of input variables)
# 3. Where are we getting over/under prediction - is this predictable/can we control for some factor?
# 4. Partial dependence plots

