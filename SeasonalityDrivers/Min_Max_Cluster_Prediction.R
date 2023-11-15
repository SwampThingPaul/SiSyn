require(tibble)
require(ggpubr)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

monthly_results<-read.csv("Full_Results_Monthly_GFN_WRTDS.csv")

avg_monthly_results<-monthly_results %>%
  dplyr::group_by(stream, Month) %>%
  dplyr::summarise(mean_conc=median(Conc_mgL)) %>%
  dplyr::summarise(min_conc=min(mean_conc), max_conc=max(mean_conc))

colnames(avg_monthly_results)<-c("Stream_Name","Min_Conc","Max_Conc")

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

drivers<-merge(drivers, avg_monthly_results, by="Stream_Name")

#remove variables not interested in ever including
drivers<-dplyr::select(drivers, -c("cycle1","X","X.1","ClimateZ","Latitude","Longitude","Stream_ID","Name",
                                   "LTER","rndCoord.lat","rndCoord.lon","major_rock","major_land","major_soil"))

#look at distribution of NA across columns
#sapply(drivers, function(x) sum(is.na(x)))

#remove sites w NA
drivers<-drivers[complete.cases(drivers$npp),]

#select only features to be included in model
drivers_df<-drivers[,c("Centroid_Name","CV_Q","precip","evapotrans","temp","npp","cycle0","q_95","q_5",
                         "prop_area","N","P","Max_Daylength", "Min_Conc","Max_Conc")]

keep_these_too<-drivers[,colnames(drivers) %like% c("rock|land")]

drivers_df<-bind_cols(drivers_df, keep_these_too)

drivers_df[,c(16:29)]<-replace(drivers_df[,c(16:29)], is.na(drivers_df[,c(16:29)]), 0)

vars_order<-c("temp","precip","cycle0","evapotrans","prop_area","npp","N","P","Max_Daylength","CV_Q","q_5","q_95",
              "rocks_volcanic","rocks_sedimentary","rocks_plutonic","rocks_metamorphic","rocks_carbonate_evaporite",
              "land_evergreen_needleleaf_forest","land_tundra","land_shrubland_grassland","land_cropland","land_mixed_forest",
              "land_urban_and_built_up_land","land_barren_or_sparsely_vegetated","land_wetland","land_evergreen_broadleaf_forest")

cor_plot_drivers<-drivers_df[,vars_order]

drive_cor<-cor(cor_plot_drivers)

pdf("Corr_AllVars.pdf", width = 10, height = 10)

corrplot(drive_cor, type="lower", pch.col = "black", tl.col = "black", diag = F)

dev.off()

##define function for testing number of trees when predicting min conc
test_numtree_min <- function(ntree_list) {
  
  OOB<-list()
  
  for (k in 1:length(ntree_list)) {
    
    rf_model<-randomForest(Min_Conc~.,
                           data=one_cluster, importance=TRUE, proximity=TRUE, ntree=ntree_list[k])
    
    OOB[[k]]<-rf_model$mse
    
  }
  
  return(OOB)
  
}

##define function for testing number of trees when predicting min conc
test_numtree_max <- function(ntree_list) {
  
  OOB<-list()
  
  for (k in 1:length(ntree_list)) {
    
    rf_model<-randomForest(Max_Conc~.,
                           data=one_cluster, importance=TRUE, proximity=TRUE, ntree=ntree_list[k])
    
    OOB[[k]]<-rf_model$mse
    
  }
  
  return(OOB)
  
}

#set control functions for recursive feature elimination on RF
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

clusters<-unique(drivers$Centroid_Name)

#open lists to save important information from each cluster
rf_model_min_results<-list()

rfe_model_min_results<-list()

impotance_list<-list()

pred_actual_df<-list()

####Min Prediction####
for (i in 1:length(clusters)) {
  
  #this is just to keep track of where I am in the loop
  print(clusters[i])
  
  #subset out one cluster
  one_cluster<-subset(drivers_df, drivers_df$Centroid_Name==clusters[i])
  
  #original model to predict min conc - all parameters
  remove_col<-c("Max_Conc","Centroid_Name")
  
  #remove columns specified above from DF
  one_cluster<-one_cluster[,!(colnames(one_cluster) %in% remove_col)]
  
  #always set seed! this is what makes it reproducable
  set.seed(123)
  #test trees 100-1000
  OOB_list<-test_numtree_min(ntree_list=c(100,200,300,400,500,600,700,800,900,1000))
  
  tree_list<-c(100,200,300,400,500,600,700,800,900,1000)
  
  OOB_df<-as.data.frame(unlist(OOB_list))
  
  OOB_num<-list()
  
  for (q in 1:length(tree_list)) {
    
    OOB_num[[q]]<-rep(tree_list[q], tree_list[q])
    
  }
  
  OOB_df$tree_num<-unlist(OOB_num)
  
  OOB_mean<-OOB_df %>% group_by(tree_num) %>%
    summarise(mean_oob=mean(`unlist(OOB_list)`))
  
  #pull out tree number with min MSE (called OOB from previous classification code this is copied from)
  OOB_min<-OOB_mean %>%
    slice_min(mean_oob)
  
  #run RF with optimized tree number and ALL input variables
  set.seed(123)
  rf_model1<-randomForest(Min_Conc~.,
                          data=one_cluster, importance=TRUE, proximity=TRUE, ntree=OOB_min$tree_num)
  
  #record results
  rf_model_min_results[[i]]<-c(mean(rf_model1$mse), mean(rf_model1$rsq))
  
  #set up dataframes for RFE
  x<-one_cluster[,!(colnames(one_cluster)=="Min_Conc")]
  
  y<-one_cluster$Min_Conc
  
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
                    sizes = c(1:(ncol(one_cluster)-1)),
                    rfeControl = control)
  
  #Put selected features into variable
  new_rf_input<-paste(predictors(result_rfe), collapse = "+")
  
  #Format those features into a formula to put in the optimized random forest model
  rf_formula<-formula(paste("Min_Conc~", new_rf_input))
  
  #run optimized random forest model, with same number of trees
  set.seed(123)
  rf_model2<-randomForest(rf_formula,
                          data=one_cluster, importance=TRUE, proximity=TRUE, ntree=OOB_min$tree_num)
  
  #pull out importance
  impotance_list[[i]]<-data.frame(rf_model2$importance)
  
  #pull out predicted vs actual data
  pred_actual_df[[i]]<-data.frame(rf_model2$predicted, y)
  
  #run lm model to get R2, R2 given by RF is not correct
  lm_mod<-lm(y~rf_model2$predicted)
  
  #bind MSE and lm R2 into results
  rfe_model_min_results[[i]]<-c(mean(rf_model2$mse), summary(lm_mod)$adj.r.squared)
  
}

#get results for og rf and rfe rf
rf_model_min_results_df<-data.frame(do.call(rbind, rf_model_min_results))
rf_model_min_results_df$Centroid_Name<-clusters
colnames(rf_model_min_results_df)<-c("MSE","R2","Centroid_Name")

rfe_model_min_results_df<-data.frame(do.call(rbind, rfe_model_min_results))
rfe_model_min_results_df$Centroid_Name<-clusters
colnames(rfe_model_min_results_df)<-c("MSE","R2","Centroid_Name")

importance_list_centroid <- mapply(cbind, impotance_list, "Centroid_Name"=clusters, SIMPLIFY=F)
importance_list_centroid_vars <- lapply(importance_list_centroid, rownames_to_column)

importance_df<-do.call(bind_rows, importance_list_centroid_vars)

colnames(importance_df)<-c("Variable","Inc_MSE","Inc_Node_Purity","Centroid_Name")

setdiff(vars_order_2, importance_df$Variable)

vars_order_2<-c("temp","precip","cycle0","evapotrans","prop_area","npp","N","P","Max_Daylength","CV_Q","q_95","q_5",
                "rocks_volcanic","rocks_sedimentary","rocks_plutonic","rocks_metamorphic","rocks_carbonate_evaporite",
                "land_evergreen_needleleaf_forest","land_evergreen_broadleaf_forest","land_tundra","land_shrubland_grassland",
                "land_cropland","land_mixed_forest","land_urban_and_built_up_land","land_barren_or_sparsely_vegetated","land_wetland")

importance_df$Variable<-factor(importance_df$Variable, levels = vars_order_2)

pred_list_centroid <- mapply(cbind, pred_actual_df, "Centroid_Name"=clusters, SIMPLIFY=F)

pred_df<-do.call(bind_rows, pred_list_centroid)

colnames(pred_df)<-c("Predicted","Actual","Centroid_Name")

p1<-ggplot(pred_df, aes(Predicted, Actual))+geom_point()+geom_abline(intercept = 0, slope = 1, col="red")+
  theme_bw()+facet_wrap(~Centroid_Name, scales="free", nrow = 1)+theme(text = element_text(size=15))+
  labs(x="", y="Actual Si Concentration (mg/L)")+ggtitle("Minimum Si Concentration")

k1<-ggplot(importance_df, aes(Centroid_Name, Variable))+geom_raster(aes(fill=Inc_MSE))+
  scale_fill_gradient(low="grey90", high="red")+theme_bw()+labs(x="Cluster", y="Variable",fill="Increase MSE")+
  theme(text = element_text(size=15))+scale_x_discrete(labels=c("FP","FT","ST","STFP","STVS"))+scale_y_discrete(limits=rev)+
  labs(tag="a")


####Max Prediction####
#open lists to save important information from each cluster
rf_model_max_results<-list()

rfe_model_max_results<-list()

impotance_list<-list()

pred_actual_df<-list()

####Min Prediction####
for (i in 1:length(clusters)) {
  
  print(clusters[i])
  
  one_cluster<-subset(drivers_df, drivers_df$Centroid_Name==clusters[i])
  
  #original model to predict min conc - all parameters
  remove_col<-c("Min_Conc","Centroid_Name")
  
  one_cluster<-one_cluster[,!(colnames(one_cluster) %in% remove_col)]
  
  set.seed(123)
  OOB_list<-test_numtree_max(ntree_list=c(100,200,300,400,500,600,700,800,900,1000))
  
  tree_list<-c(100,200,300,400,500,600,700,800,900,1000)
  
  OOB_df<-as.data.frame(unlist(OOB_list))
  
  OOB_num<-list()
  
  for (q in 1:length(tree_list)) {
    
    OOB_num[[q]]<-rep(tree_list[q], tree_list[q])
    
  }
  
  OOB_df$tree_num<-unlist(OOB_num)
  
  OOB_mean<-OOB_df %>% group_by(tree_num) %>%
    summarise(mean_oob=mean(`unlist(OOB_list)`))
  
  OOB_min<-OOB_mean %>%
    slice_min(mean_oob)
  
  set.seed(123)
  rf_model1<-randomForest(Max_Conc~.,
                          data=one_cluster, importance=TRUE, proximity=TRUE, ntree=OOB_min$tree_num)
  
  rf_model_max_results[[i]]<-c(mean(rf_model1$mse), mean(rf_model1$rsq))
  
  x<-one_cluster[,!(colnames(one_cluster)=="Max_Conc")]
  
  y<-one_cluster$Max_Conc
  
  inTrain <- createDataPartition(y, p = .80, list = FALSE)[,1]
  
  x_train <- x[ inTrain, ]
  x_test  <- x[-inTrain, ]
  
  y_train <- y[ inTrain]
  y_test  <- y[-inTrain]
  
  set.seed(123)
  result_rfe <- rfe(x = x_train, 
                    y = y_train, 
                    sizes = c(1:(ncol(one_cluster)-1)),
                    rfeControl = control)
  
  # Print the selected features
  new_rf_input<-paste(predictors(result_rfe), collapse = "+")
  
  rf_formula<-formula(paste("Max_Conc~", new_rf_input))
  
  set.seed(123)
  rf_model2<-randomForest(rf_formula,
                          data=one_cluster, importance=TRUE, proximity=TRUE, ntree=OOB_min$tree_num)
  
  rfe_model_max_results[[i]]<-c(mean(rf_model2$mse), mean(rf_model2$rsq))
  
  impotance_list[[i]]<-data.frame(rf_model2$importance)
  
  pred_actual_df[[i]]<-data.frame(rf_model2$predicted, y)
  
}

rf_model_max_results_df<-data.frame(do.call(rbind, rf_model_max_results))
rf_model_max_results_df$Centroid_Name<-clusters
colnames(rf_model_max_results_df)<-c("MSE","R2","Centroid_Name")

rfe_model_max_results_df<-data.frame(do.call(rbind, rfe_model_max_results))
rfe_model_max_results_df$Centroid_Name<-clusters
colnames(rfe_model_max_results_df)<-c("MSE","R2","Centroid_Name")

importance_list_centroid <- mapply(cbind, impotance_list, "Centroid_Name"=clusters, SIMPLIFY=F)
importance_list_centroid_vars <- lapply(importance_list_centroid, rownames_to_column)

importance_df<-do.call(bind_rows, importance_list_centroid_vars)

unique(importance_df$rowname)

colnames(importance_df)<-c("Variable","Inc_MSE","Inc_Node_Purity","Centroid_Name")

vars_order_1<-c("temp","precip","cycle0","evapotrans","prop_area","npp","N","P","Max_Daylength","CV_Q","q_95", "q_5",
                "rocks_volcanic","rocks_sedimentary","rocks_plutonic","rocks_metamorphic","rocks_carbonate_evaporite",
                "land_evergreen_needleleaf_forest","land_shrubland_grassland","land_cropland","land_mixed_forest",
                "land_urban_and_built_up_land", "land_tundra", "land_barren_or_sparsely_vegetated", "land_wetland")

setdiff(vars_order_1, importance_df$Variable)

importance_df$Variable<-factor(importance_df$Variable, levels=vars_order_2)

pred_list_centroid <- mapply(cbind, pred_actual_df, "Centroid_Name"=clusters, SIMPLIFY=F)

pred_df<-do.call(bind_rows, pred_list_centroid)

colnames(pred_df)<-c("Predicted","Actual","Centroid_Name")

p2<-ggplot(pred_df, aes(Predicted, Actual))+geom_point()+geom_abline(intercept = 0, slope = 1, col="red")+
  theme_bw()+facet_wrap(~Centroid_Name, scales="free", nrow = 1)+theme(text = element_text(size=15))+
  labs(x="Predicted Si Concentration (mg/L)", y="Actual Si Concentration (mg/L)")+ggtitle("Maximum Si Concentration")

k2<-ggplot(importance_df, aes(Centroid_Name, Variable))+geom_raster(aes(fill=Inc_MSE))+
  scale_fill_gradient(low="grey90", high="red")+theme_bw()+labs(x="Cluster", y="Variable",fill="Increase MSE")+
  theme(text = element_text(size=15))+scale_x_discrete(labels=c("FP","FT","ST","STFP","STVS"))+scale_y_discrete(limits=rev)+
  labs(tag="b", y="")


pdf("Actual_Predicted_MinMax_SiConc.pdf", width = 18, height = 8)

ggarrange(p1, p2, nrow = 2, align = "v")

dev.off()

tiff("VariablesRetained_MinMax_SiConc.tiff", width = 16, height = 10, units="in", res = 300)

ggarrange(k1, k2, nrow = 1, align = "h")

dev.off()



