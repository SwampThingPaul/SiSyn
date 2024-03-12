require(tibble)
require(ggpubr)
require(pdp)
require(data.table)
require(caret)
require(grid)
require(randomForest)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

monthly_results<-read.csv("Full_Results_Monthly_GFN_WRTDS.csv")

avg_monthly_results<-monthly_results %>%
  dplyr::group_by(stream, Month) %>%
  dplyr::summarise(mean_conc=median(Conc_mgL)) %>%
  dplyr::summarise(min_conc=min(mean_conc), max_conc=max(mean_conc))

colnames(avg_monthly_results)<-c("Stream_Name","Min_Conc","Max_Conc")

#read in drivers data
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

# drivers_url<-"https://drive.google.com/file/d/102LAmZFHOg64kMvorybxMiy9vCrFF1Cd/view?usp=drive_link"
# 
# file_get<-drive_get(as_id(drivers_url))
# 
# drive_download(file_get$drive_resource, overwrite = T)

drivers<-read.csv("AllDrivers_Harmonized_20231129.csv")

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
                         "prop_area","N","P","Max_Daylength", "Min_Conc","Max_Conc","q_max_day","q_min_day")]

keep_these_too<-drivers[,colnames(drivers) %like% c("rock|land")]

drivers_df<-bind_cols(drivers_df, keep_these_too)

drivers_df[,c(18:31)]<-replace(drivers_df[,c(18:31)], is.na(drivers_df[,c(18:31)]), 0)

vars_order<-c("temp","precip","cycle0","evapotrans","prop_area","npp","N","P","Max_Daylength","CV_Q","q_5","q_95","q_min_day","q_max_day",
              "rocks_volcanic","rocks_sedimentary","rocks_plutonic","rocks_metamorphic","rocks_carbonate_evaporite",
              "land_evergreen_needleleaf_forest","land_tundra","land_shrubland_grassland","land_cropland","land_mixed_forest",
              "land_urban_and_built_up_land","land_barren_or_sparsely_vegetated","land_wetland","land_evergreen_broadleaf_forest")

#cor_plot_drivers<-drivers_df[,vars_order]

#drive_cor<-cor(cor_plot_drivers)

#pdf("Corr_AllVars.pdf", width = 10, height = 10)

#corrplot(drive_cor, type="lower", pch.col = "black", tl.col = "black", diag = F)

#dev.off()

##define function for testing number of trees when predicting min conc
test_numtree_min <- function(ntree_list) {
  
  OOB<-list()
  
  for (k in 1:length(ntree_list)) {
    
    set.seed(123)
    rf_model<-randomForest(Min_Conc~.,
                           data=one_cluster, importance=TRUE, proximity=TRUE, ntree=ntree_list[k])
    
    OOB[[k]]<-rf_model$mse
    
  }
  
  return(OOB)
  
}

##define function for testing number of trees when predicting min conc
test_numtree_min_opt <- function(ntree_list) {
  
  OOB<-list()
  
  for (k in 1:length(ntree_list)) {
    
    set.seed(123)
    rf_model<-randomForest(rf_formula,
                           data=one_cluster, importance=TRUE, proximity=TRUE, ntree=ntree_list[k])
    
    OOB[[k]]<-rf_model$mse
    
  }
  
  return(OOB)
  
}


##define function for testing number of trees when predicting min conc
test_numtree_max <- function(ntree_list) {
  
  OOB<-list()
  
  for (k in 1:length(ntree_list)) {
    
    set.seed(123)
    rf_model<-randomForest(Max_Conc~.,
                           data=one_cluster, importance=TRUE, proximity=TRUE, ntree=ntree_list[k])
    
    OOB[[k]]<-rf_model$mse
    
  }
  
  return(OOB)
  
}


##define function for testing number of trees when predicting min conc
test_numtree_max_opt <- function(ntree_list) {
  
  OOB<-list()
  
  for (k in 1:length(ntree_list)) {
    
    set.seed(123)
    rf_model<-randomForest(rf_formula,
                           data=one_cluster, importance=TRUE, proximity=TRUE, ntree=ntree_list[k])
    
    OOB[[k]]<-rf_model$mse
    
  }
  
  return(OOB)
  
}


size=ncol(drivers_df)-3
cv_repeats = 5
cv_number = 5

total_repeats<-(cv_repeats*cv_number)+1

seeds <- vector(mode = "list", length = total_repeats)

for(i in 1:(total_repeats-1)){
  
    seeds[[i]]<-rep(123, size)
    
}
  
seeds[[total_repeats]]<-123

#set control functions for recursive feature elimination on RF
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = cv_repeats, # number of repeats
                      number = cv_number,
                      seeds = seeds) # number of folds

clusters<-unique(drivers$Centroid_Name)

#define variables wanted for PDPs for minimum Si concentration
pdp_vars<-c("precip","N", "P", "CV_Q", "land_evergreen_needleleaf_forest", "land_cropland")

#open lists to save important information from each cluster
rf_model_min_results<-list()

rfe_model_min_results<-list()

impotance_list<-list()

pred_actual_df<-list()

all_models_partial_df<-list()

#i=4

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
  OOB_list<-test_numtree_min(ntree_list=c(seq(100,2000,100)))
  
  tree_list<-c(seq(100,2000,100))
  
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
  
  set.seed(123)
  opt_mtry<-tuneRF(one_cluster[,!colnames(one_cluster) %in% "Min_Conc"], one_cluster[,"Min_Conc"], 
                   ntreeTry = OOB_min$tree_num, stepFactor = 1, improve = 0.5, plot = FALSE)
    
  #run RF with optimized tree number and ALL input variables
  set.seed(123)
  rf_model1<-randomForest(Min_Conc~.,
                          data=one_cluster, importance=TRUE, proximity=TRUE, ntree=OOB_min$tree_num, mtry=opt_mtry[1])
  
  #record results
  rf_model_min_results[[i]]<-c(mean(rf_model1$mse), mean(rf_model1$rsq))
  
  #set up dataframes for RFE
  x<-one_cluster[,!(colnames(one_cluster)=="Min_Conc")]
  
  y<-one_cluster$Min_Conc
  # 
  # #split into testing and training data
  # inTrain <- createDataPartition(y, p = .80, list = FALSE)[,1]
  # 
  # x_train <- x[ inTrain, ]
  # x_test  <- x[-inTrain, ]
  # 
  # y_train <- y[ inTrain]
  # y_test  <- y[-inTrain]
  
  #run RFE, this will take a bit
  #we are allowing the number of variables retained to range from 1 to all of them here
  #to change that changes input into the "sizes" variable
  set.seed(123)
  result_rfe <- rfe(x = x, 
                    y = y, 
                    sizes = c(1:(ncol(one_cluster)-1)),
                    rfeControl = control)
  
  #Put selected features into variable
  new_rf_input<-paste(predictors(result_rfe), collapse = "+")
  
  #Format those features into a formula to put in the optimized random forest model
  rf_formula<-formula(paste("Min_Conc~", new_rf_input))
  
  #test trees 100-1000
  OOB_list<-test_numtree_min_opt(ntree_list=c(seq(100,2000,100)))
  
  tree_list<-c(seq(100,2000,100))
  
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
  
  set.seed(123)
  opt_mtry<-tuneRF(one_cluster[,colnames(one_cluster) %in% predictors(result_rfe)], one_cluster[,"Min_Conc"], 
                   ntreeTry = OOB_min$tree_num, stepFactor = 1, improve = 0.5, plot = FALSE)
  
  #run optimized random forest model, with same number of trees
  set.seed(123)
  rf_model2<-randomForest(rf_formula,
                          data=one_cluster, importance=TRUE, proximity=TRUE, ntree=OOB_min$tree_num, mtry=opt_mtry[1])
  
  #pdp::partial(rf_model2, pred.var = "land_cropland", plot = TRUE)
  
  #pull out importance
  impotance_list[[i]]<-data.frame(rf_model2$importance)
  
  #pull out predicted vs actual data
  pred_actual_df[[i]]<-data.frame(rf_model2$predicted, y)
  
  #run lm model to get R2, R2 given by RF is not correct
  lm_mod<-lm(y~rf_model2$predicted)
  
  #bind MSE and lm R2 into results
  rfe_model_min_results[[i]]<-c(mean(rf_model2$mse), summary(lm_mod)$adj.r.squared)
  
  one_var_partial_data<-list()
  
  for (g in 1:length(pdp_vars)) {
    
    if(!pdp_vars[g] %in% rownames(impotance_list[[i]])) next
      
    one_var_partial_data[[g]]<-pdp::partial(rf_model2, pred.var=pdp_vars[g])
    
  }
  
  is_df <- sapply(one_var_partial_data, is.data.frame)
  
  dfList <- one_var_partial_data[is_df]
  
  var_partial_list <- mapply(cbind, dfList, 
                             "Variable"=pdp_vars[pdp_vars %in% rownames(impotance_list[[i]])], SIMPLIFY=F)
  
  colnames<-c("response", "yhat", "variable")
  
  dfList_newCols<-lapply(var_partial_list, setNames, colnames)
  
  var_partial_df<-do.call(bind_rows, dfList_newCols)
  
  all_models_partial_df[[i]]<-var_partial_df
}

#get results for og rf and rfe rf
rf_model_min_results_df<-data.frame(do.call(rbind, rf_model_min_results))
rf_model_min_results_df$Centroid_Name<-clusters
colnames(rf_model_min_results_df)<-c("MSE","R2","Centroid_Name")

rfe_model_min_results_df<-data.frame(do.call(rbind, rfe_model_min_results))
rfe_model_min_results_df$Centroid_Name<-clusters
colnames(rfe_model_min_results_df)<-c("MSE","R2","Centroid_Name")

all_models_partials_plots <- mapply(cbind, all_models_partial_df, "Centroid_Name"=clusters, SIMPLIFY=F)
all_models_partials_plots_df<-data.frame(do.call(rbind, all_models_partials_plots))

all_models_partials_plots_df_min<-all_models_partials_plots_df
all_models_partials_plots_df_min$si_concentration<-"min"

# ggplot(all_models_partials_plots_df, aes(x=response, y=yhat))+geom_line(aes(col=Centroid_Name), linewidth=0.8)+
#   facet_wrap(~variable, scales = "free")+theme_bw()+
#   scale_color_manual(values = carto_pal(n=5, "Bold"))+
#   theme(text = element_text(size = 20))

importance_list_centroid <- mapply(cbind, impotance_list, "Centroid_Name"=clusters, SIMPLIFY=F)
importance_list_centroid_vars <- lapply(importance_list_centroid, tibble::rownames_to_column)

importance_df<-do.call(bind_rows, importance_list_centroid_vars)

colnames(importance_df)<-c("Variable","Inc_MSE","Inc_Node_Purity","Centroid_Name")

vars_order_min<-c("temp","precip","cycle0","evapotrans","prop_area","npp","N","P","Max_Daylength","CV_Q","q_5","q_95","q_min_day","q_max_day",
                  "rocks_volcanic","rocks_sedimentary","rocks_plutonic","rocks_metamorphic","rocks_carbonate_evaporite",
                  "land_evergreen_needleleaf_forest","land_tundra","land_shrubland_grassland","land_cropland","land_mixed_forest",
                  "land_urban_and_built_up_land","land_barren_or_sparsely_vegetated","land_wetland","land_evergreen_broadleaf_forest")

remove_this<-setdiff(vars_order_min, importance_df$Variable)

#vars_order_min_cropped<-vars_order_min[-c(vars_order_min %in% remove_this)]

importance_df$Variable<-factor(importance_df$Variable, levels = vars_order_min)

pred_list_centroid <- mapply(cbind, pred_actual_df, "Centroid_Name"=clusters, SIMPLIFY=F)

pred_df<-do.call(bind_rows, pred_list_centroid)

colnames(pred_df)<-c("Predicted","Actual","Centroid_Name")

vars_legend<-c("Temperature","Precipitation","Green Up Day","Evapotranspiration","Maximum Snow Covered Area", "NPP",
               "N Concentration","P Concentration","Maximum Daylength","CV(Q)", "q(95)", "q(5)", "Day of Minimum Q",
               "Day of Maximum Q", "Volcanic", "Sedimentary", "Plutonic", "Metamorphic", "Carbonate/Evaporite",
               "Evergreen Needleaf Forest", "Tundra", "Shrubland/Grassland", "Cropland", "Mixed Forest", "Urban",
               "Barren", "Wetland", "Evergreen Broadleaf Forest")

p1<-ggplot(pred_df, aes(Predicted, Actual))+geom_point()+geom_abline(intercept = 0, slope = 1, col="red")+
  theme_classic()+facet_wrap(~Centroid_Name, nrow = 1)+theme(text = element_text(size=15))+
  labs(x="", y="Actual Si Concentration (mg/L)")+ggtitle("Minimum Si Concentration")+
  lims(x=c(0,10), y=c(0,10))

p1

k1<-ggplot(importance_df, mapping=aes(Centroid_Name, Variable, fill=Inc_MSE))+
  geom_tile()+
  geom_tile(data=importance_df[importance_df$Inc_MSE < 0.05,], mapping=aes(Centroid_Name, Variable), fill="grey90")+
  scale_fill_gradientn(colors = c("lightyellow","lightsalmon", "red"), breaks=c(0.05, 0.3, 0.6, 0.9, 1.2))+
  theme_classic()+labs(x="Cluster", y="Variable",fill="Increase MSE")+
  theme(text = element_text(size=15, family = "Times"))+scale_x_discrete(labels=c("FP","FT","ST","STFP","STVS"))+
  scale_y_discrete(limits=rev, labels=rev(vars_legend))+
  labs(tag="a")+ggtitle("Minimum Si Prediction")

k1

####Max Prediction####
#open lists to save important information from each cluster
#define variables wanted for PDPs for minimum Si concentration
#pdp_vars<-c("N", "P", "q_95", "CV_Q", "evapotrans", "land_evergreen_needleleaf_forest", "land_cropland")

rf_model_max_results<-list()

rfe_model_max_results<-list()

impotance_list<-list()

pred_actual_df<-list()

all_models_partial_df<-list()

####Max Prediction####
for (i in 1:length(clusters)) {
  
  print(clusters[i])
  
  one_cluster<-subset(drivers_df, drivers_df$Centroid_Name==clusters[i])
  
  #original model to predict min conc - all parameters
  remove_col<-c("Min_Conc","Centroid_Name")
  
  one_cluster<-one_cluster[,!(colnames(one_cluster) %in% remove_col)]
  
  set.seed(123)
  OOB_list<-test_numtree_max(ntree_list=c(seq(100,2000,100)))
  
  tree_list<-c(seq(100,2000,100))
  
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
  opt_mtry<-tuneRF(one_cluster[,!colnames(one_cluster) %in% "Max_Conc"], one_cluster[,"Max_Conc"], 
                   ntreeTry = OOB_min$tree_num, stepFactor = 1, improve = 0.5, plot = FALSE)
  
  set.seed(123)
  rf_model1<-randomForest(Max_Conc~.,
                          data=one_cluster, importance=TRUE, proximity=TRUE, ntree=OOB_min$tree_num, mtry=opt_mtry[1])
  
  rf_model_max_results[[i]]<-c(mean(rf_model1$mse), mean(rf_model1$rsq))
  
  x<-one_cluster[,!(colnames(one_cluster)=="Max_Conc")]
  
  y<-one_cluster$Max_Conc
  
  # inTrain <- createDataPartition(y, p = .80, list = FALSE)[,1]
  # 
  # x_train <- x[ inTrain, ]
  # x_test  <- x[-inTrain, ]
  # 
  # y_train <- y[ inTrain]
  # y_test  <- y[-inTrain]
  
  set.seed(123)
  result_rfe <- rfe(x = x, 
                    y = y, 
                    sizes = c(1:(ncol(one_cluster)-1)),
                    rfeControl = control)
  
  # Print the selected features
  new_rf_input<-paste(predictors(result_rfe), collapse = "+")
  
  rf_formula<-formula(paste("Max_Conc~", new_rf_input))
  
  #test trees 100-1000
  OOB_list<-test_numtree_max_opt(ntree_list=c(seq(100,2000,100)))
  
  tree_list<-c(seq(100,2000,100))
  
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
  
  if(length(predictors(result_rfe)) > 3){
    
    set.seed(123)
    opt_mtry<-tuneRF(one_cluster[,colnames(one_cluster) %in% predictors(result_rfe)], one_cluster[,"Max_Conc"], 
                     ntreeTry = OOB_min$tree_num, stepFactor = 1, improve = 0.5, plot = FALSE) 
    
  }else{
    
    opt_mtry<-1
    
  }
  
  set.seed(123)
  rf_model2<-randomForest(rf_formula,
                          data=one_cluster, importance=TRUE, proximity=TRUE, ntree=OOB_min$tree_num, mtry=opt_mtry[1])
  
  rfe_model_max_results[[i]]<-c(mean(rf_model2$mse), mean(rf_model2$rsq))
  
  impotance_list[[i]]<-data.frame(rf_model2$importance)
  
  pred_actual_df[[i]]<-data.frame(rf_model2$predicted, y)
  
  one_var_partial_data<-list()
  
  for (g in 1:length(pdp_vars)) {
    
    if(!pdp_vars[g] %in% rownames(impotance_list[[i]])) next
    
    one_var_partial_data[[g]]<-pdp::partial(rf_model2, pred.var=pdp_vars[g])
    
  }
  
  is_df <- sapply(one_var_partial_data, is.data.frame)
  
  dfList <- one_var_partial_data[is_df]
  
  var_partial_list <- mapply(cbind, dfList, 
                             "Variable"=pdp_vars[pdp_vars %in% rownames(impotance_list[[i]])], SIMPLIFY=F)
  
  colnames<-c("response", "yhat", "variable")
  
  dfList_newCols<-lapply(var_partial_list, setNames, colnames)
  
  var_partial_df<-do.call(bind_rows, dfList_newCols)
  
  all_models_partial_df[[i]]<-var_partial_df
  
}

rf_model_max_results_df<-data.frame(do.call(rbind, rf_model_max_results))
rf_model_max_results_df$Centroid_Name<-clusters
colnames(rf_model_max_results_df)<-c("MSE","R2","Centroid_Name")

rfe_model_max_results_df<-data.frame(do.call(rbind, rfe_model_max_results))
rfe_model_max_results_df$Centroid_Name<-clusters
colnames(rfe_model_max_results_df)<-c("MSE","R2","Centroid_Name")

all_models_partials_plots <- mapply(cbind, all_models_partial_df, "Centroid_Name"=clusters, SIMPLIFY=F)
all_models_partials_plots_df<-data.frame(do.call(rbind, all_models_partials_plots))

all_models_partials_plots_df_max<-all_models_partials_plots_df
all_models_partials_plots_df_max$si_concentration<-"max"

# ggplot(all_models_partials_plots_df, aes(x=response, y=yhat))+geom_line(aes(col=Centroid_Name), linewidth=0.8)+
#   facet_wrap(~variable, scales = "free")+theme_bw()+
#   scale_color_manual(values = carto_pal(n=5, "Bold"))+
#   theme(text = element_text(size = 20))

importance_list_centroid <- mapply(cbind, impotance_list, "Centroid_Name"=clusters, SIMPLIFY=F)
importance_list_centroid_vars <- lapply(importance_list_centroid, rownames_to_column)

importance_df<-do.call(bind_rows, importance_list_centroid_vars)

unique(importance_df$rowname)

colnames(importance_df)<-c("Variable","Inc_MSE","Inc_Node_Purity","Centroid_Name")

vars_order_max<-c("temp","precip","cycle0","evapotrans","prop_area","npp","N","P","Max_Daylength","CV_Q","q_5","q_95","q_min_day","q_max_day",
                   "rocks_volcanic","rocks_sedimentary","rocks_plutonic","rocks_metamorphic","rocks_carbonate_evaporite",
                   "land_evergreen_needleleaf_forest","land_tundra","land_shrubland_grassland","land_cropland","land_mixed_forest",
                   "land_urban_and_built_up_land","land_barren_or_sparsely_vegetated","land_wetland","land_evergreen_broadleaf_forest")

#remove_this<-setdiff(vars_order_max, importance_df$Variable)

#vars_order_max_cropped<-vars_order_max[-c(vars_order_max %in% remove_this)]

importance_df$Variable<-factor(importance_df$Variable, levels=vars_order_max)

pred_list_centroid <- mapply(cbind, pred_actual_df, "Centroid_Name"=clusters, SIMPLIFY=F)

pred_df<-do.call(bind_rows, pred_list_centroid)

colnames(pred_df)<-c("Predicted","Actual","Centroid_Name")

p2<-ggplot(pred_df, aes(Predicted, Actual))+geom_point()+geom_abline(intercept = 0, slope = 1, col="red")+
  theme_classic()+facet_wrap(~Centroid_Name, nrow = 1)+theme(text = element_text(size=15))+
  labs(x="Predicted Si Concentration (mg/L)", y="Actual Si Concentration (mg/L)")+ggtitle("Maximum Si Concentration")+
  lims(x=c(0,14), y=c(0,14))

p2

k2<-ggplot(importance_df, mapping=aes(Centroid_Name, Variable, fill=Inc_MSE))+
  geom_tile()+
  geom_tile(data=importance_df[importance_df$Inc_MSE < 0.05,], mapping=aes(Centroid_Name, Variable), fill="grey90")+
  scale_fill_gradientn(colors = c("lightyellow","lightsalmon", "red"), breaks=c(0.05, 0.5, 1, 1.5, 2))+
  theme_classic()+labs(x="Cluster", y="Variable",fill="Increase MSE")+
  theme(text = element_text(size=15, family = "Times"))+scale_x_discrete(labels=c("FP","FT","ST","STFP","STVS"))+
  scale_y_discrete(limits=rev, labels=rev(vars_legend))+
  labs(tag="b")+ggtitle("Maximum Si Prediction")

k2

importance_df %>%
  group_by(Variable) %>%
  summarise(mean(Inc_MSE))


pdf("Actual_Predicted_MinMax_SiConc.pdf", width = 18, height = 8)

ggarrange(p1, p2, nrow = 2, align = "v")

dev.off()

tiff("VariablesRetained_MinMax_SiConc_March2024.tiff", width = 16, height = 10, units="in", res = 300)

ggarrange(k1, k2, nrow = 1, align = "h")

dev.off()

all_model_results<-bind_rows(rfe_model_min_results_df, rfe_model_max_results_df, .id="model")

write.csv(all_model_results, "Model_Output_MinMax.csv")


###partial dependence plots####

pdps<-bind_rows(all_models_partials_plots_df_max, all_models_partials_plots_df_min)

vars<-unique(pdps$variable)

x_axis_labs<-c("uM", "uM", "unitless", "proportion of watershed", "mm/day", "proportion of watershed")

title<-c("N Concentration", "P Concentration", "CV(Q)", "Cropland", "Precipitation", "Evergreen Needleleaf Forest")

tag_val<-c("a","b","c","e","d","f")

unique(pdps$Centroid_Name)

cols<-c("Fall Peak" = "#7F3C8D", "Fall Trough" = "#11A579", "Spring Trough" = "#3969AC",
        "Spring Trough, Fall Peak" = "#F2B701", "Spring Trough, Variable Summer" = "#A5AA99")

colnames(pdps)<-c(colnames(pdps)[1:3], "Cluster", "Prediction Model")

pdps$`Prediction Model`<-ifelse(pdps$`Prediction Model`=="min", "Minimum", "Maximum")

pdps$`Prediction Model`<-factor(pdps$`Prediction Model`, levels = c("Minimum", "Maximum"))

#filter out datat that is higher than 800 uM for N and 30 uM for P
pdps_cropped<-pdps %>%
  filter(!((variable=="N" & response > 800)|(variable=="P" & response > 30)))

plot_list_pdp<-list()

for (i in 1:length(vars)) {
  
  one_pdp<-pdps_cropped[pdps_cropped$variable==vars[i],]
  
  plot_list_pdp[[i]]<-ggplot(one_pdp, aes(response, yhat))+
    geom_line(aes(col=Cluster, lty=`Prediction Model`),lwd=1)+theme_classic()+
    scale_color_manual(values = cols)+
    theme(text = element_text(size = 15), legend.position = "null", plot.title = element_text(size=15))+
    labs(y="", x=x_axis_labs[i], tag = tag_val[i])+ggtitle(title[i])

}

gg_plot<-ggarrange(plotlist = list(plot_list_pdp[[1]], plot_list_pdp[[2]], plot_list_pdp[[3]], 
                       plot_list_pdp[[5]], plot_list_pdp[[4]], plot_list_pdp[[6]]), 
          common.legend = TRUE, legend = "right")

pdf("PDP_min_and_max_cropped_NP_axes.pdf", width = 15, height = 8.5, family = "Times")

annotate_figure(gg_plot, left = textGrob("Marginal Variable Impact (yhat)", rot = 90, vjust = 1, gp = gpar(cex = 1.3)))

dev.off()











