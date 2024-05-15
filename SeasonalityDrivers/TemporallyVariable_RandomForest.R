##temporal cluster prediction###
#install.packages("desplot")
require(desplot)
require(rcartocolor)
require(ggpubr)
require(tidyr)
require(dplyr)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

# annual_driver_link<-"https://drive.google.com/file/d/1lThwvCvJ5NVHbODBiWMyl5zjjCaaCWCE/view?usp=drive_link"
# 
# file_get<-drive_get(as_id(annual_driver_link))
# 
# drive_download(file_get$drive_resource, overwrite = T)

drivers<-read.csv("Annual_Driver_Data.csv")

drivers<-subset(drivers, !drivers$driver %in% c("cycle1","num_days"))

drivers_cropped<-subset(drivers, drivers$year > 2001 & drivers$year < 2020)

drivers_cast<-drivers_cropped %>% 
  dplyr::select(!c(units_annual,X)) %>%
  tidyr::pivot_wider(names_from = driver, values_from=value)

# Q_url<-"https://drive.google.com/file/d/16wHi1jmUPyvIoUutrdheV0195YWvUTXT/view?usp=drive_link"
# 
# file_get<-drive_get(as_id(Q_url))
# 
# drive_download(file_get$drive_resource, overwrite = T)

#read in Q input file  
q_df<-read.csv("WRTDS-input_discharge.csv")
q_df$Date<-as.Date(q_df$Date)

q_df$year<-lubridate::year(q_df$Date)

q_stats<-q_df %>%
  dplyr::group_by(Stream_ID, year) %>%
  dplyr::summarise(mean_q = mean(Q), med_q=median(Q), sd_q=sd(Q), min_Q=min(Q), max_Q=max(Q),
                   q_95=quantile(Q, 0.95), q_5=quantile(Q, 0.05))

q_min_stats<-q_df %>%
  dplyr::group_by(Stream_ID, year) %>%
  dplyr::slice_min(Q)

q_min_stats$q_min_day<-format(as.Date(q_min_stats$Date),"%j")

q_max_stats<-q_df %>%
  dplyr::group_by(Stream_ID, year) %>%
  dplyr::slice_max(Q)

q_max_stats$q_max_day<-format(as.Date(q_max_stats$Date),"%j")

q_max_stats <- q_max_stats %>%
  dplyr::group_by(Stream_ID, year) %>%
  dplyr::filter(q_max_day==min(q_max_day))

q_min_stats <- q_min_stats %>%
  dplyr::group_by(Stream_ID, year) %>%
  dplyr::filter(q_min_day==min(q_min_day))

q_stats_all<-left_join(q_stats, q_min_stats, by=c("Stream_ID", "year"))

q_stats_all<-left_join(q_stats_all, q_max_stats, by=c("Stream_ID", "year"))

q_stats_all<-q_stats_all[!colnames(q_stats_all) %in% c("Date.x","Q.x","Date.y","Q.y")]

q_stats_all$Stream_Name<-sub("^[^_]*__", "", q_stats_all$Stream_ID)

all_annual_vars<-left_join(drivers_cast, q_stats_all, by=c("Stream_Name", "year"))

all_annual_vars$cv_q<-all_annual_vars$sd_q/all_annual_vars$mean_q

all_annual_vars<-all_annual_vars[!colnames(all_annual_vars) %in% c("Stream_ID", "mean_q", "med_q","min_Q","max_Q","sd_q")]

clusters<-read.csv("AllSiteYearClusters.csv")

clusters<-clusters[,c("Site", "Year", "clust")]  

colnames(clusters)<-c("Stream_Name", "year", "Cluster")  

tot<-left_join(clusters, all_annual_vars, by=c("Stream_Name", "year"))

tot<-subset(tot, tot$year > 2001 & tot$year < 2020)

keep_these<-tot %>%
  group_by(Stream_Name) %>%
  tally() %>%
  filter(n > 9) %>%
  select(Stream_Name)

tot_10years<-tot[tot$Stream_Name %in% keep_these$Stream_Name,]  

tot_10years_clust<-tot_10years

df <- apply(tot_10years_clust,2,as.character)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/USGS_DataReview_SeasonalityDrivers")

write.csv(df, "SeasonalityDrivers_USGS_AnnualDrivers.csv")

tot_10years_clust$Centroid_Name<-case_when(tot_10years_clust$Cluster== 1~"ST",
                                            tot_10years_clust$Cluster== 2~"FT",
                                            tot_10years_clust$Cluster== 3~"FP",
                                            tot_10years_clust$Cluster== 4~"STFP",
                                            tot_10years_clust$Cluster== 5~"STVS")

count_by_group<-tot_10years %>%
  group_by(Stream_Name) %>%
  count(Cluster)

count_by_group<-count_by_group %>% 
  mutate_all(~replace(., is.na(.), 0))
  
num_years<-tot_10years %>%
  group_by(Stream_Name) %>%
  tally()

mat_df<-count_by_group %>%
  left_join(num_years, by="Stream_Name")

colnames(mat_df)<-c("Stream_Name","cluster", "cluster_count", "total_count")

mat_df$cluster_prop<-mat_df$cluster_count/mat_df$total_count

num_clusters<-mat_df %>%
  group_by(Stream_Name) %>%
  tally()

# two_clusts<-subset(num_clusters, num_clusters$n==2)
# 
# three_clusts<-subset(num_clusters, num_clusters$n==3)
# 
# four_clusts<-subset(num_clusters, num_clusters$n==4)
# 
# mat_df_two<-subset(mat_df, mat_df$Stream_Name %in% two_clusts$Stream_Name)
# 
# mat_df_two_shorterm<-subset(mat_df_two, mat_df_two$cluster_prop < 0.25)
# 
# mat_df_two_longterm<-subset(mat_df_two, !mat_df_two$Stream_Name %in% mat_df_two_shorterm$Stream_Name)
# 
# tot_four_longterm<-subset(tot_10years_clust, tot_10years_clust$Stream_Name %in% four_clusts$Stream_Name)
# 
# table(num_clusters$n)
# 
# one_cluster<-subset(mat_df, mat_df$cluster_prop==1)

modal_clust<-mat_df %>%
  group_by(Stream_Name) %>%
  slice_max(cluster_prop) %>%
  distinct(Stream_Name, .keep_all = TRUE)

mat_df<-left_join(mat_df, modal_clust[c(1,2)], by="Stream_Name")


colnames(mat_df)<-c("Stream_Name", "cluster","cluster_count","total_count","cluster_prop","modal_cluster")

mat_df$Centroid_Name<-case_when(mat_df$modal_cluster== 1~"ST",
                                mat_df$modal_cluster== 2~"FT",
                                mat_df$modal_cluster== 3~"FP",
                                  mat_df$modal_cluster== 4~"STFP",
                                mat_df$modal_cluster== 5~"STVS")

tot_10years_clust<-left_join(tot_10years_clust, mat_df, by=c("Stream_Name"))

mat_df$Centroid_Name<-factor(mat_df$Centroid_Name, levels = c("FP", "FT", "ST", "STFP", "STVS"))

pdf("Membership_Shifting.pdf", width = 18, height = 12, family = "Times")

#visualize cluster membership over time
p1<-ggplot(tot_10years_clust, aes(x=year, y=Stream_Name))+
  geom_tile(aes(fill=Centroid_Name.x))+
  scale_fill_manual(values = carto_pal(n=5, "Bold"))+
  scale_y_discrete(limits=unique(rev(mat_df$Stream_Name[order(mat_df$Centroid_Name)])))+
  labs(x="Year", y="", fill="Regime Membership")+
  theme_classic()+
  theme(axis.text.y = element_blank(), text = element_text(size=20), axis.ticks.y = element_blank())

dev.off()

#visualize proportional cluster membership
p2<-ggplot(mat_df, aes(x=as.character(cluster), y=Stream_Name))+
  geom_tile(aes(fill=cluster_prop))+
  scale_fill_gradient(low="grey90", high="#3969AC")+
  scale_y_discrete(limits=unique(rev(mat_df$Stream_Name[order(mat_df$Centroid_Name)])))+
  scale_x_discrete(limits=c("3","2","1","4","5"), labels=c("FP","FT","ST","STFP","STVS"))+
  theme_classic()+
  theme(axis.text.y = element_blank(), text = element_text(size=20), axis.ticks.y = element_blank())+
  labs(x="Regime",y="", fill="Membership Proportion", tag = "a")

pdf("Membership_Propotion_Timing_UpdatedFeb2024.pdf", width = 15, height = 8, family = "Times")

ggarrange(p2, p1)

dev.off()

#remove any rows with na
tot_10years_clean<- tot_10years %>%
  drop_na()

#convert green up day to julian day
tot_10years_clean$cycle0<-as.Date(unlist(tot_10years_clean$cycle0))

tot_10years_clean$cycle0<-format(as.Date(tot_10years_clean$cycle0), "%j")

#add centroid name from cluster number
tot_10years_clean$Centroid_Name<-case_when(tot_10years_clean$Cluster== 1~"ST",
                                           tot_10years_clean$Cluster== 2~"FT",
                                           tot_10years_clean$Cluster== 3~"FP",
                                           tot_10years_clean$Cluster== 4~"STFP",
                                           tot_10years_clean$Cluster== 5~"STVS")

#turn centroid name to factor
tot_10years_clean$Centroid_Name<-as.factor(tot_10years_clean$Centroid_Name)

tot_10years_clean[,c(4:14)]<-lapply(tot_10years_clean[,c(4:14)], as.numeric)

tot_10years_clean<-tot_10years_clean[complete.cases(tot_10years_clean),]

#remove any sites with less than 10 years of data
check_numyears<-tot_10years_clean %>%
  group_by(Stream_Name) %>%
  tally() %>%
  filter(n < 10)

tot_10years_clean<-tot_10years_clean[!c(tot_10years_clean$Stream_Name %in% check_numyears$Stream_Name),]

#remove column names
tot_df<-tot_10years_clean[,!colnames(tot_10years_clean) %in% c("Stream_Name", "year", "Cluster")]

test_numtree_annual <- function(ntree_list) {
  
  OOB<-list()
  
  for (i in 1:length(ntree_list)) {
    
    print(ntree_list[[i]])
    
    set.seed(123)
    
    rf_model<-randomForest(Centroid_Name~.,
                           data=tot_df, importance=TRUE, proximity=TRUE, ntree=ntree_list[[i]],sampsize=c(rep(260,5)))
    
    OOB[[i]]<-rf_model$err.rate[,1]
    
  }
  
  return(OOB)
  
}


#original model, all parameters
#test number of trees 100-1000
OOB_list<-test_numtree_annual(c(seq(100,2000,100)))

tre_list<-c(seq(100,2000,100))

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

set.seed(123)
tuneRF(tot_df[,c(1:11)], tot_df[,12], ntreeTry = 2000, stepFactor = 1, improve = 0.5, plot = FALSE)

set.seed(123)
rf_model1<-randomForest(Centroid_Name~.,
                        data=tot_df, importance=TRUE, proximity=TRUE, ntree=2000,sampsize=c(260,260,260,260,260), mtry=3)

rf_model1

randomForest::varImpPlot(rf_model1)

#set seeds for RFE
size=ncol(tot_df)-1
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
x<-tot_df[,!(colnames(tot_df)=="Centroid_Name")]

y<-tot_df$Centroid_Name

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
test_numtree_annual_optimized <- function(ntree_list) {
  
  OOB<-list()
  
  for (i in 1:length(ntree_list)) {
    
    set.seed(123)
    rf_model<-randomForest(rf_formula,
                           data=tot_df, importance=TRUE, proximity=TRUE, ntree=ntree_list[[i]],sampsize=c(260,260,260,260,260))
    OOB[[i]]<-rf_model$err.rate[,1]
    
  }
  
  return(OOB)
  
}

OOB_list<-test_numtree_annual_optimized(c(seq(100,2000,100)))

tre_list<-c(seq(100,2000,100))

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
kept_drivers<-tot_df[,c(colnames(tot_df) %in% predictors(result_rfe))]

set.seed(123)
tuneRF(kept_drivers, tot_df[,1], ntreeTry = 2000, stepFactor = 1, improve = 0.5, plot = FALSE)

set.seed(123)
rf_model2<-randomForest(rf_formula,
                        data=tot_df, importance=TRUE, proximity=TRUE, ntree=2000,sampsize=c(260,260,260,260,260), mtry=3)

rf_model2

#plot confusion matrix
df<-as.data.frame(rf_model2$confusion)

df_new<-data.frame(t(apply(df[,c(1:5)],1, function(x) x/sum(x))))
colnames(df_new)<-c("FP", "FT", "ST", "STFP", "STVS")
rownames(df_new)<-c("FP", "FT", "ST", "STFP", "STVS")
df_new$cluster<-rownames(df_new)

df_new_melt<-melt(df_new, id.vars = "cluster")
df_new_melt$same<-ifelse(df_new_melt$cluster==df_new_melt$variable, "yes","no")

tiff("Annual_Regime_Confusion_Matrix.tiff", width = 8, height = 7, units = "in", res = 300)

#visualize matrix
ggplot(df_new_melt, aes(variable, cluster))+geom_raster(aes(fill=same))+
  scale_fill_manual(values=c("yes"="forestgreen", "no"="salmon"))+
  geom_text(aes(label=round(value, 2)), size=8, family="Times")+theme_classic()+labs(x="",y="",fill="")+
  theme(legend.position = "null", text = element_text(size = 22, family = "Times"))+
  ggtitle("Annual Regime Prediction Confusion Matrix")

dev.off()

importance_df<-data.frame(rf_model2$importance)
importance_df$driver<-rownames(importance_df)

vars_order<-importance_df %>%
  dplyr::arrange(desc(MeanDecreaseAccuracy), driver) %>%
  dplyr::select(driver)

importance_melt<-melt(importance_df[,-7], id.vars = "driver")

importance_melt$driver<-factor(importance_melt$driver, levels = vars_order$driver)

vars_labels<-c("Temperature", "Evapotranspiration", "q(5)", "q(95)", "Maximum Snow Covered Area", "Precipitation",
               "NPP", "CV(Q)", "Green Up Day")

tiff("Annual_Regime_Variable_Importance.tiff", width = 10, height = 10, units = "in", res = 300)

ggplot(importance_melt, aes(variable, driver))+geom_raster(aes(fill=value))+
  scale_fill_gradient(low="grey90", high="red")+theme_classic()+labs(x="", y="Variable",fill="Mean Decrease Accuracy")+
  theme(text = element_text(size=15, family = "Times"))+scale_y_discrete(limits=rev, labels=rev(vars_labels))+
  scale_x_discrete(labels=c("FP","FT","ST","STFP","STVS","Overall Model"))+
  ggtitle("Annual Regime Prediction Variable Importance")

dev.off()

###plot most important variables across clusters
import_factors<-tot_10years_clean[,c("Centroid_Name","temp", "evapotrans", "q_5",
                              "q_95","prop_area","precip", "Stream_Name")]

import_factors$q_5<-log(import_factors$q_5)

import_factors$q_95<-log(import_factors$q_95)

import_factors_melt<-melt(import_factors, id.vars = c("Centroid_Name", "Stream_Name"))

colnames(import_factors_melt)[1]<-"Cluster"

vars<-unique(import_factors_melt$variable)

y_axis_labs<-c("deg C", "kg/m2", "log(cms)", "log(cms)", "proportion of watershed", "mm/day")

title<-c("Temperature", "Evapotranspiration", "Log(q(5))", "Log(q(95))", "Maximum Snow Covered Area", "Precipitation")

tag_val<-c("a","b","c","d","e","f")

variable_plot_list<-list()

for (i in 1:length(vars)) {
  
  one_var<-import_factors_melt[import_factors_melt$variable==vars[i],]
  
  variable_plot_list[[i]]<-
    ggplot(one_var, aes(Cluster, value))+
    geom_boxplot(aes(fill=Cluster), alpha=0.5, outlier.shape = NA)+
    theme_bw()+
    theme(text = element_text(size = 20), legend.position = "null", plot.title = element_text(size=15))+
    geom_jitter(aes(col=Cluster))+
    scale_fill_manual(values=carto_pal(n=5, "Bold"))+scale_color_manual(values=carto_pal(n=5, "Bold"))+
    theme(axis.text.x = element_blank())+
    labs(x="", y=y_axis_labs[i], tag = tag_val[i])+ggtitle(title[i])
  
}

pdf("Annual_Prediction_MostImportVars.pdf", width = 14, height = 9, family="Times")

ggarrange(plotlist = variable_plot_list, common.legend = TRUE, legend = "right")

dev.off()

gg_plot

pdf("MostImportVars.pdf", width = 14, height = 9, family="Times")

ggplot(import_factors_melt, aes(Centroid_Name, value))+
  geom_boxplot(aes(fill=Centroid_Name), alpha=0.5, outlier.shape = NA)+facet_wrap(~variable, scales = "free")+theme_bw()+
  theme(text = element_text(size = 20))+labs(x="", y="Driver Value")+
  geom_jitter(aes(col=Centroid_Name))+
  scale_fill_manual(values=carto_pal(n=5, "Bold"))+scale_color_manual(values=carto_pal(n=5, "Bold"))+
  theme(axis.text.x = element_blank())+labs(fill="Cluster", color="Cluster")

dev.off()






