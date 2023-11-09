require(googledrive)
require(ggplot2)
require(dplyr)
require(rcartocolor)
require(reshape2)
require(stringr)
require(comperes)
require(EflowStats)
require(ggpubr)
require(tidyr)

#get link to folder where WRTDS inputs are saved
#link<-"https://drive.google.com/drive/folders/1QEofxLdbWWLwkOTzNRhI6aorg7-2S3JE?usp=share_link"

#get folder
#folder = drive_get(as_id(link))

#get list of csv files from folder
#csv_files = drive_ls(folder, type="csv")

#check working directory where files will be stored locally; separate folder within project folder
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#download each file to the working directory; files are saved locally
#for (i in 1:length(csv_files$drive_resource)) {
#  drive_download(csv_files$drive_resource[i],  overwrite=T)
#}

#read in chem input file
chem_df<-read.csv("WRTDS-input_chemistry.csv")
#crop to only include DSi
chem_si<-subset(chem_df, chem_df$variable=="DSi")

#read in Q input file  
q_df<-read.csv("WRTDS-input_discharge.csv")

# pdf("Q_Dist.pdf")
# 
# streamsQ<-unique(q_df$Stream_ID)
# 
# for (i in 1:length(streamsQ)) {
#   
#   site<-subset(chem_si, chem_si$Stream_ID==streamsQ[i])
#   
#   p1<-ggplot(site, aes(x=value_mgL))+geom_histogram()
#   
#   print(p1)
#   
# }
# 
# dev.off()

#get mean and SD of Si
chem_df_cv<-chem_si %>%
  dplyr::group_by(Stream_ID) %>%
  dplyr::summarise(sd_Si=sd(value_mgL), mean_Si=mean(value_mgL), med_Si=median(value_mgL))

#get mean and SD of Q
q_df_cv<-q_df %>%
  dplyr::group_by(Stream_ID) %>%
  dplyr::summarise(sd_Q=sd(Q), mean_Q=mean(Q), med_Q=median(Q))

#merge them into one
cv_tot<-merge(chem_df_cv, q_df_cv, by="Stream_ID")

#calculate CVC/CVQ
cv_tot$cvc_cvq<-(cv_tot$mean_Q/cv_tot$mean_Si)*(cv_tot$sd_Si/cv_tot$sd_Q)

#get list of streams
streams<-unique(chem_si$Stream_ID)

#open list to calculate slope and r2 of each stream
slope_list<-list()
rsquared_list<-list()

#calculate CQ slope and CQ r2 for Si-Q for each stream
for (i in 1:length(streams)) {
  
  si<-subset(chem_si, chem_si$Stream_ID==streams[i])
  si$Date<-as.Date(si$Date)
  
  q<-subset(q_df, q_df$Stream_ID==streams[i])
  q$Date<-as.Date(q$Date)
  
  tot<-merge(si, q, by="Date")
  
  tot<-tot[tot$Q > 0,]
  
  lm1<-lm(log(value_mgL)~log(Q), tot)
  
  #ggplot(tot, aes(log(Q), log(value_mgL)))+geom_point()
  
  sum<-summary(lm1)
  
  slope_list[[i]]<-sum$coefficients[2,1]
  
  rsquared_list[[i]]<-sum$r.squared
  
}

#put slopes into df
slope_df<-as.data.frame(do.call(rbind, slope_list))
slope_df$Stream_ID<-streams
colnames(slope_df)[1]<-"slope"

#put r2 into df
r2_df<-as.data.frame(do.call(rbind, rsquared_list))
r2_df$Stream_ID<-streams
colnames(r2_df)[1]<-"r2"

#merge cv, slope, and r2 into df
tot<-merge(cv_tot, slope_df, by="Stream_ID")
tot<-merge(tot, r2_df, by="Stream_ID")

tot$Site<-sub(".*_", "", tot$Stream_ID)

finn<-read.csv("FinnishSites.csv")

for (i in 1:nrow(finn)) {
  
  site_id<-finn[i,2]
  
  row_num<-which(tot$Site==site_id)
  
  tot[row_num, "Site"]<-finn[i,1]
  
}

tot$Site<-ifelse(tot$Site=="TORNIONJ KUKKOLA 14310  ", "TORNIONJ KUKKOLA 14310", tot$Site)

tot$Site<-ifelse(tot$Site=="SIMOJOKI AS. 13500      ", "SIMOJOKI AS. 13500", tot$Site)

#read in cluster data for average cluster, modal cluster, number of clusters
si_clust<-read.csv("MonthClustersNov2022.csv")

si_clust_mode<-read.csv("SiClustersMode.csv")

siteyear_clust_num<-read.csv("SiNumClusters.csv")

SiStab<-read.csv("StabilitySiClusters.csv")
SiStab<-SiStab[,c(2,7)]
colnames(SiStab)[2]<-"SiStab"

#merge and clean
si_clist_mode_mean<-merge(si_clust, si_clust_mode, by="Site")

si_clist_mode_mean<-si_clist_mode_mean[,-c(2,18)]

colnames(si_clist_mode_mean)[2:13]<-seq(1,12,1)

si_clust_mode_mean_num<-merge(si_clist_mode_mean, siteyear_clust_num, by="Site")

si_clust_mode_mean_num<-si_clust_mode_mean_num[,-18]

si_clust_mode_mean_num<-merge(si_clust_mode_mean_num, SiStab, by="Site")

#calculate clster stability
si_clust_mode_mean_num$clust_stab<-1+((5-si_clust_mode_mean_num$num_clusts)/5)

#calculate overall stability
si_clust_mode_mean_num$overall_stab<-si_clust_mode_mean_num$SiStab/si_clust_mode_mean_num$clust_stab

#merge with tot which includes si mean, sd, q mean, sd, cvc/cvq
tot<-merge(tot, si_clust_mode_mean_num, by="Site")

#create df of centroid names
centroid_names<-as.data.frame(t(tibble("1"="Spring Trough",
                                       "2"="Fall Trough",
                                       "3"="Fall Peak",
                                       "4"="Spring Trough, Fall Peak",
                                       "5"="Spring Trough, Variable Summer")))
centroid_names$mode<-c(1:5)

colnames(centroid_names)<-c("Centroid_Name", "mode")

#merge
tot<-merge(tot, centroid_names, by="mode")

tot<-tot[!duplicated(tot$Site),]

#anova on median Si by cluster
##perform ANOVA on climate
model<-aov(med_Si~Centroid_Name, data=tot)
summary(model)

centroid_tukey<-TukeyHSD(model, conf.level = 0.95)
plot(centroid_tukey)

#convert to df
tukey_df<-as.data.frame(centroid_tukey$Centroid_Name)
tukey_df<-rownames_to_column(tukey_df)
tukey_df$test1<-str_extract(tukey_df$rowname, "[^-]+")
tukey_df$test2<-str_replace(tukey_df$rowname, '.+-(.+)', '\\1')

colnames(tukey_df)[5]<-"p_val"

p_mat<-tukey_df %>% long_to_mat("test1", "test2", "p_val")
mat_long<-melt(p_mat)
mat_long$sig5<-ifelse(mat_long$value <= 0.05, "yes", "no")

p1<-ggplot(mat_long, aes(Var1, Var2))+geom_raster(aes(fill=sig5))+theme_classic()+
  scale_fill_manual(values = c("yes" = "#008080", "no" = "#ca562c"), na.value = "white")+
  labs(x="", y="", fill="Significant", tag="a")+theme(text = element_text(size = 20, family="Times"), 
                                             axis.text.x = element_text(angle=45, hjust=1))+
  scale_x_discrete(labels=c("FT", "ST", "STFP", "STVS"))+
  scale_y_discrete(labels=c("FP", "FT", "ST", "STFP"))+
  ggtitle("Median Si Concentration")


#anova on median Si by cluster
##perform ANOVA on climate
model<-aov(med_Q~Centroid_Name, data=tot)
summary(model)

centroid_tukey<-TukeyHSD(model, conf.level = 0.95)
plot(centroid_tukey)

#convert to df
tukey_df<-as.data.frame(centroid_tukey$Centroid_Name)
tukey_df<-rownames_to_column(tukey_df)
tukey_df$test1<-str_extract(tukey_df$rowname, "[^-]+")
tukey_df$test2<-str_replace(tukey_df$rowname, '.+-(.+)', '\\1')

colnames(tukey_df)[5]<-"p_val"

p_mat<-tukey_df %>% long_to_mat("test1", "test2", "p_val")
mat_long<-melt(p_mat)
mat_long$sig5<-ifelse(mat_long$value <= 0.05, "yes", "no")

p2<-ggplot(mat_long, aes(Var1, Var2))+geom_raster(aes(fill=sig5))+theme_classic()+
  scale_fill_manual(values = c("yes" = "#008080", "no" = "#ca562c"), na.value = "white")+
  labs(x="", y="", fill="Significant", tag="b")+theme(text = element_text(size = 20, family="Times"), 
                                             axis.text.x = element_text(angle=45, hjust=1))+
  scale_x_discrete(labels=c("FT", "ST", "STFP", "STVS"))+
  scale_y_discrete(labels=c("FP", "FT", "ST", "STFP"))+
  ggtitle("Median Q")

#anova on median Si by cluster
##perform ANOVA on climate
model<-aov(slope~Centroid_Name, data=tot)
summary(model)

centroid_tukey<-TukeyHSD(model, conf.level = 0.95)
plot(centroid_tukey)

#convert to df
tukey_df<-as.data.frame(centroid_tukey$Centroid_Name)
tukey_df<-rownames_to_column(tukey_df)
tukey_df$test1<-str_extract(tukey_df$rowname, "[^-]+")
tukey_df$test2<-str_replace(tukey_df$rowname, '.+-(.+)', '\\1')

colnames(tukey_df)[5]<-"p_val"

p_mat<-tukey_df %>% long_to_mat("test1", "test2", "p_val")
mat_long<-melt(p_mat)
mat_long$sig5<-ifelse(mat_long$value <= 0.05, "yes", "no")

p3<-ggplot(mat_long, aes(Var1, Var2))+geom_raster(aes(fill=sig5))+theme_classic()+
  scale_fill_manual(values = c("yes" = "#008080", "no" = "#ca562c"), na.value = "white")+
  labs(x="", y="", fill="Significant", tag="c")+theme(text = element_text(size = 20, family="Times"), 
                                             axis.text.x = element_text(angle=45, hjust=1))+
  scale_x_discrete(labels=c("FT", "ST", "STFP", "STVS"))+
  scale_y_discrete(labels=c("FP", "FT", "ST", "STFP"))+
  ggtitle("CQ Slope")


#anova on median Si by cluster
##perform ANOVA on climate
model<-aov(cvc_cvq~Centroid_Name, data=tot)
summary(model)

centroid_tukey<-TukeyHSD(model, conf.level = 0.95)
plot(centroid_tukey)

#convert to df
tukey_df<-as.data.frame(centroid_tukey$Centroid_Name)
tukey_df<-rownames_to_column(tukey_df)
tukey_df$test1<-str_extract(tukey_df$rowname, "[^-]+")
tukey_df$test2<-str_replace(tukey_df$rowname, '.+-(.+)', '\\1')

colnames(tukey_df)[5]<-"p_val"

p_mat<-tukey_df %>% long_to_mat("test1", "test2", "p_val")
mat_long<-melt(p_mat)
mat_long$sig5<-ifelse(mat_long$value <= 0.05, "yes", "no")

p4<-ggplot(mat_long, aes(Var1, Var2))+geom_raster(aes(fill=sig5))+theme_classic()+
  scale_fill_manual(values = c("yes" = "#008080", "no" = "#ca562c"), na.value = "white")+
  labs(x="", y="", fill="Significant", tag="d")+theme(text = element_text(size = 20, family="Times"), 
                                             axis.text.x = element_text(angle=45, hjust=1))+
  scale_x_discrete(labels=c("FT", "ST", "STFP", "STVS"))+
  scale_y_discrete(labels=c("FP", "FT", "ST", "STFP"))+
  ggtitle("CVc/CVq")

tiff("CQ_Q_Si_SigMatrix.tiff", width = 12, height = 10, units = "in", res = 300)

ggarrange(p1, p2, p3, p4)

dev.off()

count_table<-as.data.frame(table(tot$Centroid_Name, tot$Name))

count_table<-tot %>% 
  group_by(Centroid_Name, Name) %>% 
  summarise(count=n())

count_table<-subset(count_table, count_table$n > 1)
count_table<-subset(count_table, count_table$Name!="Humid Temperate")

chisq.test(count_table$Freq)

pairwise_chisq_gof_test(count_table$Freq)

table(tot$Name)

count_matrix<-as.matrix(table(tot$Centroid_Name, tot$Name))

fisher.test(count_matrix, simulate.p.value = TRUE)

dimnames(count_matrix)[[1]]<-c("FP", "FT", "ST", "STFP", "STVS")
dimnames(count_matrix)[[2]]<-rep("", 8)

pdf("ClusterMosaic.pdf", width = 7, height = 7, family="Times")

mosaicplot(count_matrix,
           main = "",
           color = carto_pal(n=8, "Bold"), cex.axis = 1.5, ylab = ""
)

colnames(count_table)<-c("Cluster", "CZ", "Freq")

ggplot(count_table)+geom_mosaic(mapping=aes(weight=Freq, Cluster, fill=CZ))

ggplot(count_table)+geom_mosaic(aes(weight=Freq, x=product(Cluster), fill=CZ), na.rm = FALSE)+
  theme_classic()+scale_fill_manual(values = carto_pal(n=8, "Bold"))
  

dev.off()

tot %>%
  group_by(Centroid_Name) %>%
  summarise((sd(med_Q)/mean(med_Q))*100)

mat_df<-as.data.frame(count_matrix)

count_sum<-mat_df %>%
  group_by(Var1) %>%
  summarise(sum(Freq))

mat_df<-merge(mat_df, count_sum, by="Var1")

mat_df$prop<-mat_df$Freq/mat_df$`sum(Freq)`

mat_df_cast<-t(dcast(mat_df, Var1~Var2, fill = mat_df$prop))

write.csv(mat_df_cast, "Climate_Cluster_Table.csv")
