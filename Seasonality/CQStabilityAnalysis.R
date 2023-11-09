require(googledrive)
require(ggplot2)
require(dplyr)
require(rcartocolor)
require(reshape2)
require(stringr)
require(comperes)
require(EflowStats)
require(ggpubr)

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

sum_meds<-tot %>%
  dplyr::group_by(Centroid_Name) %>%
  dplyr::summarise(mean_med_si=mean(med_Si), mean_sd_si=sd(med_Si), mean_med_q=mean(med_Q), mean_sd_q=sd(med_Q))

sum_meds$per_dev_si<-(sum_meds$mean_sd_si/sum_meds$mean_med_si)*100
sum_meds$per_dev_q<-(sum_meds$mean_sd_q/sum_meds$mean_med_q)*100

#si concentration density plots by cluster
p1<-ggplot(tot, aes(med_Si))+geom_density(aes(fill=Centroid_Name), alpha=0.5)+
  scale_fill_manual(values = carto_pal(n=5, "Bold"))+theme_classic()+
  theme(text = element_text(size=20, family="Times"), legend.position = "null")+
  labs(x="Si Concentration (mg/L)", y="Density", fill="Cluster", tag="a")

#discharge density plots by cluster
p2<-ggplot(tot, aes(log(med_Q)))+geom_density(aes(fill=Centroid_Name), alpha=0.5)+
  scale_fill_manual(values = carto_pal(n=5, "Bold"))+theme_classic()+
  theme(text = element_text(size=20, family = "Times"))+labs(x="Log Q (cms)", y="", fill="Cluster", tag="b")

pdf("SiQDensity.pdf", width = 14, height = 8)

#put into one plot
ggarrange(p1, p2, widths = c(0.37, 0.63))

dev.off()

#order
tot$Centroid_Name<-factor(tot$Centroid_Name,
                          levels = c("Fall Peak", "Fall Trough", "Spring Trough",
                                     "Spring Trough, Fall Peak", "Spring Trough, Variable Summer"))

biomes<-read.csv("Koeppen_Geiger.csv")

colnames(biomes)[3]<-"Site"

tot_biomes<-merge(tot, biomes, by="Site")

tot_biomes<-tot_biomes[-which(duplicated(tot_biomes$Site)),]

write.csv(tot_biomes, "AllSiClusterData.csv")

## for CQ plots ##
#classify as mobilization, chemostatic, or dilution based on CQ slope
tot$char<-ifelse(tot$slope > 0.1, "mobilization",
                 ifelse(-0.1 < tot$slope & tot$slope < 0.1 & tot$r2 < 0.1, "chemostatic", "dilution"))
#ifelse(tot$V1.x < 0.1, "dilution", "chemostatic"))
#order
tot$char<-factor(tot$char,levels = c("mobilization", "chemostatic", "dilution"))

#tot<-tot[-which(duplicated(tot$Site)),]

pdf("CQStability.pdf", width = 13, height = 7.5, family = "Times")

#CVC/CVq CQ plot, colored by overall stability
ggplot(tot, aes(cvc_cvq, slope))+geom_point(aes(col=1-overall_stab, size=r2), alpha=0.85)+theme_classic()+
  #scale_color_manual(values = carto_pal(n=8, "Bold"))+
  labs(x="CVc/CVq", y="CQ slope", col="Overall Stability", size="CQ R2")+
  theme(text = element_text(size = 20), panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
  facet_wrap(~Centroid_Name)+
  scale_color_gradientn(colours =  c("firebrick", "goldenrod", "forestgreen"))+
  geom_vline(xintercept = 1, color="grey")+
  geom_hline(yintercept = 0, color="grey")

dev.off()

pdf("CQCluster.pdf", width = 13, height = 7.5, family = "Times")

ggplot(tot, aes(cvc_cvq, slope))+geom_point(aes(col=Name, size=r2), alpha=0.9)+theme_classic()+
  #scale_color_manual(values = carto_pal(n=8, "Bold"))+
  labs(x="CVc/CVq", y="CQ slope", col="Climate Zone", size="CQ R2")+
  theme(text = element_text(size = 20), panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
  facet_wrap(~Centroid_Name)+
  scale_color_manual(values = carto_pal(n=8, "Bold"))+
  geom_vline(xintercept = 1, color="grey")+
  geom_hline(yintercept = 0, color="grey")

dev.off()