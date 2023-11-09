#check working directory where files will be stored locally; separate folder within project folder
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#read in chem input file
chem_df<-read.csv("WRTDS-input_chemistry.csv")
#crop to only include DSi
chem_si<-subset(chem_df, chem_df$variable=="DSi")

#read in Q input file  
q_df<-read.csv("WRTDS-input_discharge.csv")

chem_table<-as.data.frame(table(chem_si$Stream_ID))

chem_table$Site<-sub(".*__", "", chem_table$Var1) 

finn<-read.csv("FinnishSites.csv")

for (i in 1:nrow(finn)) {
  
  site_id<-finn[i,2]
  
  row_num<-which(chem_table$Site==site_id)
  
  chem_table[row_num, "Site"]<-finn[i,1]
  
}

chem_table$Site<-ifelse(chem_table$Site=="TORNIONJ KUKKOLA 14310  ", "TORNIONJ KUKKOLA 14310", chem_table$Site)

chem_table$Site<-ifelse(chem_table$Site=="SIMOJOKI AS. 13500      ", "SIMOJOKI AS. 13500", chem_table$Site)

#read in cluster data for average cluster, modal cluster, number of clusters
si_clust<-read.csv("MonthClustersNov2022.csv")

chem_table_merge<-merge(chem_table, si_clust, by="Site")

si_clust_mode<-read.csv("SiClustersMode.csv")

siteyear_clust_num<-read.csv("SiNumClusters.csv")

SiStab<-read.csv("StabilitySiClusters.csv")
SiStab<-SiStab[,c(2,7)]
colnames(SiStab)[2]<-"SiStab"

#merge and clean
chem_table_merge<-merge(chem_table_merge, si_clust_mode, by="Site")

chem_table_merge<-merge(chem_table_merge, siteyear_clust_num, by="Site")

chem_table_merge<-merge(chem_table_merge, SiStab, by="Site")

chem_table_merge<-chem_table_merge[,-c(2, 4:16, 20, 22)]

#calculate clster stability
chem_table_merge$clust_stab<-1+((5-chem_table_merge$num_clusts)/5)

#calculate overall stability
chem_table_merge$overall_stab<-chem_table_merge$SiStab/chem_table_merge$clust_stab

#create df of centroid names
centroid_names<-as.data.frame(t(tibble("1"="Spring Trough",
                                       "2"="Fall Trough",
                                       "3"="Fall Peak",
                                       "4"="Spring Trough, Fall Peak",
                                       "5"="Spring Trough, Variable Summer")))
centroid_names$mode<-c(1:5)

colnames(centroid_names)<-c("Centroid_Name", "mode")

#merge
chem_table_merge<-merge(chem_table_merge, centroid_names, by="mode")

chem_table_merge<-chem_table_merge[!duplicated(chem_table_merge$Site),]

error<-read.csv("Full_Results_ErrorStats_WRTDS.csv")
colnames(error)[2]<-"Site"

error<-subset(error, error$chemical=="DSi")

fluxbias<-read.csv("Full_Results_FluxBias_WRTDS.csv")
colnames(fluxbias)[2]<-"Site"

fluxbias<-subset(fluxbias, fluxbias$chemical=="DSi")


chem_table_merge<-merge(chem_table_merge, error, by="Site")
chem_table_merge<-merge(chem_table_merge, fluxbias, by="Site")
chem_table_merge$bias<-rowMeans(chem_table_merge[,c(20:22)])


##get CQ measurements
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

cv_tot$Site<-sub("^[^_]*__", "", cv_tot$Stream_ID)

#calculate CVC/CVQ
cv_tot$cvc_cvq<-(cv_tot$mean_Q/cv_tot$mean_Si)*(cv_tot$sd_Si/cv_tot$sd_Q)

chem_si$Site<-sub("^[^_]*__", "", chem_si$Stream_ID)

q_df$Site<-sub("^[^_]*__", "", q_df$Stream_ID)

#get list of streams
streams<-unique(chem_si$Site)

streams<-streams[streams %in% chem_table_merge$Site]

#open list to calculate slope and r2 of each stream
slope_list<-list()
rsquared_list<-list()

#calculate CQ slope and CQ r2 for Si-Q for each stream
for (i in 1:length(streams)) {
  
  si<-subset(chem_si, chem_si$Site==streams[i])
  si$Date<-as.Date(si$Date)
  
  q<-subset(q_df, q_df$Site==streams[i])
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
slope_df$Site<-streams
colnames(slope_df)[1]<-"slope"

#put r2 into df
r2_df<-as.data.frame(do.call(rbind, rsquared_list))
r2_df$Site<-streams
colnames(r2_df)[1]<-"r2"

#merge cv, slope, and r2 into df
tot<-merge(cv_tot, slope_df, by="Site")
tot<-merge(tot, r2_df, by="Site")

finn<-read.csv("FinnishSites.csv")

for (i in 1:nrow(finn)) {
  
  site_id<-finn[i,2]
  
  row_num<-which(tot$Site==site_id)
  
  tot[row_num, "Site"]<-finn[i,1]
  
}

tot$Site<-ifelse(tot$Site=="TORNIONJ KUKKOLA 14310  ", "TORNIONJ KUKKOLA 14310", tot$Site)

tot$Site<-ifelse(tot$Site=="SIMOJOKI AS. 13500      ", "SIMOJOKI AS. 13500", tot$Site)


chem_table_merge<-merge(chem_table_merge, tot, by="Site")

chem_table_merge$two_hundo<-ifelse(chem_table_merge$Freq > 199, "> 200", "< 200")

###make some error stat plots

p1<-ggplot(chem_table_merge, aes(Freq, RsqLogC))+geom_point()+theme_bw()+
  theme(text = element_text(size=20))+labs(x="Number of Si Observations", y="WRTDS Si R2", tag="a")+
  lims(x=c(0,1000))

p6<-ggplot(chem_table_merge, aes(Freq, bias))+geom_point()+theme_bw()+
  theme(text = element_text(size=20))+labs(x="Number of Si Observations", y="WRTDS Flux Bias Statistic", tag="b")+
  lims(x=c(0,1000))

p2<-ggplot(chem_table_merge, aes(Freq, as.character(mode)))+geom_boxplot()+theme_bw()+
  theme(text = element_text(size=20))+labs(y="Cluster Number", x="Number of Si Observations", tag="c")+
  lims(x=c(0,1000))

p3<-ggplot(chem_table_merge, aes(Freq, 1-overall_stab))+geom_point()+theme_bw()+
  theme(text = element_text(size=20))+labs(x="Number of Si Observations", y="Stability", tag="d")+
  lims(x=c(0,1000))

p4<-ggplot(chem_table_merge, aes(Freq, slope))+geom_point()+theme_bw()+
  theme(text = element_text(size=20))+labs(x="Number of Si Observations", y="CQ Slope", tag="e")+
  lims(x=c(0,1000))

p5<-ggplot(chem_table_merge, aes(Freq, r2))+geom_point()+theme_bw()+
  theme(text = element_text(size=20))+labs(x="Number of Si Observations", y="CQ R2", tag="f")+
  lims(x=c(0,1000))

pdf("SamplingFreqPlots.pdf", width = 10, height = 13, family = "Times")
ggarrange(p1, p6, p2, p3, p4, p5, ncol=2, nrow=3)
dev.off()


ggplot(chem_table_merge, aes(two_hundo, RsqLogC))+geom_boxplot()+theme_bw()+
  theme(text = element_text(size=20))+labs(x="Number of Si Observations", y="WRTDS Si R2")

ggplot(chem_table_merge, aes(x=Centroid_Name, fill=Centroid_Name))+geom_density(alpha=0.5)+theme_bw()+
  theme(text = element_text(size=20), axis.text.x = element_blank())+facet_wrap(~two_hundo)+labs(x="", fill="Cluster")

pdf("ClusterStability_PerformancePlots.pdf", width = 10, height = 10, family = "Times")

ggarrange(p1, p2, p3, p4)

dev.off()

p1<-ggplot(chem_table_merge, aes(y=as.character(mode), x=RsqLogC))+geom_boxplot()+theme_bw()+
  theme(text = element_text(size=20))+labs(y="Cluster Membership", x="WRTDS Si R2", tag="a")

p2<-ggplot(chem_table_merge, aes(y=as.character(mode), x=bias))+geom_boxplot()+theme_bw()+
  theme(text = element_text(size=20))+labs(y="Cluster Membership", x="WRTDS Flux Bias Statistic", tag="b")

p3<-ggplot(chem_table_merge, aes(y=1-overall_stab, x=RsqLogC))+geom_point()+theme_bw()+
  theme(text = element_text(size=20))+labs(y="Stability", x="WRTDS Si R2", tag="c")

p4<-ggplot(chem_table_merge, aes(y=1-overall_stab, x=bias))+geom_point()+theme_bw()+
  theme(text = element_text(size=20))+labs(y="Stability", x="WRTDS Flux Bias Statistic", tag="d")



