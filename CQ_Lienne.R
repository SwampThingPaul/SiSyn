require(ggplot2)
require(dplyr)
require(rcartocolor)
require(reshape2)
require(stringr)
require(comperes)
require(EflowStats)
require(ggpubr)

#check working directory where files will be stored locally; separate folder within project folder
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#read in chem input file
chem_df<-read.csv("WRTDS-input_chemistry.csv")
#crop to only include DSi
chem_si<-subset(chem_df, chem_df$variable=="DSi")

#read in Q input file  
q_df<-read.csv("WRTDS-input_discharge.csv")

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



