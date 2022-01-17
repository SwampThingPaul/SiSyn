#for monthly slope tables and synthesis plots

require(tidyverse)
require(googledrive)
require(stringr)
require(lubridate)
require(reshape)
require(gtools)
require(data.table)
require(dataRetrieval)
require(EGRET)
require(EGRETci)
require(ggpubr)
require(Kendall)
require(trend)
#install.packages("ggallin")
require(ggallin)
#install.packages("ggbreak")
require(ggbreak)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_Prep_Si")

#make list of all files that you just downloaded to local folder - can also use Data-Years_Streams_WRTDS.csv
WRTDS_files_List<-list.files(path = "/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_Prep_Si")

#remove sites with missing discharge
remove_these<-grep("Toolik Inlet|TW Weir", WRTDS_files_List)

WRTDS_files_List<-WRTDS_files_List[-c(remove_these)]

#make list of only Q files
WRTDS_files_List_Q<-WRTDS_files_List[WRTDS_files_List %like% "Q_WRTDS"]

#make list of only Si files
WRTDS_files_List_Si<-WRTDS_files_List[WRTDS_files_List %like% "Si"]

#make list of only INFO files
WRTDS_files_List_Info<-WRTDS_files_List[WRTDS_files_List %like% "INFO"]

#make list of site names
WRTDS_files<-sub("*_Q_WRTDS.csv", "", WRTDS_files_List_Q)

#read in monthly results files
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_Prep_Si/monthly")

monthly_results<-list.files(path = "/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_Prep_Si/monthly",
                            pattern = "ResultsTable_WRTDS.csv")

site_slope_list<-list()

#open list for sens slope analysis of all sites
#this loop is for sites
for (i in 1:length(WRTDS_files)) {
  
  #get site numbers from monthly results list
  site_data_nums<-grep(WRTDS_files[i], monthly_results)
  
  #get only first 12 files, some sites also bring up results from others bc same name
  site_data_nums<-site_data_nums[1:12]
  
  #subset monthly results to only include right files
  site_data<-monthly_results[site_data_nums]
  
  month_slope_list<-list()
  
  months<-month.name
  
  #this loop is for months
  for (k in 1:length(site_data)) {
    
    #read in file
    results<-read.csv(site_data[k])
    
    #perform sens slope test - MAKE SURE TO CHANGE COLUMN (after $) for conc, flux, etc
    SS<-sens.slope(results$FN.Conc..mg.L.)
    
    #extract p value
    pvalue<-SS$p.value
    
    #extract trend
    trend<-SS$estimates
    
    #append trend to list - if significant add "*"
    month_slope_list[[k]]<-ifelse(pvalue < 0.05, paste0(trend, "*"), trend)
    
  }
  
  #append all months from same site to list
  site_slope_list[[i]]<-unlist(month_slope_list)
  
  
}

#unlist all sites into data frame
slope_df<-as.data.frame(do.call(rbind, site_slope_list))

#rename columns
rownames(slope_df)<-WRTDS_files
colnames(slope_df)<-month.name

#write to csv
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

write.csv(slope_df, "Conc_Slope.csv")

#read in sites list files
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

sites<-read.csv("Data_years_streams_WRTDS.csv")

#add files to slope dataframe
slope_df$Stream.Site<-WRTDS_files

#merge slope_df with sites list
slope_tot<-merge(slope_df, sites, by="Stream.Site")

slope_tot<-slope_tot[,c(1:14)]

#remove * from slope_tot df
for (i in 2:13) {
  
  slope_tot[,i]<-str_remove(slope_tot[,i], "\\*")
  
}

#convert to numeric and melt in prep for plotting
slope_tot[,c(2:13)]<-lapply(slope_tot[,c(2:13)], as.numeric)

slope_tot_melt<-melt(slope_tot, id=c("Stream.Site", "LTER"))

#remove MCM sites not in Jan and Dec
MCM_months<-c("January", "December")

`%!in%` <- Negate(`%in%`)

slope_tot_melt$value<-ifelse(slope_tot_melt$LTER=="MCM" & slope_tot_melt$variable %!in% MCM_months, 
                             NA, slope_tot_melt$value) 

#complete cases
slope_tot_melt<-slope_tot_melt[complete.cases(slope_tot_melt),]

jpeg("LTER_FNFlux_Slope_Final.jpeg", width = 1000, height = 700)

#plot all slopes on log axis with free scales
ggplot(slope_tot_melt, aes(variable, value))+geom_boxplot(aes(col=variable))+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45))+
  ggtitle("LTER FN Flux Slopes")+facet_wrap(~LTER, scales = "free_y")+xlab("Month")+ylab("Slope")
  
dev.off()

#these (p1-p4) all plot on non-log axes with fixed scales
p1<-ggplot(slope_tot_melt, aes(variable, value))+geom_boxplot(aes(col=variable))+theme(legend.position = "none", 
                                                                               axis.text.x = element_text(angle = 45))+
  ggtitle("LTER Concentration Slopes")+facet_wrap(~LTER)

p2<-ggplot(slope_tot_melt, aes(variable, value))+geom_boxplot(aes(col=variable))+theme(legend.position = "none", 
                                                                               axis.text.x = element_text(angle = 45))+
  ylim(c(-100,100))+ggtitle("LTER Concentration Slopes")+facet_wrap(~LTER)

p3<-ggplot(slope_tot_melt, aes(variable, value))+geom_boxplot(aes(col=variable))+theme(legend.position = "none", 
                                                                               axis.text.x = element_text(angle = 45))+
  ylim(c(-1,1))+ggtitle("LTER Concentration Slopes")+facet_wrap(~LTER)

p4<-ggplot(slope_tot_melt, aes(variable, value))+geom_boxplot(aes(col=variable))+theme(legend.position = "none", 
                                                                               axis.text.x = element_text(angle = 45))+
  ylim(c(-.5,.5))+ggtitle("LTER Concentration Slopes")+facet_wrap(~LTER)

jpeg("LTER_Concentration_Slope_Boxplot_byLTER.jpeg", width = 1400, height = 1100)

ggarrange(p1, p2, p3, p4)

dev.off()

#these summarize positive/negative/significant slopes from slopes df
positive<-list()
negative<-list()
significant<-list()

for (i in 1:12) {
  
  positive[[i]]<-length(which(slope_df[[i]] > 0))
  negative[[i]]<-length(which(slope_df[[i]] < 0))
  significant[[i]]<-length(grep("\\*", slope_df[[i]]))
  
}

p_df<-unlist(positive)
n_df<-unlist(negative)
s_df<-unlist(significant)

summary_df<-as.data.frame(rbind(p_df, n_df, s_df))

rownames(summary_df)<-c("Positive", "Negative", "Significant")
colnames(summary_df)<-month.name

write.csv(summary_df, "Flux_Slope_Summary.csv")
