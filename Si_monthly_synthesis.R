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

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_Prep_Si")

#make list of all files that you just downloaded to local folder
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

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_Prep_Si/monthly")

monthly_results<-list.files(path = "/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_Prep_Si/monthly",
                            pattern = "ResultsTable_WRTDS.csv")

site_slope_list<-list()

for (i in 1:length(WRTDS_files)) {
  
  site_data_nums<-grep(WRTDS_files[i], monthly_results)
  
  site_data_nums<-site_data_nums[1:12]
  
  site_data<-monthly_results[site_data_nums]
  
  month_slope_list<-list()
  
  months<-month.name
  
  for (k in 1:length(site_data)) {
    
    results<-read.csv(site_data[k])
    
    SS<-sens.slope(results$FN.Flux..10.6kg.yr.)
    
    pvalue<-SS$p.value
    
    trend<-SS$estimates
    
    month_slope_list[[k]]<-ifelse(pvalue < 0.05, paste0(trend, "*"), trend)
    
  }
  
  site_slope_list[[i]]<-unlist(month_slope_list)
  
  
}

slope_df<-as.data.frame(do.call(rbind, site_slope_list))
rownames(slope_df)<-WRTDS_files
colnames(slope_df)<-month.name

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

write.csv(slope_df, "FN_Flux_Slope.csv")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

sites<-read.csv("Data_years_streams_WRTDS.csv")

slope_df$Stream.Site<-WRTDS_files

slope_tot<-merge(slope_df, sites, by="Stream.Site")

slope_tot<-slope_tot[,c(1:14)]

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_Prep_Si")

info_files<-list.files(path = "/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_Prep_Si/",
                       pattern = "INFO.csv")

for (i in 1:length(info_files)) {
  
  info_file<-read.csv(info_files[i])
  area_list[[i]]<-as.numeric(info_file$drainSqKm)
  
}

area_df<-as.data.frame(unlist(area_list))
area_df$Stream.Site<-WRTDS_files

# slope_tot<-merge(slope_tot, area_df, by=c("Stream.Site"))

for (i in 2:13) {
  
  slope_tot[,i]<-str_remove(slope_tot[,i], "\\*")
  
}

slope_tot[,c(2:13)]<-lapply(slope_tot[,c(2:13)], as.numeric)

# slope_tot_scaled<-slope_tot
# 
# for (i in 2:13) {
#   
#   slope_tot_scaled[,i]<-slope_tot_scaled[,i]/slope_tot_scaled$`unlist(area_list)`
#   
# }

# slope_tot_scaled<-slope_tot_scaled[,-15]

slope_tot_melt<-melt(slope_tot, id=c("Stream.Site", "LTER"))

MCM_months<-c("January", "December")

`%!in%` <- Negate(`%in%`)

slope_tot_melt$value<-ifelse(slope_tot_melt$LTER=="MCM" & slope_tot_melt$variable %!in% MCM_months, 
                             NA, slope_tot_melt$value) 

p1<-ggplot(slope_tot_melt, aes(LTER, value))+geom_boxplot(aes(col=LTER))+theme(legend.position = "none", 
                                                                               axis.text.x = element_text(angle = 45))+
  ggtitle("LTER Flux Slopes")+facet_wrap(~variable)

p2<-ggplot(slope_tot_melt, aes(LTER, value))+geom_boxplot(aes(col=LTER))+theme(legend.position = "none", 
                                                                               axis.text.x = element_text(angle = 45))+
  ylim(c(-100,100))+ggtitle("LTER Flux Slopes")+facet_wrap(~variable)

p3<-ggplot(slope_tot_melt, aes(LTER, value))+geom_boxplot(aes(col=LTER))+theme(legend.position = "none", 
                                                                               axis.text.x = element_text(angle = 45))+
  ylim(c(-1,1))+ggtitle("LTER Flux Slopes")+facet_wrap(~variable)

p4<-ggplot(slope_tot_melt, aes(LTER, value))+geom_boxplot(aes(col=LTER))+theme(legend.position = "none", 
                                                                               axis.text.x = element_text(angle = 45))+
  ylim(c(-.5,.5))+ggtitle("LTER Flux Slopes")+facet_wrap(~variable)

jpeg("LTER_Flux_Slope_Boxplot.jpeg", width = 1400, height = 1100)

ggarrange(p1, p2, p3, p4)

dev.off()

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
