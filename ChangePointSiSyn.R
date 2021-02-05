#detect change point in SiSyn project discharge

install.packages("segmented")

require(tidyverse)
require(googledrive)
require(stringr)
require(lubridate)
require(reshape)
require(gtools)
require(dplyr)
require(data.table)
require(dataRetrieval)
require(EGRET)
require(ggplot2)
require(changepoint)
require(segmented)

WRTDSfolder_url<-"https://drive.google.com/drive/folders/1e9GEr2GSHXagg6sAK4a7e47dKE6wghR9"

#get ID of folder
WRTDSfolder<-drive_get(as_id(WRTDSfolder_url))

#get list of csv files from folder
WRTDScsv_files<-drive_ls(WRTDSfolder, type="csv")

#extract only WRTDS files - there are some regular discharge files in the WRTDScsv_files dataframe
WRTDScsv_files_final<-WRTDScsv_files[WRTDScsv_files$name %like% "ContSi_WRTDS.csv",]

#create new column of just the site
WRTDScsv_files_final$files<-word(WRTDScsv_files_final$name, 1, sep = "Cont")

site_files<-WRTDScsv_files_final$name

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/ChangePoint")

for (i in 1:length(site_files)) {
  
  drive_download(site_files[i], type = "csv", overwrite = TRUE)
  
}

column<-c("Q", "Flux", "FNFlux")

site_list<-list.files(path="/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/ChangePoint")

for (q in 1:3) {

  pdf(paste(column[q], "Mean ChangePoint.pdf"))
  
  for (i in 1:length(site_list)) {
    
    sitedata<-read.csv(site_list[i])
    
    sitedataMEAN <- aggregate(list(sitedata$Q, sitedata$FluxDay, sitedata$FNFlux), 
                              by = list(sitedata$waterYear), mean, na.rm=TRUE)
    
    colnames(sitedataMEAN)<-c("waterYear", "Q", "FluxDay", "FNFlux")
    
    cp<-cpt.mean(sitedataMEAN[,q+1])
    
    plot(cp, main = paste(site_list[i], column[q], "MEAN")) 
    
  } 
  
  dev.off()
  
  pdf(paste(column[q], "Median ChangePoint.pdf"))
  
  for (k in 1:length(site_list)) {
    
    sitedata<-read.csv(site_list[i])
    
    sitedataMED <- aggregate(list(sitedata$Q, sitedata$FluxDay, sitedata$FNFlux), 
                             by = list(sitedata$waterYear), median, na.rm=TRUE)
    
    colnames(sitedataMED)<-c("waterYear", "Q", "FluxDay", "FNFlux")
    
    cp<-cpt.mean(sitedataMED[,q+1])
    
    plot(cp, main = paste(site_list[i], column[q], "MEDIAN")) 
    
    
  }
  
  dev.off()
  
}



  


