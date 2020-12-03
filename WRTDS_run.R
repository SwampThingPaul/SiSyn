#install.packages("googledrive")
#install.packages("tidyverse")
#install.packages("data.table)
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


#get folder URL from google drive with discharge data
WRTDSfolder_url<-"https://drive.google.com/drive/folders/1YWjyxA1yIEVVVXQD-5xnoFlsnsOt1fmj"

#get ID of folder
WRTDSfolder<-drive_get(as_id(WRTDSfolder_url))

#get list of csv files from folder
WRTDScsv_files<-drive_ls(WRTDSfolder, type="csv")

#extract only WRTDS files - there are some regular discharge files in the WRTDScsv_files dataframe
WRTDScsv_files_final<-WRTDScsv_files[WRTDScsv_files$name %like% "WRTDS.csv",]

#create new column of just the site
WRTDScsv_files_final$files<-word(WRTDScsv_files_final$name, 1, sep = "_")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#read in file of 20 year data
data_20yearQ<-read.csv("Data_years_streams_20yrs.csv")

#filter out to get daily data
data_20yearQ_daily<-subset(data_20yearQ, data_20yearQ$Q.Data.type=="daily")

#create list of unique sites in this LTER
files20years<-unique(data_20yearQ_daily$Stream.Site)

#remove files with inconsistencies in Q data
files20years<-files20years[-c(18, 21, 25, 26)]

#filter the original csv file to include only sites from your LTER
RefTable<-subset(WRTDScsv_files_final, WRTDScsv_files_final$files %in% files20years)

#create list of unique files from this LTER
site_files<-unique(RefTable$name)

#set wd to new folder where these files will be downloaded - WRTDS requires them to be stored locally
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_20years")

#download each file to the above specified folder
for (i in 1:length(site_files)) {
  
  drive_download(site_files[i], type = "csv", overwrite = TRUE)
  
}

#make list of all files that you just downloaded to local folder
WRTDS_files_List<-list.files(path = "/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_20years")

#make list of only Q files
WRTDS_files_List_Q<-WRTDS_files_List[WRTDS_files_List %like% "Q_WRTDS"]

#make list of only Si files
WRTDS_files_List_Si<-WRTDS_files_List[WRTDS_files_List %like% "Si"]

#make list of only INFO files
WRTDS_files_List_Info<-WRTDS_files_List[WRTDS_files_List %like% "INFO"]

WRTDS_files<-sub("*_Q_WRTDS.csv", "", WRTDS_files_List_Q)

#run WRTDS! output will be saved to the same file
for (i in 1:length(WRTDS_files)) {
  
  #read in Q file
  Daily<-readUserDaily("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_20years", WRTDS_files_List_Q[i],
                       qUnit = 2)
  
  #read in Si file
  Sample<-readUserSample("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_20years", WRTDS_files_List_Si[i])
  
  #read in Info file
  Info<-readUserInfo("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_20years", WRTDS_files_List_Info[i])
  
  #remove duplicates from sample file
  Sample<-removeDuplicates(Sample)
  
  Sample<-aggregate(Sample, by=list(Sample$Date), mean)
  
  Sample<-Sample[,-c(1)]
  
  #merge into eList
  eList<-mergeReport(Info, Daily, Sample)
  
  #save workspace so it can be accessed later
  savePath<-"/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_20years/workspaces/"
  saveResults(savePath, eList)
  
  #estimate continuous Si
  eList<-modelEstimation(eList)
  
  #write output to this folder
  setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDSfinal")
  
  #extract continuous Si file from eList
  ContConc<-eList$Daily
  
  #write csv of continuous Si data
  write.csv(ContConc, paste0(WRTDS_files[i], "ContSi_WRTDS.csv"))
  
  #average yearly stats
  Results<-tableResults(eList)
  
  #write csv of results dataframe
  write.csv(Results, paste0(WRTDS_files[i], "ResultsTable_WRTDS.csv"))
  
  #make new column for year
  ContConc$Year<-format(as.Date(ContConc$Date), "%Y")
  
  #find min year
  minYP<-as.numeric(min(ContConc$Year))+1
  
  #find max year
  maxYP<-as.numeric(max(ContConc$Year))-1
  
  #set year points for 
  yearPoints<-c(minYP, maxYP)
  
  #calculate concentration trend
  Conc<-tableChangeSingle(eList, fluxUnit = 8, yearPoints)
  
  #calculate flux trend
  Flux<-tableChangeSingle(eList, fluxUnit = 8, yearPoints, flux = TRUE)
  
  #bind into one dataframe
  Trends<-cbind(Conc, Flux)
  
  #write csv of trends dataframe
  write.csv(Trends, paste0(WRTDS_files[i], "TrendsTable_WRTDS.csv"))
  
  #open pdf for graphical output
  pdf(paste0(WRTDS_files[i], "_WRTDS_output.pdf"))
  
  #residual plots
  fluxBiasMulti(eList)
  
  #examine model fit
  plotConcTimeDaily(eList)
  
  #plot concentration
  plotConcHist(eList, minYP, maxYP)
  
  #plot flux
  plotFluxHist(eList, minYP, maxYP)
  
  dev.off()
  
}
