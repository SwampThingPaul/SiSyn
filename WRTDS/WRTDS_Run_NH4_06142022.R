#install.packages("googledrive")
#install.packages("tidyverse")
#install.packages("data.table)
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
require(remotes)

# #### Google Drive ############
# #get folder URL from google drive with discharge data
# WRTDSfolder_url<-"https://drive.google.com/drive/folders/1s6irhuhH3qTdEi8gHRu5COFuHZyz4T9d?usp=sharing"
# 
# #get ID of folder
# WRTDSfolder<-drive_get(as_id(WRTDSfolder_url))
# 
# #get list of csv files from folder
# WRTDScsv_files<-drive_ls(WRTDSfolder, type="csv")
# 
# #extract only WRTDS files - there are some regular discharge files in the WRTDScsv_files dataframe
# WRTDScsv_files_final<-WRTDScsv_files[WRTDScsv_files$name %like% "WRTDS.csv",]
# 
# #create new column of just the site
# WRTDScsv_files_final$files<-word(WRTDScsv_files_final$name, 1, sep = "_")
# 
# setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")
# #setwd("U:/Jankowski/My Documents/Projects/Silica Synthesis/Data/WRTDS")
# 
# #read in file of 20 year data
# cryo_sites<-read.csv("NH4_MDL.csv")
# 
# #filter out to get daily data
# #data_20yearQ_daily<-subset(data_20yearQ, data_20yearQ$Q.Data.type=="daily")
# 
# #create list of unique sites in this LTER
# files20years<-unique(data_20yearQ$Stream.Site)
# 
# #remove files with inconsistencies in Q data
# #files20years<-files20years[-c(18, 21, 25, 26)]
# 
# #filter the original csv file to include only sites from your LTER
# RefTable<-subset(WRTDScsv_files_final, WRTDScsv_files_final$files %in% files20years)
# 
# #create list of unique files from this LTER
# site_files<-unique(RefTable$name)
# 
# # for some reason "GSWS09_Si_WRTDS" was listed twice in google drive and wouldn't download, skipped it and did it manually..
# site_files = site_files[1:length(site_files)]
# 
# #set wd to new folder where these files will be downloaded - WRTDS requires them to be stored locally
# setwd("C:/Users/kjankowski/Desktop/WRTDS_prep_MCM")
# 
# #download each file to the above specified folder
# for (i in 1:length(site_files)) {
#   
#   drive_download(site_files[i], type = "csv", overwrite = TRUE)
#   
# }

#################################################################################
### start here if not downloading from Google Drive
## set up files for loop
##read in cryo sites
#get folder URL from google drive with discharge data
folder_url<-"https://drive.google.com/drive/folders/19lemMC09T1kajzryEx5RUoOs3KDSbeAX"

#get ID of folder
folder<-drive_get(as_id(folder_url))

#get list of csv files from folder
csv_files<-drive_ls(folder, type="csv")

#split ".csv" from document names
csv_files$files<-word(csv_files$name, 1, sep = "\\.csv")

# read in discharge log
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

QLog<-read.csv("DischargeLog_All_011422.csv")

names(QLog)[3]<-"files"

#merge discharge log and list of csv files in google drive
RefTable<-merge(QLog, csv_files, by="files")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#read in cryo sites
NH4_cryo<-read.csv("NH4_MDL.csv")

cryo_sites<-NH4_cryo$site

#crop ref table to include only cryo sites
RefTable<-RefTable[RefTable$Stream %in% cryo_sites,]

#remove imnavait weir
RefTable<-RefTable[-which(RefTable$Stream == "Imnavait Weir"),]

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NH4PrepWRTDS")

##for some reason the prep files included all sites - not just long term sites
#I uploaded the info files, which just included long term files and then created a list
#of Q and P files by adding "Q_WRTDS" and "P_WRTDS" to the end of info file names

#here I am using P INFO files since I dont have the code to make NH4 ones
INFO_files_List<-list.files(path = "/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_Prep_P",
                            pattern = "INFO.csv")

#create list of files
files_list<-sub("_INFO.csv", "", INFO_files_List)

#make list of only INFO files
WRTDS_files_List_Info<-INFO_files_List[files_list %in% cryo_sites]

#make list of all files that you just downloaded to local folder
WRTDS_files_List<-list.files(path = "/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NH4PrepWRTDS")

#make list of only Q files
WRTDS_files_List_Q<-WRTDS_files_List[WRTDS_files_List %like% "Q_WRTDS"]

WRTDS_files_List_NH4<-WRTDS_files_List[WRTDS_files_List %like% "NH4_WRTDS"]

WRTDS_files<-sub("_NH4_WRTDS.csv", "", WRTDS_files_List_NH4)

#run WRTDS! output will be saved to the same file
for (i in 6:length(WRTDS_files)) {
  
  #read in Q file
  Daily<-readUserDaily("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NH4PrepWRTDS", WRTDS_files_List_Q[i],
                       qUnit = 2)
  
  #read in Si file
  Sample<-readUserSample("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NH4PrepWRTDS", WRTDS_files_List_NH4[i])
  
  #read in Info file
  Info<-readUserInfo("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_Prep_P", WRTDS_files_List_Info[i])
  
  Info$paramShortName<-"Ammonia"
  Info$constitAbbrev<-"NH4"
  Info$param.nm<-"NH4"
  
  #remove duplicates from sample file
  Sample<-removeDuplicates(Sample)
  
  Sample<-aggregate(Sample, by=list(Sample$Date), mean)
  
  Sample<-Sample[,-c(1)]
  
  #merge into eList
  eList<-mergeReport(Info, Daily, Sample)
  
  #save workspace so it can be accessed later
  savePath<-"/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_NH4_Results/workspaces"
  saveResults(savePath, eList)
  
  # fit original WRTDS model 
  eList1<-modelEstimation(eList, minNumObs=50)
  
  # fit GFN - estimate continuous Si with Q and CQ components ("GFN" method)
  eListOut <- runSeries(eList, windowSide = 11, minNumObs=50)
  
  #pull out stream ref information
  ref<-subset(RefTable, RefTable$Stream==WRTDS_files[i])
  
  #adjust PA based on LTER - 
  # KJo - these may need to be adjusted by stream for NWT
  # KJo - Albion - all 12 months; Martinelli - OK as is; Saddle - paStart = 5, paLong = 3 
  if(ref$Stream == "MARTINELLI") {
    
    eList1 <- setPA(eList1, paStart=5, paLong=5)
    eListOut <- setPA(eListOut, paStart=5, paLong=5)
    
  } else if(ref$Stream == "SADDLE STREAM 007") {
    
    eList1 <- setPA(eList1, paStart=5, paLong=3)
    eListOut <- setPA(eListOut, paStart=5, paLong=3)
    
  } else if(ref$LTER == "MCM") {
    
    eList1 <- setPA(eList1, paStart=12, paLong=2)
    eListOut <- setPA(eListOut, paStart=12, paLong=2)
    
  } else {
    
    eList1 <- eList1
    eListOut <- eListOut
    
  }
  
  #write output to this folder
  setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_NH4_Results")
  
  # extract error statistics
  error <- errorStats(eList1)
  write.csv(error, paste0(WRTDS_files[i], "_NH4_ErrorStats_WRTDS.csv"), row.names=FALSE)
  
  #extract continuous Si file from eList
  ContConc<-eListOut$Daily
  
  #write csv of continuous Si data
  write.csv(ContConc, paste0(WRTDS_files[i], "_NH4_Cont_WRTDS.csv"))
  
  #average yearly stats
  Results<-tableResults(eListOut)
  
  #write csv of results dataframe
  write.csv(Results, paste0(WRTDS_files[i], "_NH4_ResultsTable_WRTDS.csv"))
  
  #make new column for year
  ContConc$Year<-format(as.Date(ContConc$Date), "%Y")
  
  # calculate monthly values
  months=calculateMonthlyResults(eListOut)
  write.csv(months, paste0(WRTDS_files[i], "_NH4_Monthly_WRTDS.csv"))
  
  #find min year 
  # KJ - I had an issue with this when the discharge data were extended, changed to: 
  # e.g., minYP<-as.numeric(min(ContConc$Year))+1
  minYP<-as.numeric(min(ContConc$waterYear))+1
  
  #find max year
  maxYP<-as.numeric(max(ContConc$waterYear))-1
  
  #set year points for 
  yearPoints<-c(minYP, maxYP)
  
  #calculate concentration trend
  Conc<-tableChangeSingle(eListOut, fluxUnit = 8, yearPoints)
  
  #calculate flux trend
  Flux<-tableChangeSingle(eListOut, fluxUnit = 8, yearPoints, flux = TRUE)
  
  #bind into one dataframe
  Trends<-cbind(Conc, Flux)
  
  #write csv of trends dataframe
  write.csv(Trends, paste0(WRTDS_files[i], "_NH4_TrendsTable_WRTDS.csv"))
  
  #open pdf for graphical output
  pdf(paste0(WRTDS_files[i], "_NH4_WRTDS_output.pdf"))
  
  #residual plots - this function only works on original model file - not sure why
  fluxBiasMulti(eList1)
  
  #examine model fit
  plotConcTimeDaily(eListOut)
  
  #plot concentration
  plotConcHist(eListOut) # minYP, maxYP)
  
  #plot flux
  plotFluxHist(eListOut) #, minYP, maxYP)
  
  #plot data
  multiPlotDataOverview(eListOut)
  
  dev.off()
  
  # Set up for bootstrapping for WRTDS GFN - For most streams that don't need monthly adjustment
  # units - conc = mg/L; flux = million kg/year
  eListPairs <- runPairs(eList, windowSide = 11, 
                         minNumObs=50, 
                         year1=minYP, 
                         year2=maxYP)
  
  if(ref$Stream == "MARTINELLI") {
    
    eListPairs <- setPA(eListPairs, paStart=5, paLong=5)
    
  } else if(ref$Stream == "SADDLE STREAM 007") {
    
    eListPairs <- setPA(eListPairs, paStart=5, paLong=3)
    
  } else if(ref$LTER == "MCM") {
    
    eListPairs <- setPA(eListPairs, paStart=12, paLong=2)
    
  } else {
    
    eListPairs <- eListPairs
    
  }
  
  
  # for streams with month adjustment
  # KJ - paStart and paLong should be adjusted as you did above! 
  
  #eListPairs <- runPairs(eList, windowSide = 11, 
  #                      minNumObs=50, 
  #                     year1=minYP, 
  #                    year2=maxYP, paStart=5, paLong=3)
  
  write.csv(eListPairs, paste0(WRTDS_files[i], "_NH4_GFN.csv"))
  
  # Bootstrapping - runPairsBoot
  eBoot<-runPairsBoot(eList,eListPairs, nBoot=100,blockLength = 200)
  bootResults <- cbind(eBoot$xConc, eBoot$xFlux, eBoot$pConc, eBoot$pFlux)
  bootSummary <- eBoot$bootOut
  
  ## keep results
  CIs=as.data.frame(bootResults)
  CIs$solute=rep("NH4", nrow(bootResults))
  colnames(CIs)=c("xConc", "xFlux", "pConc", "pFlux", "solute")
  write.csv(CIs, paste0(WRTDS_files[i], "_NH4_EGRETCi_GFN_bootstraps.csv"), row.names=FALSE)
  
  Summary=as.data.frame(bootSummary)
  write.csv(Summary, paste0(WRTDS_files[i], "_NH4_EGRETCi_GFN_Trend.csv"))
  
  Summary=as.data.frame(bootSummary)
  write.csv(Summary, paste0(WRTDS_files[i], "_NH4_EGRETCi_Trend.csv"))
  

  
}

