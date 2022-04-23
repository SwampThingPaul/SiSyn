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

#### Google Drive ############
#get folder URL from google drive with discharge data
WRTDSfolder_url<-"https://drive.google.com/drive/folders/1s6irhuhH3qTdEi8gHRu5COFuHZyz4T9d?usp=sharing"

#get ID of folder
WRTDSfolder<-drive_get(as_id(WRTDSfolder_url))

#get list of csv files from folder
WRTDScsv_files<-drive_ls(WRTDSfolder, type="csv")

#extract only WRTDS files - there are some regular discharge files in the WRTDScsv_files dataframe
WRTDScsv_files_final<-WRTDScsv_files[WRTDScsv_files$name %like% "WRTDS.csv",]

#create new column of just the site
WRTDScsv_files_final$files<-word(WRTDScsv_files_final$name, 1, sep = "_")

#setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")
setwd("U:/Jankowski/My Documents/Projects/Silica Synthesis/Data/WRTDS")

#read in file of 20 year data
data_20yearQ<-read.csv("Data_years_streams_WRTDS.csv")

#filter out to get daily data
#data_20yearQ_daily<-subset(data_20yearQ, data_20yearQ$Q.Data.type=="daily")

#create list of unique sites in this LTER
files20years<-unique(data_20yearQ$Stream.Site)

#remove files with inconsistencies in Q data
#files20years<-files20years[-c(18, 21, 25, 26)]

#filter the original csv file to include only sites from your LTER
RefTable<-subset(WRTDScsv_files_final, WRTDScsv_files_final$files %in% files20years)

#create list of unique files from this LTER
site_files<-unique(RefTable$name)

# for some reason "GSWS09_Si_WRTDS" was listed twice in google drive and wouldn't download, skipped it and did it manually..
site_files = site_files[1:length(site_files)]

#set wd to new folder where these files will be downloaded - WRTDS requires them to be stored locally
setwd("C:/Users/kjankowski/Desktop/WRTDS_prep_MCM")

#download each file to the above specified folder
for (i in 1:length(site_files)) {
  
  drive_download(site_files[i], type = "csv", overwrite = TRUE)
  
}

#################################################################################
### start here if not downloading from Google Drive
## set up files for loop
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/PPrepWRTDS_Updated")

##for some reason the prep files included all sites - not just long term sites
#I uploaded the info files, which just included long term files and then created a list
#of Q and P files by adding "Q_WRTDS" and "P_WRTDS" to the end of info file names

INFO_files_List<-list.files(path = "/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_Prep_P",
                            pattern = "INFO.csv")

#make list of only INFO files
WRTDS_files_List_Info<-INFO_files_List[INFO_files_List %like% "INFO"]

WRTDS_files<-sub("*_INFO.csv", "", INFO_files_List)


#make list of all files that you just downloaded to local folder
WRTDS_files_List<-list.files(path = "/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/PPrepWRTDS_Updated")

#make list of only Q files
WRTDS_files_List_Q<-WRTDS_files_List[WRTDS_files_List %like% "Q_WRTDS"]
WRTDS_file_Q<-sub("_Q_WRTDS.csv", "", WRTDS_files_List_Q)

WRTDS_files_List_Q<-list()

for (i in 1:length(WRTDS_files)) {
  
  WRTDS_files_List_Q[[i]]<-paste0(WRTDS_files[i], "_Q_WRTDS.csv")
  
}

WRTDS_files_List_Q<-unlist(WRTDS_files_List_Q)

WRTDS_files_List_P<-list()

for (i in 1:length(WRTDS_files)) {
  
  WRTDS_files_List_P[[i]]<-paste0(WRTDS_files[i], "_P_WRTDS.csv")
  
}

WRTDS_files_List_P<-unlist(WRTDS_files_List_P)


#run WRTDS! output will be saved to the same file
for (i in 1:length(WRTDS_files)) {
  
  #read in Q file
  Daily<-readUserDaily("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/PPrepWRTDS_Updated", WRTDS_files_List_Q[i],
                       qUnit = 2)
  
  #read in Si file
  Sample<-readUserSample("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/PPrepWRTDS_Updated", WRTDS_files_List_P[i])
  
  #read in Info file
  Info<-readUserInfo("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_Prep_P", WRTDS_files_List_Info[i])
  
  #remove duplicates from sample file
  Sample<-removeDuplicates(Sample)
  
  Sample<-aggregate(Sample, by=list(Sample$Date), mean)
  
  Sample<-Sample[,-c(1)]
  
  #merge into eList
  eList<-mergeReport(Info, Daily, Sample)
  
  #save workspace so it can be accessed later
  savePath<-"/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_P_Results_Updated/workspaces"
  saveResults(savePath, eList)
  
  #estimate continuous Si
  eList<-modelEstimation(eList, minNumObs=50)
  
  # adjust for MCM 
  #eList <- setPA(eList, paStart=12, paLong=2)
  
  # Adjust for NWT
  #eList <- setPA(eList, paStart=5, paLong=5)
  
  #write output to this folder
  setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_P_Results_Updated")
  
  # extract error statistics
  error <- errorStats(eList)
  write.csv(error, paste0(WRTDS_files[i], "_ErrorStats_WRTDS.csv"), row.names=FALSE)
  
  #extract continuous Si file from eList
  ContConc<-eList$Daily
  
  #write csv of continuous Si data
  write.csv(ContConc, paste0(WRTDS_files[i], "_ContP_WRTDS.csv"))
  
  #average yearly stats
  Results<-tableResults(eList)
  
  #write csv of results dataframe
  write.csv(Results, paste0(WRTDS_files[i], "_ResultsTable_WRTDS.csv"))
  
  #make new column for year
  ContConc$Year<-format(as.Date(ContConc$Date), "%Y")
  
  # calculate monthly values
  months=calculateMonthlyResults(eList)
  write.csv(months, paste0(WRTDS_files[i], "_Monthly_WRTDS.csv"))
  
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
  write.csv(Trends, paste0(WRTDS_files[i], "_TrendsTable_WRTDS.csv"))
  
  #open pdf for graphical output
  pdf(paste0(WRTDS_files[i], "_WRTDS_output.pdf"))
  
  #residual plots
  fluxBiasMulti(eList)
  
  #examine model fit
  plotConcTimeDaily(eList)
  
  #plot concentration
  plotConcHist(eList) # minYP, maxYP)
  
  #plot flux
  plotFluxHist(eList) #, minYP, maxYP)
  
  #plot data
  multiPlotDataOverview(eList)
  
  dev.off()
  
  ## EGRETCi Trends
  caseSetUp <- trendSetUp(eList, 
                          year1=minYP,
                          year2=maxYP,
                          nBoot = 100, 
                          bootBreak = 50,
                          blockLength = 200)
  eBoot<-wBT(eList, caseSetUp=caseSetUp, saveOutput = TRUE, jitterOn = FALSE)
  bootResults <- cbind(eBoot$xConc, eBoot$xFlux, eBoot$pConc, eBoot$pFlux)
  bootSummary <- eBoot$bootOut
  # calculates CI around model fit - TAKES FOREVER
  #CIAnnualResults <- ciCalculations(eList,nBoot=100,blockLength=200)
  
  ## keep results
  CIs=as.data.frame(bootResults)
  CIs$solute=rep("P", nrow(bootResults))
  colnames(CIs)=c("xConc", "xFlux", "pConc", "pFlux", "solute")
  write.csv(CIs, paste0(WRTDS_files[i], "_EGRETCi_bootstraps.csv"), row.names=FALSE)
  
  Summary=as.data.frame(bootSummary)
  write.csv(Summary, paste0(WRTDS_files[i], "_P_EGRETCi_Trend.csv"))
  # saves bootstrapped CI values calculated above to file
  #write.csv(CIAnnualResults, paste0(WRTDS_files[i], "_Si_EGRETCi_TrendCIs.csv"), row.names=FALSE)
  
  #plotContours(eList,yearStart=2000,yearEnd=2019, qBottom=1000, qTop=50000, qUnit=2)
  
  ### GFN - estimate continuous Si with Q and CQ components ("GFN" method)
  eListOut <- runPairs(eList, windowSide = 6, minNumObs=50, year1=minYP, year2=maxYP)
  write.csv(eListOut, paste0(WRTDS_files[i], "_P_GFN.csv"))
  
  #tableResults(eListOut)
  #plotConcHist(eListOut)
  #plotFluxHist(eListOut)
  #tableChange(eListOut, yearPoints = c(2002, 2010, 2019))
  
#}

