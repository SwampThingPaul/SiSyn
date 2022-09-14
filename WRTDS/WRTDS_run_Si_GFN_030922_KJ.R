## Running GFN Version of EGRET/WRTDS

library(tidyverse)
library(googledrive)
library(stringr)
library(lubridate)
library(reshape)
library(gtools)
library(data.table)
library(dataRetrieval)
library(EGRET)
library(EGRETci)

#################################################################################
### 

#make list of all "prep" files in local folder
# setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/MacroSiPrepWRTDS")

#WRTDS_files_List<-list.files(path = "C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Data/WRTDS/WRTDS_prep_Si_01162022")
WRTDS_files_List <- list.files(path = file.path("WRTDS_test"))

#make list of only Q files
WRTDS_files_List_Q<-WRTDS_files_List[WRTDS_files_List %like% "ALBION_Q_WRTDS"]

#make list of only Si files
WRTDS_files_List_Si<-WRTDS_files_List[WRTDS_files_List %like% "ALBION_Si_WRTDS"]

#make list of only INFO files
WRTDS_files_List_Info<-WRTDS_files_List[WRTDS_files_List %like% "ALBION_INFO"]

WRTDS_files<-sub("*_Q_WRTDS.csv", "", WRTDS_files_List_Q)

# for testing 
#i=i

#run WRTDS! output will be saved to the same file
# for (i in 1:length(WRTDS_files)) {
  
# Set i to 1
i <- 1

  #read in Q file
  Daily<-readUserDaily(filePath = "WRTDS_test", WRTDS_files_List_Q[i],
                       qUnit = 2)
  
  #read in Si file
  Sample<-readUserSample(filePath = "WRTDS_test", WRTDS_files_List_Si[i])
  
  #read in Info file
  Info<-readUserInfo(filePath = "WRTDS_test", WRTDS_files_List_Info[i])
  
  #remove duplicates from sample file
  Sample<-removeDuplicates(Sample)
  
  Sample<-aggregate(Sample, by=list(Sample$Date), mean)
  
  Sample<-Sample[,-c(1)]
  
  #merge discharge, concentration, and "INFO" into eList
  eList<-mergeReport(Info, Daily, Sample)
  
  #save workspace so it can be accessed later
  savePath<-"WRTDS_test"
  saveResults(savePath, eList)
  
  # Run original model
  eList1<-modelEstimation(eList, minNumObs=50)
  
  # Run updated "GFN" model
  eListOut <- runSeries(eList, windowSide = 11, minNumObs=50)
  
  # changed from
  # if(ref$Stream == "MARTINELLI")
  # to
  # if(WRTDS_files[i] == "")
  
  ### make site-specific adjustments to PA list
  if(WRTDS_files[i] == "MARTINELLI") {

    eList1 <- setPA(eList1, paStart=5, paLong=5)
    eListOut <- setPA(eListOut, paStart=5, paLong=5)

  } else if(WRTDS_files[i] == "SADDLE STREAM 007") {

    eList1 <- setPA(eList1, paStart=5, paLong=3)
    eListOut <- setPA(eListOut, paStart=5, paLong=3)

  } else if(WRTDS_files[i] == "MCM") {

    eList1 <- setPA(eList1, paStart=12, paLong=2)
    eListOut <- setPA(eListOut, paStart=12, paLong=2)

  } else if(WRTDS_files[i] == "Sagehen") {
  
    eList1 <- blankTime(eList1, startBlank = "1996-01-01", endBlank = "2001-01-01")
    eListOut <- blankTime(eListOut, startBlank = "1996-01-01", endBlank = "2001-01-01")
    
  } else {

    eList1 <- eList1
    eListOut <- eListOut

  }

  ############
  #write output to this folder
  
  
  # extract error statistics
  error <- errorStats(eList1)
  write.csv(error, file.path("WRTDS_test", paste0(WRTDS_files[i], "_ErrorStats_WRTDS.csv")), row.names=FALSE)
  
  #open pdf for graphical output 
  pdf(file.path("WRTDS_test", paste0(WRTDS_files[i], "_WRTDS_GFN_output.pdf")))
  
  #residual plots
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
  
  #extract daily Si file from model run
  ContConc<-eListOut$Daily
  
  #write csv of daily Si data
  write.csv(ContConc, file.path("WRTDS_test", paste0(WRTDS_files[i], "_ContSi_GFN_WRTDS.csv")))
  
  #average annual values
  Results<-tableResults(eListOut)
  
  #write csv of annual results dataframe
  write.csv(Results, file.path("WRTDS_test", paste0(WRTDS_files[i], "_ResultsTable_GFN_WRTDS.csv")))
  
  #make new column for year
  ContConc$Year<-format(as.Date(ContConc$Date), "%Y")
  
  # calculate monthly values
  months=calculateMonthlyResults(eListOut)
  write.csv(months, file.path("WRTDS_test", paste0(WRTDS_files[i], "_Monthly_GFN_WRTDS.csv")))
  
  #find min year
  minYP<-as.numeric(min(ContConc$Year))+1
  
  #find max year
  maxYP<-as.numeric(max(ContConc$Year))-1
  
  #set year points for 
  yearPoints<-c(minYP, maxYP)
  
  #calculate concentration trend
  Conc<-tableChangeSingle(eListOut, fluxUnit = 8, yearPoints)
  
  #calculate flux trend
  Flux<-tableChangeSingle(eListOut, fluxUnit = 8, yearPoints, flux = TRUE)
  
  #bind into one dataframe
  Trends<-cbind(Conc, Flux)
  
  #write csv of trends dataframe
  write.csv(Trends, file.path("WRTDS_test", paste0(WRTDS_files[i], "_TrendsTable_GFN_WRTDS.csv")))
  
  # Run trend estimate for GFN method between start/end years
  eListPairs <- runPairs(eListOut, windowSide = 11, 
                         minNumObs=50, 
                         year1=minYP, 
                         year2=maxYP,
                        )

  write.csv(eListPairs, file.path("WRTDS_test", paste0(WRTDS_files[i], "_Si_GFN.csv")))
  
 
  ## Estimate trend uncertainty  
  # runPairsBoot
  eBoot<-runPairsBoot(eListOut,eListPairs, nBoot=100,blockLength = 200)
  bootResults <- cbind(eBoot$xConc, eBoot$xFlux, eBoot$pConc, eBoot$pFlux)
  bootSummary <- eBoot$bootOut
  
## keep bootstrapped results
  CIs=as.data.frame(bootResults)
  CIs$solute=rep("Si", nrow(bootResults))
  colnames(CIs)=c("xConc", "xFlux", "pConc", "pFlux", "solute")
  write.csv(CIs, file.path("WRTDS_test", paste0(WRTDS_files[i], "_EGRETCi_GFN_bootstraps.csv")), row.names=FALSE)
  
  Summary=as.data.frame(bootSummary)
  write.csv(Summary, file.path("WRTDS_test", paste0(WRTDS_files[i], "_Si_EGRETCi_GFN_Trend.csv")))
  
  #calculates CI around model fit - TAKES FOREVER
  #CIAnnualResults <- ciCalculations(eList,nBoot=100,blockLength=200)
  # saves bootstrapped CI values calculated above to file
  #write.csv(CIAnnualResults, paste0(WRTDS_files[i], "_Si_EGRETCi_TrendCIs.csv"), row.names=FALSE)
  
 
# }
