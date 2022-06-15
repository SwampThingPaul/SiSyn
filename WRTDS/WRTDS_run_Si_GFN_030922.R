## Running GFN Version of EGRET/WRTDS

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

#################################################################################
### start here if not downloading from Google Drive
## set up files for loop
#setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Data/WRTDS/WRTDS_prep_Si_01162022")
setwd("C:/Users/kjankowski/Desktop/WRTDS_prep_special")

#make list of all files that you just downloaded to local folder
#WRTDS_files_List<-list.files(path = "C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Data/WRTDS/WRTDS_prep_Si_01162022")
WRTDS_files_List<-list.files(path = "C:/Users/kjankowski/Desktop/WRTDS_prep_special")

#make list of only Q files
WRTDS_files_List_Q<-WRTDS_files_List[WRTDS_files_List %like% "Q_WRTDS"]

#make list of only Si files
WRTDS_files_List_Si<-WRTDS_files_List[WRTDS_files_List %like% "Si"]

#make list of only INFO files
WRTDS_files_List_Info<-WRTDS_files_List[WRTDS_files_List %like% "INFO"]

WRTDS_files<-sub("*_Q_WRTDS.csv", "", WRTDS_files_List_Q)

# Subsetting list for only those in final WRTDS analysis
#setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Data/WRTDS")
#long_term=read.csv("Data_Years_Streams_WRTDS_010522.csv")
#long_term_sites=long_term$site

# for testing 
#i=i

#run WRTDS! output will be saved to the same file
for (i in 1:length(WRTDS_files)) {
  
  #read in Q file
  Daily<-readUserDaily("C:/Users/kjankowski/Desktop/WRTDS_prep_special", WRTDS_files_List_Q[i],
                       qUnit = 2)
  
  #read in Si file
  Sample<-readUserSample("C:/Users/kjankowski/Desktop/WRTDS_prep_special", WRTDS_files_List_Si[i])
  
  #read in Info file
  Info<-readUserInfo("C:/Users/kjankowski/Desktop/WRTDS_prep_special", WRTDS_files_List_Info[i])
  
  #remove duplicates from sample file
  Sample<-removeDuplicates(Sample)
  
  Sample<-aggregate(Sample, by=list(Sample$Date), mean)
  
  Sample<-Sample[,-c(1)]
  
  #merge into eList
  eList<-mergeReport(Info, Daily, Sample)
  
  #save workspace so it can be accessed later
  savePath<-"C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDS_Si_results_GFN"
  saveResults(savePath, eList)
  
  #estimate continuous Si - no longer using
  #eList<-modelEstimation(eList, minNumObs=50)
  
  ### GFN - estimate continuous Si with Q and CQ components ("GFN" method)
  eListOut <- runPairs(eList, windowSide = 7, minNumObs=50, year1=minYP, year2=maxYP)
  #eListOut <- runSeries(eList, windowSide = 6, minNumObs=50)
  
  #CIAnnualResults <- ciCalculations(eListOut, 
   #                                 verbose = TRUE, 
    #                                nBoot = 100,
     #                               blockLength = 200, 
      #                              widthCI = 90)
  
  ###########
  ##  Adjustments to period of analysis for specific sites
  # For Sagehen Site
  # eListOut <- blankTime(eListOut, startBlank = "1996-01-01", endBlank = "2001-01-01")
  
  # adjust for MCM 
  #eListOut <- setPA(eListOut, paStart=12, paLong=2)
  
  # Adjust for NWT
  # Martinelli
  #eListOut <- setPA(eListOut, paStart=5, paLong=5)
  # Saddle
  #eListOut <- setPA(eListOut, paStart=5, paLong=3)
  
  ############
  #write output to this folder
  setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDS_Si_results_GFN")
  
  # extract error statistics
  #error <- errorStats(eListOut)
  #write.csv(error, paste0(WRTDS_files[i], "_ErrorStats_WRTDS.csv"), row.names=FALSE)
  
  #extract continuous Si file from eList
  ContConc<-eListOut$Daily
  
  #write csv of continuous Si data
  write.csv(ContConc, paste0(WRTDS_files[i], "_ContSi_GFN_WRTDS.csv"))
  
  #average yearly stats
  Results<-tableResults(eListOut)
  
  #write csv of results dataframe
  write.csv(Results, paste0(WRTDS_files[i], "_ResultsTable_GFN_WRTDS.csv"))
  
  #make new column for year
  ContConc$Year<-format(as.Date(ContConc$Date), "%Y")
  
  # calculate monthly values
  months=calculateMonthlyResults(eListOut)
  write.csv(months, paste0(WRTDS_files[i], "_Monthly_GFN_WRTDS.csv"))
  
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
  write.csv(Trends, paste0(WRTDS_files[i], "_TrendsTable_GFN_WRTDS.csv"))
  
  #open pdf for graphical output
  pdf(paste0(WRTDS_files[i], "_WRTDS_GFN_output.pdf"))
  
  #residual plots
  #fluxBiasMulti(eListOut)
  
  #examine model fit
  plotConcTimeDaily(eListOut)
  
  #plot concentration
  plotConcHist(eListOut) # minYP, maxYP)
  
  #plot flux
  plotFluxHist(eListOut) #, minYP, maxYP)
  
  #plot data
  multiPlotDataOverview(eListOut)
  
  dev.off()
  
  # For most streams that don't need monthly adjustment
  eListPairs <- runPairs(eList, windowSide = 6, 
                         minNumObs=50, 
                         year1=minYP, 
                         year2=maxYP,
                        )
  
  # for streams with month adjustment
  #eListPairs <- runPairs(eList, windowSide = 11, 
  #                      minNumObs=50, 
  #                     year1=minYP, 
  #                    year2=maxYP, paStart=5, paLong=3)

  write.csv(eListPairs, paste0(WRTDS_files[i], "_Si_GFN.csv"))
  
 
## Trend uncertainty  
  # runPairsBoot
  eBoot<-runPairsBoot(eList,eListPairs, nBoot=100,blockLength = 200)
  bootResults <- cbind(eBoot$xConc, eBoot$xFlux, eBoot$pConc, eBoot$pFlux)
  bootSummary <- eBoot$bootOut
  
## keep bootstrapped results
  CIs=as.data.frame(bootResults)
  CIs$solute=rep("Si", nrow(bootResults))
  colnames(CIs)=c("xConc", "xFlux", "pConc", "pFlux", "solute")
  write.csv(CIs, paste0(WRTDS_files[i], "_EGRETCi_GFN_bootstraps.csv"), row.names=FALSE)
  
  Summary=as.data.frame(bootSummary)
  write.csv(Summary, paste0(WRTDS_files[i], "_Si_EGRETCi_GFN_Trend.csv"))
  
  #calculates CI around model fit - TAKES FOREVER
  #CIAnnualResults <- ciCalculations(eList,nBoot=100,blockLength=200)
  # saves bootstrapped CI values calculated above to file
  #write.csv(CIAnnualResults, paste0(WRTDS_files[i], "_Si_EGRETCi_TrendCIs.csv"), row.names=FALSE)
  
 
}
