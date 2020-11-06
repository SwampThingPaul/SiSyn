#RUN WRTDS
require(dataRetrieval)
require(EGRET)
require(ggplot2)
require(lubridate)
require(tidyr)
require(dplyr)
install.packages(plyr)

#pull files from storage - need to be in own folder
SicsvList<-list.files(pattern="_Si_WRTDS.csv")
QcsvList<-list.files(pattern="_Q_WRTDS.csv")

#GSLOOK

Daily<-readUserDaily("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS", QcsvList[1])
Sample<-readUserSample("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS", SicsvList[1])
Info<-readNWISInfo("", "")
Sample<-removeDuplicates(Sample)

eList<-mergeReport(Info, Daily, Sample)

savePath<-"/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS"
saveResults(savePath, eList)    
eList<-modelEstimation(eList)

fluxBiasMulti(eList)

plotConcTimeDaily(eList)

write.csv(eList$Daily, "GSLOOK_Si_Cont.csv")


#GSMACK (need to fix - names of Q and Si do not match)

Daily<-readUserDaily("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS", QcsvList[2])
Sample<-readUserSample("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS", SicsvList[2])
Info<-readNWISInfo("", "")
Sample<-removeDuplicates(Sample)

eList<-mergeReport(Info, Daily, Sample)

savePath<-"/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS"
saveResults(savePath, eList)    
eList<-modelEstimation(eList)

fluxBiasMulti(eList)

plotConcTimeDaily(eList)

write.csv(eList$Daily, "GSMACK_Si_Cont.csv")


#GSWS01

Daily<-readUserDaily("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS", QcsvList[3])
Sample<-readUserSample("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS", SicsvList[3])
Info<-readNWISInfo("", "")
Sample<-removeDuplicates(Sample)

eList<-mergeReport(Info, Daily, Sample)

savePath<-"/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS"
saveResults(savePath, eList)    
eList<-modelEstimation(eList)

fluxBiasMulti(eList)

plotConcTimeDaily(eList)

write.csv(eList$Daily, "GSWS01_Si_Cont.csv")


##GSWS02

Daily<-readUserDaily("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS", QcsvList[4])
Sample<-readUserSample("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS", SicsvList[4])
Info<-readNWISInfo("", "")
Sample<-removeDuplicates(Sample)

eList<-mergeReport(Info, Daily, Sample)

savePath<-"/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS"
saveResults(savePath, eList)    
eList<-modelEstimation(eList)

fluxBiasMulti(eList)

plotConcTimeDaily(eList)

write.csv(eList$Daily, "GSWS02_Si_Cont.csv")



##GSWS06

Daily<-readUserDaily("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS", QcsvList[5])
Sample<-readUserSample("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS", SicsvList[5])
Info<-readNWISInfo("", "")
Sample<-removeDuplicates(Sample)

eList<-mergeReport(Info, Daily, Sample)

savePath<-"/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS"
saveResults(savePath, eList)    
eList<-modelEstimation(eList)

fluxBiasMulti(eList)

plotConcTimeDaily(eList)

write.csv(eList$Daily, "GSWS02_Si_Cont.csv")



##GSWS07 - missing discharge data from 1987-09-30 to 1994-10-01

Daily<-readUserDaily("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS", QcsvList[6])
Sample<-readUserSample("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS", SicsvList[6])
Info<-readNWISInfo("", "")
Sample<-removeDuplicates(Sample)

eList<-mergeReport(Info, Daily, Sample)

savePath<-"/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS"
saveResults(savePath, eList)    
eList<-modelEstimation(eList)

fluxBiasMulti(eList)

plotConcTimeDaily(eList)

write.csv(eList$Daily, "GSWS07_Si_Cont.csv")


#GSWS08

Daily<-readUserDaily("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS", QcsvList[7])
Sample<-readUserSample("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS", SicsvList[7])
Info<-readNWISInfo("", "")
Sample<-removeDuplicates(Sample)

eList<-mergeReport(Info, Daily, Sample)

savePath<-"/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS"
saveResults(savePath, eList)    
eList<-modelEstimation(eList)

fluxBiasMulti(eList)

plotConcTimeDaily(eList)

write.csv(eList$Daily, "GSWS08_Si_Cont.csv")



#GSWS09

Daily<-readUserDaily("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS", QcsvList[8])
Sample<-readUserSample("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS", SicsvList[8])
Info<-readNWISInfo("", "")
Sample<-removeDuplicates(Sample)

eList<-mergeReport(Info, Daily, Sample)

savePath<-"/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS"
saveResults(savePath, eList)    
eList<-modelEstimation(eList)

fluxBiasMulti(eList)

plotConcTimeDaily(eList)

write.csv(eList$Daily, "GSWS09_Si_Cont.csv")



#GSWS10

Daily<-readUserDaily("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS", QcsvList[9])
Sample<-readUserSample("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS", SicsvList[9])
Info<-readNWISInfo("", "")
Sample<-removeDuplicates(Sample)

eList<-mergeReport(Info, Daily, Sample)

savePath<-"/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS"
saveResults(savePath, eList)    
eList<-modelEstimation(eList)

fluxBiasMulti(eList)

plotConcTimeDaily(eList)

write.csv(eList$Daily, "GSWS10_Si_Cont.csv")


#can be used to get fluxBiasMulti and plotConcTimeDaily plot and 
#write csv for all sites - does not have to be done individually
#this loop requires eLists to be saved as RData workspaces

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS")

RList<-list.files(pattern = ".SI.RData")

pdf("HJA_SI_WRTDS_output.pdf")

for (i in 1:length(RList)) {
  
  NameList<-unlist(strsplit(RList[i], "\\."))
  
  load(RList[i])
  
  eList<-modelEstimation(eList)
  
  fluxBiasMulti(eList)
  
  plotConcTimeDaily(eList)
  
  write.csv(eList$Daily, paste(NameList[1],"_Si_Cont.csv"))
  
}

dev.off()