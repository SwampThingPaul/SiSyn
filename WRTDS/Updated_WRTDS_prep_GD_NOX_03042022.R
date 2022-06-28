#install.packages("googledrive")
#install.packages("tidyverse")
require(tidyverse)
require(googledrive)
require(stringr)
require(lubridate)
require(reshape)
require(gtools)


#get folder URL from google drive with discharge data
folder_url<-"https://drive.google.com/drive/folders/19lemMC09T1kajzryEx5RUoOs3KDSbeAX"

#get ID of folder
folder<-drive_get(as_id(folder_url))

#get list of csv files from folder
csv_files<-drive_ls(folder, type="csv")

# not sure what this is removing... 
# csv_files<-csv_files[-c(26),]

#split ".csv" from document names
csv_files$files<-word(csv_files$name, 1, sep = "\\.csv")

#setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

QLog<-read.csv("DischargeLog_All_011422.csv")

names(QLog)[3]<-"files"

years_20<-read.csv("Data_years_streams_WRTDS.csv")

names(years_20)[2]<-"Stream"

#merge discharge log and list of csv files in google drive
RefTable<-merge(QLog, csv_files, by="files")

RefTable<-merge(RefTable, years_20, by="Stream")

# look for files that aren't merging
test=anti_join(years_20, RefTable)

#extract columns of the google drive files and site name
# files, Stream, Units
RefTable<-RefTable[,c(1,2,4)]

# not sure what this is removing...  
# RefTable<-RefTable[-c(132),]

#read in master chemistry data
#setwd("U:/Jankowski/My Documents/Projects/Silica Synthesis/Data/Chem Data")

MDL<-read.csv("WRTDS_MDL_N_P.csv")

MDL_N<-MDL[,c(1:3)]

master<-read.csv("20220531_masterdata.csv")

#rename column
names(master)[2]<-"Stream"
#subset Si data
masterNOX<-subset(master, master$variable=="NOx")

# convert NOX to mgL from uM
masterNOX$value_mgL = ((masterNOX$value/1000000)*14.0067)*1000

# remove dates before 1982 for HJ Andrews sites
earlyAND = filter(masterNOX, LTER == "AND" & year(Sampling.Date) < 1983)

# NOTE - this overwrites new data file as "masterNOX" without early data
# did that way to avoid re-writing and messing up later code
masterNOX=anti_join(masterNOX, earlyAND)

#rename columns
names(masterNOX)[4]<-"Date"
# value [6] = mM concentration, # value_mgL [7] = mg/L concentration
names(masterNOX)[7]<-"NOX"

#convert date to date format
masterNOX$Date<-as.Date(masterNOX$Date, "%Y-%m-%d")

#make list of unique sites from reference table
StreamList<-unique(RefTable$Stream)
QList<-unique(RefTable$files)

#create function to turn date into day of water year
hydro.day.new = function(x, start.month = 10L){
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}

#create lists of Q and Date names used in different files
DischargeList<-c("MEAN_Q", "Q_m3sec","Q_cms","Discharge", "InstantQ", "Q", "discharge")
DateList<-c("Date", "dateTime", "dates", "date")

#start loop - will replicate code inside for each unique site
for (i in 1:length(StreamList)) {
  
  setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")
  
  print(i)
  
  #extract name from site list
  stream<-StreamList[i]
  
  #extract row of reference table corresponding to site (extract stream site)
  ref<-subset(RefTable, RefTable$Stream==stream)
  
  #extract row of csv list table corresponding discharge file (extract discharge site)
  csv<-subset(csv_files, csv_files$files==ref$files)
  
  MDL_N_value<-subset(MDL_N, MDL_N$site==stream)
  
  #read in proper discharge file
  #Q<-read.csv(drive_download(file = as_id(csv$id), overwrite = TRUE)$name)
  Q<-read.csv(csv$name)
  
  #name discharge column "Q"
  names(Q)[which(colnames(Q) %in% DischargeList)]<-"Q"
  
  #name date column "Date"
  names(Q)[which(colnames(Q) %in% DateList)]<-"Date"
  
  #set dates to date format
  Q$Date<-as.Date(Q$Date)
  
  Q<-aggregate(Q, by=list(Q$Date), mean)
  
  #remove rows with NA in discharge column
  Q<-Q[!(is.na(Q$Q)),]
  
  #paste units from reference file to units column in current datafile
  Q$Units<-ref$Units[1]
  
  #convert all Q file units to CMS
  Q$Qcms<-ifelse(Q$Units=="cms", Q$Q, 
                 ifelse(Q$Units=="cfs", Q$Q*0.0283,
                        ifelse(Q$Units=="Ls", Q$Q*0.001,
                               ifelse(Q$Units=="cmd", Q$Q*1.15741e-5, ""))))
  
  #subset master silica file to individual site
  NOX<-subset(masterNOX, masterNOX$Stream==StreamList[i])
  
  #find minimum date of NOX file
  NOXmin<-min(NOX$Date)
  
  #convert to days since 1970
  NOXmin_julian<-as.numeric(NOXmin)
  
  #subtract 10 years from Si min to get Q min
  Qmin<-(NOXmin_julian-10*365.25)-1
  
  #subset Q file associated with Si file starting 10 years before Si file starts 
  #and ending when the Q file ends
  #extra space of Q file on ends of Si help moving flow weighted average for flux perform better
  Qshort<-Q[Q$Date > Qmin,]
  
  # #convert to day of water year
  # MinDay<-as.numeric(hydro.day.new(NOXmin))
  # 
  # #find maximum date of NOX file
  # NOXmax<-max(NOX$Date)
  # #convert to day of water year
  # MaxDay<-as.numeric(hydro.day.new(NOXmax))
  # 
  # #find difference between beginning of next water year and end of NOX file
  # si_water_year_diff<-365-MaxDay
  # 
  # #subset Q file associated with NOX file starting at beginning of water year of start of NOX file and ending at end
  # #of water year of last NOX file date
  # Qshort<-Q[Q$Date > (NOXmin - MinDay) & Q$Date < (NOXmax + si_water_year_diff),]
  
  #extract date and discharge columns
  Qshort<-Qshort %>%
    dplyr::select(Date, Qcms) ### NEED TO CHANGE "Q" to Qcms - correct??? ####
  
  #write to new folder
  setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NPrepWRTDS_Updated")
  
  #write csv of discharge file
  write.csv(Qshort, paste0(StreamList[i], "_NOX_Q_WRTDS.csv"), row.names = FALSE)
  
  #find minimum date of NOX file
  Qmin<-min(Q$Date)
  
  #convert to day of water year
  QMinDay<-as.numeric(hydro.day.new(Qmin))
  
  #find maximum date of NOX file
  Qmax<-max(Qshort$Date)
  #convert to day of water year
  QMaxDay<-as.numeric(hydro.day.new(Qmax))
  
  #find difference between beginning of next water year and end of NOX file
  Q_water_year_diff<-365-QMaxDay
  
  #subset Q file associated with NOX file starting at beginning of water year of start of NOX file and ending at end
  #of water year of last NOX file date
  NOXShort<-NOX[NOX$Date > (Qmin) & NOX$Date < (Qmax),]
  
  #extract date and NOX columns of NOX file
  NOXdata<-NOXShort %>%
    dplyr::select(Date, NOX)
  
  #create remarks variable
  remarks<-""
  
  #add remarks column between date and NOX columns - required for WRTDS
  NOXdata<-add_column(NOXdata, remarks, .after = "Date")
  
  #add < when value is less than MDL
  NOXdata$remarks<-ifelse(NOXdata$NOX < MDL_N_value$NO3_MDL, "<", "")
  
  #write NOX file for WRTDS
  write.csv(NOXdata, paste0(StreamList[i], "_NOX_WRTDS.csv"), row.names = FALSE)
  
}
