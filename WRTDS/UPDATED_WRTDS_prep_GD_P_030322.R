## Code to create files for running WRTDS model
# written by Keira Johnson Nov 2020
# modified by Kathi Jo Jankowski April 2021

### Code matches sample data to discharge data by date range
### generates discharge and nutrient data files formatted for input into WRTDS model 

## Load packages
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

# load discharge data
# google drive link: https://drive.google.com/file/d/1onCHASglPwxMR2ZmE44a5xBM4K2OBIO8/view?usp=sharing
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

QLog<-read.csv("DischargeLog_All_011422.csv")

names(QLog)[3]<-"files"

#merge discharge log and list of csv files in google drive
RefTable<-merge(QLog, csv_files, by="files")

# look for files that aren't merging - ignored if they weren't WRTDS sites!
test=anti_join(QLog, RefTable)

#extract columns of the google drive files and site name
# columns = files, Stream, Units
RefTable<-RefTable[,c(1,3,4)]

# not sure what this is removing...  
# RefTable<-RefTable[-c(132),]

#read in master chemistry data
#setwd("U:/Jankowski/My Documents/Projects/Silica Synthesis/Data/Chem Data")

master<-read.csv("20210907_masterdata.csv")

#rename column
names(master)[2]<-"Stream"

#subset P data - using SRP and PO4 interchangeably
masterP<-subset(master, master$variable=="PO4" | master$variable == "SRP")

# convert PO4-P to mgL from uM
masterP$value_mgL = ((masterP$value/1000000)*30.973762)*1000

# remove dates before 1982 for HJ Andrews sites
earlyAND = filter(masterP, LTER == "AND" & year(Sampling.Date) < 1983)

# NOTE - this overwrites new data file as "masterP" without early data
# did that way to avoid re-writing and messing up later code
masterP=anti_join(masterP, earlyAND)

#rename columns
names(masterP)[4]<-"Date"
# value [6] = mM concentration, # value_mgL [7] = mg/L concentration
names(masterP)[7]<-"P"

#convert date to date format
masterP$Date<-as.Date(masterP$Date, "%Y-%m-%d")

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
                        ifelse(Q$Units=="Ls", Q$Q*0.001, "")))
  
  #subset master phosphate file to individual site
  P<-subset(masterP, masterP$Stream==StreamList[i])
  
  #find minimum date of PO4 file
  Pmin<-min(P$Date)
  
  #convert to days since 1970
  Pmin_julian<-as.numeric(Pmin)
  
  #subtract 10 years from Si min to get Q min
  Qmin<-(Pmin_julian-10*365.25)-1
  
  #subset Q file associated with Si file starting 10 years before Si file starts 
  #and ending when the Q file ends
  #extra space of Q file on ends of Si help moving flow weighted average for flux perform better
  Qshort<-Q[Q$Date > Qmin,]
  
  #convert to day of water year
  #MinDay<-as.numeric(hydro.day.new(Pmin))
  
  #find maximum date of PO4 file
  #Pmax<-max(P$Date)
  #convert to day of water year
  #MaxDay<-as.numeric(hydro.day.new(Pmax))
  
  #find difference between beginning of next water year and end of P file
  #P_water_year_diff<-365-MaxDay
  
  #subset Q file associated with P file starting at beginning of water year of start of P file and ending at end
  #of water year of last P file date
  #Qshort<-Q[Q$Date > (Pmin - MinDay) & Q$Date < (Pmax + P_water_year_diff),]
  
  #extract date and discharge columns
  Qshort<-Qshort %>%
    dplyr::select(Date, Qcms)
  
  #write to new folder
  setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/PPrepWRTDS_Updated")
  
  #write csv of discharge file
  write.csv(Qshort, paste0(StreamList[i], "_Q_WRTDS.csv"), row.names = FALSE)
  
  #find minimum date of P file
  Qmin<-min(Q$Date)
  #convert to day of water year
  QMinDay<-as.numeric(hydro.day.new(Qmin))
  
  #find maximum date of P file
  Qmax<-max(Qshort$Date)
  #convert to day of water year
  QMaxDay<-as.numeric(hydro.day.new(Qmax))
  
  #find difference between beginning of next water year and end of P file
  Q_water_year_diff<-365-QMaxDay
  
  #subset Q file associated with P file starting at beginning of water year of start of P file and ending at end
  #of water year of last P file date
  PShort<-P[P$Date > (Qmin) & P$Date < (Qmax),]
  
  #extract date and P columns of P file
  Pdata<-PShort %>%
    dplyr::select(Date, P)
  
  #create remarks variable
  remarks<-""
  
  #add remarks column between date and P columns - required for WRTDS
  Pdata<-add_column(Pdata, remarks, .after = "Date")
  
  #write P file for WRTDS
  write.csv(Pdata, paste0(StreamList[i], "_P_WRTDS.csv"), row.names = FALSE)
  
}
  