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

#split ".csv" from document names
csv_files$files<-word(csv_files$name, 1, sep = "\\.csv")

# read in discharge log
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

QLog<-read.csv("DischargeLog_All_011422.csv")

names(QLog)[3]<-"files"

#merge discharge log and list of csv files in google drive
RefTable<-merge(QLog, csv_files, by="files")

# look for files that aren't merging
test=anti_join(QLog, RefTable)

#extract columns of the google drive files and site name
# files, Stream, Units
RefTable<-RefTable[,c(1,3,4)]

# to do a subset of sites
#RefTable<-filter(RefTable, Stream == "Q1" | Stream == "Q2"| Stream == "Q3")

#read in master chemistry data
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")
master<-read.csv("20220426_masterdata.csv")
master$Sampling.Date<-as.Date(master$Sampling.Date)

#rename column
names(master)[2]<-"Stream"

#subset Si data
masterSi<-subset(master, master$variable=="DSi")

# convert Si to mgL from uM
masterSi$value_mgL = ((masterSi$value/1000000)*28.0855)*1000

# remove dates before 1982 for HJ Andrews sites
earlyAND = filter(masterSi, LTER == "AND" & year(Sampling.Date) < 1983)

# NOTE - this overwrites new data file as "masterSi" without early data
# did that way to avoid re-writing and messing up later code
masterSi=anti_join(masterSi, earlyAND)

#rename columns
names(masterSi)[4]<-"Date"
# value [6] = mM concentration, # value_mgL [7] = mg/L concentration
names(masterSi)[7]<-"Si"

#convert date to date format
masterSi$Date<-as.Date(masterSi$Date, "%Y-%m-%d")

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
  
  #read in proper discharge file - use bottom line when csv have already been downloaded
  #Q<-read.csv(drive_download(file = as_id(csv$id), overwrite = TRUE)$name)
  Q<-read.csv(csv$name)
  
  #name discharge column "Q"
  names(Q)[which(colnames(Q) %in% DischargeList)]<-"Q"
  
  #name date column "Date"
  names(Q)[which(colnames(Q) %in% DateList)]<-"Date"
  
  #set dates to date format
  Q$Date<-as.Date(Q$Date)
  #Q$Date <- as.POSIXct(strptime(Q$Date, "%Y-%m-%d"))
  
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
  Si<-subset(masterSi, masterSi$Stream==StreamList[i])
  
  #find minimum date of Si file
  Simin<-min(Si$Date)
  
  #convert to days since 1970
  Simin_julian<-as.numeric(Simin)
  
  #subtract 10 years from Si min to get Q min
  Qmin<-(Simin_julian-10*365.25)-1
  
  #subset Q file associated with Si file starting 10 years before Si file starts 
  #and ending when the Q file ends
  #extra space of Q file on ends of Si help moving flow weighted average for flux perform better
  Qshort<-Q[Q$Date > Qmin,]
  
  # #convert to day of water year
  # MinDay<-as.numeric(hydro.day.new(Simin))
  # 
  # #find maximum date of Si file
  # Simax<-max(Si$Date)
  # #convert to day of water year
  # MaxDay<-as.numeric(hydro.day.new(Simax))
  # 
  # #find difference between beginning of next water year and end of Si file
  # si_water_year_diff<-365-MaxDay
  
  #subset Q file associated with Si file starting at beginning of water year of start of Si file and ending at end
  #of water year of last Si file date
  # Qshort<-Q[Q$Date > (Simin - MinDay) & Q$Date < (Simax + si_water_year_diff),]
  
  #extract date and discharge columns
  Qshort<-Qshort %>%
    dplyr::select(Date, Qcms)
  
  #write to new folder
  setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/SiPrepWRTDS_Updated")
  
  #write csv of discharge file
  write.csv(Qshort, paste0(StreamList[i], "_Q_WRTDS.csv"), row.names = FALSE)
  
  #find minimum date of Q file
  Qmin<-min(Q$Date)
  
  #convert to day of water year
  QMinDay<-as.numeric(hydro.day.new(Qmin))
  
  #find maximum date of Si file
  Qmax<-max(Qshort$Date)
  
  #convert to day of water year
  QMaxDay<-as.numeric(hydro.day.new(Qmax))
  
  #find difference between beginning of next water year and end of Si file
  Q_water_year_diff<-365-QMaxDay
  
  #subset Q file associated with Si file starting at beginning of water year of start of Si file and ending at end
  #of water year of last Si file date
  SiShort<-Si[Si$Date > (Qmin) & Si$Date < (Qmax),]
  
  #extract date and Si columns of Si file
  Sidata<-SiShort %>%
    dplyr::select(Date, Si)
  
  #create remarks variable
  remarks<-""
  
  #add remarks column between date and Si columns - required for WRTDS
  Sidata<-add_column(Sidata, remarks, .after = "Date")
  
  #write Si file for WRTDS
  write.csv(Sidata, paste0(StreamList[i], "_Si_WRTDS.csv"), row.names = FALSE)
  
}








min_date_df<-data.frame(matrix(ncol = 2, nrow = 0))

#start loop - will replicate code inside for each unique site
for (i in 1:length(StreamList)) {
  
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
  #Q$Date <- as.POSIXct(strptime(Q$Date, "%Y-%m-%d"))
  
  Q<-aggregate(Q, by=list(Q$Date), mean)
  
  #remove rows with NA in discharge column
  Q<-Q[!(is.na(Q$Q)),]
  
  Qmin<-max(Q$Date)
  
  #subset master silica file to individual site
  Si<-subset(masterSi, masterSi$Stream==StreamList[i])
  
  #find minimum date of Si file
  Simin<-max(Si$Date)
  
  min_date<-cbind(Qmin, Simin)
  
  min_date_df[i,]<-min_date
  
}

min_date_df_dup<-min_date_df

min_date_df<-min_date_df_dup

min_date_df$X1<-as.Date(min_date_df$X1, origin = "1970-01-01")

min_date_df$X2<-as.Date(min_date_df$X2, origin = "1970-01-01")

rownames(min_date_df)<-StreamList
colnames(min_date_df)<-c("Q_max", "Si_max")

min_date_df$min_date_diff<-difftime(min_date_df$Q_max, min_date_df$Si_max, units = "days")

min_date_df$date_diff_year<-as.numeric(min_date_df$min_date_diff)/365.25

remove_inf<-which(min_date_df$date_diff_year==Inf)

min_date_df<-min_date_df[-remove_inf,]

write.csv(min_date_df,"Q_Si_Date_Max.csv")

