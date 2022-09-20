#install.packages("googledrive")
#install.packages("tidyverse")
require(tidyverse)
require(googledrive)
require(stringr)
require(lubridate)
require(reshape)
require(gtools)

# Make an experimental folder to export/import to/from
dir.create(path = "WRTDS_test", showWarnings = F)

#get folder URL from google drive with discharge data
folder_url<-"https://drive.google.com/drive/folders/19lemMC09T1kajzryEx5RUoOs3KDSbeAX"

#get ID of folder
folder<-drive_get(as_id(folder_url))

#get list of csv files from folder
csv_files<-drive_ls(folder, type="csv")

#split ".csv" from document names
csv_files$files<-word(csv_files$name, 1, sep = "\\.csv")

# setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")
# 

# add check that its the most recent date?
QLog<-read.csv(file.path("WRTDS_test", "011422_DischargeLog.csv"))

names(QLog)[3]<-"files"

# Not sure what the first line is for? but copied both lines from NH4 prep
##we will need to put a master MDL file here - then maybe can just select the solute column
#NH4_MDL<-read.csv("NH4_MDL.csv")

#merge discharge log and list of csv files in google drive
RefTable<-merge(QLog, csv_files, by="files")

#extract columns of the google drive files and site name
# files, Stream, Units
RefTable<-RefTable[,c(1,3,4)]

# not sure what this is removing...  
# RefTable<-RefTable[-c(132),]

#read in master chemistry data - add check that its the most recent date?
master<-read.csv(file.path("WRTDS_test", "20220816_masterdata.csv"))

#rename column
names(master)[3]<-"Stream"
#subset Si data
masterSi<-subset(master, master$variable=="DSi")

# NOTE: for P model - subset P data - using SRP and PO4 interchangeably
# masterP<-subset(master, master$variable=="PO4" | master$variable == "SRP")

# Unit conversions for Si, NO3 and P
# convert DSi to mgL from uM
masterSi$value_mgL = ((masterSi$value/1000000)*28.0855)*1000
# convert NOX to mgL from uM
#masterNOX$value_mgL = ((masterNOX$value/1000000)*14.0067)*1000
# convert PO4-P to mgL from uM
#masterP$value_mgL = ((masterP$value/1000000)*30.973762)*1000
# convert NH4 to mgL from uM
#masterNH4$value_mgL = ((masterNH4$value/1000000)*14.0067)*1000

# remove dates before 1982 for HJ Andrews sites
earlyAND <- filter(masterSi, LTER == "AND" & year(Sampling.Date) < 1983)

# NOTE - this overwrites new data file as "masterSi" without early data
# did that way to avoid re-writing and messing up later code
masterSi <- anti_join(masterSi, earlyAND)

#rename columns
names(masterSi)[5]<-"Date"
# value [6] = mM concentration, # value_mgL [7] = mg/L concentration
# we want value in mg/L for all chemicals!
colnames(masterSi)[which(names(masterSi)=="value_mgL")]<-"DSi"

#convert date to date format
masterSi$Date<-as.Date(masterSi$Date, "%Y-%m-%d")

#make list of unique sites from reference table
StreamList <- unique(RefTable$Stream)
QList <- unique(RefTable$files)

#create function to turn date into day of water year
hydro.day.new = function(x, start.month = 10L){
  start.yr = lubridate::year(x) - (lubridate::month(x) < start.month)
  start.date = lubridate::make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}

#create lists of Q and Date names used in different files - not sure we got all the names but 
#looks like you added "val" for most recent files from Macrosheds?
DischargeList<-c("MEAN_Q", "Q_m3sec","Q_cms","Discharge", "InstantQ", "Q", "discharge","val")
DateList<-c("Date", "dateTime", "dates", "date", "datetime", "Sampling.Date")

#start loop - will replicate code inside for each unique site
for (i in 1:length(StreamList)) {
  
  # setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/MacroSheds_Sites/Si")
  
  print(i)
  
  #extract name from site list
  stream <- StreamList[i]
  
  #extract row of reference table corresponding to site (extract stream site)
  ref <- subset(RefTable, Stream == stream)
  
  #extract row of csv list table corresponding discharge file (extract discharge site)
  csv <- subset(csv_files, files == ref$files)
  
  # Download proper discharge file
  drive_download(file = as_id(csv$id),
                 path = file.path("WRTDS_test", csv$name),
                 overwrite = TRUE)
  
  #read in proper discharge file
  Q <- read.csv(file.path("WRTDS_test", csv$name))
  
  #name discharge column "Q"
  names(Q)[which(colnames(Q) %in% DischargeList)]<-"Q"
  
  #name date column "Date"
  names(Q)[which(colnames(Q) %in% DateList)]<-"Date"
  
  #set dates to date format
  Q$Date<-as.Date(Q$Date)
  
  # Average discharge within each date
  Q<-aggregate(Q, by=list(Q$Date), mean)
  
  #remove rows with NA in discharge column
  Q<-Q[!(is.na(Q$Q)),]
  
  #paste units from reference file to units column in current datafile
  Q$Units<-ref$Units[1]
  
  #convert all Q file units to CMS
  Q <- Q %>%
    dplyr::mutate(Qcms = dplyr::case_when(
      Units == "cms" ~ Q,
      Units == "cfs" ~ Q * 0.0283,
      Units == "Ls" ~ Q * 0.001,
      Units == "cmd" ~ Q * 1.15741e-5))
  
  #subset master silica file to individual site
  Si<-subset(masterSi, masterSi$Stream==StreamList[i])
  
  #find minimum date of NOX file
  Simin<-min(Si$Date)
  
  #convert to days since 1970
  Simin_julian<-as.numeric(Simin)
  
  #subtract 10 years from Si min to get Q min
  Qmin<-(Simin_julian - 10 * 365.25) - 1
  
  #subset Q file associated with Si file starting 10 years before Si file starts 
  #and ending when the Q file ends
  #extra space of Q file on ends of Si helps moving flow weighted average for flux perform better
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
    dplyr::select(Date, Qcms) ### needs to be Qcms here! ####
  
  #write to new folder
  # setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/MacroSiPrepWRTDS")
  
  #write csv of discharge file
  write.csv(Qshort, file.path("WRTDS_test", paste0(StreamList[i], "_Si_Q_WRTDS.csv")), row.names = FALSE)
  
  #find minimum date of NOX file
  Qmin<-min(Q$Date)
  
  #convert to day of water year
  QMinDay<-as.numeric(hydro.day.new(Qmin))
  
  #find maximum date of NOX file
  Qmax<-max(Qshort$Date)
  #convert to day of water year
  QMaxDay<-as.numeric(hydro.day.new(Qmax))
  
  #find difference between beginning of next water year and end of NOX file
  Q_water_year_diff <- 365 - QMaxDay
  
  #subset Q file associated with NOX file starting at beginning of water year of start of NOX file and ending at end
  #of water year of last NOX file date
  SiShort<-Si[Si$Date > (Qmin) & Si$Date < (Qmax),]
  
  #extract date and DSi columns of DSi file
  Sidata<-SiShort %>%
    dplyr::select(Date, variable)
  
  #create remarks variable
  remarks<-""
  
  #add remarks column between date and NOX columns - required for WRTDS
  Sidata<-add_column(Sidata, remarks, .after = "Date")
  
<<<<<<< HEAD:WRTDS/Updated_WRTDS_prep_GD_MacroSheds_Si_08162022.R
  #MDL<-subset(NH4_MDL, NH4_MDL$site==stream)
  
  #add < when value is less than MDL
  # we don't have MDL values for Si, but do for NO3, NH4 and PO4/SRP
  #Sidata$remarks<-ifelse(NH4data$NH4 < MDL$MDL..mg.L., "<", "")
=======
  # MDL<-subset(NH4_MDL, NH4_MDL$site==stream)
  
  #add < when value is less than MDL
  # we don't have MDL values for Si, but do for NO3, NH4 and PO4/SRP
  # Sidata$remarks<-ifelse(NH4data$NH4 < MDL$MDL..mg.L., "<", "")
>>>>>>> e9b95534bbc9a22791e7f962db27ad2547288c28:WRTDS/Archive/Updated_WRTDS_prep_GD_MacroSheds_Si_08162022.R
  
  #write Si file for WRTDS
  write.csv(Sidata, file.path("WRTDS_test", paste0(StreamList[i], "_Si_WRTDS.csv")), row.names = FALSE)
  
}
