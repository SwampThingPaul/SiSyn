##use file to format files for WRTDS model

#install.packages("tibble")
require(ggplot2)
require(dplyr)
require(tibble)


#set WD and read in discharge files
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/HJ_Andrews/Data")
HJAQ<-read.csv("HJAndrewsDischarge.csv")
HJAQ<-HJAQ[,c(3, 5, 6)]

names(HJAQ)[2]<-"Date"
names(HJAQ)[1]<-"site"

#use this to format dates before 1970 (R will covert any date before 1970 to 2000s)
HJAQ$Year<-format(as.Date(HJAQ$Date, format = "%m/%d/%y"), "%y")

HJAQearly<-subset(HJAQ, HJAQ$Year > 48 & HJAQ$Year <= 99)

HJAlate<-subset(HJAQ, HJAQ$Year < 48)

HJAQearly$Date<-format(as.Date(HJAQearly$Date, format = "%m/%d/%y"), "19%y-%m-%d")
HJAlate$Date<-format(as.Date(HJAlate$Date, format = "%m/%d/%y"))

HJAQ2<-rbind(HJAQearly, HJAlate)
HJAQ2<-HJAQ2[c(1,2,3)]


#read master dataset
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

master<-read.csv("master.csv")

#extract HJA sites
HJA<-subset(master, master$LTER=="AND")

HJA<-HJA[,-c(1, 2)]

#reshape so that parameters are columns
HJA2<-reshape(HJA, direction = "wide", timevar= c("variable"), v.names = c("value"), 
              idvar = c("site", "Sampling.Date"))

#rename the columns
names(HJA2)<-c("Site", "Date", unique(HJA$variable))

HJA2$Date<-as.Date(HJA2$Date, "%m/%d/%y")

#extract Si data - remove NAs from data
HJASi<-HJA2[,c(1,2,6)]
HJASi<-na.omit(HJASi)

#extract unique sites
SiSiteList<-unique(HJASi$Site)

#create dataframe of silica date ranges

#create initial datafile for loop - calculate min, max, and length
data<-subset(HJASi, HJASi$Site==SiSiteList[1])
min<-as.Date(min(data$Date))
max<-as.Date(max(data$Date))
len<-length(data$DSi)

#paste above calculated values into list
list<-c(paste(SiSiteList[1]), min, max, len)

#do for each site
for (i in 2:length(SiSiteList)) {
  
  data<-subset(HJASi, HJASi$Site==SiSiteList[i])
  min<-min(data$Date)
  max<-max(data$Date)
  len<-length(data$DSi)
  
  list2<-c(paste(SiSiteList[i]), min, max, len)
  
  list<-as.data.frame(rbind(list, list2))
  
}

#convert dates back to date format
list$V2<-as.Date(as.numeric(list$V2), origin = "1970-01-01")
list$V3<-as.Date(as.numeric(list$V3), origin = "1970-01-01")

#rename columns
names(list)<-c("Site", "startDate", "endDate", "Obs")


#create dataframe of discharge data date ranges - similar to above but includes "missing" column that 
#indicates how many days are missing in date range

QSiteList<-unique(HJAQ2$site)
HJAQ2$Date<-as.Date(HJAQ2$Date)

data<-subset(HJAQ2, HJAQ2$site==QSiteList[1])
min<-min(data$Date)
max<-max(data$Date)
date_range<-seq(min, max, by=1)

missing<-length(date_range[!date_range %in% data$Date])

Qlist<-c(paste(QSiteList[1]), min, max, missing)

for (i in 2:length(QSiteList)) {
  
  data<-subset(HJAQ2, HJAQ2$site==QSiteList[i])
  min<-min(data$Date)
  max<-max(data$Date)
  
  date_range<-seq(min, max, by=1)
  
  missing<-length(date_range[!date_range %in% data$Date])
  
  Qlist2<-c(paste(QSiteList[i]), min, max, missing)
  
  Qlist<-as.data.frame(rbind(Qlist, Qlist2))
  
}

Qlist$V2<-as.Date(as.numeric(Qlist$V2), origin = "1970-01-01")
Qlist$V3<-as.Date(as.numeric(Qlist$V3), origin = "1970-01-01")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/HJA_WRTDS")


#subset each discharge file to have a date range that matches the silica date range
#save each site as individual csv
for (i in 1:length(QSiteList)) {
  
  maxmin<-subset(list, list$V1==QSiteList[i])
  min<-maxmin$V2
  max<-maxmin$V3
  dataQ<-subset(HJAQ2, HJAQ2$site==QSiteList[i])
  dataQ<-dataQ[,-c(1)]
  dataQ<-subset(dataQ, dataQ$Date > min-10  & dataQ$Date < max+10)
  write.csv(dataQ, paste(QSiteList[i], "_Q_WRTDS.csv"), row.names = FALSE)
  
}

#create "remarks" column - required for WRTDS
remarks<-""

#add remarks column to each silica file - save each site as individual csv
for (i in 1:length(SiSiteList)) {
  
  dataSi<-subset(HJASi, HJASi$Site==SiSiteList[i])
  dataSi<-dataSi[,-c(1)]
  dataSi<-add_column(dataSi, remarks, .after = "Date")
  write.csv(dataSi, paste(SiSiteList[i], "_Si_WRTDS.csv"), row.names = FALSE)
  
}
