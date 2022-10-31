#install.packages("googledrive")
#install.packages("tidyverse")
require(tidyverse)
require(googledrive)
require(stringr)
require(lubridate)
require(reshape)
require(gtools)
require(dplyr)
require(googledrive)

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

master<-read.csv("20220531_masterdata.csv")
master_si<-subset(master, master$variable=="DSi")
master_si$Sampling.Date<-as.Date(master_si$Sampling.Date)

min_date <- master_si %>%
  dplyr::group_by(site) %>%
  dplyr::summarise(min=min(Sampling.Date))
  
  
max_date <- master_si %>%
  dplyr::group_by(site) %>%
  dplyr::summarise(max(Sampling.Date))

counts_table<- master_si %>%
  dplyr::group_by(site) %>%
  dplyr::tally()

Si_df<-merge(min_date, max_date, by="site")
Si_df<-merge(Si_df, counts_table, by="site")

colnames(Si_df)<-c("Stream", "MinSi", "MaxSi", "n")

names(QLog)[3]<-"files"

#merge discharge log and list of csv files in google drive
RefTable<-merge(QLog, csv_files, by="files")

# look for files that aren't merging
test=anti_join(QLog, RefTable)

#extract columns of the google drive files and site name
# files, Stream, Units
#RefTable<-RefTable[,c(1,3,4)]

DateList<-c("Date", "dateTime", "dates", "date")

date_diff<-list()
min_date<-list()
max_date<-list()

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/All_Q")

for (i in 1:length(RefTable$Stream)) {
  
  file<-RefTable$name[i]
  
  data_file<-read.csv(file)
  
  names(data_file)[which(colnames(data_file) %in% DateList)]<-"Date"
  
  data_file$Date<-as.Date(data_file$Date)
  
  min_date[[i]]<-min(data_file$Date, na.rm = TRUE)
  
  max_date[[i]]<-max(data_file$Date, na.rm = TRUE)
  
  date_diff[[i]]<-difftime(max_date[[i]][1],min_date[[i]][1], units = "days")/365 
  
}


dates_df<-do.call(rbind, date_diff)
min_df<-do.call(rbind, min_date)
max_df<-do.call(rbind, max_date)

dates_df<-cbind(RefTable[,c(2,3)], min_df, max_df, dates_df)
colnames(dates_df)<-c("LTER", "Stream", "MinDate", "MaxDate", "POR")
dates_df<-dates_df[complete.cases(dates_df),]
dates_df$MinDate<-as.Date(dates_df$MinDate, origin = "1970-01-01")
dates_df$MaxDate<-as.Date(dates_df$MaxDate, origin = "1970-01-01")

master_q<-merge(Si_df, dates_df, by="Stream")
master_q$overlapmin<-ifelse(master_q$MinDate > master_q$MinSi, master_q$MinDate, master_q$MinSi)
master_q$overlapmax<-ifelse(master_q$MaxDate > master_q$MaxSi, master_q$MaxSi, master_q$MaxDate)

master_q$overlapmin<-as.Date(master_q$overlapmin, origin = "1970-01-01")
master_q$overlapmax<-as.Date(master_q$overlapmax, origin = "1970-01-01")
master_q$POR_overlap<-difftime(master_q$overlapmax, master_q$overlapmin, units = "days")/365

dates_df_5<-subset(master_q, master_q$POR_overlap > 4)

dates_df_5_sites<-dates_df_5$Stream


LT<-read.csv("Data_years_streams_WRTDS.csv")

LT_streams<-LT$Stream.Site

newsites<-setdiff(dates_df_5_sites, LT_streams)

write.csv(newsites, "NewSites.csv")

dates_df_10<-subset(master_q, master_q$POR_overlap > 9.99)

write.csv(master_q, "Si_Q_POR.csv")

pdf("SiPeriodRecordPlots.pdf")

ggplot(dates_df_10, aes(LTER))+geom_histogram(stat = "count")+theme_classic()+
  ggtitle("10 years")

dev.off()


##make plots of monthly distribution of samples

si_5_year<-master_si[master_si$Site.Stream.Name %in% dates_df_5$Stream,]
si_5_year$month<-format(as.Date(si_5_year$Sampling.Date), "%m")

pdf("Seasonality_Distribution.pdf", width = 15, height = 10)

si_5_year %>%
  group_by(site, month) %>%
  tally() %>%
  ggplot(aes(x=month, y=n))+geom_point()+facet_wrap(~site, scales = "free_y")+theme_bw()
  
dev.off()
  







