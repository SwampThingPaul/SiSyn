# install.packages("googledrive")
# install.packages("tidyverse")
# install.packages("gtools")
# install.packages("rtools")
#install.packages("plyr")
require(tidyverse)
require(googledrive)
require(stringr)
require(lubridate)
require(reshape)
require(gtools)
require(plyr)
require(dplyr)
require(tidyr)
require(data.table)
require(dataRetrieval)


#get folder URL from google drive with discharge data
#use "cleaned up" WRTDS prep files Analysis > WRTDS Analysis > WRTDS_Si_prep
#only use files ending in "_Q_WRTDS.csv"
folder_url = "https://drive.google.com/drive/folders/19lemMC09T1kajzryEx5RUoOs3KDSbeAX?usp=sharing"

#get ID of folder
folder = drive_get(as_id(folder_url))

#get list of csv files from folder
csv_files = drive_ls(folder, type="csv")

#get just site names from csv file name
#keeps text (site name) from WRTDS Q prep files
#csv_files$site = str_extract(csv_files$name, pattern="(?<=).*(?=_Q_WRTDS.csv)") 

#extract only WRTDS prep discharge files
#WRTDS_discharge = csv_files[csv_files$name %like% "Q_WRTDS.csv",]


#check working directory where files will be stored locally; separate folder within project folder
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/All_Q")
#"L:/GitHub/SiSyn/Merge Site Discharge"
#setwd("L:/GitHub/SiSyn/Merge Site Discharge/discharge files")

#download each file to the working directory; files are saved locally
for (i in 1:length(csv_files$drive_resource)) {
  drive_download(csv_files$drive_resource[i],  overwrite=T)
}

#add column for site name
#loop through each downloaded csv file and add appropriate site name
discharge_files = list.files(path="/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/All_Q", pattern = ".csv")

#remove all "Master Q" files
discharge_files<-discharge_files[!(discharge_files %like% "UpdatedAll_Q_master")]
remove_these<-setdiff(discharge_files, csv_files$name)
discharge_files<-discharge_files[!(discharge_files %in% remove_these)]

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#read in reference table - you might need to change this link
ref_table_link<-"https://docs.google.com/spreadsheets/d/11t9YYTzN_T12VAQhHuY5TpVjGS50ymNmKznJK4rKTIU/edit?usp=sharing"

ref_table_folder = drive_get(as_id(ref_table_link))

ref_table<-drive_download(ref_table_folder$drive_resource, overwrite = T)

QLog<-read_xlsx(ref_table$local_path)

#get list of csv files from folder
#csv_files = drive_ls(folder, type="csv")

#read in discharge log
#QLog<-read.csv("WRTDS_Reference_Table_LTER_V2.csv")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/All_Q")

#Q<-read.csv("UpdatedAll_Q_master.csv")

#create list to store output from for loop
data_list = list()

#this is where you add different names for discharge and date columns
#you will need to add more to incorporate new data
DischargeList<-c("MEAN_Q", "Discharge", "InstantQ", "Q_m3sec", "discharge", "Q", "var", "Value")
DateList<-c("Date", "dateTime", "dates", "date", "datetime")

#i=4

#loop through each discharge file
#rename columns, convert units, keep only important columns

for (i in 1:length(discharge_files)) {
  #LTER_name = substr(discharge_files[i],start=1,stop=3)
  file_name_nocsv<-substr(discharge_files[i],start=1,stop=nchar(discharge_files[i])-4)
  file_name = discharge_files[i]
  d = fread(file_name, sep=",")
  names(d)[which(colnames(d) %in% DischargeList)]<-"Q"
  names(d)[which(colnames(d) %in% DateList)]<-"Date"
  d<-d[,c("Q", "Date")]
  d$DischargeFileName<-file_name_nocsv
  ref_site<-subset(QLog, QLog$Discharge_File_Name==file_name_nocsv)
  ref_site<-ref_site[1,]
  d$Units<-ref_site$Units
  #d$LTER = LTER_name
  
  #convert all Q file units to CMS
  d$Qcms<-ifelse(d$Units=="cms", d$Q, 
                 ifelse(d$Units=="cfs", d$Q*0.0283,
                        ifelse(d$Units=="Ls", d$Q*0.001,
                               ifelse(d$Units=="cmd", d$Q*1.15741e-5, ""))))
  
  d<-d[,c("Qcms", "Date", "DischargeFileName")]
  
  #convert date to date format
  if(is.Date(d$Date)){

    d$Date<-d$Date

  } else{

    d<-date_format_guess(d,"Date", groups = TRUE, group_col = "LTER")
    format<-"%m/%d/%Y"
    d$Date<-as.Date(d$Date, format)

  }

  data_list[[i]] = d
}

#use bind_rows to concatenate each new discharge file
#should have 3 columns: date, site, discharge
all_discharge = bind_rows(data_list)

all_discharge<-all_discharge[,c("Qcms", "Date", "DischargeFileName")]

#change date to reflect new file creation
write.csv(all_discharge, "UpdatedAll_Q_master_10272022.csv")

# allQ<-read.csv("UpdatedAll_Q_master_10262022.csv")
# 
# 
# GRO_min<-all_discharge %>%
#   filter(LTER=="GRO") %>%
#   dplyr::group_by(DischargeFileName) %>%
#   slice_min(Date)
# 
# #which sites are included in master discharge file? Different from input files?
# discharge_sites = data.frame("site.name"=unique(all_discharge$site.name))
# WRTDS_sites = data.frame("site"=unique(WRTDS_discharge$site))
# 
# #long term sites
# Data_years_streams_WRTDS = read_csv("L:/GitHub/SiSyn/Merge Site Discharge/Data_years_streams_WRTDS.csv") #download directly from "https://drive.google.com/drive/folders/1q92ee9nKct_nCJ3NVD2-tm8KCuRBfm2U"
# longterm_list = data.frame(LTER=Data_years_streams_WRTDS$LTER,
#                            site.name=Data_years_streams_WRTDS$Stream.Site)
# #are all sites in long term site list in all_discharge?
# longterm_check = merge(discharge_sites,longterm_list, by="site.name", all=T)
# 
# #merge long-term list with all_discharge to add LTER name
# all_discharge_longterm = merge(all_discharge, longterm_list, all=T)
# 
# #write master discharge file to .csv
# setwd("L:/GitHub/SiSyn/Merge Site Discharge")
# write.csv(all_discharge_longterm, file="WRTDS_discharge_allsites_11Aug21.csv")
