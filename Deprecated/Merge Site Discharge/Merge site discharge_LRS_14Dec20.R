install.packages("googledrive")
install.packages("tidyverse")
install.packages("gtools")
install.packages("rtools")
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
folder_url = "https://drive.google.com/drive/folders/1PSa4QDG1FJyuzvZC6lbLPWgO7VgMX4ef"

#get ID of folder
folder = drive_get(as_id(folder_url))

#get list of csv files from folder
csv_files = drive_ls(folder, type="csv")

#get just site names from csv file name
csv_files$site = str_extract(csv_files$name, pattern="(?<=Copy of ).*(?=.csv)")
csv_files$site = strsplit(csv_files$site, "_")
library(dplyr)
library(tidyr)
sites = unnest_wider(csv_files, site)
sites = sites[,4:7]
sites$...2 = ifelse(sites$...2=="Si"|sites$...2=="Q"|sites$...2=="WRTDS",NA,sites$...2)
sites$...3 = ifelse(sites$...3=="Si"|sites$...3=="Q"|sites$...3=="WRTDS",NA,sites$...3)
sites$...4 = ifelse(sites$...4=="Si"|sites$...4=="Q"|sites$...4=="WRTDS",NA,sites$...4)
site_names = 
              sites %>%
                unite("site", ...1:...4,sep="_",na.rm=T)
#insert site names back into csv_files dataframe
csv_files$site = site_names

#extract only WRTDS discharge files
WRTDS_discharge = csv_files[csv_files$name %like% "Q_WRTDS.csv",]

#check working directory where files will be stored locally; separate folder within project folder
getwd()
#"L:/GitHub/SiSyn/Merge Site Discharge"
setwd("L:/GitHub/SiSyn/Merge Site Discharge/discharge files")

#download each file to the working directory; files are saved locally
for (i in 1:length(WRTDS_discharge$drive_resource)) {
  drive_download(WRTDS_discharge$drive_resource[i],  overwrite=T)
}

#add column for site name
#loop through each downloaded csv file and add appropriate site name
discharge_files = list.files(path="L:/GitHub/SiSyn/Merge Site Discharge/discharge files")

#create list to store output from for loop
data_list = list()

for (i in 1:length(discharge_files)) {
  site = substr(discharge_files[i],start=9,stop=nchar(discharge_files[i])-12)
  
  file_name = discharge_files[i]
  d = fread(file_name, sep=",")
  d$site.name = site
  
  data_list[[i]] = d
}

#use rbind to concatenate each new discharge file
#should have 3 columns: date, site, discharge
#all_discharge = rbindlist(data_list)
all_discharge = ldply(data_list, data.frame)

#which sites are included in master discharge file? Different from input files?
discharge_sites = data.frame("site"=unique(all_discharge$site.name))
WRTDS_sites = data.frame("site"=unique(WRTDS_discharge$site))

#write master discharge file to .csv
setwd("L:/GitHub/SiSyn/Merge Site Discharge")
write.csv(all_discharge, file="WRTDS_discharge_allsites.csv")
