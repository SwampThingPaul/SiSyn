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


#get folder URL from google drive with DSi data
folder_url = "https://drive.google.com/drive/u/0/folders/1YnTT36pUI_3fnGJGDsoVKKMVnQZ69Y0L"

#get ID of folder
folder = drive_get(as_id(folder_url))

#get list of csv files from folder
csv_files = drive_ls(folder, type="csv")

#extract only WRTDS DSi files
WRTDS_DSi = csv_files[csv_files$name %like% "Si_WRTDS",]

#check working directory where files will be stored locally; separate folder within project folder
getwd()
#"L:/GitHub/SiSyn/Merge Site Discharge"
setwd("L:/GitHub/SiSyn/Merge Site Discharge/Merge site DSi")

#download each file to the working directory; files are saved locally
for (i in 1:length(WRTDS_DSi$drive_resource)) {
  drive_download(WRTDS_DSi$drive_resource[i],  overwrite=T)
}

#add column for site name
#loop through each downloaded csv file and add appropriate site name
DSi_files = list.files(path="L:/GitHub/SiSyn/Merge Site Discharge/Merge site DSi")

#create list to store output from for loop
data_list = list()

for (i in 1:length(DSi_files)) {
  site = substr(DSi_files[i],start=1,stop=nchar(DSi_files[i])-13)
  
  file_name = DSi_files[i]
  d = fread(file_name, sep=",")
  d$site.name = site
  
  data_list[[i]] = d
}

#use rbind to concatenate each new discharge file
#should have 3 columns: date, site, discharge
#all_discharge = rbindlist(data_list)
all_DSi = ldply(data_list, data.frame)
length(unique(all_DSi$site.name))

#which sites are included in master discharge file? Different from input files?
DSi_sites = data.frame("site"=unique(all_DSi$site.name))
#60 files, 60 sites in data frame; ALL GOOD!

#write master DSi file to .csv
setwd("L:/GitHub/SiSyn/Merge Site Discharge")
write.csv(all_DSi, file="WRTDS_DSi_mergedsites_3Jun22.csv")
