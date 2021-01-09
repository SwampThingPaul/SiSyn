require(googledrive)
require(stringr)
require(lubridate)
require(reshape)
require(gtools)
require(dplyr)
require(data.table)
require(dataRetrieval)

#get folder URL from google drive with WRTDS output
WRTDS_output_URL = "https://drive.google.com/drive/u/0/folders/1e9GEr2GSHXagg6sAK4a7e47dKE6wghR9"
#get ID of folder
WRTDS_output_ID = drive_get(as_id(WRTDS_output_URL))
#get list of csv files in WRTDS output folder
WRTDS_csvs = drive_ls(WRTDS_output_ID, type="csv")
#extract only "Cont Si" files with daily Si load estimates
WRTDS_csvs_SiEsts = WRTDS_csvs[WRTDS_csvs$name %like% "ContSi_WRTDS.csv",]
#create new column with site name
WRTDS_csvs_SiEsts$site = substr(WRTDS_csvs_SiEsts$name, start=1, stop=nchar(WRTDS_csvs_SiEsts$name)-16)

#repeat for Loadflex output files
Loadflex_output_URL = "https://drive.google.com/drive/u/0/folders/1f2Bwpr0R8eTm6VZTjuB7THZLIy-ogaNC"
Loadflex_output_ID = drive_get(as_id(Loadflex_output_URL))
Loadflex_csvs = drive_ls(Loadflex_output_ID, type="csv")
Loadflex_csvs_SiEst = Loadflex_csvs
Loadflex_csvs_SiEst$site = word(Loadflex_csvs_SiEst$name, 1, sep="_")

#merge WRTDS and Loadflex files by site name
compare_csvs = merge(WRTDS_csvs_SiEsts, Loadflex_csvs_SiEst, by="site")

#download WRTDS csv files to local drive
getwd()
#"L:/GitHub/SiSyn/Loadflex/Loadflex-WRTDS compare"
setwd("L:/GitHub/SiSyn/Loadflex/Loadflex-WRTDS compare/WRTDS results")
for (i in 1:length(WRTDS_csvs_SiEsts$drive_resource)) {
  drive_download(WRTDS_csvs_SiEsts$drive_resource[i],  overwrite=T)
}

WRTDS_files = list.files(path="L:/GitHub/SiSyn/Loadflex/Loadflex-WRTDS compare/WRTDS results")

#create list to store output of loop
WRTDS_output_list = list()

#loop through all WRTDS output files and save date, site name, and daily load estimate
for (i in 1:length(WRTDS_files)) {
  site = substr(WRTDS_files[i], start=1, stop=nchar(WRTDS_files[i])-16)
  
  file_name = WRTDS_files[i]
  #Keep only date and daily flux
  d = fread(file_name,select=c(2,17))
  d$site = site
  
  WRTDS_output_list[[i]] = d
}

#repeat for Loadflex files
setwd("L:/GitHub/SiSyn/Loadflex/Loadflex-WRTDS compare/Loadflex results")
for (i in 1:length(Loadflex_csvs_SiEst$drive_resource)) {
  drive_download(Loadflex_csvs_SiEst$drive_resource[i],  overwrite=T)
}
Loadflex_files = list.files(path="L:/GitHub/SiSyn/Loadflex/Loadflex-WRTDS compare/Loadflex results")
Loadflex_output_list = list()
for (i in 1:length(Loadflex_files)) {
  site = word(Loadflex_files[i], 1, sep="_")
  file_name = Loadflex_files[i]
  d = fread(file_name,select=c(2,3))
  d$site = site
  Loadflex_output_list[[i]] = d
}

#expand WRTDS and Loadflex lists into dataframes
library(plyr)
WRTDS_allsites = ldply(WRTDS_output_list)
Loadflex_allsites = ldply(Loadflex_output_list)

head(WRTDS_allsites)
head(Loadflex_allsites)

#rename columns
names(WRTDS_allsites)[names(WRTDS_allsites) == "FluxDay"] = "WRTDS_DailyFlux"
names(Loadflex_allsites)[names(Loadflex_allsites) == "Flux_Rate"] = "Loadflex_DailyFlux"
names(Loadflex_allsites)[names(Loadflex_allsites) == "Day"] = "Date"

#merge WRTDS and Loadflex results
#keep all results from both models
WRTDS_Loadflex_compare = merge(WRTDS_allsites, Loadflex_allsites, by=c("site","Date"),all=T)

setwd("L:/GitHub/SiSyn/Loadflex/Loadflex-WRTDS compare")
write.csv(WRTDS_Loadflex_compare, file="WRTDS-Loadflex_compare.csv")
