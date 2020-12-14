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
require(dplyr)

#get folder URL from google drive with discharge data
folder_url = "https://drive.google.com/drive/folders/1PSa4QDG1FJyuzvZC6lbLPWgO7VgMX4ef"

#get ID of folder
folder = drive_get(as_id(folder_url))

#get list of csv files from folder
csv_files = drive_ls(folder, type="csv")

#get just site names from csv file names
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
