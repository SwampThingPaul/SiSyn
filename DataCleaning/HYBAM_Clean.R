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
require(readxl)

#get folder URL from google drive with discharge data
#use "cleaned up" WRTDS prep files Analysis > WRTDS Analysis > WRTDS_Si_prep
#only use files ending in "_Q_WRTDS.csv"
folder_url = "https://drive.google.com/drive/folders/1G9IrszYW1Ig7OBWRWZr_pzaccKn6GbzW"

#get ID of folder
folder = drive_get(as_id(folder_url))

#get list of csv files from folder
xls_files = drive_ls(folder, type="xls")

#check working directory where files will be stored locally; separate folder within project folder
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/RawData_2023")
#"L:/GitHub/SiSyn/Merge Site Discharge"
#setwd("L:/GitHub/SiSyn/Merge Site Discharge/discharge files")

#download each file to the working directory; files are saved locally
for (i in 1:length(xls_files$drive_resource)) {
  drive_download(xls_files$drive_resource[i],  overwrite=T)
}

#add column for site name
#loop through each downloaded csv file and add appropriate site name
files = list.files(path="/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/RawData_2023", pattern = ".xls")

files<-files[-29]

discharge_dfs<-list()

chem_dfs<-list()

for (i in 1:length(files)) {
  
  if(grepl("xxxxx", files[i])){
    
    fname<-paste0("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/RawData_2023/", files[i])
    
    sheets <- readxl::excel_sheets(fname)
    tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
    data_frame <- lapply(tibble, as.data.frame)
      
    # assigning names to data frames
    names(data_frame) <- sheets
    
    data_frame <- mapply(cbind, data_frame, "solute"=sheets, SIMPLIFY=F)
    
    chem_dfs[[i]]<-bind_rows(data_frame)
    
  }else{
    
    discharge_dfs[[i]]<-read_xls(path=paste0("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/RawData_2023/", files[i]), sheet = "QJ")
    
  }
  
  
}


chem_master_hybam<-bind_rows(chem_dfs)
chem_master_hybam<-chem_master_hybam[complete.cases(chem_master_hybam$id_station),]

discharge_master_hybam<-bind_rows(discharge_dfs)
discharge_master_hybam<-discharge_master_hybam[complete.cases(discharge_master_hybam$id_station),]

write.csv(chem_master_hybam, "Chem_HYBAM.csv")

write.csv(discharge_master_hybam, "Discharge_HYBAM.csv")


hybam<-read.csv("Chem_HYBAM.csv")
hybam_si<-subset(hybam, hybam$solute=="Si")
hybam_si$date<-as.Date(hybam_si$date, "%m/%d/%y")

pdf("HYBAM_QAQC.pdf", width = 16, height = 8)

ggplot(hybam_si, aes(date, valeur))+geom_point()+theme_bw()+theme(text = element_text(size=20))+
  facet_wrap(~nom, scales = "free")

dev.off()


Q<-read.csv("Discharge_HYBAM.csv")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/Q_files")

names<-unique(Q$nom)

for (i in 1:length(names)) {
  
  Q_one_site<-subset(Q, Q$nom==names[i])
  
  write.csv(Q_one_site, paste0(names[i], "_Q.csv"))
}



