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

#add column for site name
#loop through each downloaded csv file and add appropriate site name
files = list.files(path="/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/Data_Summer2023/Cameroon", pattern = ".xls")

discharge_dfs<-list()

chem_dfs<-list()

for (i in 1:length(files)) {
  
  if(grepl("hydrochemistry", files[i])){
    
    fname<-paste0("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/Data_Summer2023/Cameroon/", files[i])
    
    df<-readxl::read_excel(fname)
    
    if(is.POSIXct(df$Date)){
      
      df$Date<-df$Date
      
    }else{
      
      df$Date<-as.numeric(df$Date)
      df$Date<-as.Date(df$Date, origin="1970-01-01")
      
    }

    df[,c(2:ncol(df))]<-lapply(df[,c(2:ncol(df))], as.numeric)
    df$site<-files[i]
    
    chem_dfs[[i]]<-bind_rows(df)
  
    
  }else{
    
    df<-read_excel(path=paste0("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/Data_Summer2023/Cameroon/", files[i]))
    df$site<-files[i]
    
    discharge_dfs[[i]]<-df
    
  }
  
  
}


chem_master_cameroon<-bind_rows(chem_dfs)
chem_master_cameroon<-chem_master_cameroon[complete.cases(chem_master_cameroon$Date),]

chem_master_cameroon$new_date<-as.POSIXct(ifelse(chem_master_cameroon$Date > "2050-01-01", 
                                              chem_master_cameroon$Date-years(70), chem_master_cameroon$Date), origin = "1970-01-01")

chem_master_cameroon$new_date<-as.Date(chem_master_cameroon$new_date)
chem_master_cameroon$new_site<-sub("\\_hydro.*", "", chem_master_cameroon$site)


discharge_master_cameroon<-bind_rows(discharge_dfs)
discharge_master_cameroon<-discharge_master_cameroon[complete.cases(discharge_master_cameroon$Q),]
discharge_master_cameroon$new_site<-sub("\\_hydro.*", "", discharge_master_cameroon$site)

write.csv(chem_master_cameroon, "Chem_Cameroon.csv")

write.csv(discharge_master_cameroon, "Discharge_Cameroon.csv")

getwd()




