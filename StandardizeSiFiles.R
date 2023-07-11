## combine all Si into file with same column names, include units
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

units<-read.csv("NewSiteSiUnits.csv")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NewSites_Sept2022")

sites<-list.files(path = ".", pattern = "Si.csv")

sites_nocsv<-gsub(".csv", "", sites)

si_cols<-c("Si", "si", "val", "Conc", "Si_mgL", "Conc_mgL", "SiO2")

date_cols<-c("Date", "dateTime", "dates", "date", "datetime", "sample_start_dt")

data_list<-list()

for (i in 1:length(sites)) {
  
  data<-read.csv(sites[i])
  
  units_site<-subset(units, units$File.Name==sites_nocsv[i])
    
  names(data)[which(colnames(data) %in% si_cols)]<-"Si" #convert all Q columns to be called "Q"
  names(data)[which(colnames(data) %in% date_cols)]<-"Date"
  
  data<-data[,c("Si", "Date")]
  
  data$File.Name<-sites_nocsv[i]
  
  data<-merge(data, units_site, by="File.Name")
  
  data_list[[i]]<-data
}

allsi<-do.call(rbind, data_list)

write.csv(allsi,"AllSiStandardized_10102022.csv")

