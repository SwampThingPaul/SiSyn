#prep info files

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

info_all<-read.csv("INFO_all_revised.csv")

info_sites<-unique(info_all$station.nm)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_20years/")

for (i in 1:nrow(info_all)) {
  
  info_one<-info_all[i,]
  
  write.csv(info_one, paste0(info_sites[i], "_INFO.csv"), row.names = FALSE)
  
}
