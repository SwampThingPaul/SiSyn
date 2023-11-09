require(data.table)
require(tidyr)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/Q_files")

files<-list.files(path = ".", pattern = "filled_Q")

mcm<-c("andrsn|canada|common|crescent|delta|green|harnish|onyx|priscu|vguerard|lawson")

mcm_files<-files[files %like% mcm]

mcm_names<-sub("\\_filled_Q.csv.*", "", mcm_files)

for (i in 1:length(mcm_files)) {
  
  one_file<-read.csv(mcm_files[i])
  
  one_file<-one_file[,c("DATE_TIME" ,"AVG_DISCHARGE")]
  
  one_file<-one_file[complete.cases(one_file$AVG_DISCHARGE),]
  
  one_file_filled<- one_file %>%
    mutate(DATE_TIME = as.Date(DATE_TIME, "%Y-%m-%d")) %>%
    complete(DATE_TIME = seq.Date(min(DATE_TIME), max(DATE_TIME), by="day"), fill=list(AVG_DISCHARGE=0))
  
  write.csv(one_file_filled, paste0(mcm_names[i], "_filled_Q.csv"))
  
}
