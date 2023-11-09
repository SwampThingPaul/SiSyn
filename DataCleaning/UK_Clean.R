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


folder_url="https://drive.google.com/drive/folders/1UC7TTfwM8arXesrhZF0eEZ1CmQhz7v8Y"

#get ID of folder
folder = drive_get(as_id(folder_url))

#get list of csv files from folder
Q_files = drive_ls(folder, type="csv", pattern = "df")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/RawData_2023/")

for (i in 1:nrow(Q_files)) {

  drive_download(Q_files$drive_resource[i],  overwrite=T)
  
}

file_names<-list.files(path = ".", pattern = "df")

q_list<-list()

for (i in 1:length(file_names)) {

  
  q_list[[i]]<-read.csv(file_names[i])
  
    
}

# assigning names to data frames
names(q_list) <- file_names

data_frame <- mapply(cbind, q_list, "site"=file_names, SIMPLIFY=F)

q_df_all<-bind_rows(data_frame)

q_df_all$site_number<-sub("df.", "", q_df_all$site)
q_df_all$site_number<-paste("Site", sub(".csv", "", q_df_all$site_number))

si<-read.csv("English_hms_Si_1974_2022flow.csv")
si$site<-paste("Site", si$id)

remove_sites<-c(setdiff(unique(si$site), unique(q_df_all$site_number)), setdiff(unique(q_df_all$site_number), unique(si$site)))

si<-si[-(which(si$site %in% remove_sites)),]

q_df_all<-q_df_all[-(which(q_df_all$site_number %in% remove_sites)),]

c(setdiff(unique(si$site), unique(q_df_all$site_number)), setdiff(unique(q_df_all$site_number), unique(si$site)))

write.csv(q_df_all, "UK_Q.csv")

write.csv(si, "UK_Si.csv")


UK<-read.csv("UK_Si.csv")

UK$date<-as.Date(UK$date, "%d/%m/%Y")

UK_Q<-read.csv("UK_Q.csv")

UK_Q$date<-as.Date(UK_Q$Date)

UK<-UK[order(UK$catalogue.name),]

rivers<-unique(UK$catalogue.name)

#i=3

pdf("UK_Si_Q_check.pdf")

for (i in 1:length(rivers)) {
  
  site<-subset(UK, UK$catalogue.name==rivers[i])
  
  Q_site_number<-unique(paste("Site", site$id))
  
  site_Q<-subset(UK_Q, UK_Q$site_number==Q_site_number)
  
  p1<-ggplot()+
    geom_line(site_Q, mapping=aes(date, Flow))+
    geom_point(site, mapping=aes(date, Silica..mg.SiO2.l.), col="red")+
    theme_bw()+ggtitle(rivers[i])
  
  print(p1)
  
  
}
dev.off()





