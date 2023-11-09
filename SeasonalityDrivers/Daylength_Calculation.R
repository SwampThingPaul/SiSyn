install.packages("chillR")
require("chillR")

#read in reference table from google drive
ref_table_url<-"https://docs.google.com/spreadsheets/d/11t9YYTzN_T12VAQhHuY5TpVjGS50ymNmKznJK4rKTIU/edit?usp=sharing"

file_get<-drive_get(as_id(ref_table_url))

drive_download(file_get$drive_resource, overwrite = T)

ref_table<-read_xlsx("Site_Reference_Table.xlsx")

#keep lat and long
lat_long<-ref_table[,c("Stream_Name","Latitude")]

lat_long<-lat_long[complete.cases(lat_long),]

lat_long$Latitude<-as.numeric(lat_long$Latitude)

#open lists for daylengths
daylength_list<-list()

daylength_site<-list()

for (i in 1:nrow(lat_long)) {
  
  for (k in 1:365) {
    
    #get daylength for each day for one site
    daylength_list[[k]]<-daylength(lat_long$Latitude[i], JDay = k)$Daylength
    
  }

  #add all sites to list
  daylength_site[[i]]<-unlist(daylength_list)
  
}

#put into df and format
all_sites_daylength<-do.call(bind_cols, daylength_site)
colnames(all_sites_daylength)<-lat_long$Stream_Name

all_sites_daylength$doy<-seq(1,365,1)

all_sites_daylength_melt<-melt(all_sites_daylength, id.vars="doy")

#convert day of year to date month
all_sites_daylength_melt$day<-as.Date(all_sites_daylength_melt$doy-1, origin = "2015-01-01")

all_sites_daylength_melt$month<-format(as.Date(all_sites_daylength_melt$day), "%m")

#aggreage to month scale
daylength_site_month <- all_sites_daylength_melt %>%
  dplyr::group_by(variable, month) %>%
  dplyr::summarise(mean_daylength=mean(value))

#write csv
write.csv(daylength_site_month, "Monthly_Daylength.csv")







