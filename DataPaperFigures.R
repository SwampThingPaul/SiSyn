setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

drivers<-read.csv("all-data_si-extract_2_20240623.csv")

#merge in DA
ref_table_link<-"https://docs.google.com/spreadsheets/d/11t9YYTzN_T12VAQhHuY5TpVjGS50ymNmKznJK4rKTIU/edit?usp=sharing"
ref_table_folder = drive_get(as_id(ref_table_link))

#download ref table
ref_table<-drive_download(ref_table_folder$drive_resource, overwrite = T)

ref_table<-readxl::read_xlsx("Site_Reference_Table.xlsx")

ref_table<-ref_table[,c(3,9,10)]

ref_table<-ref_table %>%
  mutate(Stream_Name = case_when(
    Stream_Name=="Kiiminkij 13010 4-tien s"~"Kiiminkij 13010 4tien s",
    Stream_Name=="Lestijoki 10800 8-tien s"~"Lestijoki 10800 8tien s",
    Stream_Name=="Mustijoki 4,2  6010"~"Mustijoki 42  6010",
    Stream_Name=="Mustionjoki 4,9  15500"~"Mustionjoki 49  15500",
    Stream_Name=="Porvoonjoki 11,5  6022"~"Porvoonjoki 115  6022",
    Stream_Name=="SIMOJOKI AS. 13500"~"SIMOJOKI AS 13500",
    Stream_Name=="Vantaa 4,2  6040"~"Vantaa 42  6040",
    .default = Stream_Name
  ))

drivers_latlong<-merge(ref_table, drivers, by="Stream_Name")

#format data to pull using KGC
coord_data<-data.frame(drivers_latlong[,c(1:3)], rndCoord.lon=RoundCoordinates(drivers_latlong$Longitude), 
                       rndCoord.lat=RoundCoordinates(drivers_latlong$Latitude))

#get KG for each site
data <- data.frame(coord_data,ClimateZ=LookupCZ(coord_data))

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

KG_name<-read.csv("KG_Clim_Name.csv")

data<-merge(data, KG_name, by="ClimateZ")

write.csv(data, "KGClass_06162024.csv")

drivers<-read.csv("all-data_si-extract_2_20240623.csv")

sites<-c("Arkansas River at Murray Dam","COLUMBIA RIVER AT PORT WESTWARD","DMF Brazos River","Iijoki Raasakan voimal",
         "Koskenkylanjoki 6030","Narpionjoki mts 6761","Pyhajoki Hourunk 11400","YAMPA RIVER AT DEERLODGE PARK",
         "YAMPA RIVER BELOW CRAIG")

missing_sites<-drivers[drivers$Stream_Name %in% sites,]

missing_sites<-missing_sites[!duplicated(missing_sites$Stream_Name),]

spatial_drivers<-missing_sites

spatial_drivers$Stream_ID<-paste0(spatial_drivers$LTER, "__", spatial_drivers$Stream_Name)

months_abb<-c("jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec_")

monthly_drivers<-spatial_drivers[,c(344,which(colnames(spatial_drivers) %like% months_abb))]

spatial_drivers<-spatial_drivers[,-c(which(colnames(spatial_drivers) %like% months_abb))]

major_cat_vars<-which(colnames(spatial_drivers) %like% c("soil|land|rock"))

cat_vars<-spatial_drivers[,c(major_cat_vars)]

spatial_vars<-cbind(spatial_drivers, cat_vars)

elevation<-which(colnames(spatial_drivers) %like% "elevation")

cat_vars<-spatial_drivers[,c(major_cat_vars, elevation)]

cat_vars$Stream_ID<-spatial_drivers$Stream_ID

drivers_list_quant<-c("num_days", "prop_area", "precip", "evapotrans", "temp", "npp")

greenup<-c("cycle0", "cycle1")

site_mean<-list()

for (i in 1:length(drivers_list_quant)) {
  
  drive_cols<-grep(drivers_list_quant[i], colnames(spatial_drivers))
  
  one_driver<-spatial_drivers[,c(301, drive_cols)]
  
  site_mean[[i]]<-rowMeans(one_driver[,c(2:length(one_driver))], na.rm = TRUE)
  
}

mean_df<-as.data.frame(do.call(cbind, site_mean))

colnames(mean_df)<-drivers_list_quant

mean_df$Stream_ID<-spatial_drivers$Stream_ID

greenup_mean<-list()

for (i in 1:length(greenup)) {
  
  drive_cols<-grep(greenup[i], colnames(spatial_drivers))
  
  one_driver<-spatial_drivers[,c(301, drive_cols)]
  one_driver<-one_driver[!duplicated(one_driver$Stream_ID),]
  
  driver_melt<-melt(one_driver, id.vars="Stream_ID")
  
  driver_melt$doy<-yday(as.Date(driver_melt$value, "%Y-%m-%d"))
  
  one_driver<-dcast(driver_melt, Stream_ID~variable, value.var = "doy")
  
  greenup_mean[[i]]<-rowMeans(one_driver[,c(2:length(one_driver))], na.rm = TRUE)
  
}

green_df<-as.data.frame(do.call(cbind, greenup_mean))

colnames(green_df)<-greenup
green_df$Stream_ID<-one_driver$Stream_ID

mean_df<-merge(mean_df, green_df, by="Stream_ID")
mean_df<-merge(mean_df, cat_vars, by="Stream_ID")

write.csv(mean_df, "Missing_204_Sites_Drivers.csv")


lat_long<-read.csv("LatLong_204.csv")

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





