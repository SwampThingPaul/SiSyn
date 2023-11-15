## harmonize drivers - first get mean Q, Si, and other nutrients if they exist
require(data.table)
require(dplyr)
require(lubridate)
require(googledrive)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#read in cluster data for average cluster, modal cluster, number of clusters
si_clust<-read.csv("MonthClustersNov2022.csv")

si_clust$Stream_ID<-paste0(si_clust$LTER, "__", si_clust$Site)

chem_url<-"https://drive.google.com/file/d/1MGJaytUOfg0oO7csvaqCfvHmek8vpAeR/view?usp=drive_link"

file_get<-drive_get(as_id(chem_url))

drive_download(file_get$drive_resource, overwrite = T)

Q_url<-"https://drive.google.com/file/d/16wHi1jmUPyvIoUutrdheV0195YWvUTXT/view?usp=drive_link"

file_get<-drive_get(as_id(Q_url))

drive_download(file_get$drive_resource, overwrite = T)

#read in chem input file
chem_df<-read.csv("WRTDS-input_chemistry.csv")
chem_df$Date<-as.Date(chem_df$Date)

#read in Q input file  
q_df<-read.csv("WRTDS-input_discharge.csv")
q_df$Date<-as.Date(q_df$Date)

finn<-read.csv("FinnishSites.csv")
finn$Stream_ID<-paste0("Finnish Environmental Institute__", finn$Site.ID)
finn$Stream_ID2<-paste0("Finnish Environmental Institute__", finn$Site)

for (i in 1:nrow(finn)) {
  
  site_id<-finn[i,3]
  
  row_num<-which(chem_df$Stream_ID==site_id)
  
  chem_df[row_num, "Stream_ID"]<-finn[i,4]
  
}

chem_df$Stream_ID<-ifelse(chem_df$Stream_ID=="Finnish Environmental Institute__TORNIONJ KUKKOLA 14310  ", "Finnish Environmental Institute__TORNIONJ KUKKOLA 14310", chem_df$Stream_ID)

chem_df$Stream_ID<-ifelse(chem_df$Stream_ID=="Finnish Environmental Institute__SIMOJOKI AS. 13500      ", "Finnish Environmental Institute__SIMOJOKI AS. 13500", chem_df$Stream_ID)

chem_df<-subset(chem_df, chem_df$Stream_ID %in% si_clust$Stream_ID)

chem_df<-subset(chem_df, chem_df$variable == "DSi")

q_df<-subset(q_df, q_df$Stream_ID %in% si_clust$Stream_ID)

#unique(q_df$Stream_ID)

si_stats<-chem_df %>%
  dplyr::group_by(Stream_ID) %>%
  dplyr::summarise(mean_si = mean(value_mgL), med_si=median(value_mgL), sd_si=sd(value_mgL), min_Si=min(value_mgL), max_Si=max(value_mgL))

q_stats<-q_df %>%
  dplyr::group_by(Stream_ID) %>%
  dplyr::summarise(mean_q = mean(Q), med_q=median(Q), sd_q=sd(Q), min_Q=min(Q), max_Q=max(Q),
                   q_95=quantile(Q, 0.95), q_5=quantile(Q, 0.05))

# si_stats_annual<-chem_df %>%
#   dplyr::group_by(Stream_ID, year(as.Date(Date))) %>%
#   dplyr::summarise(an_mean_si = mean(value_mgL), an_med_si=median(value_mgL), an_sd_si=sd(value_mgL))
# 
# q_stats_annual<-q_df %>%
#   dplyr::group_by(Stream_ID, year(as.Date(Date))) %>%
#   dplyr::summarise(an_mean_q = mean(Q), an_med_q=median(Q), an_sd_q=sd(Q))

si_stats$CV_C<-si_stats$sd_si/si_stats$mean_si

q_stats$CV_Q<-q_stats$sd_q/q_stats$mean_q

#merge them into one
cv_tot<-merge(si_stats, q_stats, by="Stream_ID")

#calculate CVC/CVQ
cv_tot$cvc_cvq<-(cv_tot$CV_C)*(cv_tot$CV_Q)

#get list of streams
streams<-unique(cv_tot$Stream_ID)

#open list to calculate slope and r2 of each stream
slope_list<-list()
rsquared_list<-list()

#calculate CQ slope and CQ r2 for Si-Q for each stream
for (i in 1:length(streams)) {
  
  si<-subset(chem_df, chem_df$Stream_ID==streams[i])
  si$Date<-as.Date(si$Date)
  
  q<-subset(q_df, q_df$Stream_ID==streams[i])
  q$Date<-as.Date(q$Date)
  
  tot<-merge(si, q, by="Date")
  
  tot<-tot[tot$Q > 0,]
  
  lm1<-lm(log(value_mgL)~log(Q), tot)
  
  #ggplot(tot, aes(log(Q), log(value_mgL)))+geom_point()
  
  sum<-summary(lm1)
  
  slope_list[[i]]<-sum$coefficients[2,1]
  
  rsquared_list[[i]]<-sum$r.squared
  
}

#put slopes into df
slope_df<-as.data.frame(do.call(rbind, slope_list))
slope_df$Stream_ID<-streams
colnames(slope_df)[1]<-"slope"

#put r2 into df
r2_df<-as.data.frame(do.call(rbind, rsquared_list))
r2_df$Stream_ID<-streams
colnames(r2_df)[1]<-"r2"

cv_tot<-merge(cv_tot, slope_df)

# cv_tot<-merge(cv_tot, si_stats_annual, by="Stream_ID")
# colnames(cv_tot)[12]<-"Year"
# colnames(q_stats_annual)[2]<-"Year"
# 
# cv_tot<-merge(cv_tot, q_stats_annual, by=c("Stream_ID", "Year"))

#merge in climate data

KG<-read.csv("Koeppen_Geiger.csv")

KG$Stream_ID<-paste0(KG$LTER, "__", KG$Stream_Name)

tot<-merge(cv_tot, KG, by="Stream_ID")
tot<-tot[!duplicated(tot$Stream_ID),]

#tot<-tot[!duplicated(tot$Stream_ID),]

#merge in DA
ref_table_link<-"https://docs.google.com/spreadsheets/d/11t9YYTzN_T12VAQhHuY5TpVjGS50ymNmKznJK4rKTIU/edit?usp=sharing"
ref_table_folder = drive_get(as_id(ref_table_link))

#download ref table
ref_table<-drive_download(ref_table_folder$drive_resource, overwrite = T)

ref_table<-readxl::read_xlsx("Site_Reference_Table.xlsx")
ref_table$Stream_ID<-paste0(ref_table$LTER, "__", ref_table$Stream_Name)
area<-ref_table[,c("drainSqKm", "Stream_ID")]

renamed_sites<-c("Catalina Jemez__MG_WEIR", "Catalina Jemez__OR_low", "NWT__COMO", "Walker Branch__East Fork", "Walker Branch__West Fork")
old_name<-c("Catalina Jemez__Marshall Gulch", "Catalina Jemez__Oracle Ridge", "NWT__Como Creek", "Walker Branch__east fork", "Walker Branch__west fork")

name_conversion<-data.frame(renamed_sites, old_name)

colnames(name_conversion)<-c("Stream_ID", "Updated_StreamName")

missing_sites<-area[area$Stream_ID %in% renamed_sites,]

missing_sites<-left_join(missing_sites, name_conversion, by="Stream_ID")

missing_sites<-missing_sites[,-2]

colnames(missing_sites)[2]<-"Stream_ID"

area<-bind_rows(area, missing_sites)

tot<-merge(tot, area, by="Stream_ID")
tot<-tot[!duplicated(tot$Stream_ID),]

#merge in spatial drivers
drivers_link<-"https://drive.google.com/file/d/1Wv0Jw__vEWdM60YV-xYkHXNwFub_bISh/view?usp=share_link"
drivers_folder = drive_get(as_id(drivers_link))

#download ref table
drivers<-drive_download(drivers_folder$drive_resource, overwrite = T)

spatial_drivers<-read.csv("all-data_si-extract.csv")
spatial_drivers$Stream_ID<-paste0(spatial_drivers$LTER, "__", spatial_drivers$Stream_Name)

months_abb<-c("jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec")

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
  
  one_driver<-spatial_drivers[,c(282, drive_cols)]
  
  site_mean[[i]]<-rowMeans(one_driver[,c(2:length(one_driver))], na.rm = TRUE)
  
}

mean_df<-as.data.frame(do.call(cbind, site_mean))

colnames(mean_df)<-drivers_list_quant

mean_df$Stream_ID<-spatial_drivers$Stream_ID


greenup_mean<-list()

for (i in 1:length(greenup)) {
  
  drive_cols<-grep(greenup[i], colnames(spatial_drivers))
  
  one_driver<-spatial_drivers[,c(282, drive_cols)]
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

tot<-merge(tot, mean_df, by="Stream_ID")

#unique(tot$Stream_ID)

###add in N and P
N_P_link<-"https://drive.google.com/file/d/1Ucvv9quIjnkgK6YOP1TTAfH-CAVlwj63/view?usp=drive_link"

file_get<-drive_get(as_id(N_P_link))

drive_download(file_get$drive_resource, overwrite = T)

N_P<-read.csv("Median_N_P_200.csv")

N_P_cast<-dcast(N_P, Stream_Name~solute_simplified, value.var = "mean_val", fun.aggregate = mean)

tot<-merge(tot, N_P_cast, by="Stream_Name")

##add in daylength
daylength_url<-"https://drive.google.com/file/d/1jpmsjnZBwLHlvF1rwV_JTYPliTAQ27uU/view?usp=drive_link"
  
file_get<-drive_get(as_id(daylength_url))

drive_download(file_get$drive_resource, overwrite = T)

daylen<-read.csv("Monthly_Daylength.csv")

daylen<-daylen[,-1]

daylen_range<-daylen %>%
  group_by(variable) %>%
  summarise(min_day=min(mean_daylength), max_len=max(mean_daylength))

colnames(daylen_range)<-c("Stream_Name", "Min_Daylength", "Max_Daylength")

renamed_sites<-c("MG_WEIR", "OR_low", "COMO", "East Fork", "West Fork")
old_name<-c("Marshall Gulch", "Oracle Ridge", "Como Creek", "east fork", "west fork")

name_conversion<-data.frame(renamed_sites, old_name)

colnames(name_conversion)<-c("Stream_Name", "Updated_StreamName")

missing_sites<-daylen_range[daylen_range$Stream_Name %in% renamed_sites,]

missing_sites<-left_join(missing_sites, name_conversion, by="Stream_Name")

missing_sites<-missing_sites[,-1]

colnames(missing_sites)[3]<-"Stream_Name"

daylen_range<-bind_rows(daylen_range, missing_sites)

tot<-merge(tot, daylen_range, by="Stream_Name")

tot<-tot[!duplicated(tot$Stream_Name),]

write.csv(tot, "AllDrivers_Harmonized_20231114.csv")

