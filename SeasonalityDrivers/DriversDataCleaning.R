require(googledrive)
require(reshape2)
require(data.table)
require(stringr)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#link to monthly WRTDS data
link<-"https://drive.google.com/file/d/1Wv0Jw__vEWdM60YV-xYkHXNwFub_bISh/view?usp=drive_link"

#get file
file_get<-drive_get(as_id(link))

#download
drive_download(file_get$drive_resource,  overwrite=T)

si_drivers<-read.csv("all-data_si-extract.csv")

si_drivers<-si_drivers[,-c(1,3:9)]

#parse out and clean monthly data
months<-c("jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec")

months_cols<-si_drivers[,(colnames(si_drivers) %like% months)]

months_cols<-months_cols[,-c(61,62)]

months_cols$Stream_Name<-si_drivers$Stream_Name

months_cols_melt<-melt(months_cols, id.vars = "Stream_Name")

months_cols_melt$variable<-as.character(months_cols_melt$variable)

months_cols_melt$month<-sapply(strsplit(months_cols_melt$variable, "_"), function(x) x[2])

vars<-c("num_days","prop_area","evapotrans","precip","temp")

units<-c("days", "prop_watershed","kg_m2","mm_day","deg_C")

units_df<-data.frame(vars,units)

colnames(units_df)[1]<-"driver"

months_cols_melt$driver<-NA

for (i in 1:length(vars)) {
  
  months_cols_melt$driver<-ifelse(months_cols_melt$variable %like% vars[i],paste(vars[i]), 
                                  months_cols_melt$driver)
  
}

months_cols_melt<-months_cols_melt[,-2]

months_cols_melt<-merge(months_cols_melt, units_df, by="driver")

write.csv(months_cols_melt, "Monthly_Driver_Data.csv")

#parse out and clean annual data
year_cols<-si_drivers[,!(colnames(si_drivers) %in% colnames(months_cols))]

year_cols$Stream_Name<-si_drivers$Stream_Name

character_vars<-c("elevation|rock|land|soil")

year_cols<-year_cols[,!(colnames(year_cols) %like% character_vars)]

year_cols_melt<-melt(year_cols, id.vars = "Stream_Name")

year_cols_melt$variable<-as.character(year_cols_melt$variable)

year_cols_melt$year<-str_extract(year_cols_melt$variable, "(?<=_)[^_]+(?=\\MMDD$)")

year_cols_melt$year<-ifelse(is.na(year_cols_melt$year), sapply(strsplit(year_cols_melt$variable, "_"), function(x) x[2]),
                            year_cols_melt$year)

vars_annual<-c("num_days","prop_area","evapotrans","precip","temp","cycle0","cycle1","npp")

units_annual<-c("days", "prop_watershed","kg_m2","mm_day","deg_C","MMDD","MMDD","kgC_m2_year")

units_df_annual<-data.frame(vars_annual,units_annual)

colnames(units_df_annual)[1]<-"driver"

year_cols_melt$driver<-NA

for (i in 1:length(vars_annual)) {
  
  year_cols_melt$driver<-ifelse(year_cols_melt$variable %like% vars_annual[i],paste(vars_annual[i]), 
                                year_cols_melt$driver)
  
}

year_cols_melt<-year_cols_melt[,-2]

year_cols_melt<-merge(year_cols_melt, units_df_annual, by="driver")

write.csv(year_cols_melt, "Annual_Driver_Data.csv")  

#parse out character data
character_cols<-si_drivers[,(colnames(si_drivers) %like% character_vars)]

character_cols$Stream_Name<-si_drivers$Stream_Name

write.csv(character_cols, "Character_Driver_Data.csv")




