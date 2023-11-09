##check Si sites against macroSheds sites

#install.packages('devtools') #you may need to install devtools too

#devtools::install_github("https://github.com/MacroSHEDS/macrosheds")
require("macrosheds")
require("data.table")

ms_sites<-macrosheds::ms_download_site_data()

ms_vars<-macrosheds::ms_catalog()

ms_vars_list<-macrosheds::ms_download_variables()

ms_silica<-ms_vars_list[ms_vars_list$variable_code %like% "Si"&ms_vars_list$variable_type=="chem_discrete",]

silica_names<-ms_silica$variable_code

ms_vars_si<-ms_vars[ms_vars$variable_code %in% silica_names,]

ms_vars_si_50obs<-subset(ms_vars_si, ms_vars_si$observations > 49)

ms_vars_si_50obs$num_years<-as.numeric(difftime(ms_vars_si_50obs$last_record_utc, ms_vars_si_50obs$first_record_utc,
                                     units = "days")/365.25)

getnames_numyears <- function(num_years_wanted) {
  
  df<-subset(ms_vars_si_50obs, ms_vars_si_50obs$num_years > num_years_wanted)
  
  names_years<-df[!duplicated(df[,c("site_code")]),]
  
  return(names_years)
  
}

si_5yrs<-getnames_numyears(4.99)
# 
# si_10yrs<-getnames_numyears(9.99)
# 
# si_15yrs<-getnames_numyears(14.99)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

write.csv(si_15yrs, "15yrs_SiMacroSheds.csv")


ms_vars_q<-ms_vars[ms_vars$variable_code == "discharge",]

ms_vars_q$days<-as.numeric(difftime(ms_vars_q$last_record_utc, ms_vars_q$first_record_utc,
                                    units = "days"))

ms_vars_q$days_obs_diff<-ms_vars_q$days-ms_vars_q$observations

ms_vars_q<-ms_vars_q[,c(4:6,8,9)]

Q_5yrs<-merge(si_5yrs, ms_vars_q, by=c("network", "domain", "site_code"))

colnames(Q_5yrs)[c(8,9,12,13)]<-c("First_Si", "Last_Si", "First_Q", "Last_Q")

Q_5yrs[,c("First_Si", "Last_Si", "First_Q", "Last_Q")]<-lapply(Q_5yrs[,c("First_Si", "Last_Si", "First_Q", "Last_Q")], as.Date)

Q_5yrs$overlap_min<-as.Date(ifelse(Q_5yrs$First_Q > Q_5yrs$First_Si, Q_5yrs$First_Q, Q_5yrs$First_Si), origin = "1970-01-01")

Q_5yrs$overlap_max<-as.Date(ifelse(Q_5yrs$Last_Q < Q_5yrs$Last_Si, Q_5yrs$Last_Q, Q_5yrs$Last_Si),origin = "1970-01-01")

Q_5yrs$overlap_diff<-as.numeric(difftime(Q_5yrs$overlap_max, Q_5yrs$overlap_min,
                                         units = "days")/365)

Q_5yrs<-subset(Q_5yrs, Q_5yrs$overlap_diff > 4.99)

Q_10yrs<-subset(Q_5yrs, Q_5yrs$overlap_diff > 9.99)

Q_15yrs<-subset(Q_5yrs, Q_5yrs$overlap_diff > )

already_have_sites<-c("mcmurdo", "luquillo", "niwot", "hbef", "hjandrews", "bonanza", "boulder")

Q_5yrs_new<-Q_5yrs[-which(Q_5yrs$domain %in% already_have_sites),]

Q_10yrs_new<-subset(Q_5yrs_new, Q_5yrs_new$overlap_diff > 9.99)


#for checking NH4 availablity for new sites
ms_NH4<-ms_vars_list[ms_vars_list$variable_code %like% "NH4"&ms_vars_list$variable_type=="chem_discrete",]

NH4_names<-ms_NH4$variable_code

ms_vars_NH4<-ms_vars[ms_vars$variable_code %in% NH4_names,]

ms_vars_NH4<-subset(ms_vars_NH4, ms_vars_NH4$unit=="mg/L")

ms_vars_NH4<-ms_vars_NH4 %>%
  group_by(site_code) %>%
  top_n(1, abs(observations))

new_sites<-merge(Q_5yrs_new, ms_vars_NH4, by=c("network", "domain", "site_code"), all.x=TRUE)


#for checking NOx availablity for new sites
ms_NOx<-ms_vars_list[ms_vars_list$variable_code %like% "NO3_NO2"&ms_vars_list$variable_type=="chem_mix",]

NOx_names<-ms_NOx$variable_code

ms_vars_NOx<-ms_vars[ms_vars$variable_code %in% NOx_names,]

ms_vars_NOx<-subset(ms_vars_NOx, ms_vars_NOx$unit=="mg/L")

ms_vars_NOx<-ms_vars_NOx %>%
  group_by(site_code) %>%
  top_n(1, abs(observations))

new_sites<-merge(new_sites, ms_vars_NOx, by=c("network", "domain", "site_code"), all.x=TRUE)


#for checking PO4 availablity for new sites
ms_P<-ms_vars_list[ms_vars_list$variable_code %like% "PO4"&ms_vars_list$variable_type=="chem_discrete",]

P_names<-ms_P$variable_code

ms_vars_P<-ms_vars[ms_vars$variable_code %in% P_names,]

ms_vars_P<-subset(ms_vars_P, ms_vars_P$unit=="mg/L")

ms_vars_P<-ms_vars_P %>%
  group_by(site_code) %>%
  top_n(1, abs(observations))

new_sites<-merge(new_sites, ms_vars_P, by=c("network", "domain", "site_code"), all.x=TRUE)




##download data

my_sites<-Q_5yrs_new$site_code

krycklan_sites<-subset(Q_5yrs_new, Q_5yrs_new$domain=="krycklan")

my_sites<-krycklan_sites$site_code

my_domains<-unique(Q_5yrs_new$domain)

my_directory<-"/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/MacroSheds_Sites"

ms_download_core_data(
  my_directory,
  domains = my_domains,
  quiet = FALSE
)

my_q <- ms_load_product(
  my_directory,
  prodname = "discharge",
  site_codes = my_sites,
  sort_result = TRUE,
  warn = TRUE
)

my_chem <- ms_load_product(
  my_directory,
  prodname = "stream_chemistry",
  filter_vars = "PO4_P",
  site_codes = my_sites,
  sort_result = TRUE,
  warn = TRUE
)

#ms_vars_counts<-as.data.table(table(ms_vars$variable_code))

my_sites<-ms_sites[ms_sites$site_code %in% my_sites,]

my_sites_latlong<-my_sites[,c("domain", "site_fullname", "latitude", "longitude", "epsg_code")]

write.csv(my_sites_latlong, "NewSitesLatLong.csv")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/MacroSheds_Sites")

my_q %>%
  group_by(site_code) %>%
  group_walk(~write.csv(.x, paste0(.y$site_code, "_Discharge.csv")))

my_chem %>%
  group_by(site_code) %>%
  group_walk(~write.csv(.x, paste0(.y$site_code, "_PO4.csv")))

my_sites<-my_sites %>%
  select("domain_fullname", "site_code", "site_fullname", "latitude", "longitude", "ws_area_ha")

colnames(my_sites)<-c("LTER", "Unique ID", "Site/Stream", "Latitude", "Longitude", "Watershed Area (km2)")

my_sites$`Watershed Area (km2)`<-my_sites$`Watershed Area (km2)`/100

my_sites %>%
  group_by(`Unique ID`) %>%
  #select(c("LTER", "Unique ID", "Site/Stream", "Latitude", "Longitude", "Watershed Area (km2)")) %>%
  group_walk(~write.csv(.x, paste0(.y$`Unique ID`, "_Charatceristics.csv")))

for (i in 1:nrow(my_sites)) {
  
  one_row<-my_sites[i,]
  
  write.csv(one_row, paste0(one_row$`Unique ID`, "_Characteristics.csv"))
  
}

unique(ms_vars$variable_code)




