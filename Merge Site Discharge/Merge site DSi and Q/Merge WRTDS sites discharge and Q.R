#merge all site discharge and DSi dataframes by site and date
WRTDS_DSi_Q = merge(WRTDS_discharge_allsites,WRTDS_DSi_mergedsites, by=c("site.name","Date"), all=T)
length(unique(WRTDS_DSi_Q$site.name))

write.csv(WRTDS_DSi_Q, file="merged_DSi_Q_WRTDSlongterm_3Jun22.csv")

#filter merged file by sites run in WRTDS
WRTDS_longterm_site_list = Data_years_streams_WRTDS$Stream.Site
View(WRTDS_longterm_site_list)

library(dplyr)
WRTDS_data = 
  WRTDS_DSi_Q %>%
  filter(WRTDS_DSi_Q$site.name %in% WRTDS_site_list)

#what is in WRTDS_data_list that's not in WRTDS_site_list
difs = setdiff(WRTDS_data_list, WRTDS_site_list)
#what is in WRTDS_site_list that's not in WRTDS_data?
difs = setdiff(WRTDS_site_list, WRTDS_data_list)
#missing Toolik Inlet and TW Weir from WRTDS_data