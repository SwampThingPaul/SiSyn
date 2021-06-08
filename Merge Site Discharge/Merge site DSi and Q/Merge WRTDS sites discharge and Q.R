#merge all site discharge and DSi dataframes by site and date
#remove NA dates from WRTDS_discharge file
WRTDS_discharge_allsites_NArm = na.omit(WRTDS_discharge_allsites_21Apr21)

WRTDS_DSi_Q = merge(WRTDS_discharge_allsites_NArm,WRTDS_DSi_mergedsites_19May21, by=c("site.name","Date"), all=T)

#filter merged file by sites run in WRTDS
WRTDS_site_list = Data_years_streams_LongTerm$Stream.Site
WRTDS_site_list = sort(WRTDS_site_list)

library(dplyr)
WRTDS_data = 
  WRTDS_DSi_Q %>%
  filter(WRTDS_DSi_Q$site.name %in% WRTDS_site_list)

#what is in WRTDS_data_list that's not in WRTDS_site_list
difs = setdiff(WRTDS_data_list, WRTDS_site_list)
#what is in WRTDS_site_list that's not in WRTDS_data?
difs = setdiff(WRTDS_site_list, WRTDS_data_list)
#missing Toolik Inlet and TW Weir from WRTDS_data