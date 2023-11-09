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

#add in Toolik Inlet and TW Weir from masterdata
unique(X20220531_masterdata$variable)
unique(X20220531_masterdata$LTER)

ARCsites_masterdata = subset(X20220531_masterdata, X20220531_masterdata$LTER=="ARC")
ARCsites = unique(ARCsites_masterdata$site)

ARCsites_DSi = subset(ARCsites, ARCsites$variable=="DSi")
ARCsites_Q = subset(X03042022_discharge_master, X03042022_discharge_master$site.name%in%ARCsites)
#merge ARC sites DSi and Q, add to WRTDS DSi-Q data
colnames(ARCsites_DSi) = c("LTER","site","site.name","Date","variable","Si")
ARCsites_DSi = ARCsites_DSi[,c(1,3,4,6)]
#get mean of duplicate DSi values from same date
ARCsites_DSi_nodups = aggregate(ARCsites_DSi,by=c("LTER","site.name","Date"),FUN=mean)
colnames(ARCsites_Q)[2] = "Q"
ARC_DSi_Q = merge(ARCsites_DSi,ARCsites_Q)

#merge ARC DSi, Q dat with WRTDS data
colnames(WRTDS_DSi_Q)
colnames(ARC_DSi_Q)
ARC_WRTDS_DSi_Q = merge(WRTDS_DSi_Q, ARC_DSi_Q, all=T)

#export to .csv
write.csv(ARC_WRTDS_DSi_Q, file="DSi_Q_dat_allsites_13Jul22.csv")
