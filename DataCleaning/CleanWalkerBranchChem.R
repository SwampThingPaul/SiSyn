setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/Data_Summer2023")

load("WalkerBranch.1989.2013.RData")

Q_cols<-colnames(wbwWestEast)[colnames(wbwWestEast) %like% "Q"]

wbwWestEast_noQ<-wbwWestEast[,!colnames(wbwWestEast) %in% Q_cols]

wbw_melt<-melt(wbwWestEast_noQ, id.vars="DATE")

wbw_melt$site<-sub('.*\\.', '', wbw_melt$variable)

wbw_melt$variable<-as.character(wbw_melt$variable)

wbw_melt$site<-ifelse(wbw_melt$site %in% c("East", "West"), wbw_melt$site, "Walker Branch")

wbw_melt$clean_name<-ifelse(wbw_melt$site %in% c("East", "West"), 
                            sub("^(.*)[.].*", "\\1", wbw_melt$variable), wbw_melt$variable)

wbw_melt<-wbw_melt[complete.cases(wbw_melt),]

wbw_melt$site_name<-ifelse(wbw_melt$site=="East", "East Fork",
                           ifelse(wbw_melt$site=="West", "West Fork", "WALK"))

wbw_melt<-wbw_melt[c("DATE","clean_name","value","site_name")]

wbw_cast<-dcast(wbw_melt, site_name+DATE~clean_name, value.var = "value")

write.csv(wbw_cast, "WalkerBranch_Chem.csv")

