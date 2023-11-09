require(tidyverse)
require(googledrive)
require(stringr)
require(lubridate)
require(reshape)
require(gtools)

##reformat MacroSheds Data

##for Discharge Log
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/MacroSheds_Sites/Si")

MSQ<-list.files(pattern = "Discharge.csv")

MSChars<-list.files(pattern = "Char")

MSQ_names<-word(MSQ, 1, sep = "\\.csv")

MSQ_site<-word(MSQ_names,1,sep = "_Discharge")

df<-data.frame(MSQ_site, MSQ_names)

LTER_site<-list()

for (i in 1:length(MSChars)) {
  
  dat<-read.csv(MSChars[i])
  
  LTER_site[[i]]<-dat[,c("LTER", "Unique.ID")]
  
}

LTER_site_df<-do.call(rbind,LTER_site)
colnames(LTER_site_df)<-c("LTER_full","MSQ_site")

df<-merge(df, LTER_site_df, by=c("MSQ_site"))

LTER_full<-unique(df$LTER_full)

LTER_abb<-c("WB", "BB", "CAT", "SU", "ER", "SH", "KRY")

LTER_lookup<-data.frame(LTER_full, LTER_abb)

df<-merge(df, LTER_lookup, by=c("LTER_full"))
df$units<-c("Ls")

colnames(df)<-c("LTER_full", "Stream", "DischargeFileName", "LTER","units")

write.csv(df,"Qlog_Macrosheds.csv")

for (i in 1:length(MSChars)) {
  
  dat<-read.csv(MSChars[i])
  
  info_file<-data.frame(c("mg/L", dat[,c("Unique.ID")], "Silicon", "DSi", dat[,c("Watershed.Area..km2.")],
               dat[,c("Unique.ID")], "DSi", dat[,c("Unique.ID")]))
  
  info_file_df<-as.data.frame(t(info_file), row.names = 1)
  
  colnames(info_file_df)<-c("param.units", "shortName", "paramShortName", "constitAbbrev",
                            "drainSqKm", "station.nm", "param.nm", "staAbbrev")
  
  write.csv(info_file_df, paste0(MSQ_site[i], "_INFO.csv"))
  
}


