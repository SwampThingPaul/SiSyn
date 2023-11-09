setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

CJ<-read.csv("CatalinaJemez_chemistry_2009-2019.csv")

table(CJ$SiteCode)

CJ<-CJ[CJ$SiteCode %in% c("MG_S_OUT","MG_S_SEEP","MG_WEIR","MG_G_OUT","OR_low","OR_mid","OR_up","BGZOB_FLUME"),]

write.csv(CJ, "CatalinaJemez_chemistry_2009-2019_V2.csv")
