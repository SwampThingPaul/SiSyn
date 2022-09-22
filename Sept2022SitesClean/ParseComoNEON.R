##download COMO Creek for Si Seasonality ###

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NEON_data")

NEON_chem<-read.csv("NEON_chem_data.csv")

NEON_Q<-read.csv("NEON_Q_Data.csv")

COMO_Q<-subset(NEON_Q, NEON_Q$Group.2=="COMO")

COMO_Q<-subset(COMO_Q, COMO_Q$dischargeFinalQF < 0.5)

COMO_Q<-COMO_Q[,c(2,15)]

names(COMO_Q)<-c("Date", "Q")

COMO_Q$Date<-as.Date(COMO_Q$Date)

COMO_Si<-subset(NEON_chem, NEON_chem$siteID=="COMO" & NEON_chem$analyte=="Si")

COMO_Si<-COMO_Si[,c(8,12)]
names(COMO_Si)<-c("Date", "Conc_mgL")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NewSites_Sept2022")

write.csv(COMO_Si, "ComoCreek_Si.csv")

write.csv(COMO_Q, "ComoCreek_Q.csv")
