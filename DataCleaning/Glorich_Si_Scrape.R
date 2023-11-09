##check GLORICH

#change
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/Data_Summer2023/Glorich_V01_CSV_plus_Shapefiles_2019_05_24")

#read in chem
glorich<-read.csv("hydrochemistry.csv")

elbe<-subset(glorich, glorich$STAT_ID=="303017")

write.csv(elbe, "ElbeRiverChem.csv")

#keep only relavent columns
glorich<-glorich[,c("STAT_ID", "RESULT_DATETIME", "SAMPLING_MODE", "SiO2", "Discharge_inst", "Ref", "Country")]

#remove columns that dont have Si and Q
glorich<-glorich[complete.cases(glorich$SiO2),]

#format date
glorich$RESULT_DATETIME<-as.Date(glorich$RESULT_DATETIME, "%d/%m/%Y")

#count number of Si obs
glo_count<-glorich %>%
  count(STAT_ID)

#keep only sites with > 60 obs
glo_count_60<-subset(glo_count, glo_count$n > 59)

glorich<-glorich[c(glorich$STAT_ID %in% glo_count_60$STAT_ID),]

#remove non-grab sample data
glorich<-glorich[c(glorich$SAMPLING_MODE=="single"),]

#read in and merge in source location data
loc<-read.csv("sampling_locations.csv")

germ<-subset(loc, loc$Country=="Germany")

germ<-germ[grepl("Elbe", germ$STATION_NAME),]

glorich<-merge(glorich, loc, by="STAT_ID")

cameroon<-subset(glorich, glorich$Country=="Germany")

write.csv(cameroon, "CameroonChem.csv")

getwd()

ggplot(cameroon, aes(RESULT_DATETIME, SiO2))+geom_point()+
  facet_wrap(~STAT_ID)


german_stations<-unique(germany$STATION_NAME)

pdf("Germany_Si.pdf")

for (i in 1:length(german_stations)) {
  
  one_stat<-subset(germany, germany$STATION_NAME==german_stations[i])
  
  p1<-ggplot(one_stat, aes(RESULT_DATETIME, SiO2))+geom_point()+theme_bw()+
    ggtitle(german_stations[i])
  
  print(p1)
  
  
}

dev.off()



