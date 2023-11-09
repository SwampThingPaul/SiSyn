setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

drivers<-read.csv("AllDrivers_Harmonized.csv")

drivers<-drivers[,!colnames(drivers) %in% c("X.1", "X", "LTER", "Stream_Name", "rndCoord.lon", "rndCoord.lat", "Name", "cycle1")]
drivers<-drivers[!duplicated(drivers$Stream_ID),]

#crop to only relevant drivers
drivers_cropped<-drivers[,c("med_si","CV_C","med_q","CV_Q", "cvc_cvq","slope","ClimateZ","Latitude","Longitude",
                            "drainSqKm", "num_days","prop_area","precip", "evapotrans", "temp", "npp",
                            "cycle0", "elevation_mean_m", colnames(drivers[30:58]))]

#replace NA with 0 in proportion of land/lithology/soils
drivers_cropped[,c(19:47)]<-replace(drivers_cropped[,c(19:47)], is.na(drivers_cropped[,c(19:47)]), 0)

small_da<-50
large_da<-1000

drivers_cropped$drainage_class<-ifelse(drivers_cropped$drainSqKm < small_da, "small",
                               ifelse(drivers_cropped$drainSqKm > large_da, "large", "medium"))

table(drivers_cropped$drainage_class)

litho<-as.data.table(table(drivers_cropped$major_rock))


ggplot(drivers_cropped, aes(x=med_si))+geom_histogram(bins = 100)
