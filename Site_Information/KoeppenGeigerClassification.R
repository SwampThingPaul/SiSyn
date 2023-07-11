install.packages("kgc")
require(kgc)
require(readxl)
require("Polychrome")
require(googledrive)
require(ggplot2)

#get link to ref table from GD, get folder
ref_table_link<-"https://docs.google.com/spreadsheets/d/11Noj6UnAliyA_R96c0a0nrDlukweJAW_O017E88DLBo/edit?usp=share_link"
ref_table_folder = drive_get(as_id(ref_table_link))

#download ref table
ref_table<-drive_download(ref_table_folder$drive_resource, overwrite = T)

#read it in
QLog<-read_xlsx("WRTDS_Reference_Table.xlsx")

#extract columns you need
QLog<-QLog[,c("Stream_Name", "Latitude", "Longitude", "LTER")]

#remove all sites with no lat/long - these would be ones not included in WRTDS
QLog<-QLog[complete.cases(QLog$Latitude),]

#remove BNZ - not using
QLog<-QLog[!QLog$LTER=="BNZ",]

#format data to pull using KGC
coord_data<-data.frame(QLog, rndCoord.lon=RoundCoordinates(QLog$Longitude), 
         rndCoord.lat=RoundCoordinates(QLog$Latitude))

#get KG for each site
data <- data.frame(coord_data,ClimateZ=LookupCZ(coord_data))

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

KG_name<-read.csv("KG_Clim_Name.csv")

data<-merge(data, KG_name, by="ClimateZ")

#write.csv(data, "Koeppen_Geiger.csv")

data$Name<-factor(data$Name, levels = c("Humid Continental", "Humid Subtropical", 
                                         "Humid Temperate", "Humid Tropical", "Mediterranean",
                                        "Polar Desert", "Subarctic",
                                         "Tundra"))

write.csv(data, "Koeppen_Geiger.csv")


col_vals<-c("Humid Tropical"=="#DFC27D", "Humid Subtropical"=="#C3924B", 
            "Mediterranean"=="#B0C89F","Humid Temperate"=="#C8C58E", 
            "Humid Continental"=="#A6611A", "Subarctic"=="#80CDC1",
            "Tundra"=="#40A999", "Polar Desert"=="#98CBB0")

cols<-c("Humid Tropical"=="green", "Humid Subtropical"=="blue", 
            "Mediterranean"=="blue","Humid Temperate"=="blue", 
            "Humid Continental"=="green", "Subarctic"=="red",
            "Tundra"=="pink", "Polar Desert"=="pink")

#plot
ggplot(data, aes(Name, fill=Name))+geom_bar()+
  theme_classic()+labs(x="", y="Count")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1), text = element_text(size = 20))+
  scale_fill_brewer(palette = "BrBG")


data<-data[!duplicated(data$LTER,fromLast=TRUE),]

write.csv(data, "KGClass.csv")



