install.packages("kgc")
require(kgc)
require(readxl)
require("Polychrome")

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

#write.csv(data, "Koeppen_Geiger.csv")

#plot
ggplot(data, aes(ClimateZ, fill=LTER))+geom_bar()+
  theme_classic()+labs(x="Koeppen-Geiger Climate Zone", y="Count")+
  theme(text = element_text(size = 20))+scale_fill_viridis_d(option = "magma")


