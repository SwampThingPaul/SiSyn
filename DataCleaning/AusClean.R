setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

require(dplyr)

NT_NSW<-read.csv("NT_NSW_chem.csv")
si<-subset(NT_NSW, NT_NSW$Parameter=="Si")

NT_NSW$Site_Code<-sub("\\_WQ.*", "", NT_NSW$Site)
NT_NSW$Site_Code<-sub('.+_(.+)', '\\1', NT_NSW$Site_Code)

NT_NSW_clean<-NT_NSW[,c(1:4,9)]

names<-read.csv("NT_NSW_Names.csv")

NT_NSW_name<-merge(NT_NSW_clean, names, by="Site_Code")

write.csv(NT_NSW_name, "NT_NSW_Chem_Cleaned.csv")

params<-data.frame(table(NT_NSW_name$Parameter))

NT_NSW_name$unique<-paste(NT_NSW_name$Parameter,NT_NSW_name$Unit)

params_units<-NT_NSW_name[!duplicated(NT_NSW_name$unique),]



si$Date2<-format(as.Date(si$Date, format="%d/%m/%Y"), "%d/%m/%y")
si$Date2 <- as.Date(si$Date2,"%d/%m/%y")

sits<-unique(si$Site)

pdf("NT_NSW_QAQC.pdf")

for (i in 1:length(sits)) {

  one_site<-subset(si, si$Site==sits[i])
  
  p1<-ggplot(one_site, aes(Date2, Value))+geom_point()+theme_bw()+ggtitle(sits[i])
  
  print(p1)
  
}

dev.off()

MD1<-read.csv("MurrayDarling_Chem_Rob.csv")
MD2<-read.csv("MurrayDarling_Chem_Paul.csv")

MDnames<-read.csv("MD_Names.csv")

MD1Names<-merge(MDnames, MD1, by="Station")

MD2Names<-merge(MDnames, MD2, by="Site")

MD1params<-data.frame(table(MD1Names$Varnam))

MD1params<-subset(MD1params, MD1params$Freq > 100)

MD1params$shortname<-c("Alk_pH", "Temp", "BiCarbAlk_CaCO3", "Ca", "Cl", "Chla", "Colour", "Conduct", "Depth", "DOC", "DO", "Field_EC",
                       "SRP", "Fe", "TKN", "Mg", "Mn", "NOx", "pH", "Phaeophytin", "K", "Salinity", "Sample_Temp", "Si", "Na", 
                       "SO4", "Alk_CaCO3", "TN", "TP", "TSS", "Turbidity", "Water_Temp")


colnames(MD1params)[1]<-"Varnam"

MD1Names<-merge(MD1Names, MD1params, by="Varnam")

MD1Names<-MD1Names[,c(1,2,3,5,6,11)]
MD1Names$Datetime<-as.Date(MD1Names$Datetime, "%d/%m/%Y")

MD2NamesMelt<-melt(MD2Names, id.vars = c("Site", "Station", "Date"))

MD2NamesMelt$Date<-as.Date(MD2NamesMelt$Date)
colnames(MD2NamesMelt)<-c("Site", "Station", "Datetime", "shortname", "Value")

MD_all<-bind_rows(MD1Names, MD2NamesMelt)

MD_all$unique<-paste(MD_all$Station, MD_all$Datetime, MD_all$shortname)

MD_all<-MD_all[!duplicated(MD_all$unique),]

MD_all$Site<-ifelse(MD_all$Site=="Torrumbarry Weir", "Torrumbarry", MD_all$Site)

unique(MD_all$Site)

MD_all<-MD_all[complete.cases(MD_all$Value),]

table(MD_all$shortname)

write.csv(MD_all, "MurrayDarlingChem_Merged.csv")

si<-subset(MD_all, MD_all$shortname=="Si")

ggplot(si, aes(Datetime, Value))+geom_point()+theme_bw()+facet_wrap(~Site, scales = "free")
