require(dplyr)
require(ggplot2)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

chem<-read.csv("NT_NSW_chem.csv")

chem$Date2<-as.Date(chem$Date, "%d/%m/%y")

chem_date<-chem[is.na(chem$Date2),]

chem_date$Date2<-as.Date(chem_date$Date, "%d/%m/%Y")

chem<-chem[complete.cases(chem$Date2),]

chem<-bind_rows(chem, chem_date)

chem$site_no<-sub("\\_WQ.*", "", chem$Site)

chem$site_no<-sub('.+_(.+)', '\\1', chem$site_no)

counts_table<-chem %>%
  group_by(site_no) %>%
  count(Parameter)

Si<-counts_table %>%
  filter(Parameter == "Si") %>%
  filter(n > 49)

NSW_NT_sites<-data.frame(unique(counts_table$site_no))

spatial<-read.csv("Si_spatial.csv")

colnames(NSW_NT_sites)<-"Site_ID"

NSW_NT_sites<-merge(NSW_NT_sites, spatial, by="Site_ID")

NSW_NT_sites_50<-NSW_NT_sites[c(NSW_NT_sites$Site_ID %in% Si$site_no),]

write.csv(NSW_NT_sites_50, "NSW_NT_sites.csv")


chem<-chem[c(chem$site_no %in% NSW_NT_sites_50$Site_ID),]

chem_si<-subset(chem, chem$Parameter=="Si")
chem_si<-subset(chem_si, chem_si$Flag_LT==0)

ggplot(chem_si, aes(Date2, Value))+geom_point()+theme_bw()+facet_wrap(~site_no, scales = "free")+
  ylim(c(0,100))


MD<-read.csv("MurrayDarling_Chem.csv")

MD$Date<-as.Date(MD$Date)

ggplot(MD, aes(Date, Si))+geom_point()+theme_bw()+facet_wrap(~Site, scales = "free")

MD2<-read.csv("WQ Spot.csv")

MD2_count<-data.frame(table(MD2$Varnam))




