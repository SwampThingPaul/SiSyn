setwd("/Users/keirajohnson/Box Sync/CZ Data Repository- Work Draft/COMO CREEK/Water Chemistry")

como<-read.csv("WaterChemistry.csv")

unique(como$analyte)

como_chem<-subset(como, como$analyte %like% "NO3+NO2 - N|TDN|TN|NO2 - N|Ortho - P|TDP|Si")

table(como_chem$analyte)

como_chem$collectDate<-as.Date(como_chem$collectDate)

ggplot(como_chem, aes(collectDate, analyteConcentration))+geom_point()+
  facet_wrap(~analyte, scales="free")

write.csv(como_chem, "ComoCreekNutrients.csv")
