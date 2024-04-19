#find N and P for the 200 sites in harmonized DB
require(googledrive)
require(ggplot2)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

chem_url<-"https://drive.google.com/file/d/1wQQCZYmtgma46MTPavoGeZzMJdZSR3F6/view?usp=drive_link"

file_get<-drive_get(as_id(chem_url))

drive_download(file_get$drive_resource, overwrite = T)

chem<-read.csv("20231109_masterdata_chem.csv")

chem_NP<-chem[chem$variable %in% c("SRP", "PO4", "NO3", "NOx"),]

chem_NP<-subset(chem_NP, chem_NP$value > 0)

# #range(chem_NP$value[chem_NP$variable=="PO4"], na.rm=T)
# 
# ggplot(chem_NP[chem_NP$variable=="PO4",], aes(value))+geom_histogram(bins=100)
# 
# table(chem_NP$units)
# 
# chem_NP_uM<-subset(chem_NP, chem_NP$units=="uM")
# 
# chem_SRP<-subset(chem_NP, chem_NP$units=="uM"&chem_NP$variable=="SRP")
# 
# chem_PO4<-subset(chem_NP, chem_NP$units=="uM"&chem_NP$variable=="PO4")
# 
# chem_LMP<-subset(chem_NP, chem_NP$LTER=="LMP(Wymore)"&chem_NP$variable=="PO4")
# 
# write.csv(chem_LMP, "LMP_PO4.csv")
# 
# range(chem_NP_uM$value[chem_NP_uM$variable=="SRP"], na.rm=T)
# 
# unique(chem_NP_mgL$LTER)

#one_site<-subset(chem_NP, chem_NP$LTER=="CZO-Catalina Jemez")

chem_NP_avg<-chem_NP %>%
  dplyr::group_by(Stream_Name, variable) %>%
  dplyr::summarise(mean_val=median(value, na.rm = T))

chem_units<-chem_NP[,c("Stream_Name", "variable", "units")]

chem_units$Stream_var<-paste0(chem_units$Stream_Name, chem_units$variable)

chem_units<-chem_units[!duplicated(chem_units$Stream_var),]

chem_NP_avg<-merge(chem_NP_avg, chem_units, by=c("Stream_Name", "variable"))

chem_NP_avg<-chem_NP_avg[,c(1:4)]

harmonized_drivers<-read.csv("AllDrivers_Harmonized.csv")

harmonized_drivers<-harmonized_drivers[!duplicated(harmonized_drivers$Stream_ID),]

harmonized_drivers$site<-word(harmonized_drivers$Stream_ID, 2, sep = "__")

chem_NP_avg_200<-chem_NP_avg[chem_NP_avg$Stream_Name %in% harmonized_drivers$site,]

missing_files<-setdiff(harmonized_drivers$site, chem_NP_avg_200$Stream_Name)

missing_files

renamed_sites<-c("MG_WEIR", "OR_low", "COMO", "East Fork", "West Fork")
old_name<-c("Marshall Gulch", "Oracle Ridge", "Como Creek", "east fork", "west fork")

name_conversion<-data.frame(renamed_sites, old_name)

colnames(name_conversion)<-c("Stream_Name", "Updated_StreamName")

missing_sites<-chem_NP_avg[chem_NP_avg$Stream_Name %in% renamed_sites,]

missing_sites<-left_join(missing_sites, name_conversion, by="Stream_Name")

missing_sites<-missing_sites[,-1]

colnames(missing_sites)[4]<-"Stream_Name"

unique(missing_sites$Stream_Name)

chem_NP_avg_200_updated<-bind_rows(missing_sites, chem_NP_avg_200)

missing_files<-setdiff(harmonized_drivers$site, chem_NP_avg_200_updated$Stream_Name)

chem_NP_avg_200_updated$solute_simplified<-ifelse(chem_NP_avg_200_updated$variable %in% c("NOx", "NO3"),
                                                  "N", "P")

write.csv(chem_NP_avg_200_updated, "Median_N_P_200.csv")



