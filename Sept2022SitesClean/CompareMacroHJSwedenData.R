setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/MacroSiPrepWRTDS")

#make list of all files that you just downloaded to local folder
#WRTDS_files_List<-list.files(path = "C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Data/WRTDS/WRTDS_prep_Si_01162022")
WRTDS_files_List<-list.files(path = "/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/MacroSiPrepWRTDS")

#make list of only Q files
WRTDS_files_List_Q<-WRTDS_files_List[WRTDS_files_List %like% "Q_WRTDS"]

WRTDS_files_List_Si<-WRTDS_files_List[WRTDS_files_List %like% "Si_WRTDS"]

WRTDS_names<-gsub("_Si_WRTDS.csv", "", WRTDS_files_List_Si)

keep_these_sites<-c("Site1", "Site10", "Site12", "Site13", "Site14", "Site15", "Site16", 
                    "Site2","Site4", "Site5", "Site6", "Site7", "Site9")

K_Si<-WRTDS_files_List_Si[(WRTDS_names %in% keep_these_sites)]
K_Q<-WRTDS_files_List_Q[(WRTDS_names %in% keep_these_sites)]

K_names<-gsub("_Si_WRTDS.csv", "", K_Si)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

HJ_data<-read.csv("SwedenSiData.csv")

HJ_data$site<-paste0("Site", HJ_data$SiteID)

HJ_data$Date<-as.Date(HJ_data$Date)

HJ_data$Si_mgL<-HJ_data$Si.µg.l/1000

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/MacroSiPrepWRTDS")

pdf("HJ_Macro_Compare.pdf")

for (i in 1:length(K_Si)) {

  Si<-read.csv(K_Si[i])

  Si$Date<-as.Date(Si$Date)
  
  HJ_Si<-subset(HJ_data, HJ_data$site==K_names[i])

  p1<-ggplot()+geom_point(Si, mapping = aes(Date, DSi), col="red")+
    geom_point(HJ_Si, mapping=aes(Date, Si_mgL), col="blue")+
    ggtitle(paste0(K_names[i]))+theme_classic()+theme(text = element_text(size = 20))

  print(p1)
  
}

dev.off()

missing_df<-as.data.frame(do.call(rbind, missing))

missing_df$site<-K_names

K_names<-unique(HJ_data$site)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NewSites_Sept2022")

for (i in 1:length(K_names)) {
  
  HJ_Si<-subset(HJ_data, HJ_data$site==K_names[i])
  
  write.csv(HJ_Si, paste0(K_names[i], "_Si.csv"))
  
}

dev.off()

missing_df<-as.data.frame(do.call(rbind, missing))

missing_df$site<-K_names


setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/MacroSheds_Sites/Si")

Q_files<-list.files(path = ".", pattern = "_Discharge.csv")

SWE_files<-Q_files[grep("Site", Q_files)]

pdf("Krycklan_Q.pdf")

for (i in 1:length(SWE_files)) {
  
  Q<-read.csv(SWE_files[i])
  
  Q$val<-as.numeric(Q$val)
  
  Q$datetime<-as.Date(Q$datetime)
  
  p1<-ggplot(Q, aes(datetime, val))+geom_line()+theme_bw()+ggtitle(paste(SWE_files[i]))
  
  print(p1)
  
}


dev.off()
