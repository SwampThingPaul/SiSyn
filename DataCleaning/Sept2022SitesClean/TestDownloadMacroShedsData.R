require(data.table)
require(ggplot2)
require(stringr)
require(data.table)

##check Q for new MacroSheds Sites

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/MacroSiPrepWRTDS")

#make list of all files that you just downloaded to local folder
#WRTDS_files_List<-list.files(path = "C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Data/WRTDS/WRTDS_prep_Si_01162022")
WRTDS_files_List<-list.files(path = "/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/MacroSiPrepWRTDS")

#make list of only Q files
WRTDS_files_List_Q<-WRTDS_files_List[WRTDS_files_List %like% "Q_WRTDS"]

WRTDS_files_List_Si<-WRTDS_files_List[WRTDS_files_List %like% "Si_WRTDS"]

WRTDS_names<-gsub("_Si_WRTDS.csv", "", WRTDS_files_List_Si)

keep_these_sites<-c("MarshallGulch", "OracleRidge", "Site1", "Site10", "Site12", "Site13", "Site14", "Site15", "Site16", 
                    "Site2","Site4", "Site5", "Site6", "Site7", "Site9", "UpperJaramillo", "east_fork", "west_fork")

K_Si<-WRTDS_files_List_Si[(WRTDS_names %in% keep_these_sites)]
K_Q<-WRTDS_files_List_Q[(WRTDS_names %in% keep_these_sites)]

K_names<-gsub("_Si_WRTDS.csv", "", K_Si)

missing<-list()

for (i in 1:length(K_Q)) {
  
  Q_dat<-read.csv(K_Q[i])
  
  Q_dat$Date<-as.Date(Q_dat$Date)
  
  if(K_names[i] %like% "Site") {
    
    Q_dat<-subset(Q_dat, Q_dat$Date > as.Date("2007-09-30"))
    
  } else{
    
    Q_dat<-Q_dat
    
  }
  
  date_range <- seq(min(Q_dat$Date), max(Q_dat$Date), by = 1) 
  num_missing_days<-length(date_range[!date_range %in% Q_dat$Date])
  missing_days_prop<-num_missing_days/length(date_range)
  
  missing[[i]]<-c(num_missing_days, missing_days_prop)
  
  # Q_dat$Date<-as.Date(Q_dat$Date)
  # 
  # Si<-read.csv(K_Si[i])
  # 
  # Si$Date<-as.Date(Si$Date)
  # 
  # fact<-max(Si$DSi)/max(Q_dat$Qcms)
  # 
  # p1<-ggplot()+geom_line(Q_dat, mapping=aes(Date, Qcms))+
  #   geom_point(Si, mapping=aes(Date, DSi/fact), col="red")+
  #   scale_y_continuous(name="Q",sec.axis = sec_axis(trans = ~.*fact, name = "DSi"))+
  #   ggtitle(paste0(K_Names[i]))+theme_classic()+theme(text = element_text(size = 20))
  # 
  # print(p1)
  
}

#dev.off()

missing_df<-as.data.frame(do.call(rbind, missing))

missing_df$site<-K_names



