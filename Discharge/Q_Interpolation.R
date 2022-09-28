library(zoo)
library(plyr)
library(dplyr)

#interpolate Q data##
#setwd("G:/Shared drives/SCWRS/Sethna/SiSyn/Q interp files")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NewSites_Sept2022")

#### read in all Q and Si csv files for new sites ####
Q_files<-list.files(path = ".", pattern = "_Q.csv|_Discharge.csv")

# Si_files<-list.files(path = ".", pattern = "_Si.csv")

DischargeList<-c("MEAN_Q", "Discharge", "InstantQ", "Q_m3sec", "discharge", "Q", "var")
DateList<-c("Date", "dateTime", "dates", "date", "datetime")

#loop to read in csv files; concatenate into one dataframe for Q and Si
Q = list()

for (i in 1:length(Q_files)){
  siteQ = read.csv(Q_files[i])
  
  names(siteQ)[which(colnames(siteQ) %in% DischargeList)]<-"Q" #convert all Q columns to be called "Q"
  names(siteQ)[which(colnames(siteQ) %in% DateList)]<-"Date" #convert all Date columns to be called "Date"
  
  site_name<-gsub("_Q.csv|_Discharge.csv", "", Q_files[i])
  
  siteQ$site_from_csv<-site_name
  
  siteQ<-siteQ[,c("Date", "Q", "site_from_csv")]
  
  names(siteQ)<-c("Date", "Q", "Site")
  
  Q[[i]] = siteQ
}

allQ = ldply(Q, data.frame)

# Si = list()
# 
# for (i in 1:length(Si_files)){
#   siteSi = read.csv(Si_files[i])
#   Si[[i]] = siteSi
# }
# 
# allSi = ldply(Si, data.frame)

#get Si data from new chemistry master data file
allSi = subset(X20220922_masterdata, X20220922_masterdata$variable=="DSi")
allSi = allSi[,c(2,3,5,6)]

#### remove NA for Q, then check for missing dates ####
#only keep columns 3:6 from Q dataframe
allQ = allQ[,3:6]
allQ_noNA = na.omit(allQ)
allQ_noNA$Date = as.Date(allQ_noNA$Date)

#are sites in master data and Q data?
which(allQ_noNA$Stream %in% allSi$Stream) #new Q sites are not in new master data?

#### check if there is overlap between Si sample and missing Q dates ####
## if yes remove sample, dont want sample where there is interpolated Q ##
#merge all C-Q data, remove Si data not in Q
colnames(allQ_noNA)
colnames(allSi) = c("Site","Stream","Date","Si") #match column names for merging

Si_Q_all = merge(allSi, allQ_noNA) #This should remove any Si values where Q is NA

## record length of missing dates, just to know how long we are interpolating##
#loop for each site?
site_names = unique(allQ$Stream)
date_list = list()
Q_interp = list()

#only want to interpolate sites in Keira's list:
interp_list = subset(NewSiteQInterp_QNames, NewSiteQInterp_QNames$`Flow Interpolation`=="yes")
interp_sites = unique(interp_list$Stream)

for (i in 1:length(interp_sites)){
  
  Q_site = subset(allQ, allQ$Stream==interp_sites[i])
  
  if(anyNA(Q_site$Q==T)){
    missing_Q = subset(Q_site, is.na(Q_site$Q))
    
    length_missing_dates = 
      missing_Q %>% mutate(dummy=c(0,diff(Date))) %>% #creates dummy variable counting days from first obs
      group_by(group = cumsum(dummy != 1)) %>% #groups by continuous date chunks
      summarize(date=first(Date),
                cont_days = n())
    
    
    date_range <- seq(min(Q_site$Date), max(Q_site$Date), by = 1)
    date_site = data.frame(
      site = site_names[i],
      num_missing_days<-length(date_range[!date_range %in% Q_site$Date]),
      missing_days_prop<-num_missing_days/length(date_range),
      max_interp_length = max(length_missing_dates$cont_days)
    )
    
    date_list[[i]] = date_site
    
    #### fill new data frame NA values using na.approx ####
    Q_site_interp = na.approx(Q_site$Q) #if Q column ends in NA, they will remain NA; rule=2 carries the last measured Q value if the values end in NA
    
    Q_interp[[i]] = Q_site_interp 
  }
}

Q_interp_summary = ldply(date_list)
Q_interp_all = ldply(Q_interp)

#### export as new Q.csv file ####
write.csv(Q_interp_all,file="all_site_interp_Q.csv")

