library(zoo)
library(plyr)
library(dplyr)

#interpolate Q data##
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NewSites_Sept2022")

#### read in all Q and Si csv files for new sites ####
Q_files<-list.files(path = ".", pattern = "_Q.csv|_Discharge.csv")

Si_files<-list.files(path = ".", pattern = "_Si.csv")

#loop to read in csv files; concatenate into one dataframe for Q and Si
Q = list()

for (i in 1:length(Q_files)){
  siteQ = read.csv(Q_files[i])
  Q[[i]] = siteQ
}

allQ = ldply(Q, data.frame)

Si = list()

for (i in 1:length(Si_files)){
  siteSi = read.csv(Si_files[j])
  Si[[i]] = siteSi
}

allSi = ldply(Si, data.frame)

#### remove NA for Q, then check for missing dates ####
allQ_noNA = na.omit(allQ)

#### check if there is overlap between Si sample and missing Q dates ####
## if yes remove sample, dont want sample where there is interpolated Q ##
#merge all C-Q data, remove Si data not in Q
Si_Q_all = merge(allSi, allQ_noNA) #This should remove any Si values where Q is NA

## record length of missing dates, just to know how long we are interpolating##
#loop for each site?
site_names = unique(Si_Q_all$site)
date_list = list()
Q_interp = list()

for (i in 1:length(site_names)){
  
  Q_site = subset(allQ, allQ$site=site_names[i])
  
  missing_Q = subset(Q_site, is.na(Q_site$Q))
  
  length_missing_dates = 
  missing_Q %>% mutate(dummy=c(0,diff(date))) %>% #creates dummy variable counting days from first obs
    group_by(group = cumsum(dummy != 1)) %>% #groups by continuous date chunks
    summarize(date=first(date),
              cont_days = n())
    
  
  date_range <- seq(min(Q_site$date), max(Q_site$date), by = 1)
  date_site = data.frame(
    site = site_names[i],
    num_missing_days<-length(date_range[!date_range %in% Q_site$date]),
    missing_days_prop<-num_missing_days/length(date_range),
    max_interp_length = max(length_missing_dates$cont_days)
  )
  
  date_list[[i]] = date_site
  
  #### fill new data frame NA values using na.approx ####
  Q_site_interp = na.approx(Q_site$Q) #if Q column ends in NA, they will remain NA; rule=2 carries the last measured Q value if the values end in NA
  
  Q_interp[[i]] = Q_site_interp
}

Q_interp_summary = ldply(date_list)
Q_interp_all = ldply(Q_interp)

#### export as new Q.csv file ####
write.csv(Q_interp_all,file="all_site_interp_Q.csv")

