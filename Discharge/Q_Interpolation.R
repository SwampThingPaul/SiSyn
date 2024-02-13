library(zoo)
library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(supportR)

#interpolate Q data##
#setwd("G:/Shared drives/SCWRS/Sethna/SiSyn/Q interp files")

setwd("/Users/keirajohnson/Discharge_Files_Feb2024")

#### read in all Q and Si csv files for new sites ####
Q_files<-list.files(path = ".", pattern = "_Q.csv")

# Si_files<-list.files(path = ".", pattern = "_Si.csv")

DischargeList<-c("MEAN_Q", "Discharge", "InstantQ", "Q_m3sec", "discharge", "Q", "val", "Value", "AVG_DISCHARGE", "valuer", "valeur", "Q_cms", "Flow", "dailyQ")
DateList<-c("Date", "dateTime", "dates", "date", "datetime", "DATE_TIME", "Sampling.Date", "Dates")

#loop to read in csv files; concatenate into one dataframe for Q and Si
Q = list()

for (i in 1:length(Q_files)){
  
  print(i)
  
  siteQ = read.csv(Q_files[i])
  
  names(siteQ)[which(colnames(siteQ) %in% DischargeList)]<-"Q" #convert all Q columns to be called "Q"
  names(siteQ)[which(colnames(siteQ) %in% DateList)]<-"Date" #convert all Date columns to be called "Date"
  
  site_name<-gsub("_Q.csv|_Discharge.csv", "", Q_files[i])
  
  siteQ$site_from_csv<-site_name
  
  siteQ<-siteQ[,c("Date", "Q", "site_from_csv")]
  
  names(siteQ)<-c("Date", "Q", "Site")
  
  #convert date to date format
  if(is.Date(siteQ$Date)){
    
    siteQ$Date<-siteQ$Date
    siteQ$format_guess<-NA
    
  } else{
    
    siteQ<-date_format_guess(siteQ,"Date", groups = T, group_col = "Site")
    
  }
  
  Q[[i]] = siteQ
}

allQ = ldply(Q, data.frame)

unique(allQ$format_guess)

allQ<-allQ[!c(allQ$format_guess=="year/NA"),]

allQ$date_format<-ifelse(allQ$format_guess=="year/month/day"|allQ$format_guess=="year/NA"|allQ$format_guess=="year/day/month", "%Y-%m-%d",
                          ifelse(allQ$format_guess=="month/day/year", "%m/%d/%Y", "none"))

allQ$Date_stand<-as.Date(allQ$Date, format = allQ$date_format)

allQ_date1<-subset(allQ, allQ$format_guess=="month/day/year")

date_missing<-allQ[which(is.na(allQ$Date_stand)),]

allQ<-allQ[complete.cases(allQ$Q),]

write.csv(allQ, "allQ021324.csv") ##export to fix date column

allQ<-read.csv("allQ021324.csv")

#####start here if there is a WRTDS input file #####

#read in WRTDS input file here
allQ<-read.csv("allQ021324.csv")

site_names = unique(allQ$Site)
#date_list = list()
Q_interp = list()

for (i in 1:length(site_names)){
  
  print(i)
  
  #pull out one site
  Q_site = subset(allQ, allQ$Site==site_names[i])
  
  Q_site<-Q_site[,c("Date_stand", "Q", "Site")]
  
  #remove all NA from Q
  Q_site<-Q_site[complete.cases(Q_site$Q),]
  
  Q_site$Date<-as.Date(Q_site$Date_stand)
  
  #determine if missing data by comparing complete 
  #date range from min to max date to all dates in date columns
  date_range <- seq(from=min(Q_site$Date), to=max(Q_site$Date), by = 1) 
  num_missing_days<-length(date_range[!date_range %in% Q_site$Date])
  
  #if no missing dates, skip rest of loop
  if(num_missing_days==0){
    
    Q_site$indicate<-"measured"
    
    Q_interp[[i]] = Q_site 
    
  } else{
    
    print(site_names[i])
    
    #create new dataframe with date range as dates
    alldates<-as.data.frame(date_range)
    colnames(alldates)<-"Date"
    alldates<-merge(alldates, Q_site, by="Date", all.x=TRUE)
    alldates$indicate<-ifelse(is.na(alldates$Q), "interpolated", "measured")
    
    alldates$Site<-site_names[i]
    
    #### fill new data frame NA values using na.approx ####
    Q_site_interp = alldates
    Q_site_interp$Q<-na.approx(Q_site_interp$Q) #if Q column ends in NA, they will remain NA; rule=2 carries the last measured Q value if the values end in NA
    
    Q_interp[[i]] = Q_site_interp 
    
  }
  
}

#Q_interp_summary = ldply(date_list)
Q_interp_all = do.call(rbind, Q_interp)

#### export as new Q.csv file ####
write.csv(Q_interp_all,file="all_site_Qinterp.csv")

#interp<-read.csv("all_site_Qinterp.csv")

#look at number of days with interpolation
df2<-Q_interp_all %>% group_by(Site) %>%  
  summarise(max=max(rle(indicate)$lengths[rle(indicate)$values=="interpolated"])) %>%
  filter(max!="-Inf")

#filter to only interpolated sites
interp_sites<-Q_interp_all %>%
  group_by(Site) %>%
  filter(length(unique(indicate))==2)

#make sure everything is formatted properly
interp_sites<-as.data.frame(interp_sites)
interp_sites$Q<-as.numeric(interp_sites$Q)

interp_sites_indv<-unique(interp_sites$Site)

#make pdf to visualize where sites are interpolated
pdf("Interpolated_Sites_Fib2024.pdf")

for (i in 1:length(interp_sites_indv)) {

  print(i)
  
  one_site<-subset(interp_sites, interp_sites$Site==interp_sites_indv[i])
  
  p1<-ggplot(one_site, aes(Date, Q))+geom_line(aes(col=indicate))+
    theme_bw()+ggtitle(interp_sites_indv[i])
    
  print(p1)
}

dev.off()

# ggplot(interp_sites, aes(Date, Q))+geom_point(aes(col=indicate))+
#   theme_bw()+facet_wrap(~Site, scales = "free")
# 
# setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")
# 
# NWQA_dat<-read.csv("NWQA_Discharge_10112022.csv")
# 
# head(NWQA_dat)
# unique(NWQA_dat$Stream)
# 
# remove_these_sites<-c("Wild River", "GREEN RIVER", "VALLECITO CREEK",
#                       "SAGEHEN C", "WEST CLEAR", "North Sylamore", "SOPCHOPPY RIVER")
# 
# NWQA_dat_remove <- NWQA_dat %>%
#   filter(Stream %in% remove_these_sites)
# 
# NWQA_dat_clean<-setdiff(NWQA_dat, NWQA_dat_remove)
# 
# NWQA_sites<-unique(NWQA_dat_clean$Stream)
# 
# NWQA_dat_clean$Date<-as.Date(NWQA_dat_clean$Date)
# 
# NWQA_Q_interp<-list()
# 
# for (i in 1:length(NWQA_sites)){
#   
#   #pull out one site
#   Q_site = subset(NWQA_dat_clean, NWQA_dat_clean$Stream==NWQA_sites[i])
#   
#   Q_site<-Q_site[,c("Date", "Discharge", "Stream")]
#   
#   names(Q_site)<-c("Date", "Q", "Site")
#   
#   #remove all NA from Q
#   Q_site<-Q_site[complete.cases(Q_site$Q),]
#   
#   #determine if missing data by comparing complete 
#   #date range from min to max date to all dates in date columns
#   date_range <- seq(min(Q_site$Date), max(Q_site$Date), by = 1) 
#   num_missing_days<-length(date_range[!date_range %in% Q_site$Date])
#   
#   #if no missing dates, skip rest of loop
#   if(num_missing_days==0){
#     
#     Q_site$indicate<-"measured"
#     
#     NWQA_Q_interp[[i]] = Q_site 
#     
#   } else{
#     
#     print(NWQA_sites[i])
#     
#     #create new dataframe with date range as dates
#     alldates<-as.data.frame(date_range)
#     colnames(alldates)<-"Date"
#     alldates<-merge(alldates, Q_site, by="Date", all.x=TRUE)
#     alldates$indicate<-ifelse(is.na(alldates$Q), "interpolated", "measured")
#     
#     alldates$Site<-NWQA_sites[i]
#     
#     
#     # if(anyNA(Q_site$Q==T)){
#     #   missing_Q = subset(Q_site, is.na(Q_site$Q))
#     # #   
#     #   length_missing_dates =
#     #     missing_Q %>% mutate(dummy=c(0,diff(Date))) %>% #creates dummy variable counting days from first obs
#     #     group_by(group = cumsum(dummy != 1)) %>% #groups by continuous date chunks
#     #     summarize(date=first(Date),
#     #               cont_days = n())
#     # #   
#     # #   
#     # #   date_range <- seq(min(Q_site$Date), max(Q_site$Date), by = 1)
#     #   date_site = data.frame(
#     #     site = site_names[i],
#     #     num_missing_days<-length(date_range[!date_range %in% Q_site$Date]),
#     #     missing_days_prop<-num_missing_days/length(date_range),
#     #     max_interp_length = max(length_missing_dates$cont_days)
#     #   )
#     #   
#     #   date_list[[i]] = date_site
#     
#     #### fill new data frame NA values using na.approx ####
#     Q_site_interp = alldates
#     Q_site_interp$Q<-na.approx(Q_site_interp$Q) #if Q column ends in NA, they will remain NA; rule=2 carries the last measured Q value if the values end in NA
#     
#     NWQA_Q_interp[[i]] = Q_site_interp 
#     
#   }
#   
#   #}
# }
# 
# 
# NWQA_interpolated<-do.call(rbind, NWQA_Q_interp)
# 
# write.csv(NWQA_interpolated, "NWQA_Qinterp.csv")
# 
# NWQA_interp<-read.csv("NWQA_Qinterp.csv")
# 
# NWQA_interp_sites<-unique(NWQA_interp$Site)
# 
# NWQA_interp$continuous<-ifelse(NWQA_interp$indicate)
# 
# df<-NWQA_interp %>% group_by(Site) %>%  
#   summarise(max=max(rle(indicate)$lengths[rle(indicate)$values=="interpolated"]))
# 
# NWQA_interp %>%
#   filter(Site=="PICEANCE CREEK AT WHITE RIVER") %>%
#   ggplot(aes(as.Date(Date), Q))+geom_line()
# 
# df<-NWQA_interp %>% group_by(Site) %>%
#   filter(indicate=="interpolated")
# 
# interp_final<-bind_rows(df, df2)
# 
# interp_final$Month<-month(interp_final$Date)
# interp_final$Year<-year(interp_final$Date)
# 
# write.csv(interp_final, "interp_final.csv")
# 
# setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NewSites_Sept2022/IndividualQFiles")
# 
# NWQA_interp %>%
#   group_by(Site) %>%
#   group_walk(~ write.csv(.x, paste0(gsub(" ","_", .y$Site), "_Q.csv")))
# 
# 
# #newSiteinterp<-read.csv("all_site_Qinterp.csv")
# newSiteinterp<-Q_interp_all
# 
# newSiteinterp %>%
#   group_by(Site) %>%
#   group_walk(~ write.csv(.x, paste0(gsub(" ","_", .y$Site), "_Q.csv")))
# 
# setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NewSites_Sept2022")
# interp<-read.csv("all_site_Qinterp.csv")
# 
# setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")
# 
# NWQA_interp<-read.csv("NWQA_Qinterp.csv")
# 
# interp_tot<-bind_rows(interp, NWQA_interp)
# 
# monthly_interp<-read.csv("interp_monthly.csv")
# 
# ggplot(monthly_interp)+geom_point(aes(as.Date(Date), Q))+
#   facet_wrap(~stream, scales = "free")+theme_bw()
# 
# BP<-monthly_interp %>%
#   filter(stream=="BOGUE PHALIA")
# 
# 
# monthly_interp_summary<-read.csv("interp_monthly_summary.csv")
# 
# interp_tot<-subset(interp_tot, interp_tot$Site %in% monthly_interp$stream)
# 
# colnames(monthly_interp)[1]<-"Site"
# 
# ggplot()+geom_point(interp_tot, mapping = aes(as.Date(Date), Q, col=indicate), size=0.2)+
#   facet_wrap(~Site, scales = "free")+theme_bw()
# 
# yukon<-monthly_interp %>%
#   filter(stream == "YUKON RIVER")
# 
# yukon<-monthly_interp %>%
#   group_by(stream) %>%
#   summarise(min_date=min(Date), max_date=max(Date))
# 
# 
