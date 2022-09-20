#interpolate Q data##
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NewSites_Sept2022")

#### read in all Q and Si csv files for new sites ####
Q_files<-list.files(path = ".", pattern = "_Q.csv|_Discharge.csv")

Si_files<-list.files(path = ".", pattern = "_Si.csv")


}

#### remove NA for Q, then check for missing dates ####
## record length of missing dates, just to know how long we are interpolating##

date_range <- seq(min(Q$Date), max(Q$Date), by = 1)
num_missing_days<-length(date_range[!date_range %in% Q$Date])
missing_days_prop<-num_missing_days/length(date_range)

#### check if there is overlap between Si sample and missing Q dates ####
## if yes remove sample, dont want sample where there is interpolated Q ##

#### create new data frame of all dates, including NA ####

#### fill new data frame NA values using na.approx ####

#### export as new Q.csv file ####

