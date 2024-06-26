require(googledrive)
require(stringr)
require(lubridate)
require(reshape)
require(gtools)
require(dplyr)
require(data.table)
require(dataRetrieval)

#get folder URL from google drive with WRTDS output
WRTDS_output_URL = "https://drive.google.com/drive/u/0/folders/1e9GEr2GSHXagg6sAK4a7e47dKE6wghR9"
#get ID of folder
WRTDS_output_ID = drive_get(as_id(WRTDS_output_URL))
#get list of csv files in WRTDS output folder
WRTDS_csvs = drive_ls(WRTDS_output_ID, type="csv")
#extract only "Cont Si" files with daily Si load estimates
WRTDS_csvs_SiEsts = WRTDS_csvs[WRTDS_csvs$name %like% "ContSi_WRTDS.csv",]
#create new column with site name
WRTDS_csvs_SiEsts$site = substr(WRTDS_csvs_SiEsts$name, start=1, stop=nchar(WRTDS_csvs_SiEsts$name)-16)

#repeat for Loadflex output files
Loadflex_output_URL = "https://drive.google.com/drive/u/0/folders/1f2Bwpr0R8eTm6VZTjuB7THZLIy-ogaNC"
Loadflex_output_ID = drive_get(as_id(Loadflex_output_URL))
Loadflex_csvs = drive_ls(Loadflex_output_ID, type="csv")
Loadflex_csvs_SiEst = Loadflex_csvs
Loadflex_csvs_SiEst$site = word(Loadflex_csvs_SiEst$name, 1, sep="_")

#merge WRTDS and Loadflex files by site name
compare_csvs = merge(WRTDS_csvs_SiEsts, Loadflex_csvs_SiEst, by="site")

#download WRTDS csv files to local drive
getwd()
#"L:/GitHub/SiSyn/Loadflex/Loadflex-WRTDS compare"
setwd("L:/GitHub/SiSyn/Loadflex/Loadflex-WRTDS compare/WRTDS results")
for (i in 1:length(WRTDS_csvs_SiEsts$drive_resource)) {
  drive_download(WRTDS_csvs_SiEsts$drive_resource[i],  overwrite=T)
}

WRTDS_files = list.files(path="L:/GitHub/SiSyn/Loadflex/Loadflex-WRTDS compare/WRTDS results")

#create list to store output of loop
WRTDS_output_list = list()

#loop through all WRTDS output files and save date, site name, and daily load estimate
for (i in 1:length(WRTDS_files)) {
  site = substr(WRTDS_files[i], start=1, stop=nchar(WRTDS_files[i])-16)
  
  file_name = WRTDS_files[i]
  #Keep only date, Q, and daily flux
  d = fread(file_name,select=c(2,3,17))
  d$site = site
  
  WRTDS_output_list[[i]] = d
}

#repeat for Loadflex files
setwd("L:/GitHub/SiSyn/Loadflex/Loadflex-WRTDS compare/Loadflex results")
for (i in 1:length(Loadflex_csvs_SiEst$drive_resource)) {
  drive_download(Loadflex_csvs_SiEst$drive_resource[i],  overwrite=T)
}
Loadflex_files = list.files(path="L:/GitHub/SiSyn/Loadflex/Loadflex-WRTDS compare/Loadflex results")
Loadflex_output_list = list()
for (i in 1:length(Loadflex_files)) {
  site = word(Loadflex_files[i], 1, sep="_")
  file_name = Loadflex_files[i]
  d = fread(file_name,select=c(2,3))
  d$site = site
  Loadflex_output_list[[i]] = d
}

#expand WRTDS and Loadflex lists into dataframes
library(plyr)
WRTDS_allsites = ldply(WRTDS_output_list)
Loadflex_allsites = ldply(Loadflex_output_list)

head(WRTDS_allsites)
head(Loadflex_allsites)

#rename columns
names(WRTDS_allsites)[names(WRTDS_allsites) == "FluxDay"] = "WRTDS_DailyFlux"
names(Loadflex_allsites)[names(Loadflex_allsites) == "Flux_Rate"] = "Loadflex_DailyFlux"
names(Loadflex_allsites)[names(Loadflex_allsites) == "Day"] = "Date"

#merge WRTDS and Loadflex results
#keep all results from both models (all=T)
WRTDS_Loadflex_compare = merge(WRTDS_allsites, Loadflex_allsites, by=c("site","Date"),all=T)
WRTDS_Loadflex_compare = merge(WRTDS_allsites, Loadflex_repeat, by=c("site","Date"),all=T)

setwd("L:/GitHub/SiSyn/Loadflex/Loadflex-WRTDS compare")
write.csv(WRTDS_Loadflex_compare, file="WRTDS-Loadflex_compare_11Jan21.csv")
write.csv(Loadflex_allsites, file="Loadflex_DailySi_allsites.csv")

#add in second Loadflex run 1/11/21
Loadflex_repeat = data.frame("site" = Loadflex_DailySi_allsites$Site,
                             "Date" = Loadflex_DailySi_allsites$Date,
                             "Loadflex_DailyFlux_2" = Loadflex_DailySi_allsites$SiLoad_kg.d)
WRTDS_Loadflex_compare2 = merge(WRTDS_Loadflex_compare, Loadflex_repeat, by=c("site","Date"), all=T)
write.csv(WRTDS_Loadflex_compare2, file="WRTDS-Loadflex compare_11Jan21.csv")

#plot WRTDS v. Loadflex output
library(ggplot2)
ggplot(WRTDS_Loadflex_compare, aes(x=Loadflex_DailyFlux, y=WRTDS_DailyFlux)) +
  geom_line() +
  facet_wrap(~site,scales="free")

ggplot(WRTDS_Loadflex_compare, aes(x=Loadflex_DailyFlux, y=WRTDS_DailyFlux)) +
  geom_smooth(model=lm) +
  facet_wrap(~site,scales="free")

#plot Loadflex 1 v. Loadflex 2
ggplot(WRTDS_Loadflex_compare2, aes(x=Loadflex_DailyFlux, y=Loadflex_DailyFlux_2)) +
  geom_line() +
  facet_wrap(~site, scales="free")

#plot WRTDS v. Loadflex
#image aspect ratio 1100x850
ggplot(WRTDS_Loadflex_compare, aes(x=WRTDS_DailyFlux, y=Loadflex_DailyFlux_2)) +
  geom_line() +
  facet_wrap(~site, scales="free")
ggplot(WRTDS_Loadflex_compare, aes(x=WRTDS_DailyFlux, y=Loadflex_DailyFlux_2)) +
  geom_smooth(model=lm) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red")+
  xlab("WRTDS model output")+
  ylab("Loadflex model output")+
  facet_wrap(~site, scales="free")+
  theme_minimal()

#subtract Loadflex-WRTDS, compare results to 0 over Q
WRTDS_Loadflex_compare$WRTDSsubLoadflex = WRTDS_Loadflex_compare$WRTDS_DailyFlux - WRTDS_Loadflex_compare$Loadflex_DailyFlux_2
ggplot(WRTDS_Loadflex_compare, aes(x=Q, y=WRTDSsubLoadflex)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  xlab("Discharge (cms)")+
  ylab("WRTDS - Loadflex: modeled Si load (kg/d)")+
  facet_wrap(~site, scales="free")+
  theme_minimal()
ggplot(WRTDS_Loadflex_compare, aes(x=Date, y=WRTDSsubLoadflex)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  xlab("Year")+
  scale_x_date(date_labels="%y")+
  ylab("WRTDS - Loadflex: modeled Si load (kg/d)")+
  facet_wrap(~site, scales="free")+
  theme_minimal()
  
#plot difference between models by month and season
library(lubridate)
WRTDS_Loadflex_compare$month = month(WRTDS_Loadflex_compare$Date)
library(hydroTSM)
WRTDS_Loadflex_compare$season = time2season(WRTDS_Loadflex_compare$Date, out.fmt="seasons")

ggplot(WRTDS_Loadflex_compare, aes(x=month, y=WRTDSsubLoadflex)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_x_continuous(name="Month",breaks=c(1:12),labels=c(1:12))+
  ylab("WRTDS - Loadflex: modeled Si load (kg/d)")+
  facet_wrap(~site, scales="free")+
  theme_minimal()

WRTDS_Loadflex_compare$season = factor(WRTDS_Loadflex_compare$season, ordered=T, levels=c("winter","spring","summer","autumm"))
levels(WRTDS_Loadflex_compare$season)
ggplot(WRTDS_Loadflex_compare, aes(x=season, y=WRTDSsubLoadflex)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_x_discrete(name="Season", labels=c("W","Sp","Su","F"))+
  ylab("WRTDS - Loadflex: modeled Si load (kg/d)")+
  facet_wrap(~site, scales="free")+
  theme_minimal()
  