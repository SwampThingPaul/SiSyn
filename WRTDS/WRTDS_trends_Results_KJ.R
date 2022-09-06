### Generates summary data files for WRTDS model run
# For trend, annual, monthly, and daily results this code:
# Reads in csv files for each site, 
# merges all sites into single file and adds site name as a column
# appends LTER name by site
# reads in file with drainage area to calculate yield for each site
# reads out a merged csv for trends, Annual results, monthly results, and daily results

## Load libraries 
library(lubridate)
library(tidyverse)
library(data.table)
library(viridis)
library(reshape2)
library(cowplot)
library(RColorBrewer)

# load and modify data files for analysis and plotting
filedir<-setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_NH4_Results")

filedir <- setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDSResults_N_Updated")

### Loading data files 
file_names <- dir(filedir)

##########################################################################
### EGRETci Trends
## list of trend result files
trend_files <- file_names[file_names %like% "EGRETCi_GFN_Trend.csv"]
trends <- do.call(rbind, lapply(trend_files, function(x) cbind(read.csv(x), name=strsplit(x,'_NH4_EGRETCi_GFN_Trend.csv')[[1]][1])))
trends=trends[,-1]
colnames(trends) = c(colnames(trends[,1:28]),"site")

## REMOVE HBR site WS1 - added calcium silicate!
#trends=filter(trends, site != "ws1")

# calculate yields
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")
site.info=read.csv("SiteChars_06212022.csv")
site.info<-site.info[,-1]
colnames(site.info)=c("LTER","site","Biome",colnames(site.info[,4:ncol(site.info)]))
trends.join=left_join(trends, site.info,by="site")

trends.yield = trends.join %>% 
  dplyr::select(site,LTER,Biome,pValC,estC,lowC95,upC95,likeCUp,likeCDown,
         pValF,estF,lowF,upF,lowF95,upF95,likeFUp,likeFDown,
         baseConc,baseFlux,drainSqKm) %>% 
  mutate(FNYield_change_106_kg_y_km2=estF/drainSqKm, 
         FNYield_change_kg_y_km2=(estF/drainSqKm)*(10^6),
         baseYield=baseFlux/drainSqKm,
         #pConc = (estC/baseConc)*100,
         #pFlux = (estF/baseFlux)*100,
         pYield = (FNYield_change_106_kg_y_km2/baseYield)*100)

trends.yield %>% 
  #filter(LTER != "LUQ") %>% 
  ggplot(aes(x=LTER, pYield, fill=site))+
  geom_bar(stat="identity", position="dodge")+
  #geom_errorbar(aes(ymin=percent-(SD/sqrt(N)), ymax=percent+(SD/sqrt(N))), width=.1)+
  theme_classic()+
  theme(legend.position = "none", axis.text.x = element_text(angle=0, size=10))+
  xlab("")+ylab("")+
  scale_fill_grey()+
  scale_x_discrete(labels = scales::wrap_format(10))

## read out combined results
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_NH4_Results")
write.csv(trends.yield, "NH4_WRTDS_EGRETCi_Trends_AllSites_050422.csv", row.names=FALSE)

###########################################################################
## Annual Result Data
#setwd("C:/Users/kjankowski/Desktop/WRTDSResults_Si")
filedir<-setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_NH4_Results")

#filedir <- setwd("C:/Users/kjankowski/Desktop/WRTDSResults_Si")
filedir <- setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDS_Si_results_GFN")

### Loading data files 
file_names <- dir(filedir)

# list of annual result files
result_files <- file_names[file_names %like% "ResultsTable_WRTDS.csv"]

# combine all into a dataframe and column with site name
df <- do.call(rbind, lapply(result_files, function(x) cbind(read.csv(x), name=strsplit(x,'_NH4_ResultsTable_WRTDS.csv')[[1]][1])))
df=df[,-1]
colnames(df) = c("Year", "Discharge_cms", "Conc_mgL", "FNConc_mgL", "Flux_106_kg_y", "FNFlux_106_kg_y", "site")

## REMOVE HBR site WS1 
#df=filter(df, site != "ws1")

df$LTER = ifelse(df$site %in% c("ALBION","MARTINELLI","SADDLE STREAM 007"),"NWT",
                         ifelse(df$site %in% c("QS","Q1", "Q2","Q3","RI","MPR"), "LUQ", 
                                ifelse(df$site %in% c("ws2","ws3","ws4","ws5","ws6","ws7","ws8","ws9"), "HBR",
                                       ifelse(df$site %in% c("S65","S65A","S65D","S65E"),"KRR",
                                              ifelse(df$site %in% c("GSMACK","GSWS02","GSWS06","GSWS07","GSWS08","GSWS09","GSWS10"),"AND",
                                                     ifelse(df$site %in% c("LMP73"),"LMP",
                                                            ifelse(df$site %in% c("Kolyma","Lena","Mackenzie","Ob","Yenisey","Yukon"),"GRO",
                                                                   ifelse(df$site %in% c("Andersen Creek at H1","Canada Stream at F1",
                                                                                                 "Green Creek at F9", "Lawson Creek at B3",
                                                                                                 "Onyx River at Lake Vanda Weir", "Onyx River at Lower Wright Weir",
                                                                                                 "Priscu Stream at B1","Von Guerard Stream at F6"), "MCM",
                                                                          ifelse(df$site %in% c("Sagehen"),"Sagehen",
                                                                                 ifelse(df$site %in% c("BK01.0M","CH00.1M","CN00.1M","CU11.6M",
                                                                                                               "I080.2M","LM00.5M","M078.0B","M241.4K",
                                                                                                               "M556.4A","M701.1B","M764.3A","M786.2C",
                                                                                                               "MQ02.1M", "SG16.2C","WP02.6M"),"UMR",
                                                                                        ifelse(df$site %in% c("Imnavait Weir"),"ARC","")))))))))))



## plot results for review
df %>% 
ggplot(aes(Year,Conc_mgL,col=site))+
  geom_point()+
  geom_line(aes(Year,FNConc_mgL, col=site))+
  facet_wrap(~LTER,scales="free")+
  theme(legend.position="none")

df %>% 
  ggplot(aes(Year,Flux_106_kg_y,col=site))+
  geom_point()+
  geom_line(aes(Year,FNFlux_106_kg_y, col=site))+
  facet_wrap(~LTER,scales="free")+
  theme(legend.position="none")

# calculate yields
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")
site.info=read.csv("SiteChars_06212022.csv")
site.info<-site.info[,-1]
colnames(site.info)=c("LTER","site","Biome",colnames(site.info[,4:ncol(site.info)]))
df.join=left_join(df, site.info,by=c("LTER","site"))


df.yield = df.join %>% 
  dplyr::select(LTER,site,Year,Discharge_cms,Conc_mgL,FNConc_mgL,Flux_106_kg_y,
         FNFlux_106_kg_y, drainSqKm) %>% 
  mutate(Yield=Flux_106_kg_y/drainSqKm, 
         FNYield = FNFlux_106_kg_y/drainSqKm)

df.yield %>% 
  ggplot(aes(Year,Yield,col=site))+
  geom_point()+
  geom_line(aes(Year,FNYield, col=site))+
  facet_wrap(~LTER,scales="free")+
  theme(legend.position="none")

median.values = aggregate((Yield*(10^6))~site, FUN=median, na.rm=FALSE, data=df.yield)
median.values$Area_km2 = aggregate(drainSqKm~site, FUN=median, na.rm=TRUE, data=df.yield)[,2]
median.values$Q = aggregate(Discharge_cms~site, FUN=median, na.rm=TRUE, data=df.yield)[,2]
median.values$Conc = aggregate(Conc_mgL~site, FUN=median, na.rm=TRUE, data=df.yield)[,2]
colnames(median.values)=c("Site", "Yield_kg_yr_km2", "Area_km2", "Q_cms", "Conc_mgL")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_NH4_Results")
write.csv(median.values, "NH4_Summary_conc_yield_Q.csv", row.names=FALSE)

NWT = df.yield %>% 
  filter(LTER == "HBR") %>% 
  ggplot(aes(Year,FNYield,col=site))+
  geom_line()+
  #geom_line(aes(Year,Yield, col=site))+
  #facet_wrap(~site,scales="free")+
  theme_classic()+
  theme(legend.position="none")

NWT+scale_color_viridis_d(begin=0.2,end=0.4)
  

## read out combined results
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_NH4_Results")
write.csv(df.yield, "NH4_WRTDS_GFN_AnnualResults_AllSites_050422.csv", row.names=FALSE)

#################################3
### Monthly Results

# list of monthly result files
filedir<-setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_NH4_Results")
#filedir <- setwd("C:/Users/kjankowski/Desktop/WRTDSResults_Si")
#filedir <- setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDS_Si_results_GFN")

### Loading data files 
file_names <- dir(filedir)

## select monthly data
month_files <- file_names[file_names %like% "Monthly_WRTDS.csv"]

# combine all into a dataframe and column with site name
months <- do.call(rbind, lapply(month_files, function(x) cbind(read.csv(x), name=strsplit(x,'_NH4_Monthly_WRTDS.csv')[[1]][1])))
months=months[,-c(1,4)]
colnames(months) = c("Month","Year", "DecYear", "Q", "Conc", "FNConc", "Flux", "FNFlux","site")

## REMOVE HBR site WS1 
#months=filter(months, site != "ws1")

months$LTER = ifelse(months$site %in% c("ALBION","MARTINELLI","SADDLE STREAM 007"),"NWT",
                 ifelse(months$site %in% c("QS","Q1","Q2","Q3","RI","MPR"), "LUQ", 
                        ifelse(months$site %in% c("ws2","ws3","ws4","ws5","ws6","ws7","ws8","ws9"), "HBR",
                               ifelse(months$site %in% c("S65","S65A","S65D","S65E"),"KRR",
                                      ifelse(months$site %in% c("GSMACK","GSWS02","GSWS06","GSWS07","GSWS08","GSWS09","GSWS10"),"AND",
                                             ifelse(months$site %in% c("LMP73"),"LMP",
                                                    ifelse(months$site %in% c("Kolyma","Lena","Mackenzie","Ob","Yenisey","Yukon"),"GRO",
                                                           ifelse(months$site %in% c("Andersen Creek at H1","Canada Stream at F1",
                                                                                 "Green Creek at F9", "Lawson Creek at B3",
                                                                                 "Onyx River at Lake Vanda Weir", "Onyx River at Lower Wright Weir",
                                                                                 "Priscu Stream at B1","Von Guerard Stream at F6"), "MCM",
                                                                  ifelse(months$site %in% c("Sagehen"),"Sagehen",
                                                                         ifelse(months$site %in% c("BK01.0M","CH00.1M","CN00.1M","CU11.6M",
                                                                                               "I080.2M","LM00.5M","M078.0B","M241.4K",
                                                                                               "M556.4A","M701.1B","M764.3A","M786.2C",
                                                                                               "MQ02.1M", "SG16.2C","WP02.6M"),"UMR",
                                                                                ifelse(months$site %in% c("Imnavait Weir"),"ARC","")))))))))))



## plot results for review
colourCount = length(unique(months$Month))
getPalette = colorRampPalette(brewer.pal(11, "Spectral"))

# trend by month across years
months %>% 
  ggplot(aes(Year,FNConc,col=as.factor(Month)))+
  geom_line()+
  #geom_line(aes(Year,FNConc, col=as.factor(Month)))+
  facet_wrap(~site,scales="free")+
  theme_classic()+
  theme(legend.position="right")+
  scale_fill_manual(values = getPalette(colourCount))

months %>% 
  filter(site == "BK01.0M") %>% 
  ggplot(aes(Month,FNConc,col=as.factor(Year)))+
  geom_line()+
  #geom_line(aes(Year,FNConc, col=as.factor(Month)))+
  #facet_wrap(~site,scales="free")+
  theme_classic()+
  theme(legend.position="right")+
  scale_fill_manual(values = getPalette(colourCount))+
  labs(colour = "Year")+
  ylab("Flow-normalized Concentration (mg/L)")


months %>% 
  filter(LTER=="HBR") %>% 
  ggplot(aes(Year,FNConc,col=as.factor(Month)))+
  geom_line()+
  #geom_line(aes(Year,FNFlux_106_kg_y, col=site))+
  facet_wrap(~site,scales="free")+
  theme(legend.position="right")+
  scale_fill_manual(values = getPalette(colourCount))

## calculate monthly yields
# calculate yields
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")
site.info=read.csv("SiteChars_06212022.csv")
site.info<-site.info[,-1]
colnames(site.info)=c("LTER","site","Biome",colnames(site.info[,4:ncol(site.info)]))
month.join=left_join(months, site.info,by=c("LTER","site"))

month.yield = month.join %>% 
  dplyr::select(LTER,site,Month,Year,DecYear,Q,Conc,FNConc,Flux,
         FNFlux,drainSqKm) %>% 
  mutate(Yield=Flux/drainSqKm, 
         FNYield = FNFlux/drainSqKm)

month.yield %>% 
  #filter(Month == 1 | Month == 12) %>% 
  ggplot(aes(Year,FNYield,col=as.factor(Month)))+
  geom_line()+
  facet_wrap(~site,scales="free")+
  theme(legend.position="right")

## read out combined results
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_NH4_Results")
write.csv(month.yield, "NH4_WRTDS_GFN_MonthlyResults_AllSites_050422.csv", row.names=FALSE)


############################ Continuous Data
## Continuous Data
# list of monthly result files
filedir<-setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_NH4_Results")

#filedir <- setwd("C:/Users/kjankowski/Desktop/WRTDSResults_Si")
#filedir <- setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDS_Si_results_GFN")

### Loading data files 
file_names <- dir(filedir)
# make list of files
cont_files <-file_names[file_names %like% "NH4_Cont_WRTDS.csv"]

# merge files into one
cont <- do.call(rbind, lapply(cont_files, function(x) cbind(read.csv(x), name=strsplit(x,'_NH4_Cont_WRTDS.csv')[[1]][1])))
cont=cont[,-1]
cont$Date = as.Date(cont$Date)
cont$year = year(cont$Date)
colnames(cont)=c(colnames(cont[,1:16]),"site","year")

#cont$dates <- as.POSIXct(strptime(df.cont$Date, "%Y-%m-%d"))

cont$LTER = ifelse(cont$site %in% c("ALBION","MARTINELLI","SADDLE STREAM 007"),"NWT",
                     ifelse(cont$site %in% c("QS","Q1","Q2","Q3","RI","MPR"), "LUQ", 
                            ifelse(cont$site %in% c("ws1","ws2","ws3","ws4","ws5","ws6","ws7","ws8","ws9"), "HBR",
                                   ifelse(cont$site %in% c("S65","S65A","S65D","S65E"),"KRR",
                                          ifelse(cont$site %in% c("GSMACK","GSWS02","GSWS06","GSWS07","GSWS08","GSWS09","GSWS10"),"AND",
                                                 ifelse(cont$site %in% c("LMP73"),"LMP",
                                                        ifelse(cont$site %in% c("Kolyma","Lena","Mackenzie","Ob","Yenisey","Yukon"),"GRO",
                                                               ifelse(cont$site %in% c("Andersen Creek at H1","Canada Stream at F1",
                                                                                         "Green Creek at F9", "Lawson Creek at B3",
                                                                                         "Onyx River at Lake Vanda Weir", "Onyx River at Lower Wright Weir",
                                                                                         "Priscu Stream at B1","Von Guerard Stream at F6"), "MCM",
                                                                      ifelse(cont$site %in% c("Sagehen"),"Sagehen",
                                                                             ifelse(cont$site %in% c("BK01.0M","CH00.1M","CN00.1M","CU11.6M",
                                                                                                       "I080.2M","LM00.5M","M078.0B","M241.4K",
                                                                                                       "M556.4A","M701.1B","M764.3A","M786.2C",
                                                                                                       "MQ02.1M", "SG16.2C","WP02.6M"),"UMR",
                                                                                    ifelse(cont$site %in% c("Imnavait Weir"),"ARC","")))))))))))




# test
#cont %>% 
 # ggplot(aes(year,ConcDay))+geom_point()+
# geom_smooth(method="loess")

# calculate yields
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")
site.info=read.csv("SiteChars_06212022.csv")
site.info<-site.info[,-1]
colnames(site.info)=c("LTER","site","Biome",colnames(site.info[,4:ncol(site.info)]))
cont.join=left_join(cont, site.info,by=c("LTER","site"))

cont.yield = cont.join %>% 
  select(LTER,site,Date, Month,year, Day,DecYear, Julian,waterYear,i,Q,LogQ,Q7,
         Q30,ConcDay,FluxDay,FNConc,FNFlux,drainSqKm) %>% 
  mutate(Yield=FluxDay/drainSqKm, 
         FNYield = FNFlux/drainSqKm)

# save merged file
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_NH4_Results")
write.csv(cont, "NH4_WRTDS_GFN_DailyResults_AllSites_050422.csv", row.names=FALSE)

