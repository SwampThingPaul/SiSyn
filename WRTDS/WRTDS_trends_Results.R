### title: "SiSyn_WRTDSResults"

## Load libraries 
library(lubridate)
library(tidyverse)
library(data.table)
library(viridis)
library(reshape2)
library(hrbrthemes)
library(cowplot)

# load and modify data files for analysis and plotting
#setwd("C:/Users/kjankowski/Desktop/WRTDSResults_Si")
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDS_Si_results_GFN")

#filedir <- setwd("C:/Users/kjankowski/Desktop/WRTDSResults_Si")
filedir <- setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDS_Si_results_GFN")

### Loading data files 
file_names <- dir(filedir)

# list of GFN results
#GFN_files <- file_names[file_names %like% "GFN_WRTDS.csv"]

##########################################################################
### EGRETci Trends
## list of trend result files
trend_files <- file_names[file_names %like% "EGRETCi_GFN_Trend.csv"]
trends <- do.call(rbind, lapply(trend_files, function(x) cbind(read.csv(x), name=strsplit(x,'_Si_EGRETCi_GFN_Trend.csv')[[1]][1])))
trends=trends[,-1]
colnames(trends) = c(colnames(trends[,1:28]),"site")

# calculate yields
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Data/BasinInfo")
site.info=read.csv("SiteCharacteristics_PCA_WRTDS_122021.csv")
colnames(site.info)=c("LTER","site",colnames(site.info[,3:ncol(site.info)]))
trends.join=left_join(trends, site.info,by="site")

trends.yield = trends.join %>% 
  select(site,LTER,Biome2,pValC,estC,lowC95,upC95,likeCUp,likeCDown,
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
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/Trend Summaries")
write.csv(trends.yield, "WRTDS_EGRETCi_Trends_AllSites_031122.csv", row.names=FALSE)

###########################################################################
## Annual Result Data

# list of annual result files
result_files <- file_names[file_names %like% "ResultsTable_GFN_WRTDS.csv"]

# combine all into a dataframe and column with site name
df <- do.call(rbind, lapply(result_files, function(x) cbind(read.csv(x), name=strsplit(x,'_ResultsTable_GFN_WRTDS.csv')[[1]][1])))
df=df[,-1]
colnames(df) = c("Year", "Discharge_cms", "Conc_mgL", "FNConc_mgL", "Flux_106_kg_y", "FNFlux_106_kg_y", "site")


df$LTER = ifelse(df$site %in% c("ALBION","MARTINELLI","SADDLE STREAM 007"),"NWT",
                         ifelse(df$site %in% c("QS","Q1", "Q2","Q3","RI","MPR"), "LUQ", 
                                ifelse(df$site %in% c("ws1","ws2","ws3","ws4","ws5","ws6","ws7","ws8","ws9"), "HBR",
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
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Data/BasinInfo")
site.info=read.csv("SiteCharacteristics_PCA_WRTDS_122021.csv")
colnames(site.info)=c("LTER","site",colnames(site.info[,3:ncol(site.info)]))
df.join=left_join(df, site.info,by=c("LTER","site"))

df.yield = df.join %>% 
  select(LTER,site,Year,Discharge_cms,Conc_mgL,FNConc_mgL,Flux_106_kg_y,
         FNFlux_106_kg_y, drainSqKm) %>% 
  mutate(Yield=Flux_106_kg_y/drainSqKm, 
         FNYield = FNFlux_106_kg_y/drainSqKm)

df.yield %>% 
  ggplot(aes(Year,Yield,col=site))+
  geom_point()+
  geom_line(aes(Year,FNYield, col=site))+
  facet_wrap(~LTER,scales="free")+
  theme(legend.position="none")

## read out combined results
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS")
write.csv(df.yield, "WRTDS_GFN_AnnualResults_AllSites_030922.csv", row.names=FALSE)

#################################3
### Monthly Results

# list of monthly result files
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDS_Si_results_GFN")

#filedir <- setwd("C:/Users/kjankowski/Desktop/WRTDSResults_Si")
filedir <- setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDS_Si_results_GFN")

### Loading data files 
file_names <- dir(filedir)

## select monthly data
month_files <- file_names[file_names %like% "Monthly_GFN_WRTDS.csv"]

# combine all into a dataframe and column with site name
months <- do.call(rbind, lapply(month_files, function(x) cbind(read.csv(x), name=strsplit(x,'_Monthly_GFN_WRTDS.csv')[[1]][1])))
months=months[,-1]
colnames(months) = c("Month","Year", "DecYear", "Q", "Conc", "FNConc", "Flux", "FNFlux","site")


months$LTER = ifelse(months$site %in% c("ALBION","MARTINELLI","SADDLE STREAM 007"),"NWT",
                 ifelse(months$site %in% c("QS","Q1","Q2","Q3","RI","MPR"), "LUQ", 
                        ifelse(months$site %in% c("ws1","ws2","ws3","ws4","ws5","ws6","ws7","ws8","ws9"), "HBR",
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
  filter(site == "BK01.0M") %>% 
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
  filter(LTER=="NWT", Month >4 & Month <11) %>% 
  ggplot(aes(Year,FNFlux,col=as.factor(Month)))+
  geom_line()+
  #geom_line(aes(Year,FNFlux_106_kg_y, col=site))+
  facet_wrap(~site,scales="free")+
  theme(legend.position="right")+
  scale_fill_manual(values = getPalette(colourCount))

## calculate monthly yields
# calculate yields
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Data/BasinInfo")
site.info=read.csv("SiteCharacteristics_PCA_WRTDS_122021.csv")
colnames(site.info)=c("LTER","site",colnames(site.info[,3:ncol(site.info)]))
month.join=left_join(months, site.info,by=c("LTER","site"))

month.yield = month.join %>% 
  select(LTER,site,Month,Year,DecYear,Q,Conc,FNConc,Flux,
         FNFlux,drainSqKm) %>% 
  mutate(Yield=Flux/drainSqKm, 
         FNYield = FNFlux/drainSqKm)

month.yield %>% 
  filter(LTER == "LUQ") %>% 
  #filter(Month == 1 | Month == 12) %>% 
  ggplot(aes(Year,FNYield,col=as.factor(Month)))+
  geom_line()+
  facet_wrap(~site,scales="free")+
  theme(legend.position="right")

## read out combined results
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS")
write.csv(month.yield, "WRTDS_GFN_MonthlyResults_AllSites_030922.csv", row.names=FALSE)


############################ Continuous Data
## Continuous Data
# list of monthly result files
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDS_Si_results_GFN")

#filedir <- setwd("C:/Users/kjankowski/Desktop/WRTDSResults_Si")
filedir <- setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDS_Si_results_GFN")

### Loading data files 
file_names <- dir(filedir)
# make list of files
cont_files <-file_names[file_names %like% "ContSi_GFN_WRTDS.csv"]

# merge files into one
cont <- do.call(rbind, lapply(cont_files, function(x) cbind(read.csv(x), name=strsplit(x,'_ContSi_GFN_WRTDS.csv')[[1]][1])))
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
cont %>% 
  filter(site == "WP02.6M") %>% 
  ggplot(aes(year,ConcDay))+geom_point()+
  geom_smooth(method="loess")

# calculate yields
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Data/BasinInfo")
site.info=read.csv("SiteCharacteristics_PCA_WRTDS_122021.csv")
colnames(site.info)=c("LTER","site",colnames(site.info[,3:ncol(site.info)]))

cont.join=left_join(cont, site.info,by=c("LTER","site"))

cont.yield = cont.join %>% 
  select(LTER,site,Date, Month,year, Day,DecYear, Julian,waterYear,i,Q,LogQ,Q7,
         Q30,ConcDay,FluxDay,FNConc,FNFlux,drainSqKm) %>% 
  mutate(Yield=FluxDay/drainSqKm, 
         FNYield = FNFlux/drainSqKm)

# save merged file
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS")
write.csv(cont, "WRTDS_GFN_DailyResults_AllSites_030922.csv", row.names=FALSE)

##################################################################################
# quantify length of records
duration <- function(x){
  (max(x)-min(x))+1
}

n.years=aggregate(Year~site, FUN=duration, data=df)
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS")
write.csv(n.years, "PeriodofRecord_WRTDS.csv", row.names=FALSE)


##################################################################
## Trend Data
# importing trend files
# df <- do.call(rbind, lapply(result_files, function(x) cbind(read.csv(x), name=strsplit(x,'ResultsTable_WRTDS.csv')[[1]][1])))
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDSResults")

trends=do.call(rbind,lapply(trend_files,read.csv))

# naming rows by site
row.names(trends) = strsplit(trend_files, "_Si_EGRETCi_Trend.csv")
trends$site = row.names(trends)

# removing column "X"
trends = trends[,-1]

# merge trends with period of record to calculate slopes
slopes=merge(trends,n.years,by="site")
slopes$slopeC=slopes$estC/slopes$Year
slopes$slopeF=slopes$estF/slopes$Year

slopes1 = slopes %>% 
  select(site,estC,lowC95,upC95,likeCUp,likeCDown,estF,lowF95,upF95,likeFUp,likeFDown,slopeC,slopeF)

# create likelihood column
slopes1$likeC = ifelse(slopes1$likeCUp>=slopes1$likeCDown,slopes1$likeCUp,slopes1$likeCDown)
slopes1$likeF = ifelse(slopes1$likeFUp>=slopes1$likeFDown,slopes1$likeFUp,slopes1$likeFDown)

# significant C
slopes1$sigC <- with(slopes1, ifelse(
  likeC >= 0.67 & likeC <0.9 , 'likely', ifelse(
    likeC >= 0.9 & likeC < 0.95, 'very', ifelse(
      likeC >= 0.95 & likeC <= 1, 'highly', ""))))
  
# significant F
slopes1$sigF <- with(slopes1, ifelse(
  likeF >= 0.67 & likeF <0.9 , 'likely', ifelse(
    likeF >= 0.9 & likeF < 0.95, 'very', ifelse(
      likeF >= 0.95 & likeF <= 1, 'highly', ""))))

# assign LTER
(df$commute %in% c("walk", "bike", "subway", "ferry")) &
  
slopes1$LTER = ifelse(slopes1$site %in% c("ALBION","MARTINELLI","SADDLE STREAM 007"),"NWT","")

# save merged trend file to add LTER name 
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS")
write.csv(slopes1, "Si_EGRETCi_Trends_slopes_090721.csv", row.names=FALSE)

dat=read.csv("Si_EGRETCi_Trends_slopes_090721.csv")

ggplot(dat, aes(x=site,y=slopeC,fill=sigC))+
  geom_bar(stat="identity")+
  facet_wrap(~LTER, scales="free")+
  theme_minimal()


################## Trend analysis and plotting 
### Merge with Mann-Kendall results
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results")
mk=read.csv("MK_Test_AllSites.csv")

trends.egret = trends %>% select(site,estC,likeCUp,likeCDown,estF,likeFUp,likeFDown)
  
all.trends = left_join(mk,trends.egret,by="site")
write.csv(all.trends, "AllTrends_082621.csv", row.names=FALSE)

# order by pct_change
trends$site = row.names(trends)
trends1 = trends[order(trends$estC),]

# concentration change
par(mar=c(11,4,1,1))
barplot(height=trends1$estC, 
        names.arg=as.factor(trends1$site), las=2, xlab="", ylab="Concentration Change (mg/L)")

# flux change
# reorder by flux
trends2 = trends[order(trends$estF),]
barplot(height=trends2$estF, names.arg=as.factor(trends2$site), 
        las=2, xlab="", ylab="Flux Change", ylim=c(-100,100))

### Filtering significant sites
#Conc
sig = filter(trends, likeCUp >=0.9 | likeCDown > 0.9)
sig$site = row.names(sig)
sig1 = sig[order(sig$estC),]
barplot(height=sig1$estC, 
        names.arg=as.factor(sig1$site), las=2, xlab="", ylab="Concentration Change (mg/L)")

# Flux
sigF = filter(trends, likeFUp >=0.9 | likeFDown > 0.9)
sigF$site = row.names(sigF)
sigF1 = sigF[order(sigF$estF),]

# concentration change
barplot(height=sigF1$estF, 
        names.arg=as.factor(sigF1$site), las=2, xlab="", ylab="Flux Change")

#######################################################################
#### plot trends by LTER
wrtds_trends <- read_csv("Si_EGRETCi_Trends_082621.csv")
wrtds_por <- read_csv("WRTDS_Streams_PeriodOfRecord.csv") 
colnames(wrtds_por)=c("Site","Year.1", "Year.2","duration")

wrtds_trends <- inner_join(wrtds_trends, wrtds_por, by="Site")

## Calculate slopes
wrtds_trends$SlopeC <- wrtds_trends$estC/wrtds_trends$duration
wrtds_trends$SlopeF <- wrtds_trends$estF/wrtds_trends$duration

# didn't use
wrtds_trends_trimmed <- wrtds_trends %>% 
  filter(lowC95>-50) %>% 
  filter(upC95<50)

# did it this way rather than removing to at least show them in the plots
wrtds_trends$lowC95[wrtds_trends$lowC95 < -50] <- NA
wrtds_trends$upC95[wrtds_trends$upC95 > 50] <- NA

## Patrick's plots
## plots of slopes
wrtds_trends %>% 
  filter(LTER != "ARC") %>% 
  ggplot(aes(Site, SlopeC, col=as.factor(sigC)))+
  geom_point()+
  #geom_linerange(aes(ymin = lowC95, ymax = upC95))+
  coord_flip()+
  geom_hline(yintercept = 0)+
  scale_x_discrete(limits = rev)+
  facet_wrap(~LTER, scales="free")+
  theme(legend.position = "right")+
  ggsave("slopeC.png", width=10, height=6, units="in")

wrtds_trends %>% 
  filter(LTER != "ARC") %>% 
  ggplot(aes(Site, SlopeF, col=as.factor(sigF))) +
  geom_point()+
  #geom_linerange(aes(ymin = lowC95, ymax = upC95))+
  coord_flip()+
  geom_hline(yintercept = 0)+
  scale_x_discrete(limits = rev)+
  facet_wrap(~LTER, scales="free")+
  theme(legend.position = "right")+
  ggsave("slopeF.png", width=10, height=6, units="in")

## plots of magnitude of change (with CI)
wrtds_trends %>% 
  filter(LTER != "ARC") %>% 
  ggplot(aes(Site, estC, col=as.factor(sigC)))+
  geom_point()+
  geom_linerange(aes(ymin = lowC95, ymax = upC95))+
  coord_flip()+
  geom_hline(yintercept = 0)+
  scale_x_discrete(limits = rev)+
  facet_wrap(~LTER, scales="free")+
  theme(legend.position = "right")+
  ggsave("estC.png", width=10, height=6, units="in")

wrtds_trends %>% 
  filter(LTER != "ARC") %>% 
  ggplot(aes(Site, estF, col=as.factor(sigF))) +
  geom_point()+
  geom_linerange(aes(ymin = lowF95, ymax = upF95))+
  coord_flip()+
  geom_hline(yintercept = 0)+
  scale_x_discrete(limits = rev)+
  facet_wrap(~LTER, scales="free")+
  theme(legend.position = "right")+
  ggsave("estF.png", width=10, height=6, units="in")


#### KJ's plots
conc=trend.mod %>% 
  ggplot(aes(LTER,estC))+
  geom_boxplot(outlier.shape=NA)+
  theme_ipsum()+
  scale_fill_viridis(discrete=TRUE, alpha=0.6)+
  geom_jitter(aes(col=as.factor(LTER)),size=5, alpha=0.9)+
  coord_flip()+
  geom_hline(yintercept=0)+
  xlab("")+ylab("")+
  ggtitle("Concentration Change (mg/L)")+
  theme(legend.position="none")

flux=trend.mod %>% 
  ggplot(aes(LTER,estF))+
  geom_boxplot(outlier.shape=NA)+
  theme_ipsum()+
  scale_fill_viridis(discrete=TRUE, alpha=0.6)+
  geom_jitter(aes(col=as.factor(LTER)),size=5, alpha=0.9)+
  coord_flip()+
  geom_hline(yintercept=0)+
  xlab("")+ylab("")+
  ggtitle("Flux Change (106_kg_yr)")+
  theme(legend.position="none")+
  ylim(-2.5,2.5)
  

plot_grid(conc,flux,
          ncol = 2, nrow = 1) #+ ggsave("Fig5.1.eps", width=8, height=10)



### Plotting
## reorder sites for plotting
df$site = as.factor(df$site)

UMR=c("M786.2C", "M764.3A", "M701.1B", "M556.4A", "M241.4K", "M078.0B") 
UMR_trib=c("CN00.1M", "CH00.1M", "BK01.0M", "MQ02.1M", "WP02.6M", "CU11.6M", 
      "I080.2M","SG16.2C","LM00.5M")

KRR=c("S65", "S65A", "S65D", "S65E")
AND=c("GSMACK", "GSWS02", "GSWS06", "GSWS07", "GSWS08", "GSWS09", "GSWS10")
HBR_trt=c("ws1","ws2","ws4","ws5")
HBR_ctrl=c("ws3","ws6","ws7","ws8","ws9")
NWT=c("ALBION","MARTINELLI","SADDLE STREAM 007")
MCM=c("Andersen Creek at H1", "Canada Stream at F1", "Green Creek at F9","Lawson Creek",
      "Onyx River at Lake Vanda Weir","Onyx River at Lower Wright Weir", "Priscu Stream at B1",
      "Von Guerard Stream at F6")
LMP="LMP73"
Sage="Sagehen"
ARC="Imnavait Weir"
LUQ = c("Q1","Q2","Q3","QS","RI","MPR")
GRO=c("Yukon","Yenisey","Ob","Lena","Kolyma","Mackenzie")

# create column with LTER name
#df3=df2 %>% separate(col=site, into=c("LTER", "site")) 

# Plot by LTER 
UMR.all=filter(df, site %in% UMR)
UMRtrib.all=filter(df, site %in% UMR_trib)
KRR.all=filter(df, site %in% KRR)
AND.all=filter(df, site %in% AND)
HBR.trt=filter(df, site %in% HBR_trt)
HBR.ctrl=filter(df, site %in% HBR_ctrl)
NWT.all=filter(df, site %in% NWT)
LMP.all=filter(df, site %in% LMP)
MCM.all=filter(df, site %in% MCM)
ARC.all=filter(df, site %in% ARC)
LUQ.all=filter(df, site %in% LUQ)
GRO.all=filter(df, site %in% GRO)

# plot discharge
ggplot(HBR.ctrl, aes(Year, Discharge))+facet_wrap(~site, scales="free_y")+geom_point()+
  theme_minimal()+geom_smooth(method="loess")+xlab("")+ylab("")+xlim(1970,2020)+
  scale_x_continuous(breaks = seq(1970,2020, by = 20))

# plot conc and FNconc
setwd("U:/Jankowski/My Documents/Projects/Silica Synthesis/Results/Figures")

GRO.all %>% 
  ggplot(aes(Year, Conc_mgL, col=site))+
  geom_point(size=1.5)+
  geom_line(aes(Year, FNConc_mgL, col=site))+
  theme_classic()+
  #xlim(2000,2010)+
  theme(axis.text=element_text(size=12))+
  theme(legend.position="bottom")
  #geom_vline(xintercept=c(1967,1974,1984), col=c("darkorange","darkgreen","blue"))+
  #ggsave("NWT_conc.png", width=7, height=5)
  
# plot flux and FNflux
  GRO.all %>% 
    ggplot(aes(Year, Flux_106_kg_y, col=site))+
    geom_point(size=1.5)+
    geom_line(aes(Year, FNFlux_106_kg_y))+
    theme_classic()+
    #xlim(2002,2010)+
    theme(axis.text=element_text(size=12))+
    theme(legend.position="bottom")
    #ggsave("MCM_flux.png", width=7, height=5)

# plot C-Q
ggplot(UMR.all, aes(Discharge, Conc_mgL, col=as.factor(Year)))+facet_wrap(~site2, scales="free")+geom_point()+
  theme_minimal()




# plot data
setwd("C:/Users/kjankowski/Desktop/")
tiff("Cont.data.tif", height=8, width=8, units="in", res=100)
ggplot(df.cont, aes(x = Date, y = ConcDay)) + 
  geom_point()+ facet_wrap(~name, scales="free")
dev.off()

ggplot(df.cont, aes(x=as.factor(Month), y=ConcDay, col=as.factor(year)))+geom_point()+
  facet_wrap(~name, scales="free")
    
## seasonal change 
df.cont.month=aggregate(ConcDay~name*Month*year, FUN=mean, data=df.cont)
    
tiff("Cont.data.season.tif", height=8, width=10, units="in", res=100)
ggplot(df.cont.month, aes(x=Month, y=ConcDay, col=as.factor(year)))+
  geom_line()+facet_wrap(~name, scales="free")+theme_minimal()+
  scale_x_continuous(breaks=seq(1,12,2))
dev.off()
    

