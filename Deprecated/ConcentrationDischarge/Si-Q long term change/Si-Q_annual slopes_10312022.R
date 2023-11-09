library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mblm) #median-based linear model? Suggested by Paul

#get number of Si-Q measurements per site and year
# Si_Q_WRTDS_sites <- read_csv("L:/GitHub/SiSyn/Merge Site Discharge/Merge site DSi and Q/DSi_Q_dat_allsites_13Jul22.csv", 
#     col_types = cols(...1 = col_skip()))

#add year value from date
WRTDS_inputs_mergedDSi_Q_1Nov2022$year = year(WRTDS_inputs_mergedDSi_Q_1Nov2022$Date)

#make a copy, just in case
WRTDS_inputs_mergedDSi_Q_1Nov2022_copy = WRTDS_inputs_mergedDSi_Q_1Nov2022

#split LTER and site.name from site_ID
WRTDS_inputs_mergedDSi_Q_1Nov2022 = 
  WRTDS_inputs_mergedDSi_Q_1Nov2022 %>%
  separate(Stream_ID, into=c("LTER","site.name"),sep="__")
#how many obs per LTER, site, year?
WRTDS_inputs_mergedDSi_Q_1Nov2022_stats = 
  WRTDS_inputs_mergedDSi_Q_1Nov2022 %>%
  group_by(LTER, site.name, year) %>%
  summarize(
    n=n()
  )

#how many sites are in WRTDS_inputs_mergedDSi_Q_1Nov2022?
length(unique(WRTDS_inputs_mergedDSi_Q_1Nov2022$site.name))

#run lm model for one site in one year
AndersenCreekatH1_1995 = subset(WRTDS_inputs_mergedDSi_Q_10312022, WRTDS_inputs_mergedDSi_Q_10312022$site.name=="Andersen Creek at H1" & WRTDS_inputs_mergedDSi_Q_10312022$year==1995)
plot(log(AndersenCreekatH1_1995$Q)~log(AndersenCreekatH1_1995$value_mgL))
fit = lm(log(AndersenCreekatH1_1995$Q)~log(AndersenCreekatH1_1995$value_mgL))
summary(fit)
slope = as.numeric(fit$coefficients[2])
pvalue = summary(fit)$coefficients[2,4]

#create loop to run through all sites for each year
site_list = unique(WRTDS_inputs_mergedDSi_Q_1Nov2022$site.name)

#create empty list to store output from loop
site_slope = list()

for (i in 1:length(site_list)){
  year_slope = list() #clear year_slope list between every site
  
  site_dat = subset(WRTDS_inputs_mergedDSi_Q_1Nov2022, WRTDS_inputs_mergedDSi_Q_1Nov2022$site.name==site_list[i]) #subset all site data
  
  site_years = unique(site_dat$year) #get list of years in dataset
  
  for (j in 1:length(site_years)) { #loop through all years in site_dat; unique(site_years)[1]:unique(site_years)[length(site_years)]
    tryCatch({
      annual_site_dat = subset(site_dat, site_dat$year==site_years[j]) #subset each year
      
      fit = lm(log(annual_site_dat$Q)~log(annual_site_dat$value_mgL)) #linear regression between Si-Q
      
      slope = as.numeric(fit$coefficients[2]) #get slope from linear model
      
      pvalue = summary(fit)$coefficients[2,4]
      
      year_slope_dat = data.frame(site.name=annual_site_dat$site.name[1],
                                  year=annual_site_dat$year[1],
                                  slope=slope,
                                  pvalue=pvalue)
      
      year_slope[[j]] = year_slope_dat
    }, error=function(e){cat("ERROR: ",conditionMessage(e),"\n")}) #if there is an error with the lm model, print error message and skip
  }
  
  site_slope[[i]] = year_slope
}

annual_site_slopes = unlist(site_slope, recursive=F) #expand list
annual_site_slopes = ldply(annual_site_slopes, data.frame) #move all site slopes to one data frame
#merge site stats with lm output
annual_site_slopes_stats = merge(annual_site_slopes, WRTDS_inputs_mergedDSi_Q_1Nov2022_stats)
#are slopes significant?
annual_site_slopes_stats$sig = ifelse(annual_site_slopes_stats$pvalue<=0.1, "sig","not sig") 

#plot slopes by year, facet by site
ggplot(annual_site_slopes_stats, aes(x=year, y=slope))+
  geom_abline(intercept=0,slope=0, lty="dashed", color="black")+
  geom_point(aes(color=sig))+
  geom_smooth(color="black")+
  facet_wrap(~site.name, scales="free")

#plot by LTER?
ggplot(annual_site_slopes_stats, aes(x=year, y=slope))+
  geom_abline(intercept=0,slope=0, lty="dashed", color="black")+
  #geom_point(aes(shape=sig))+
  geom_smooth(aes(color=site.name),se=F)+
  facet_wrap(~LTER, scales="free")+
  theme(legend.position="none")
#aspect ratio 950x700

#change insignificant annual slopes to 0
annual_site_slopes_stats$sig_slopes = ifelse(annual_site_slopes_stats$sig=="sig",annual_site_slopes$slope,0)

#save as .csv
write.csv(annual_site_slopes_stats,file="annualCQ_slopes_1Nov22.csv")

#stats on slope trends
summary(mblm(slope~year, annual_site_slopes_stats[annual_site_slopes_stats$site.name=="ALBION",]))
as.numeric(mblm(sig_slopes~year, annual_site_slopes_stats[annual_site_slopes_stats$site.name=="west fork",])$coefficients[2]) #slope
summary(mblm(slope~year, annual_site_slopes_stats[annual_site_slopes_stats$site.name=="ALBION",]))$coefficients[1,1] #intercept
summary(mblm(slope~year, annual_site_slopes_stats[annual_site_slopes_stats$site.name=="ALBION",]))$coefficients[2,4] #pvalue

#loop through annual slopes, run mblm for long term change over time
annual_site_slopes_mblm = list()
annual_site_slopes_na = annual_site_slopes_stats
annual_site_slopes_stats = na.omit(annual_site_slopes_stats)

#get n for annual C-Q slopes fed into mblm
mblm_stats = 
  annual_site_slopes_stats %>%
  group_by(site.name) %>%
  summarize(
    mblm_n=n()
  )

for (i in 1:length(site_list)){
  tryCatch({
    annual_site_slope = subset(annual_site_slopes_stats, annual_site_slopes_stats$site.name==site_list[i])
    
    mblm_slope=as.numeric(mblm(sig_slopes~year, annual_site_slope)$coefficients[2])
    mblm_intercept=summary(mblm(sig_slopes~year, annual_site_slope))$coefficients[1,1]
    mblm_pvalue=summary(mblm(sig_slopes~year, annual_site_slope))$coefficients[2,4]
  }, error=function(e){cat("ERROR: ",conditionMessage(e),"\n")})
    d = data.frame(site.name=site_list[i],
                   mblm_slope=mblm_slope,
                   mblm_intercept=mblm_intercept,
                   mblm_pvalue=mblm_pvalue)
    
    annual_site_slopes_mblm[[i]] = d
}

annual_site_slopes_mblm = ldply(annual_site_slopes_mblm, data.frame)
annual_site_slopes_mblm_list = annual_site_slopes_mblm #make a copy
annual_site_slopes_mblm$mblm_sig = ifelse(annual_site_slopes_mblm$mblm_pvalue<=0.1, "sig","not sig")
annual_site_slopes_mblm = merge(annual_site_slopes_mblm, mblm_stats,all=T)
write.csv(annual_site_slopes_mblm, file="allsites_CQmblm_1Nov22.csv")

#set insignificant mblm slopes to 0?
annual_site_slopes_stats$sig_slopes = ifelse(annual_site_slopes_stats$sig=="sig",annual_site_slopes_stats$slope,0)
annual_site_slopes_mblm_stats = merge(annual_site_slopes_stats, annual_site_slopes_mblm,all=T)
write.csv(annual_site_slopes_mblm_stats, file="AnnualCQslopes_mlbmresults_1Nov22.csv")

#plot mblm slopes by site/LTER
annual_site_slopes_mblm_stats = na.omit(annual_site_slopes_mblm_stats)
annual_site_slopes_mblm_stats$year = as.Date(annual_site_slopes_mblm_stats$year, format="%Y")

ggplot(annual_site_slopes_mblm_stats, aes(x=year, y=sig_slopes))+
  geom_abline(aes(slope=mblm_slope, intercept=mblm_intercept, color=mblm_sig))+
  scale_color_manual(values=c("white","black"))+
  geom_point(aes(fill=sig), pch=21)+
  scale_fill_manual(values=c("white","black"))+
  labs(y="Slope")+
  theme_bw(base_size=12)+
  theme(axis.title.x=element_blank(),
        legend.position="none")+
  facet_wrap(~site.name, scales="free")
#aspect ratio 1750x1000

#subset sites with significant mblm slopes
sig_mblm = subset(annual_site_slopes_mblm_stats, annual_site_slopes_mblm_stats$mblm_sig=="sig")

#create time series plot 
ggplot(sig_mblm, aes(x=year, y=sig_slopes))+
  geom_abline(aes(slope=mblm_slope, intercept=mblm_intercept))+
  geom_point(aes(fill=sig), pch=21)+
  scale_fill_manual(values=c("white","black"))+
  labs(y="Slope")+
  theme_bw(base_size=12)+
  theme(axis.title.x=element_blank(),
        legend.position="none")+
  facet_wrap(~site.name, scales="free")

#group by LTER
NWT_mblm = 
  ggplot(sig_mblm[sig_mblm$LTER=="NWT",], aes(x=year, y=sig_slopes))+
  geom_abline(aes(slope=mblm_slope, intercept=mblm_intercept))+
  geom_point(aes(fill=sig), pch=21)+
  scale_fill_manual(values=c("white","black"))+
  labs(y="Slope")+
  theme_bw(base_size=12)+
  theme(axis.title.x=element_blank(),
        legend.position="none")+
  facet_wrap(~site.name, scales="free")
#trend goes across all insignificant slope values
MCM_mblm = 
  ggplot(sig_mblm[sig_mblm$LTER=="MCM",], aes(x=year, y=sig_slopes))+
  geom_abline(aes(slope=mblm_slope, intercept=mblm_intercept))+
  geom_point(aes(fill=sig), pch=21)+
  scale_fill_manual(values=c("white","black"))+
  labs(y="Slope",title="MCM LTER")+
  theme_bw(base_size=12)+
  theme(axis.title.x=element_blank(),
        legend.position="none")+
  facet_wrap(~site.name, scales="free")

AND_mblm = 
  ggplot(sig_mblm[sig_mblm$LTER=="AND",], aes(x=year, y=sig_slopes))+
  geom_abline(aes(slope=mblm_slope, intercept=mblm_intercept))+
  geom_point(aes(fill=sig), pch=21)+
  scale_fill_manual(values=c("white","black"))+
  labs(y="Slope",title="AND LTER")+
  theme_bw(base_size=12)+
  theme(axis.title.x=element_blank(),
        legend.position="none")+
  facet_wrap(~site.name, scales="free")

LUQ_mblm = 
  ggplot(sig_mblm[sig_mblm$LTER=="LUQ",], aes(x=year, y=sig_slopes))+
  geom_abline(aes(slope=mblm_slope, intercept=mblm_intercept))+
  geom_point(aes(fill=sig), pch=21)+
  scale_fill_manual(values=c("white","black"))+
  labs(y="Slope",title="LUQ LTER")+
  theme_bw(base_size=12)+
  theme(axis.title.x=element_blank(),
        legend.position="none")+
  facet_wrap(~site.name, scales="free")

HBR_mblm = 
  ggplot(sig_mblm[sig_mblm$LTER=="HBR",], aes(x=year, y=sig_slopes))+
  geom_abline(aes(slope=mblm_slope, intercept=mblm_intercept))+
  geom_point(aes(fill=sig), pch=21)+
  scale_fill_manual(values=c("white","black"))+
  labs(y="Slope", title="HBR LTER")+
  theme_bw(base_size=12)+
  theme(axis.title.x=element_blank(),
        legend.position="none")+
  facet_wrap(~site.name, scales="free")

library(patchwork)
(MCM_mblm + HBR_mblm) / (LUQ_mblm + AND_mblm) + plot_layout(heights=c(1,2))
#create boxplots for annual slopes by site - facet by LTER?
ggplot(sig_mblm, aes(y=site.name, x=sig_slopes))+
  geom_boxplot()+
  facet_wrap(~LTER,scales="free")

#create same as above, but as forest plot

