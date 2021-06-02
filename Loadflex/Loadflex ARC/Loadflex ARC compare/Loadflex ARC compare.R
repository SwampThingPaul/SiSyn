library(loadflex)
library(rloadest)
library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)
library(hydroTSM)

#list sites to run for WRTDS comparison
site_list = c("ws1","ws2","ws3","ws6","GSWS09","GSWS10","ALBION", "MARTINELLI", "SADDLE STREAM 007")

#subset discharge data for sites in site_list
site_discharge = WRTDS_discharge_allsites_21Apr21[WRTDS_discharge_allsites_21Apr21$site.name %in% site_list,]
unique(site_discharge$site.name)

#check for any negative discharge values
length(which(site_discharge$Q < 0))

#subset DSi data for sites in site_list
site_DSi = WRTDS_DSi_mergedsites_19May21[WRTDS_DSi_mergedsites_19May21$site.name %in% site_list,]
unique(site_DSi$site.name)

#check for negative DSi values
length(which(site_DSi$Si < 0))

#convert discharge at NWT sites from "cmd" to "cms"
site_discharge$Q_units = ifelse(site_discharge$site.name=="ALBION"|
                                            site_discharge$site.name=="MARTINELLI"|
                                            site_discharge$site.name=="SADDLE STREAM 007",
                                          "cmd", "cms")
site_discharge$Q_cms = ifelse(site_discharge$Q_units=="cms",
                                        site_discharge$Q,
                                        site_discharge$Q/86400)

site_discharge = data.frame("Date"=site_discharge$Date,
                            "site.name"=site_discharge$site.name,
                            "Q"=site_discharge$Q_cms)

#create interpolation data frame for all sites
intdat_allsites = merge(site_discharge, site_DSi, by=c("Date","site.name"))

#arrange by site (alphabetically) and date
site_list = sort(site_list)
intdat_allsites = arrange(intdat_allsites, site.name, Date)
site_discharge = arrange(site_discharge, site.name, Date)

#loop to run Loadflex for all 9 sites
#list to store Loadflex output for each site
data_list = list()
regmodelfit = list()
InterpRes = list()

#site list for all sites in intdat dataframe
site_list = unique(intdat_allsites$site.name)

#loop to run all sites
for (i in 1:length(site_list)) {
  site_Q = subset(site_discharge, site_discharge$site.name==site_list[i])
  site_Si = subset(site_DSi, site_DSi$site.name==site_list[i])
  
  site_Si_Q = merge(site_Q, site_Si, by=c("Date", "site.name"), all=T)
  
  site_intdat = subset(intdat_allsites, intdat_allsites$site.name==site_list[i])
  site_intdat = site_intdat[!duplicated(site_intdat$Date),]
  
  site_meta = metadata(constituent="Si", flow="Q", 
                       dates="Date", conc.units="mg L^-1", flow.units="cms", load.units="kg", 
                       load.rate.units="kg d^-1", site.name=site_list[i])
  
  #run regression model
  site_li = loadInterp(interp.format="conc", interp.function=linearInterpolation,
                       data=site_intdat, metadata=site_meta)
  
  #evaluate model fit
  regmodelfit[[i]]=getFittedModel(site_li)
  
  #get residuals from composite model
  InterpRes[[i]] = data.frame("site"=site_list[i],
                            "Date"=getResiduals(site_li,"flux")[,1],
                            "Residuals"=getResiduals(site_li,"flux")[,2])
  #point predictions and daily load estimates
  site_preds_li = predictSolute(site_li,"flux",site_Q,date=T)
  
  #merge daily point predictions with original interpolation data
  site_preds_li$site.name = site_list[i]
  names(site_preds_li)[names(site_preds_li)=="fit"] = "Si_load_kg.day"
  names(site_preds_li)[names(site_preds_li)=="date"] = "Date"
  
  site_loadflex_Q = merge(site_preds_li, site_Si_Q, by=c("Date","site.name"), all=T)
  
  data_list[[i]] = site_loadflex_Q
}

#expand data_list to data frame
dailySiLoads = ldply(data_list, data.frame)
unique(dailySiLoads$site.name)

#plot Loadflex daily estimates and measured load values
dailySiLoads$MeasLoads = (dailySiLoads$Q*1000) * dailySiLoads$Si * (86400/1e6)

ggplot(dailySiLoads)+
  geom_line(aes(x=Date, y=Si_load_kg.day), color="blue")+
  geom_point(aes(x=Date, y=MeasLoads))+
  xlab("")+
  ylab("Si load (kg/day)")+
  theme_bw(base_size=14)+
  facet_wrap(~site.name, scales="free")

write.csv(dailySiLoads, file="Loadflex_dailySi_selectsites_19May2021.csv")

