library(loadflex)
library(rloadest)
library(plyr)
library(ggplot2)
library(lubridate)
library(hydroTSM)

#get list of sites to run from INFO_all
#same as sites run with WRTDS model
site_list = c(INFO_all$shortName)
print(site_list)

#replace any negative values with NA
WRTDS_discharge_allsites[WRTDS_discharge_allsites <= 0] = NA
WRTDS_discharge_allsites = na.omit(WRTDS_discharge_allsites)

WRTDS_DSi_allsites[WRTDS_DSi_allsites <= 0] = NA
WRTDS_DSi_allsites = na.omit(WRTDS_DSi_allsites)

#convert discharge at NWT sites from "cmd" to "cms"
WRTDS_discharge_allsites$Q_units = ifelse(WRTDS_discharge_allsites$site.name=="ALBION"|
                                      WRTDS_discharge_allsites$site.name=="MARTINELLI"|
                                      WRTDS_discharge_allsites$site.name=="SADDLE STREAM 007",
                                      "cmd", "cms")
WRTDS_discharge_allsites$Q_cms = ifelse(WRTDS_discharge_allsites$Q_units=="cms",
                                        WRTDS_discharge_allsites$Q,
                                        WRTDS_discharge_allsites$Q/86400)

WRTDS_discharge_allsites = data.frame("Date"=WRTDS_discharge_allsites$Date,
                                      "site.name"=WRTDS_discharge_allsites$site.name,
                                      "Q"=WRTDS_discharge_allsites$Q_cms)


##run code manually to troubleshoot errors##
site_Q = subset(WRTDS_discharge_allsites, WRTDS_discharge_allsites$site.name=="S65A")
site_Si = subset(WRTDS_DSi_allsites, WRTDS_DSi_allsites$site.name=="S65A")

site_intdat = merge(site_Q,site_Si,by=c("Date","site.name"))
site_intdat[site_intdat <= 0] = NA
site_intdat = na.omit(site_intdat)
site_intdat = site_intdat[!duplicated(site_intdat$Date),]

site_meta = metadata(constituent="Si", flow="Q", 
                 dates="Date", conc.units="mg L^-1", flow.units="cms", load.units="kg", 
                 load.rate.units="kg d^-1", site.name="S65A")

site_lr = loadReg2(loadReg(Si~Q, data=site_intdat, station="S65A",
                           flow="Q", flow.units="cms", dates="Date", conc.units="mg/L", load.units="kg"))

#evaluate model fit
regmodelfit = list(getFittedModel(site_lr))
print(regmodelfit)
#pass to composite model
site_lc = loadComp(reg.model=site_lr, interp.format="conc", interp.data=site_intdat)
#get residuals from composite model
compres = data.frame("site"="S65A",
                     "Date"=getResiduals(site_lc,"flux")[,1],
                     "Residuals"=getResiduals(site_lc,"flux")[,2])
#generate point predictions
site_preds_lc = predictSolute(site_lc,"flux",site_Q,se.pred=T,date=T)
#aggregate point predictions for daily load estimates
site_aggs_lc = aggregateSolute(site_preds_lc,site_meta,"flux rate","day")

write.csv(site_aggs_lc, file="S65A_Loadflex_DailySi.csv")

###

#list to store Loadflex output for each site
data_list = list()
regmodelfit = list()
compres = list()

#loop to run all sites
for (i in 1:length(site_list)) {
  site_Q = subset(WRTDS_discharge_allsites, WRTDS_discharge_allsites$site.name==site_list[i])
  site_Si = subset(WRTDS_DSi_allsites, WRTDS_DSi_allsites$site.name==site_list[i])
  
  site_intdat = merge(site_Q,site_Si,by="Date")
  site_intdat = site_intdat[!duplicated(site_intdat$Date),]
  
  site_meta = metadata(constituent="Si", flow="Q", 
                       dates="Date", conc.units="mg L^-1", flow.units="cms", load.units="kg", 
                       load.rate.units="kg d^-1", site.name=site_list[i])

  #run regression model
  site_lr = loadReg2(loadReg(Si~Q, data=site_intdat, station=site_list[i],
                             flow="Q", flow.units="cms", dates="Date", conc.units="mg/L", load.units="kg"))
  #evaluate model fit
  regmodelfit[[i]]=getFittedModel(site_lr)
  #pass regression model to composite model
  site_lc = loadComp(reg.model=site_lr, interp.format="conc", interp.data=site_intdat)
  #get residuals from composite model
  compres[[i]] = data.frame("site"=site_list[i],
                            "Date"=getResiduals(site_lc,"flux")[,1],
                            "Residuals"=getResiduals(site_lc,"flux")[,2])
  #point predictions and daily load estimates
#  site_preds_lc = predictSolute(site_lc,"flux",site_Q,se.pred=T,date=T)
#  site_aggs_lc = aggregateSolute(site_preds_lc,site_meta,"flux rate","day")
  
#  site_aggs = data.frame("Date"=site_aggs_lc$Day,
#                         "SiLoad_kg.d"=site_aggs_lc$Flux_Rate,
#                         "Site"=site_list[i])
  
#  data_list[[i]] = site_aggs
}

names(data_list) = site_list
#exporting each site into its own dataframe? 

#expand lists to data frame
dailySiLoads = ldply(data_list, data.frame)
write.csv(dailySiLoads, file="Loadflex_DailySi_allsites.csv")

comp_model_residuals = ldply(compres, data.frame)
#merge composite model residuals with discharge data
names(comp_model_residuals)[names(comp_model_residuals)=="site"] = "site.name"
head(comp_model_residuals)
head(WRTDS_discharge_allsites)
comp_model_residuals = merge(comp_model_residuals, WRTDS_discharge_allsites, by=c("Date","site.name"))
comp_model_residuals$month = month(comp_model_residuals$Date)
comp_model_residuals$season = time2season(comp_model_residuals$Date, out.fmt="seasons")
comp_model_residuals$season = factor(comp_model_residuals$season,ordered=T, levels=c("winter","spring","summer","autumm"))

###plot data###
#estimated loads by date
ggplot(dailySiLoads, aes(x=Date, y=SiLoad_kg.d)) +
  geom_point() +
  facet_wrap(~Site, scales="free")
#composite model residuals
ggplot(comp_model_residuals, aes(x=Q, y=Residuals)) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  facet_wrap(~site.name, scales="free")

ggplot(comp_model_residuals, aes(x=month, y=Residuals)) +
  geom_point() +
  scale_x_continuous(name="month", breaks=c(1:12),labels=c(1:12))+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  facet_wrap(~site.name, scales="free")+
  theme_minimal()

ggplot(comp_model_residuals, aes(x=season, y=Residuals)) +
  geom_point() +
  scale_x_discrete(name="season", labels=c("W","Sp","Su","F"))+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  facet_wrap(~site.name, scales="free")
