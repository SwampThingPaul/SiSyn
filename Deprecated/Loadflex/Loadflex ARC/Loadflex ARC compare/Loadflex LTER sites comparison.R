#run HBR and AND sites with Loadflex, compare with WRTDS output
library(loadflex)
library(lubridate)
library(ggplot2)
library(plyr)

#add site name for each Q and Si file
GSWS09_Q_WRTDS$site = "GSWS09"
GSWS10_Q_WRTDS$site = "GSWS10"
ws1_Q_WRTDS$site = "ws1"
ws2_Q_WRTDS$site = "ws2"
ws3_Q_WRTDS$site = "ws3"
ws6_Q_WRTDS$site = "ws6"

GSWS09_Si_WRTDS$site = "GSWS09"
GSWS10_Si_WRTDS$site = "GSWS10"
ws1_Si_WRTDS$site = "ws1"
ws2_Si_WRTDS$site = "ws2"
ws3_Si_WRTDS$site = "ws3"
ws6_Si_WRTDS$site = "ws6"

#merge all Si and Q files from each site
Q_compare = rbind(GSWS09_Q_WRTDS, GSWS10_Q_WRTDS,ws1_Q_WRTDS,ws2_Q_WRTDS,ws3_Q_WRTDS,ws6_Q_WRTDS)
Si_compare = rbind(GSWS09_Si_WRTDS, GSWS10_Si_WRTDS,ws1_Si_WRTDS,ws2_Si_WRTDS,ws3_Si_WRTDS,ws6_Si_WRTDS)

head(Q_compare)
head(Si_compare)

#loop  through loadflex code for each site
#list to store Loadflex output for each site
data_list = list()
regmodelfit = list()
compres = list()

site_list = unique(Q_compare$site)

#loop to run all sites
for (i in 1:length(site_list)) {
  site_Q = subset(Q_compare, Q_compare$site==site_list[i])
  site_Si = subset(Si_compare, Si_compare$site==site_list[i])
  
  site_intdat = merge(site_Q,site_Si,by="Date")
  site_intdat = site_intdat[!duplicated(site_intdat$Date),]
  
  site_meta = metadata(constituent="Si", flow="Q", 
                       dates="Date", conc.units="mg L^-1", flow.units="cms", load.units="kg", 
                       load.rate.units="kg d^-1", site.name=site_list[i])
  
  #run regression model
  site_li = loadInterp(interp.format="conc", interp.function=linearInterpolation,
                       data=site_intdat, metadata=site_meta)
  #evaluate model fit
  regmodelfit[[i]]=getFittedModel(site_li)
  
  #get residuals
  compres[[i]] = data.frame("site"=site_list[i],
                            "Date"=getResiduals(site_li,"flux")[,1],
                            "Residuals"=getResiduals(site_li,"flux")[,2])
  #point predictions and daily load estimates
  preds_li = predictSolute(site_li,"flux",site_Q,se.pred=T,date=T)
  names(preds_li)[names(preds_li)=="fit"] = "Si_load_kg.day"
  
  #add point predictions to data_list
  data_list[[i]] = preds_li
}

#check which sites ran in loop
names(data_list) = site_list

#expand data_list to data frame
dailySiLoads = ldply(data_list, data.frame)
head(dailySiLoads)
unique(dailySiLoads$.id)

comp_model_residuals = ldply(compres, data.frame)
#merge composite model residuals with discharge data
head(comp_model_residuals)
head(Q_compare)
comp_model_residuals = merge(comp_model_residuals, Q_compare, by=c("Date","site"))
comp_model_residuals$month = month(comp_model_residuals$Date)
comp_model_residuals$season = time2season(comp_model_residuals$Date, out.fmt="seasons")
comp_model_residuals$season = factor(comp_model_residuals$season,ordered=T, levels=c("winter","spring","summer","autumm"))

#merge Loadflex output with sample load data
sample_loads = merge(Q_compare, Si_compare, by=c("site","Date"))
head(sample_loads)
sample_loads$load_kg.d = (sample_loads$Si/1e6) * (sample_loads$Q*86400000)

head(dailySiLoads)
names(dailySiLoads)[names(dailySiLoads)==".id"] = "site"
names(dailySiLoads)[names(dailySiLoads)=="date"] = "Date"
names(dailySiLoads)[names(dailySiLoads)=="Si_load_kg.day"] = "Loadflex_Si_load_kg.day"

Loadflex_output = merge(dailySiLoads, sample_loads, by=c("site","Date"), all=T)
head(Loadflex_output)

#plot Loadflex output compared to calculated sample loads
ggplot(Loadflex_output)+
  geom_line(aes(x=Date, y=Loadflex_Si_load_kg.day), color="blue")+
  geom_point(aes(x=Date, y=load_kg.d), color="black")+
  facet_wrap(~site, scales="free")

#combine all WRTDS Cont Si data
GSWS09ContSi_WRTDS$site = "GSWS09"
GSWS10ContSi_WRTDS$site = "GSWS10"
ws1ContSi_WRTDS$site = "ws1"
ws2ContSi_WRTDS$site = "ws2"
ws3ContSi_WRTDS$site = "ws3"
ws6ContSi_WRTDS$site = "ws6"
WRTDS_compare = rbind(GSWS09ContSi_WRTDS, GSWS10ContSi_WRTDS,ws1ContSi_WRTDS,ws2ContSi_WRTDS,ws3ContSi_WRTDS,ws6ContSi_WRTDS)
colnames(WRTDS_compare)

#merge WRTDS and Loadflex outputs
WRTDS_Loadflex_compare = merge(WRTDS_compare, dailySiLoads, by=c("site", "Date"))
colnames(WRTDS_Loadflex_compare)

#plot WRTDS v Loadflex
ggplot(WRTDS_Loadflex_compare, aes(x=FluxDay, y=Loadflex_Si_load_kg.day))+
  geom_point()+
  geom_abline(slope=1, intercept=0, lty="dashed", color="gray")+
  ylab("Loadflex (kg Si/day)")+
  xlab("WRTDS (kg Si/day)")+
  theme_classic(base_size=18)+
  facet_wrap(~site, scales="free")
