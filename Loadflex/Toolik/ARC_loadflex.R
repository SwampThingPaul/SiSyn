library(ggplot2)
library(loadflex)
library(plyr)

##merge Q and Si data for each site
Toolik_intdat = merge(Toolik_Inlet_Q_WRTDS, Toolik_Inlet_Si_WRTDS, by=c("Date"), all=T)
TW_Weir_intdat = merge(TW_Weir_Q_WRTDS, TW_Weir_Si_WRTDS, by=c("Date"), all=T)

#add site names
Toolik_intdat$site = "Toolik"
TW_Weir_intdat$site = "TW_Weir"

#concatentate both sites
ARC_sites = rbind(Toolik_intdat, TW_Weir_intdat)
#create dataframe with just Q data
site_Q = data.frame(Date=ARC_sites$Date,
                    Q=ARC_sites$Q,
                    site=ARC_sites$site)

#plot data across range of dates
ggplot(ARC_sites, aes(x=Date, y=Si, color=site))+
  geom_point()

#plot C-Q relationship for both sites
ggplot(ARC_sites, aes(x=Q, y=Si, color=site))+
  geom_point()+
  scale_x_continuous(trans="log10")

#create list of both sites
site_list = unique(ARC_sites$site)

#loop through ARC_sites to run Loadflex for both sites
#lists to store loop output
data_list = list()
regmodelfit = list()
resids = list()

for (i in 1:2) {
  site_intdat = na.omit(subset(ARC_sites, ARC_sites$site==site_list[i]))
  
  site_meta = metadata(constituent="Si", flow="Q", 
                       dates="Date", conc.units="mg L^-1", flow.units="cms", load.units="kg", 
                       load.rate.units="kg d^-1", site.name=site_list[i])
  
  #run regression model
  site_li = loadInterp(interp.format="conc", interp.function=linearInterpolation,
                       data=site_intdat, metadata=site_meta)
  
  #evaluate model fit
  regmodelfit[[i]]=getFittedModel(site_li)
  
  #get residuals
  resids[[i]] = data.frame("site"=site_list[i],
                            "Date"=getResiduals(site_li,"flux")[,1],
                            "Residuals"=getResiduals(site_li,"flux")[,2])
  
  #point predictions and daily load estimates
  #subset site Q for daily preds
  pred_Q = subset(site_Q, site_Q$site==site_list[i])
  
  preds_li = predictSolute(site_li,"flux",pred_Q,se.pred=T,date=T)
 
  #add point predictions to data_list
  data_list[[i]] = data.frame(site=site_list[i],
                              Date=preds_li$date,
                              Si_load_kg.day=preds_li$fit,
                              se.pred=preds_li$se.pred)
  
}

#expand data_list to data frame
dailySiLoads = ldply(data_list, data.frame)
head(dailySiLoads)
#calculate sample loads
ARC_sites$load = (ARC_sites$Si/1e6) * (ARC_sites$Q*86400000)
#merge loadflex output with calculated sample loads
Loadflex_output = merge(dailySiLoads, ARC_sites, by=c("site","Date"), all=T)

#plot daily load estimates by Date and site; compare with sample loads
ggplot(Loadflex_output)+
  geom_point(aes(x=Date, y=load), color="black", alpha=0.5)+
  geom_point(aes(x=Date, y=Si_load_kg.day),color="red", alpha=0.5)+
  xlab("")+
  ylab("Si load (kg/day)")+
  theme_bw(base_size=18)+
  facet_wrap(~site, scales="free")

