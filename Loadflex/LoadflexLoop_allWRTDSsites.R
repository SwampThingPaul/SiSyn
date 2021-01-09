library(loadflex)

#get list of sites to run from INFO_all
#same as sites run with WRTDS model
site_list = c(INFO_all$shortName)

#replace any negative values with NA
WRTDS_discharge_allsites[WRTDS_discharge_allsites < 0] = NA
na.omit(WRTDS_discharge_allsites)


#list to store Loadflex output for each site
data_list = list()

##run code manually to troubleshoot errors##
#NWT stream sites (albion and martinelli) have discharge in units cmd
site_Q = subset(WRTDS_discharge_allsites, WRTDS_discharge_allsites$site.name=="ALBION")
site_Si = subset(X20201111_masterdata, X20201111_masterdata$site=="ALBION" & X20201111_masterdata$variable=="DSi")

site_Q[site_Q <=0] = NA
site_Q = na.omit(site_Q)
site_Q$Q = site_Q$Q/86400

names(site_Si)[names(site_Si)=="Sampling.Date"] = "Date"

site_intdat = merge(site_Q,site_Si,by="Date")
site_intdat = data.frame("Date"=site_intdat$Date,
                         "Q"=site_intdat$Q,
                         "DSi"=site_intdat$value*0.06)
site_intdat[site_intdat <= 0] = NA
site_intdat = na.omit(site_intdat)
site_intdat = site_intdat[!duplicated(site_intdat$Date),]

library(rloadest)
site_lr = loadReg2(loadReg(DSi~Q, data=site_intdat,
                           flow="Q", dates="Date", conc.units="mg/L", load.units="kg"))
site_lc = loadComp(reg.model=site_lr, interp.format="conc", interp.data=site_intdat)

site_preds_lc = predictSolute(site_lc,"flux",site_Q,se.pred=T,date=T)
site_aggs_lc = aggregateSolute(site_preds_lc,site_meta,"flux rate","day")

write.csv(site_aggs_lc, file="ALBION_Loadflex_DailySi.csv")

library(plyr)
dailySiLoads = ldply(data_list, data.frame)
###

#loop to run all sites
for (i in 1:length(site_list)) {
  site_Q = subset(WRTDS_discharge_allsites, WRTDS_discharge_allsites$site.name==site_list[i])
  site_Si = subset(X20201111_masterdata, X20201111_masterdata$site==site_list[i] & X20201111_masterdata$variable=="DSi")

  names(site_Si)[names(site_Si)=="Sampling.Date"] = "Date"
  
  site_intdat = merge(site_Q,site_Si,by="Date")
  site_intdat = data.frame("Date"=site_intdat$Date,
                             "Q"=site_intdat$Q,
                             "DSi"=site_intdat$value*0.06)
  site_intdat[site_intdat <= 0] = NA
  site_intdat = site_intdat[!duplicated(site_intdat$Date),]

  #run composite model
  site_lr = loadReg2(loadReg(DSi~Q, data=site_intdat,
                             flow="Q", dates="Date", conc.units="mg/L", load.units="kg"))
  site_lc = loadComp(reg.model=site_lr, interp.format="conc", interp.data=site_intdat)
  #point predictions and daily load estimates
  site_preds_lc = predictSolute(site_lc,"flux",site_Q,se.pred=T,date=T)
  site_aggs_lc = aggregateSolute(site_preds_lc,site_meta,"flux rate","day")
  
  site_aggs = data.frame("Date"=site_aggs_lc$Day,
                         "SiLoad_kg.d"=site_aggs_lc$Flux_Rate,
                         "Site"=site_list[i])
  
  data_list[[i]] = site_aggs
}

###
#run simple lm model
#site_meta = metadata(constituent="DSi", flow="Q", dates="Date",
#                     conc.units="mg L^-1", flow.units="cfs", load.units="kg",
#                     load.rate.units="kg d^-1", site.name="site",
#                     consti.name="Dissolved SiO2")
#site_lm = loadLm(formula=log(DSi) ~ log(Q), pred.format="conc",
#                 data=site_intdat, metadata=site_meta, retrans=exp)



###