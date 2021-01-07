library(loadflex)

#get list of sites to run from INFO_all
#same as sites run with WRTDS model
site_list = c(INFO_all$shortName)

#replace any negative values with NA
WRTDS_discharge_allsites[WRTDS_discharge_allsites < 0] = NA
na.omit(WRTDS_discharge_allsites)


#list to store Loadflex output for each site
data_list = list()

for (i in 1:length(site_list)) {
  site_Q = subset(WRTDS_discharge_allsites, WRTDS_discharge_allsites$site.name==site_list[i])
  site_Si = subset(X20201111_masterdata, X20201111_masterdata$site==site_list[i] & X20201111_masterdata$variable=="DSi")

  names(site_Si)[names(site_Si)=="Sampling.Date"] = "Date"
  
  site_intdat = merge(site_Q,site_Si,by="Date")
  site_intdat = data.frame("Date"=site_intdat$Date,
                             "Q"=site_intdat$Q,
                             "DSi"=site_intdat$value*0.06)
  site_intdat_nodup = site_intdat[!duplicated(site_intdat$Date),]
  
  site_intdat_nodup[site_intdat_nodup < 0] = NA
  na.omit(site_intdat_nodup)
  #run composite model
  site_lr = loadReg2(loadReg(DSi~Q, data=site_intdat_nodup,
                             flow="Q", dates="Date", conc.units="mg/L", load.units="kg"))
#  site_lc = loadComp(reg.model=site_lr, interp.format="conc", interp.data=site_intdat)
#  #point predictions and daily load estimates
#  site_preds_lc = predictSolute(site_lc,"flux",site_Q,se.pred=T,date=T)
#  site_aggs_lc = aggregateSolute(site_preds_lc,site_meta,"flux rate","day")
  
  data_list[[i]] = site_intdat_nodup
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