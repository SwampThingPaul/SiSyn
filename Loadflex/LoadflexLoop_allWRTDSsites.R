library(loadflex)

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

#list to store Loadflex output for each site
data_list = list()

##run code manually to troubleshoot errors##
site_Q = subset(WRTDS_discharge_allsites, WRTDS_discharge_allsites$site.name=="S65A")
site_Si = subset(WRTDS_DSi_allsites, WRTDS_DSi_allsites$site.name=="S65A")

site_intdat = merge(site_Q,site_Si,by=c("Date","site.name"))
site_intdat[site_intdat <= 0] = NA
site_intdat = na.omit(site_intdat)
site_intdat = site_intdat[!duplicated(site_intdat$Date),]

library(rloadest)
site_lr = loadReg2(loadReg(Si~Q, data=site_intdat,
                           flow="Q", flow.units="cms", dates="Date", conc.units="mg/L", load.units="kg"))
site_lc = loadComp(reg.model=site_lr, interp.format="conc", interp.data=site_intdat)

site_preds_lc = predictSolute(site_lc,"flux",site_Q,se.pred=T,date=T)
site_aggs_lc = aggregateSolute(site_preds_lc,site_meta,"flux rate","day")

write.csv(site_aggs_lc, file="S65A_Loadflex_DailySi.csv")

###

#loop to run all sites
for (i in 1:length(site_list)) {
  site_Q = subset(WRTDS_discharge_allsites, WRTDS_discharge_allsites$site.name==site_list[i])
  site_Si = subset(WRTDS_DSi_allsites, WRTDS_DSi_allsites$site.name==site_list[i])
  
  site_intdat = merge(site_Q,site_Si,by="Date")
  site_intdat = site_intdat[!duplicated(site_intdat$Date),]

  #run composite model
  site_lr = loadReg2(loadReg(Si~Q, data=site_intdat,
                             flow="Q", flow.units="cms", dates="Date", conc.units="mg/L", load.units="kg"))
  site_lc = loadComp(reg.model=site_lr, interp.format="conc", interp.data=site_intdat)
  #point predictions and daily load estimates
  site_preds_lc = predictSolute(site_lc,"flux",site_Q,se.pred=T,date=T)
  site_aggs_lc = aggregateSolute(site_preds_lc,site_meta,"flux rate","day")
  
  site_aggs = data.frame("Date"=site_aggs_lc$Day,
                         "SiLoad_kg.d"=site_aggs_lc$Flux_Rate,
                         "Site"=site_list[i])
  
  data_list[[i]] = site_aggs
}

names(data_list) = site_list
#exporting each site into its own dataframe? 

library(plyr)
dailySiLoads = ldply(data_list, data.frame)

write.csv(dailySiLoads, file="Loadflex_DailySi_allsites.csv")
