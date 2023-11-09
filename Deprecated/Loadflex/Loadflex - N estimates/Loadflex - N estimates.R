#load required packages
library(dplyr)
library(ggplot2)
library(rloadest)
library(loadflex)
library(plyr)

#merge site N and Q files
OnyxRiverVanda = merge(MCM_Onyx_River_at_Lake_Vanda_Weir_fill_Q_update, Onyx_River_at_Lake_Vanda_Weir_NOX_WRTDS, all=T)
PriscuStream = merge(MCM_Priscu_Stream_at_B1_fill_Q, Priscu_Stream_at_B1_NOX_WRTDS, all=T)
VonGuerard = merge(MCM_Von_Guerard_Stream_at_F6_fill_Q_update, Von_Guerard_Stream_at_F6_NOX_WRTDS, all=T)
Imnavait = merge(ARC_Imnavait_fill_Q, Imnavait_Weir_NOX_WRTDS, all=T)
Toolik = merge(Toolik_Inlet_NOX_Q_WRTDS, Toolik_Inlet_NOX_WRTDS, all=T)
TWWeir = merge(TW_Weir_NOX_Q_WRTDS, TW_Weir_NOX_WRTDS, all=T)
S65 = merge(S65_NOX_Q_WRTDS, S65_NOX_WRTDS, all=T)
S65A = merge(S65A_NOX_Q_WRTDS, S65A_NOX_WRTDS, all=T)
S65D = merge(S65D_NOX_Q_WRTDS, S65D_NOX_WRTDS, all=T)
S65E = merge(S65E_NOX_Q_WRTDS, S65E_NOX_WRTDS, all=T)

#make sure all dfs have same column name and order
colnames(S65E)
col_order = c("Date", "Site", "Q", "NOX")

names(OnyxRiverVanda)[names(OnyxRiverVanda)=="Discharge"]="Q"
OnyxRiverVanda = OnyxRiverVanda[,col_order]
names(PriscuStream)[names(PriscuStream)=="Discharge"]="Q"
PriscuStream = PriscuStream[,col_order]
names(VonGuerard)[names(VonGuerard)=="Discharge"]="Q"
VonGuerard = VonGuerard[,col_order]
names(Imnavait)[names(Imnavait)=="Discharge"]="Q"
Imnavait = Imnavait[,col_order]

Toolik$Site = "Toolik Inlet"
Toolik = Toolik[,col_order]
TWWeir$Site = "TW Weir"
TWWeir = TWWeir[,col_order]
S65E$Site = "S65E"
S65E = S65E[,col_order]

#combine all sites
site_regdat = rbind(OnyxRiverVanda,PriscuStream,VonGuerard,Imnavait,Toolik,TWWeir,S65,S65A,S65D,S65E)
sites = unique(site_regdat$Site)

#remove duplicated rows
site_regdat = site_regdat %>% distinct()

site_Q_NOX = na.omit(site_regdat)
site_Q_NOX = site_Q_NOX %>%
              group_by(Date,Site) %>%
                summarize(
                  Q=mean(Q),
                  NOX=mean(NOX)
                )

#plot c-q relationship for each site
ggplot(site_regdat, aes(x=Q, y=NOX))+
  geom_point()+
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")+
  facet_wrap(~Site, scales="free")

#plot c over time
ggplot(site_regdat, aes(x=Date, y=NOX))+
  geom_point()+
  facet_wrap(~Site, scales="free_y")

site_lr = selBestModel("NOX", site_Q_NOX[site_Q_NOX$Site=="Toolik Inlet",],
                       flow="Q", dates="Date", conc.units="mg/L")
print(site_lr)
#all sites, except Toolik, have concentration data when Q=0; gives error when trying to fit regression model
#use interpolation method for all sites

#list to store Loadflex outputs
data_list = list()

#loop to run all sites via interpolation
for (i in 1:length(sites)){
  
  site_intdat = subset(site_Q_NOX, site_Q_NOX$Site==sites[i])
  
  site_meta = metadata(constituent="NOX", flow="Q",
                       dates="Date", conc.units="mg/L", flow.units="cms",
                       load.units="kg",load.rate.units="kg/d",site.name=sites[i])
  
  site_li = loadInterp(interp.format="flux", interp.fun=rectangularInterpolation,
                       data=site_intdat, metadata=site_meta)
  
  #generate point predictions
  site_estdat = data.frame(Date=site_regdat$Date[site_regdat$Site==sites[i]],
                           Q=site_regdat$Q[site_regdat$Site==sites[i]])
  
  preds_li = predictSolute(site_li, "flux", site_estdat, date=T)
  
  data_list[[i]] = preds_li
}

names(data_list) = sites

Loadflex_dailyN = ldply(data_list, data.frame)
head(Loadflex_dailyN)
names(Loadflex_dailyN)[names(Loadflex_dailyN)==".id"] = "Site"
names(Loadflex_dailyN)[names(Loadflex_dailyN)=="date"]="Date"
names(Loadflex_dailyN)[names(Loadflex_dailyN)=="fit"]="Loadflex_NOX"

#merge Loadflex with sample data
Loadflex_NOX = merge(Loadflex_dailyN, site_regdat, all=T)
head(Loadflex_NOX)

Loadflex_NOX$Sample_NOX = Loadflex_NOX$Q * Loadflex_NOX$NOX * 86.4

ggplot(Loadflex_NOX)+
  geom_line(aes(x=Date, y=Loadflex_NOX), color="blue")+
  geom_point(aes(x=Date, y=Sample_NOX), color="red")+
  facet_wrap(~Site, scales="free_y")

#write.csv
write.csv(Loadflex_NOX, file="Loadflex_NOX_selectsites.csv")
