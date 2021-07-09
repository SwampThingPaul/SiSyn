library(ggplot2)
library(loadflex)
library(rloadest)
library(plyr)

#merge site P and Q data
CanadaStream = merge(Canada_Stream_Q, Canada_Stream_P, all=T)
OnyxRiverVanda = merge(Onyx_River_at_Vanda_Q, Onyx_River_at_Vanda_P, all=T)

#duplicated dates?
which(duplicated(CanadaStream$Date))
CanadaStream[8411:8412,]
CanadaStream = CanadaStream[!duplicated(CanadaStream$Date),]

which(duplicated(OnyxRiverVanda$Date))

#concatentate merged P-Q files
sitedat = rbind(CanadaStream, OnyxRiverVanda)
siteQdat = data.frame(Date=sitedat$Date,
                      Site=sitedat$Site,
                      Discharge=sitedat$Discharge)

#plot C-Q relationship for both sites
ggplot(sitedat, aes(x=Discharge, y=P))+
  geom_point()+
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")+
  facet_wrap(~Site)

#create site list
sites = unique(sitedat$Site)

#list to store output for each site
data_list = list()

#loop to run sites
#cannot use regression model to for data with 0 Q; use interpolation model
for (i in 1:length(sites)){
  
  site_intdat = subset(sitedat, sitedat$Site==sites[i])
  
  site_meta = metadata(constituent="P", flow="Discharge", dates="Date",
                       conc.units="mg/L", flow.units="cms", 
                       load.units="kg", load.rate.units=" kg d^-1",
                       site.name=sites[i])
  
  #run interpolation model
  site_li = loadInterp(interp.format="flux", interp.fun=rectangularInterpolation,
                       data=site_intdat, metadata=site_meta)
  
  #point predictions and daily load estimates
  site_estdat = subset(siteQdat, siteQdat$Site==sites[i])
  site_preds_li = predictSolute(site_li,"flux",site_estdat,date=T)
  
  data_list[[i]] = site_preds_li
}

names(data_list) = sites

#expand list to df
LoadflexPest = ldply(data_list, data.frame)
names(LoadflexPest)[names(LoadflexPest)=="fit"] = "Loadflex_P_est_kg.d"
names(LoadflexPest)[names(LoadflexPest)==".id"] = "Site"
names(LoadflexPest)[names(LoadflexPest)=="date"] = "Date"
head(LoadflexPest)

#calculate sample P loads
sitedat$P_loads = sitedat$Discharge * sitedat$P * 86.4

#merge Loadflex and sample data
dailyPloads = merge(LoadflexPest, sitedat)
head(dailyPloads)

#plot Loadflex estimates
ggplot(dailyPloads)+
  geom_line(aes(x=Date, y=Loadflex_P_est_kg.d))+
  geom_point(aes(x=Date, y=P_loads))+
  labs(y="P loads (kg/day)")+
  facet_wrap(~Site, scales="free")+
  theme_bw(base_size=14)+
  theme(axis.title.x=element_blank())

#subset sites from dailyPloads
Canada_Stream_at_F1_DailyPLoads = subset(dailyPloads, dailyPloads$Site=="Canada Stream at F1")
Onyx_River_at_Vanda_DailyPLoads = subset(dailyPloads, dailyPloads$Site=="Onyx River at Lake Vanda Weir")

write.csv(Canada_Stream_at_F1_DailyPLoads, file="Canada_Stream_at_F1_DailyPLoads_Loadflex.csv")
write.csv(Onyx_River_at_Vanda_DailyPLoads, file="Onyx_River_at_Vanda_DailyPLoads_Loadflex.csv")
