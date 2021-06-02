library(ggplot2)
library(loadflex)
library(plyr)
library(data.table)
library(zoo)
library(lubridate)
library(rqdatatable)

##merge Q and Si data for each site
Toolik_intdat = merge(Toolik_Inlet_Q_WRTDS, Toolik_Inlet_Si_WRTDS, by=c("Date"), all=T)
TW_Weir_intdat = merge(TW_Weir_Q_WRTDS, TW_Weir_Si_WRTDS, by=c("Date"), all=T)

#add site names
Toolik_intdat$site = "Toolik"
TW_Weir_intdat$site = "TW_Weir"

#concatentate both sites
ARC_sites = rbind(Toolik_intdat, TW_Weir_intdat)

#calculate average Si value when multiple values per Date
keys = colnames(ARC_sites)[!grepl('Si',colnames(ARC_sites))]
X = as.data.table(ARC_sites)
ARC_sites_avgbydate = X[,list(meanDailySi= mean(Si)),keys]

#create dataframe with just Q data
site_Q = data.frame(Date=ARC_sites_avgbydate$Date,
                    Q=ARC_sites_avgbydate$Q,
                    site=ARC_sites_avgbydate$site)

#how many 0 Q values?
length(which(ARC_sites_avgbydate$Q==0))
#where are 0 Q values?
ARC_sites$zeroQ = ifelse(ARC_sites_$Q==0, "zero", "")
zeroQ = na.omit(subset(ARC_sites_avgbydate, ARC_sites_avgbydate$Q==0))

#plot data across range of dates
ggplot(ARC_sites, aes(x=Date, y=Si, color=site))+
  geom_point()
ggplot(ARC_sites, aes(x=Date, y=Q, color=zeroQ))+
  geom_point()+
  facet_wrap(~site, scales="free")

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
  site_intdat = na.omit(subset(ARC_sites_avgbydate, ARC_sites_avgbydate$site==site_list[i]))
  
  site_meta = metadata(constituent="meanDailySi", flow="Q", 
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

#expand data_list to data frame for each site
dailySiLoads = ldply(data_list, data.frame)
head(dailySiLoads)
#expand data_list to data frame for each site
Toolik_dailyLoadflex_Si = ldply(data_list[1], data.frame)
write.csv(Toolik_dailyLoadflex_Si, file="Toolik_dailyLoadflex_Si.csv")
TW_Weir_dailyLoadflex_Si = ldply(data_list[2], data.frame)
write.csv(TW_Weir_dailyLoadflex_Si, file="TW_Weir_dailyLoadflex_Si.csv")

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

#which dates are missing (and how many) each summer
#create dataframe with all dates during period of record, merge to add NA values for missing dates
Toolik_missingQ = data.frame(Date=seq.Date(from=as.Date("1993-06-12"), to=as.Date("2010-08-28"), by="day"),
                             Q=NA)
Toolik_missingQ = natural_join(Toolik_Inlet_Q_WRTDS, Toolik_missingQ,
                               by="Date",
                               jointype="FULL")
Toolik_missingQ$month = month(Toolik_missingQ$Date)
Toolik_missingQ = subset(Toolik_missingQ, Toolik_missingQ$month>=5 & Toolik_missingQ$month<=9)
Toolik_missingQ$site = "Toolik"
write.csv(Toolik_missingQ, file="Toolik_missingQ.csv")

TW_Weir_missingQ = data.frame(Date=seq.Date(from=as.Date("1992-06-12"), to=as.Date("2005-08-15"), by="day"),
                              Q=NA)
TW_Weir_missingQ = natural_join(TW_Weir_Q_WRTDS, TW_Weir_missingQ,
                               by="Date",
                               jointype="FULL")
TW_Weir_missingQ$month = month(TW_Weir_missingQ$Date)
TW_Weir_missingQ = subset(TW_Weir_missingQ, TW_Weir_missingQ$month>=5 & TW_Weir_missingQ$month<=9)
TW_Weir_missingQ$site = "TW_Weir"
write.csv(TW_Weir_missingQ, file="TW_Weir_missingQ.csv")

ARC_missingQ = rbind(Toolik_missingQ, TW_Weir_missingQ)
ARC_missingQ$DOY = as.numeric(format(ARC_missingQ$Date, "%j"))
ARC_missingQ$year = year(ARC_missingQ$Date)

#plot Q by DOY; facet by year and site
ggplot(ARC_missingQ[ARC_missingQ$site=="Toolik",], aes(x=DOY, y=Q))+
  geom_line()+
  xlab("")+
  ylab("Q (cms)")+
  ggtitle("Toolik")+
  theme_bw(base_size=12)+
  facet_wrap(~year, scales="free_y")
ggplot(ARC_missingQ[ARC_missingQ$site=="TW_Weir",], aes(x=DOY, y=Q))+
  geom_line()+
  xlab("")+
  ylab("Q (cms)")+
  ggtitle("TW_Weir")+
  theme_bw(base_size=12)+
  facet_wrap(~year, scales="free_y")
#aspect ratio 1100x850
###################################################
#histogram of ARC site data by month
ARC_sites_avgbydate$monthday = format(ARC_sites_avgbydate$Date, "%m-%d")
ARC_sites_avgbydate$DOY = as.numeric(format(ARC_sites_avgbydate$Date,"%j"))
ARC_sites_avgbydate$month = month(ARC_sites_avgbydate$Date)
ARC_sites_avgbydate$year = year(ARC_sites_avgbydate$Date)

ggplot(ARC_sites_avgbydate, aes(month))+
  geom_histogram(binwidth=1, bins=c(5, 6, 7, 8, 9))+
  facet_wrap(~site+year)

