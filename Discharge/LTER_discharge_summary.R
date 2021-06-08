library(dplyr)
library(ggplot2)

#create list of LTERs, sites, and date range
names(X20210524_masterdata)[names(X20210524_masterdata)=="Site/Stream.Name"] = "site.name"

SiSyn_LTER_sitesummary = 
  X20210524_masterdata %>%
  group_by(LTER, site.name) %>%
  summarize(
    StartDate=min(Sampling.Date),
    EndDate=max(Sampling.Date),
    n=n()
  )

SiSyn_LTER_sitelist = data.frame(LTER=SiSyn_LTER_sitesummary$LTER,
                                 site.name=SiSyn_LTER_sitesummary$site.name)

#summarize Q data
SiSyn_LTER_siteQsummary = 
  WRTDS_discharge_allsites_21Apr21 %>%
  group_by(site.name)%>%
  summarize(
    minQ=min(Q, na.rm=T),
    maxQ=max(Q, na.rm=T),
    medianQ=median(Q, na.rm=T),
    meanQ=mean(Q, na.rm=T)
  )

#merge LTER site list and Q data
SiSyn_LTER_discharge = merge(SiSyn_LTER_sitelist, WRTDS_discharge_allsites_21Apr21, by=c("site.name"))
SiSyn_LTER_summary = merge(SiSyn_LTER_sitelist, SiSyn_LTER_Qsummary, by=c("site.name"))

#summarize Q data by LTER
SiSyn_LTER_Qsummary =
  SiSyn_LTER_discharge[SiSyn_LTER_discharge$Q!=0,] %>%
  group_by(LTER) %>%
  summarize(
    minQ=min(Q, na.rm=T),
    maxQ=max(Q, na.rm=T),
    medianQ=median(Q,na.rm=T),
    meanQ=mean(Q, na.rm=T),
    n=n()
  )
  
#create boxplots for discharge range; group by LTER, color by site?
ggplot(SiSyn_LTER_discharge, aes(x=LTER, y=Q))+
  geom_boxplot()

