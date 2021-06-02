# libraries
library(lubridate)
library(openxlsx)
library(tidyverse)
library(readxl)
library(data.table)
library(lfstat)

setwd("U:/Jankowski/My Documents/Projects/Silica Synthesis/Data/Discharge/Discharge_templates")

# "adj" = filled in values to improve interpolations
# alb = removed 10 really high values at start of 2002 and filled with Jan average
# mar = added "0" at 12/1/17 to smooth interpolation
# sdl = filled end of 2017 and early 2018 with "0" (winter average) to smooth interpolation
alb=read.csv("NWT_alb_Q_adj.csv") 
mar=read.csv("NWT_mar_Q.csv")
sdl=read.csv("NWT_sdl_Q_adj.csv")

dat=bind_rows(alb,mar,sdl)
dat$dates <- as.POSIXct(strptime(dat$Date, "%Y-%m-%d"))
dat$year <- year(dat$dates)
dat$month <- month(dat$dates)
dat$day <- day(dat$dates)

# albion 
alb=filter(dat, DischargeSiteName == "alb")
ggplot(alb, aes(month,Discharge))+geom_point()+
  facet_wrap(~as.factor(year), scales="free_y")+ggtitle("Albion")

# martinelli 
mar=filter(dat, DischargeSiteName == "mar")
ggplot(mar, aes(month,Discharge))+geom_point()+
  facet_wrap(~as.factor(year), scales="free_y")+ggtitle("Martinelli")

# saddle 
sdl=filter(dat, DischargeSiteName == "sdl")
ggplot(sdl, aes(month,Discharge))+geom_point()+
  facet_wrap(~as.factor(year), scales="free_y")+ggtitle("Saddle")

## format for lfstat functions
alb1=alb %>% select(Discharge,day,month,year)
colnames(alb1)=c("flow","day","month","year")

mar1=mar %>% select(Discharge,day,month,year)
colnames(mar1)=c("flow","day","month","year")

sdl1=sdl %>% select(Discharge,day,month,year)
colnames(sdl1)=c("flow","day","month","year")

# create lf objects
alblf=createlfobj(alb1)
marlf=createlfobj(mar1)
sdllf=createlfobj(sdl1)

# straight line interpolations
alblf1=lfnainterpolate(alblf)
ggplot(alblf1, aes(as.factor(month),flow))+geom_point()+
  facet_wrap(~as.factor(year), scales="free_y")

marlf1=lfnainterpolate(marlf)
ggplot(marlf1, aes(as.factor(month),flow))+geom_point()+
  facet_wrap(~as.factor(year), scales="free_y")

sdllf1=lfnainterpolate(sdllf)
ggplot(sdllf1, aes(as.factor(month),flow))+geom_point()+
  facet_wrap(~as.factor(year), scales="free_y")

## write data to file
# add dates
# select columns needed for WRTDS
 
alb.fill=as.data.frame(alblf1)
alb.fill$Site=rep("ALBION", nrow(alb.fill))
mar.fill=as.data.frame(marlf1)
mar.fill$Site=rep("MARTINELLI", nrow(mar.fill))
sdl.fill=as.data.frame(sdllf1)
sdl.fill$Site=rep("SADDLE STREAM 007", nrow(sdl.fill))

dat.fill=bind_rows(alb.fill,mar.fill,sdl.fill)

dat.fill1=select(dat.fill, c(Site,year,month,day,flow))
dat.fill2=dat.fill1 %>% unite("Date", 2:4, sep="-")
dat.fill2$Date <- as.POSIXct(strptime(dat.fill2$Date, "%Y-%m-%d"))
dat.fill2$Units = rep("cmd", nrow(dat.fill2))

colnames(dat.fill2)=c("Site","Date","Discharge","Units")

alb.exp=filter(dat.fill2, Site == "ALBION")
mar.exp=filter(dat.fill2, Site == "MARTINELLI")
sdl.exp=filter(dat.fill2, Site== "SADDLE STREAM 007")

write.csv(alb.exp, "NWT_alb_Q_fill.csv")
write.csv(mar.exp, "NWT_mar_Q_fill.csv")
write.csv(sdl.exp, "NWT_sdl_Q_fill.csv")


# looking just at period with chem data
marlf2=filter(marlf1, year >=1994 & year <=2018)
ggplot(marlf2,aes(year,flow))+geom_point()

# find mean annual minimum for each year - not totally sure what this is doing
# don't think it's really what we want though
min=MAM(alblf, n = 7, year = "any",breakdays = NULL, yearly = TRUE)

# recession 
x=recessionplot(alblf,peaklevel=0.95, thresbreaks="monthly",
                start=1990,end=1991)

