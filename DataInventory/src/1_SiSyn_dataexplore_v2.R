## 
## SiSyn
##
##
## Code was compiled by Paul Julian
## contact info: pjulian@ufl.edu
## contact info: pauljulianphd@gmail.com

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

#Libraries
library(AnalystHelper);#devtools::install_github("SwampThingPaul/AnalystHelper")
library(plyr)
library(reshape)
library(openxlsx)

# GIS Libraries
library(tmap)
library(rgdal)
library(rgeos)
library(raster)

#Paths
wd="C:/Julian_LaCie/_GitHub/SiSyn"

#https://www.r-bloggers.com/structuring-r-projects/
#https://nicercode.github.io/blog/2013-04-05-projects/

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src/"))
#Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

# Extra functions 
dms2dec <- function(dms, separators = c("º", "°", "\'", "\"")) {
  # version 1.0 (25 Sep 3013)
  # dms: a vector (or column) of latitude or longitude in degrees-minutes-seconds-hemisfere, e.g. 41° 34' 10.956" N (with or without spaces)
  # separators: the characters that are separating degrees, minutes and seconds in dms
  
  dms <- as.character(dms)
  dms <- gsub(pattern = " ", replacement = "", x = dms)
  for (s in separators) dms <- gsub(pattern = s, replacement = "_splitHere_", x = dms)
  
  splits <- strsplit(dms, split = "_splitHere_")
  n <- length(dms)
  deg <- min <- sec <- hem <- vector("character", n)
  
  for (i in 1:n) {
    deg[i] <- splits[[i]][1]
    min[i] <- splits[[i]][2]
    sec[i] <- splits[[i]][3]
    hem[i] <- splits[[i]][4]
  }
  
  dec <- as.numeric(deg) + (as.numeric(min) / 60) + (as.numeric(sec) / 3600)
  sign <- ifelse (hem %in% c("N", "E"), 1, -1)
  dec <- sign * dec
  return(dec)
}

# Helper variables 
wgs84=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Data --------------------------------------------------------------------
param.list=data.frame(variable=c("Daily.Avg.Q.(Discharge)", "Instantaneous.Q.(Discharge)", "Stage.Height", 
                                 "Temp.C", "Conductivity", "Spec.Cond", "Turbidity", "TSS", "VSS", 
                                 "DSi", "TN","TDN", "DIN", "NOx","NO3","NO2", "NH4","DON", "TP","TDP", "PO4", "SRP", "DOC", 
                                 "TOC", "alkalinity", "ANC", "pH", "DIC", "Suspended.Chl", "Benthic.Chl", 
                                 "Na", "K", "Ca", "Mg", "SO4", "Cl"),
                      plot.val=1:36)
idvars=c("LTER","Site/Stream.Name","Sampling.Date")
param.vars=c("Daily.Avg.Q.(Discharge)", "Instantaneous.Q.(Discharge)", "Stage.Height", 
             "Temp.C", "Conductivity", "Spec.Cond", "Turbidity", "TSS", "VSS", 
             "DSi", "TN","TDN", "DIN", "NOx","NO3","NO2", "NH4","DON", "TP","TDP", "PO4", "SRP", "DOC", 
             "TOC", "alkalinity", "ANC", "pH", "DIC", "Suspended.Chl", "Benthic.Chl", 
             "Na", "K", "Ca", "Mg", "SO4", "Cl")
basin.vars=c("LTER","Site/Stream","Unique.ID","Latitude","Longitude")

data.filelist=list.files(data.path)

## ARCLTER
arc.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_ARCLTER_05012020.xlsx"),sheet=1,startRow=2,na.strings = "NA")
arc.dat$Sampling.Date=convertToDate(arc.dat$Sampling.Date)
names(arc.dat)
arc.note.Look=arc.dat[,c("Site/Stream.Name","Notes")]

#fix header
arc.dat=rename(arc.dat,c("Dsi"="DSi"))
arc.dat[,c("TDN","TDP","NO3","NO2","DON")]=as.numeric(NA); #Add empty columns for uniformity

##
arc.dat.melt=melt(arc.dat[,c(idvars,param.vars)],id.vars=idvars)
arc.dat.melt=subset(arc.dat.melt,is.na(value)==F)

arc.dat.melt=merge(arc.dat.melt,param.list,"variable")
arc.dat.melt$site=arc.dat.melt$'Site/Stream.Name'
arc.dat.melt$variable=as.character(arc.dat.melt$variable)

arc.dat.inv=ddply(arc.dat.melt,c("Sampling.Date","variable","plot.val"),summarise,N.val=N.obs(site))

arc.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_ARCLTER_05012020.xlsx"),sheet=4,startRow=4,na.strings = "NA")
arc.basin=arc.basin[,basin.vars]

## BcZO
bczo.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_BcCZO.xlsx"),sheet=1,startRow=2,na.strings = "NA")
bczo.dat$Sampling.Date=convertToDate(bczo.dat$Sampling.Date)
names(bczo.dat)
bczo.dat[,c("TN","TP","NO3","NO2","DON")]=as.numeric(NA);

bczo.dat.melt=melt(bczo.dat[,c(idvars,param.vars)],id.vars=idvars)
bczo.dat.melt=subset(bczo.dat.melt,is.na(value)==F)

bczo.dat.melt=merge(bczo.dat.melt,param.list,"variable")
bczo.dat.melt$site=bczo.dat.melt$'Site/Stream.Name'
bczo.dat.melt$variable=as.character(bczo.dat.melt$variable)

bczo.dat.inv=ddply(bczo.dat.melt,c("Sampling.Date","variable","plot.val"),summarise,N.val=N.obs(site))

bczo.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_BcCZO.xlsx"),sheet=4,startRow=4,na.strings = "NA")
bczo.basin=bczo.basin[,basin.vars]

## Carey
carey.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Carey_6.12.20.xlsx"),sheet=1,startRow=2,na.strings = "NA")
carey.dat$Sampling.Date=convertToDate(carey.dat$Sampling.Date)
carey.dat=carey.dat[2:nrow(carey.dat),]
carey.dat[,6:ncol(carey.dat)]=sapply(carey.dat[,6:ncol(carey.dat)],as.numeric)

carey.dat=rename(carey.dat,c("NO3.(uM)"="NO3","NO2.(uM)"="NO2"))
carey.dat[,c("TDN","TDP","DON")]=as.numeric(NA)

carey.dat.melt=melt(carey.dat[,c(idvars,param.vars)],id.vars=idvars)
carey.dat.melt=subset(carey.dat.melt,is.na(value)==F)

carey.dat.melt=merge(carey.dat.melt,param.list,"variable")
carey.dat.melt$site=carey.dat.melt$'Site/Stream.Name'
carey.dat.melt$variable=as.character(carey.dat.melt$variable)

carey.dat.inv=ddply(carey.dat.melt,c("Sampling.Date","variable","plot.val"),summarise,N.val=N.obs(site))

carey.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Carey_6.12.20.xlsx"),sheet=4,startRow=4,na.strings = "NA")
carey.basin=carey.basin[,basin.vars]
carey.basin$Longitude=carey.basin$Longitude*-1

## Coal Creek
coal.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Coal_Creek.xlsx"),sheet=1,startRow=2,na.strings = "NA")
coal.dat$Sampling.Date=convertToDate(coal.dat$Sampling.Date)
names(coal.dat)
coal.dat[,c("TDN","TDP","NO3","NO2")]=as.numeric(NA);

coal.dat.melt=melt(coal.dat[,c(idvars,param.vars)],id.vars=idvars)
coal.dat.melt=subset(coal.dat.melt,is.na(value)==F)
coal.dat.melt=merge(coal.dat.melt,param.list,"variable")
coal.dat.melt$site=coal.dat.melt$'Site/Stream.Name'
coal.dat.melt$variable=as.character(coal.dat.melt$variable)

coal.dat.inv=ddply(coal.dat.melt,c("Sampling.Date","variable","plot.val"),summarise,N.val=N.obs(site))

coal.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Coal_Creek.xlsx"),sheet=4,startRow=4,na.strings = "NA")
coal.basin=coal.basin[,basin.vars]
coal.basin$Latitude=dms2dec(coal.basin$Latitude)
coal.basin$Latitude=coal.basin$Latitude*-1
coal.basin$Longitude=dms2dec(coal.basin$Longitude)

## CPCRW
cpcrw.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_CPCRW.xlsx"),sheet=1,startRow=2,na.strings = "NA")
cpcrw.dat$Sampling.Date=convertToDate(cpcrw.dat$Sampling.Date)
names(cpcrw.dat)
cpcrw.dat[,c("TDN","TDP","NO3","NO2","DON")]=as.numeric(NA);

cpcrw.dat.melt=melt(cpcrw.dat[,c(idvars,param.vars)],id.vars=idvars)
cpcrw.dat.melt=subset(cpcrw.dat.melt,is.na(value)==F)
cpcrw.dat.melt=merge(cpcrw.dat.melt,param.list,"variable")
cpcrw.dat.melt$site=cpcrw.dat.melt$'Site/Stream.Name'
cpcrw.dat.melt$variable=as.character(cpcrw.dat.melt$variable)

cpcrw.dat.inv=ddply(cpcrw.dat.melt,c("Sampling.Date","variable","plot.val"),summarise,N.val=N.obs(site))

cpcrw.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_CPCRW.xlsx"),sheet=4,startRow=4,na.strings = "NA")
cpcrw.basin=cpcrw.basin[,basin.vars]
cpcrw.basin$Latitude=dms2dec(cpcrw.basin$Latitude)
cpcrw.basin$Longitude=dms2dec(cpcrw.basin$Longitude)

#Knonza
konza.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Konza.xlsx"),sheet=1,startRow=2,na.strings = "NA")
konza.dat$Sampling.Date=convertToDate(konza.dat$Sampling.Date)
names(konza.dat)
konza.dat[,c("TDN","TDP","NO2","DON")]=as.numeric(NA);
konza.dat=rename(konza.dat,c("no3"="NO3"))

konza.dat.melt=melt(konza.dat[,c(idvars,param.vars)],id.vars=idvars)
konza.dat.melt=subset(konza.dat.melt,is.na(value)==F)
konza.dat.melt=merge(konza.dat.melt,param.list,"variable")
konza.dat.melt$site=konza.dat.melt$'Site/Stream.Name'
konza.dat.melt$variable=as.character(konza.dat.melt$variable)

konza.dat.inv=ddply(konza.dat.melt,c("Sampling.Date","variable","plot.val"),summarise,N.val=N.obs(site))

konza.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Konza.xlsx"),sheet=4,startRow=4,na.strings = "NA")
konza.basin=konza.basin[,basin.vars]
konza.basin$Latitude=dms2dec(konza.basin$Latitude)
konza.basin$Longitude=dms2dec(konza.basin$Longitude)
konza.basin[4,4]=konza.basin[4,4]*-1

## KRR
KRR.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_KRR.xlsx"),sheet=1,startRow=2,na.strings = "NA")
KRR.dat$Sampling.Date=convertToDate(KRR.dat$Sampling.Date)
names(KRR.dat)
KRR.dat[,c("TDN","TDP","NO3","NO2","DON")]=as.numeric(NA);

KRR.dat.melt=melt(KRR.dat[,c(idvars,param.vars)],id.vars=idvars)
KRR.dat.melt=subset(KRR.dat.melt,is.na(value)==F)
KRR.dat.melt=merge(KRR.dat.melt,param.list,"variable")
KRR.dat.melt$site=KRR.dat.melt$'Site/Stream.Name'
KRR.dat.melt$variable=as.character(KRR.dat.melt$variable)

KRR.dat.inv=ddply(KRR.dat.melt,c("Sampling.Date","variable","plot.val"),summarise,N.val=N.obs(site))

KRR.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_KRR.xlsx"),sheet=4,startRow=4,na.strings = "NA")
KRR.basin=KRR.basin[,basin.vars]

## MCM
MCM.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_MCM.xlsx"),sheet=1,startRow=2,na.strings = "NA")
MCM.dat$Sampling.Date=convertToDate(MCM.dat$Sampling.Date)
names(MCM.dat)
MCM.dat[,c("TDN","TDP","NO3","NO2","DON")]=as.numeric(NA);

MCM.dat.melt=melt(MCM.dat[,c(idvars,param.vars)],id.vars=idvars)
MCM.dat.melt=subset(MCM.dat.melt,is.na(value)==F)
MCM.dat.melt=merge(MCM.dat.melt,param.list,"variable")
MCM.dat.melt$site=MCM.dat.melt$'Site/Stream.Name'
MCM.dat.melt$variable=as.character(MCM.dat.melt$variable)

MCM.dat.inv=ddply(MCM.dat.melt,c("Sampling.Date","variable","plot.val"),summarise,N.val=N.obs(site))

MCM.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_MCM.xlsx"),sheet=4,startRow=4,na.strings = "NA")
MCM.basin=MCM.basin[,basin.vars]

## NWT
NWT.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_NWT.xlsx"),sheet=1,startRow=2,na.strings = "NA")
NWT.dat$Sampling.Date=convertToDate(NWT.dat$Sampling.Date)
names(NWT.dat)
NWT.dat[,c("TDN","TDP","NO3","NO2","DON")]=as.numeric(NA);
NWT.dat=rename(NWT.dat,c("InstantaneoNAs.Q.(Discharge)"="Instantaneous.Q.(Discharge)",
                         "CondNActivity"="Conductivity",
                         "TNArbidity"="Turbidity",
                         "SNAspended.Chl"="Suspended.Chl"))

NWT.dat.melt=melt(NWT.dat[,c(idvars,param.vars)],id.vars=idvars)
NWT.dat.melt=subset(NWT.dat.melt,is.na(value)==F)
NWT.dat.melt=merge(NWT.dat.melt,param.list,"variable")
NWT.dat.melt$site=NWT.dat.melt$'Site/Stream.Name'
NWT.dat.melt$variable=as.character(NWT.dat.melt$variable)

NWT.dat.inv=ddply(NWT.dat.melt,c("Sampling.Date","variable","plot.val"),summarise,N.val=N.obs(site))

NWT.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_NWT.xlsx"),sheet=4,startRow=4,na.strings = "NA")
NWT.basin=NWT.basin[,basin.vars]

## LMP
LMP.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_LMP.xlsx"),sheet=1,startRow=2,na.strings = "NA")
LMP.dat$Sampling.Date=convertToDate(LMP.dat$Sampling.Date)
names(LMP.dat)
LMP.dat[,c("TDP","NO3","NO2","DON")]=as.numeric(NA);

LMP.dat.melt=melt(LMP.dat[,c(idvars,param.vars)],id.vars=idvars)
LMP.dat.melt=subset(LMP.dat.melt,is.na(value)==F)
LMP.dat.melt=merge(LMP.dat.melt,param.list,"variable")
LMP.dat.melt$site=LMP.dat.melt$'Site/Stream.Name'
LMP.dat.melt$variable=as.character(LMP.dat.melt$variable)

LMP.dat.inv=ddply(LMP.dat.melt,c("Sampling.Date","variable","plot.val"),summarise,N.val=N.obs(site))

LMP.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_LMP.xlsx"),sheet=4,startRow=4,na.strings = "NA")
LMP.basin=LMP.basin[,basin.vars]

## LUQ
LUQ.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_LUQ.xlsx"),sheet=1,startRow=2,na.strings = "NA")
LUQ.dat$Sampling.Date=convertToDate(LUQ.dat$Sampling.Date)
names(LUQ.dat)
LUQ.dat[,c("TDP","NO3","NO2","DON")]=as.numeric(NA);

LUQ.dat.melt=melt(LUQ.dat[,c(idvars,param.vars)],id.vars=idvars)
LUQ.dat.melt=subset(LUQ.dat.melt,is.na(value)==F)
LUQ.dat.melt=merge(LUQ.dat.melt,param.list,"variable")
LUQ.dat.melt$site=LUQ.dat.melt$'Site/Stream.Name'
LUQ.dat.melt$variable=as.character(LUQ.dat.melt$variable)

LUQ.dat.inv=ddply(LUQ.dat.melt,c("Sampling.Date","variable","plot.val"),summarise,N.val=N.obs(site))

LUQ.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_LUQ.xlsx"),sheet=4,startRow=4,na.strings = "NA")
LUQ.basin=LUQ.basin[,basin.vars]
LUQ.basin=subset(LUQ.basin,is.na(Latitude)==F)

## PIE
pie.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_PIEWatersheds.xlsx"),sheet=1,startRow=2,na.strings = "NA")
pie.dat$Sampling.Date=convertToDate(pie.dat$Sampling.Date)
names(pie.dat)
pie.dat[,c("TN","TDP","NO3","NO2","DON")]=as.numeric(NA);

pie.dat.melt=melt(pie.dat[,c(idvars,param.vars)],id.vars=idvars)
pie.dat.melt=subset(pie.dat.melt,is.na(value)==F)
pie.dat.melt=merge(pie.dat.melt,param.list,"variable")
pie.dat.melt$site=pie.dat.melt$'Site/Stream.Name'
pie.dat.melt$variable=as.character(pie.dat.melt$variable)

pie.dat.inv=ddply(pie.dat.melt,c("Sampling.Date","variable","plot.val"),summarise,N.val=N.obs(site))

pie.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_PIEWatersheds.xlsx"),sheet=4,startRow=4,na.strings = "NA")
pie.basin=pie.basin[,basin.vars]

#lat long got flipped
pie.basin$long=pie.basin$Latitude
pie.basin$lat=pie.basin$Longitude
pie.basin$Longitude=pie.basin$long
pie.basin$Latitude=pie.basin$lat

pie.basin=pie.basin[,basin.vars]


##
sites=rbind(cpcrw.basin,KRR.basin)
sites=rbind(sites,arc.basin)
sites=rbind(sites,LMP.basin)
sites=rbind(sites,LUQ.basin)
sites=rbind(sites,pie.basin)
sites=rbind(sites,carey.basin)
sites=rbind(sites,coal.basin)
sites=rbind(sites,konza.basin)
sites=rbind(sites,MCM.basin)
sites=rbind(sites,NWT.basin)
sites=rbind(sites,bczo.basin)

sites$Longitude=as.numeric(sites$Longitude)
subset(sites,is.na(Longitude)==T)
sites=subset(sites,is.na(Longitude)==F)


sites=SpatialPointsDataFrame(coords=sites[,c("Longitude","Latitude")],
                                   data=sites,
                                   proj4string = wgs84)
# tmap_mode("view")
tm_shape(sites)+tm_dots()

##