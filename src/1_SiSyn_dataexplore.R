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
cpcrw.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_CPCRW.xlsx"),sheet=1,startRow=2,na.strings = "NA")
cpcrw.dat$Sampling.Date=convertToDate(cpcrw.dat$Sampling.Date)

idvars=c("LTER","Site/Stream.Name","Sampling.Date")
param.vars=c("Daily.Avg.Q.(Discharge)", "Instantaneous.Q.(Discharge)", "Stage.Height", 
             "Temp.C", "Conductivity", "Spec.Cond", "Turbidity", "TSS", "VSS", 
             "DSi", "TN", "DIN", "NOx", "NH4", "TP", "PO4", "SRP", "DOC", 
             "TOC", "alkalinity", "ANC", "pH", "DIC", "Suspended.Chl", "Benthic.Chl", 
             "Na", "K", "Ca", "Mg", "SO4", "Cl")
cpcrw.dat.melt=melt(cpcrw.dat[,c(idvars,param.vars)],id.vars=idvars)
cpcrw.dat.melt=subset(cpcrw.dat.melt,is.na(value)==F)

unique(cpcrw.dat.melt$variable)
param.list=data.frame(variable=c("Daily.Avg.Q.(Discharge)", "Instantaneous.Q.(Discharge)", "Stage.Height", 
             "Temp.C", "Conductivity", "Spec.Cond", "Turbidity", "TSS", "VSS", 
             "DSi","TDN", "TN", "DIN", "NOx", "NH4", "TP", "PO4", "SRP", "DOC", 
             "TOC", "alkalinity", "ANC", "pH", "DIC", "Suspended.Chl", "Benthic.Chl", 
             "Na", "K", "Ca", "Mg", "SO4", "Cl"),plot.val=1:32)
cpcrw.dat.melt=merge(cpcrw.dat.melt,param.list,"variable")
cpcrw.dat.melt$site=cpcrw.dat.melt$'Site/Stream.Name'
cpcrw.dat.melt$variable=as.character(cpcrw.dat.melt$variable)

cpcrw.dat.inv=ddply(cpcrw.dat.melt,c("Sampling.Date","variable","plot.val"),summarise,N.val=N.obs(site))

#ggplot experiment
library(ggplot2)
xlim.val=as.Date(c("1991-01-01","2020-01-01"));xmaj=seq(xlim.val[1],xlim.val[2],"5 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
ggplot(cpcrw.dat.inv,aes(x=Sampling.Date,y=plot.val))+
  geom_point(shape="|",colour="red",alpha=0.5,size=2)+
  theme(panel.background=element_rect(fill="white"),
        panel.border = element_rect(fill=NA,colour = "black"),
        panel.grid.major=element_line(size=0.5,linetype = 3,colour="grey"),
        axis.line=element_line(colour="black"),
        text=element_text(family="serif"))+
  scale_x_date("Date (Month-Year)",breaks=xmaj,labels=format(xmaj,"%m-%Y"),limits=xlim.val)+
  scale_y_continuous(name="",position="left",breaks=1:length(param.list$variable),labels=param.list$variable)+
  labs(title="Bonanza Creek LTER",
       subtitle="Data Inventory")
  
  


par(family="serif",mar=c(1,3,0.75,0.5),oma=c(2,3,0.5,0.5));
xlim.val=as.Date(c("1991-01-01","2020-01-01"));xmaj=seq(xlim.val[1],xlim.val[2],"5 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
ylim.val=c(1,nrow(param.list));ymaj=seq(ylim.val[1],ylim.val[2],1)
plot(xlim.val,0:1,type="n",ylim=ylim.val,xlim=xlim.val,axes=F,ylab=NA,xlab=NA)
abline(v=xmin,h=ymaj,lty=2,col="grey80")
with(cpcrw.dat.inv,points(Sampling.Date,plot.val,pch="|",col=adjustcolor("red",0.5)))
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj,"%m-%Y"))
axis_fun(2,ymaj,ymaj,param.list$variable,cex=0.8)
box(lwd=1)
mtext(side=3,"Bonanza Creek LTER",cex=0.8)
mtext(side=1,line=1.75,"Date (Month-Year)")

cpcrw.basin=read.xlsx(paste0(data.path,"SiSyn_Data_CPCRW.xlsx"),sheet=4,startRow=4,na.strings = "NA")
cpcrw.basin$Latitude=dms2dec(cpcrw.basin$Latitude)
cpcrw.basin$Longitude=dms2dec(cpcrw.basin$Longitude)
names(cpcrw.basin)

vars=c("LTER","Site/Stream","Unique.ID","Latitude","Longitude")
cpcrw.basin=cpcrw.basin[,vars]

#
krr.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_KRR.xlsx"),sheet=1,startRow=2,na.strings = "NA")
krr.dat$Sampling.Date=convertToDate(krr.dat$Sampling.Date)

krr.dat.melt=melt(krr.dat[,c(idvars,param.vars)],id.vars=idvars)
krr.dat.melt=subset(krr.dat,is.na(value)==F)

krr.dat.melt=merge(krr.dat.melt,param.list,"variable")
krr.dat.melt$site=krr.dat.melt$'Site/Stream.Name'
krr.dat.melt$variable=as.character(krr.dat.melt$variable)

krr.dat.inv=ddply(krr.dat.melt,c("Sampling.Date","variable","plot.val"),summarise,N.val=N.obs(site))
xlim.val=as.Date(c("1991-01-01","2020-01-01"));xmaj=seq(xlim.val[1],xlim.val[2],"5 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
ggplot(krr.dat.inv,aes(x=Sampling.Date,y=plot.val))+
  geom_point(shape="|",colour="red",alpha=0.5,size=2)+
  theme(panel.background=element_rect(fill="white"),
        panel.border = element_rect(fill=NA,colour = "black"),
        panel.grid.major=element_line(size=0.5,linetype = 3,colour="grey"),
        axis.line=element_line(colour="black"),
        text=element_text(family="serif"))+
  scale_x_date("Date (Month-Year)",breaks=xmaj,labels=format(xmaj,"%m-%Y"),limits=xlim.val)+
  scale_y_continuous(name="",position="left",breaks=1:length(param.list$variable),labels=param.list$variable)+
  labs(title="Kissimmee River",
       subtitle="Data Inventory")

krr.basin=read.xlsx(paste0(data.path,"SiSyn_Data_KRR.xlsx"),sheet=4,startRow=4,na.strings = "NA")
krr.basin=krr.basin[,vars]
#
arclter.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_ARCLTER_05012020.xlsx"),sheet=1,startRow=2,na.strings = "NA")
arclter.dat$Sampling.Date=convertToDate(arclter.dat$Sampling.Date)

arclter.dat=rename(arclter.dat,c("Dsi"="DSi"))
arclter.dat=melt(arclter.dat[,c(idvars,param.vars)],id.vars=idvars)
arclter.dat=subset(arclter.dat,is.na(value)==F)

arc.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_ARCLTER_05012020.xlsx"),sheet=4,startRow=4,na.strings = "NA")
arc.basin=arc.basin[,vars]
#
lmp.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_LMP.xlsx"),sheet=1,startRow=2,na.strings = "NA")
lmp.dat$Sampling.Date=convertToDate(lmp.dat$Sampling.Date)

lmp.dat.melt=melt(lmp.dat[,c(idvars,param.vars)],id.vars=idvars)
lmp.dat.melt=subset(lmp.dat.melt,is.na(value)==F)

lmp.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_LMP.xlsx"),sheet=4,startRow=4,na.strings = "NA")
lmp.basin=lmp.basin[,vars]
#
luq.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_LUQ.xlsx"),sheet=1,startRow=2,na.strings = "NA")
luq.dat$Sampling.Date=convertToDate(luq.dat$Sampling.Date)

luq.dat.melt=melt(luq.dat[,c(idvars,param.vars)],id.vars=idvars)
luq.dat.melt=subset(luq.dat.melt,is.na(value)==F)

luq.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_LUQ.xlsx"),sheet=4,startRow=4,na.strings = "NA")
luq.basin=luq.basin[,vars]
#
pie.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_PIEWatersheds.xlsx"),sheet=1,startRow=2,na.strings = "NA")
pie.dat$Sampling.Date=convertToDate(pie.dat$Sampling.Date)
pie.dat$TN=NA

pie.dat.melt=melt(pie.dat[,c(idvars,"TDN",param.vars)],id.vars=idvars)
pie.dat.melt=subset(pie.dat.melt,is.na(value)==F)

pie.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_PIEWatersheds.xlsx"),sheet=4,startRow=4,na.strings = "NA")
#lat long got flipped
pie.basin$long=pie.basin$Latitude
pie.basin$lat=pie.basin$Longitude
pie.basin$Longitude=pie.basin$long
pie.basin$Latitude=pie.basin$lat

pie.basin=pie.basin[,vars]

##
sites=rbind(cpcrw.basin,krr.basin)
sites=rbind(sites,arc.basin)
sites=rbind(sites,lmp.basin)
sites=rbind(sites,luq.basin)
sites=rbind(sites,pie.basin)
sites=subset(sites,is.na(Latitude)==F)

sites=SpatialPointsDataFrame(coords=sites[,c("Longitude","Latitude")],
                                   data=sites,
                                   proj4string = wgs84)
tm_shape(sites)+tm_dots()

##