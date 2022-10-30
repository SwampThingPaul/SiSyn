## 
## SiSyn
## Maps
##
## Code was compiled by Paul Julian
## contact info: pjulian@ufl.edu/pauljulianphd@gmail.com/pjulian@sccf.org

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()


#Libraries
library(AnalystHelper);
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(plyr)
library(reshape)
library(openxlsx)
library(lubridate)

#GIS Libraries
library(sp)
library(rgdal)
library(rgeos)
library(tmap)
library(raster)


#Paths
wd="C:/Julian_LaCie/_GitHub/SiSyn/DataInventory"

#https://www.r-bloggers.com/structuring-r-projects/
#https://nicercode.github.io/blog/2013-04-05-projects/

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src/"))
#Folder.Maker(paths[c(1,2,4)]);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paste0(dirname(wd),"/Data/")

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

wgs84=CRS("+init=epsg:4326")
NAD83=CRS("+init=epsg:4269")
WGSutm32=CRS("+init=epsg:32632")
WGSutm33=CRS("+init=epsg:32633")
WGSutm34=CRS("+init=epsg:32634")
WGSutm35=CRS("+init=epsg:32635")
WGSutm36=CRS("+init=epsg:32636")
# -------------------------------------------------------------------------
basin.vars=c("LTER","Site/Stream","Unique.ID","Latitude","Longitude")
site.names=c("LTER", "Country","Biome.Type", "mean.annual.temp","mean.annual.precip","site")
site.all=data.frame()

## ARCLTER
arc.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_ARCLTER_05012020.xlsx"),sheet=4,startRow=4,na.strings = "NA")
arc.basin=arc.basin[,basin.vars]

arc.site=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_ARCLTER_05012020.xlsx"),sheet=5,na.strings = "NA")
arc.site$site=NA
colnames(arc.site)=site.names
site.all=rbind(site.all,arc.site)
## BcZO
bczo.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_BcCZO.xlsx"),sheet=4,startRow=4,na.strings = "NA")
bczo.basin=bczo.basin[,basin.vars]

bczo.site=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_BcCZO.xlsx"),sheet=5,na.strings = "NA")
bczo.site$site=NA
colnames(bczo.site)=site.names
site.all=rbind(site.all,bczo.site)
## Carey
carey.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Carey_6.12.20.xlsx"),sheet=4,startRow=4,na.strings = "NA")
carey.basin=carey.basin[,basin.vars]
carey.basin$Longitude=carey.basin$Longitude*-1
carey.basin$LTER="Ipswich"

carey.site=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Carey_6.12.20.xlsx"),sheet=5,startRow=1,na.strings = "NA")
carey.site$LTER="Ipswich"
carey.site$site=NA
colnames(carey.site)=site.names
site.all=rbind(site.all,carey.site)
## Coal Creek
coal.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Coal_Creek.xlsx"),sheet=4,startRow=4,na.strings = "NA")
coal.basin=coal.basin[,basin.vars]
coal.basin$Latitude=dms2dec(coal.basin$Latitude)
coal.basin$Latitude=coal.basin$Latitude*-1
coal.basin$Longitude=dms2dec(coal.basin$Longitude)
coal.basin$LTER="Coal Crk"

coal.site=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Coal_Creek.xlsx"),sheet=5,startRow=1,na.strings = "NA")
coal.site$`Mean.annual.temp.(⁰)`=0.9
coal.site$`Mean.annual.precip.(mm)`=612+(551*10)
coal.site$site=NA
colnames(coal.site)=site.names
site.all=rbind(site.all,coal.site)

## CPCRW
cpcrw.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_CPCRW.xlsx"),sheet=4,startRow=4,na.strings = "NA")
cpcrw.basin=cpcrw.basin[,basin.vars]
cpcrw.basin$Latitude=dms2dec(cpcrw.basin$Latitude)
cpcrw.basin$Longitude=dms2dec(cpcrw.basin$Longitude)

cpcrw.site=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_CPCRW.xlsx"),sheet=5,startRow=1,na.strings = "NA")
cpcrw.site$site=NA
colnames(cpcrw.site)=site.names
site.all=rbind(site.all,cpcrw.site)
#Knonza
konza.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Konza.xlsx"),sheet=4,startRow=4,na.strings = "NA")
konza.basin=konza.basin[,basin.vars]
konza.basin$Latitude=dms2dec(konza.basin$Latitude)
konza.basin$Longitude=dms2dec(konza.basin$Longitude)
konza.basin[4,4]=konza.basin[4,4]*-1

konza.site=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Konza.xlsx"),sheet=5,startRow=1,na.strings = "NA")
konza.site$site=NA
colnames(konza.site)=site.names
site.all=rbind(site.all,konza.site)

## KRR
KRR.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_KRR.xlsx"),sheet=4,startRow=4,na.strings = "NA")
KRR.basin=KRR.basin[,basin.vars]
KRR.basin$LTER="KRR"

KRR.site=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_KRR.xlsx"),sheet=5,startRow=1,na.strings = "NA")
KRR.site$LTER="KRR"
KRR.site$site=NA
colnames(KRR.site)=site.names
site.all=rbind(site.all,KRR.site)
## MCM
MCM.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_MCM.xlsx"),sheet=4,startRow=4,na.strings = "NA")
MCM.basin=MCM.basin[,basin.vars]

MCM.site=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_MCM.xlsx"),sheet=5,startRow=1,na.strings = "NA")
MCM.site$`Mean.annual.precip.(mm)`=50
MCM.site$site=NA
colnames(MCM.site)=site.names
site.all=rbind(site.all,MCM.site)
## NWT
NWT.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_NWT.xlsx"),sheet=4,startRow=4,na.strings = "NA")
NWT.basin=NWT.basin[,basin.vars]

NWT.site=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_NWT.xlsx"),sheet=5,startRow=1,na.strings = "NA")
NWT.site$site=NA
colnames(NWT.site)=site.names
site.all=rbind(site.all,NWT.site)
## LMP
LMP.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_LMP.xlsx"),sheet=4,startRow=4,na.strings = "NA")
LMP.basin=LMP.basin[,basin.vars]

LMP.site=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_LMP.xlsx"),sheet=5,startRow=1,na.strings = "NA")
colnames(LMP.site)=site.names
site.all=rbind(site.all,LMP.site)
## LUQ
LUQ.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_LUQ.xlsx"),sheet=4,startRow=4,na.strings = "NA")
LUQ.basin=LUQ.basin[,basin.vars]
LUQ.basin=subset(LUQ.basin,is.na(Latitude)==F)

LUQ.site=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_LUQ.xlsx"),sheet=5,startRow=1,na.strings = "NA")
colnames(LUQ.site)=site.names
site.all=rbind(site.all,LUQ.site)

## PIE
pie.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_PIEWatersheds.xlsx"),sheet=4,startRow=4,na.strings = "NA")
pie.basin=pie.basin[,basin.vars]

#lat long got flipped
pie.basin$long=pie.basin$Latitude
pie.basin$lat=pie.basin$Longitude
pie.basin$Longitude=pie.basin$long
pie.basin$Latitude=pie.basin$lat

pie.basin=pie.basin[,basin.vars]

pie.site=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_PIEWatersheds.xlsx"),sheet=5,startRow=1,na.strings = "NA")
pie.site$site=NA
colnames(pie.site)=site.names
site.all=rbind(site.all,pie.site)
# HJ Andrews
hja.basin=read.xlsx(paste0(data.path,"HJAndrewsSiSyn.xlsx"),sheet=4,startRow=4,na.strings = "NA")
hja.basin=hja.basin[,basin.vars]

hja.site=read.xlsx(paste0(data.path,"HJAndrewsSiSyn.xlsx"),sheet=5,startRow=1,na.strings = "NA")
hja.site$site=NA
colnames(hja.site)=site.names
site.all=rbind(site.all,hja.site)
# Sagehen
sage.basin=read.xlsx(paste0(data.path,"SagehenSiSyn.xlsx"),sheet=4,startRow=4,na.strings = "NA")
sage.basin=sage.basin[,basin.vars]
sage.basin$Longitude=sage.basin$Longitude*-1
sage.basin$LTER="Sagehen"
sage.site=read.xlsx(paste0(data.path,"SagehenSiSyn.xlsx"),sheet=5,startRow=1,na.strings = "NA")
sage.site=data.frame(LTER="Sagehen",
                     Country=NA,
                     Biome.Type=NA,
                     mean.annual.temp=NA,
                     mean.annual.precip=NA,
                     Site.Stream=NA
)

colnames(sage.site)=site.names
site.all=rbind(site.all,sage.site)
# UMR
umr.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_UMR.xlsx"),sheet=4,startRow=4,na.strings = "NA")
umr.basin=umr.basin[,basin.vars]
umr.basin$LTER="UMR"

umr.site=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_UMR.xlsx"),sheet=5,startRow=1,na.strings = "NA")
umr.site$LTER="UMR"
umr.site$site=NA
colnames(umr.site)=site.names
site.all=rbind(site.all,umr.site)
# Tanguro
tango.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Tanguro.xlsx"),sheet=4,startRow=4,na.strings = "NA")
tango.basin=tango.basin[,basin.vars]

tango.site=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Tanguro.xlsx"),sheet=5,startRow=1,na.strings = "NA")
tango.site$site=NA
colnames(tango.site)=site.names
site.all=rbind(site.all,tango.site)
# HBR
HBR.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_HBR.xlsx"),sheet=4,startRow=4,na.strings = "NA")
HBR.basin=HBR.basin[,basin.vars]

HBR.extent=data.frame(site=paste0("WS",1:9),
                      west=c(-71.730,-71.728,-71.725,-71.737,-71.739,-71.743,-71.773,-71.762,-71.758),
                      east=c(-71.725,-71.723,-71.717,-71.725,-71.731,-71.735,-71.758,-71.752,-71.742),
                      noth=c(43.952,43.953,43.955,43.949,43.950,43.950,43.916,43.918,43.916),
                      soth=c(43.959,43.960,43.962,43.958,43.957,43.957,43.928,43.930,43.926))

tmp=apply(HBR.extent[,2:5], 1, FUN = function(x) as(extent(x), "SpatialPolygons"))

for(i in 1:9){
  tmp2=coordinates(gCentroid(tmp[[i]]))
  HBR.basin$Longitude[i]=tmp2[1]
  HBR.basin$Latitude[i]=tmp2[2]
}

HBR.site=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_HBR.xlsx"),sheet=5,startRow=1,na.strings = "NA")
HBR.site$site=NA
colnames(HBR.site)=site.names
site.all=rbind(site.all,HBR.site)
# GRO
GRO.basin=data.frame(LTER="GRO",
                     Site.Stream=c("Kolyma", "Lena", "Mackenzie", "Yenisey", "Yukon", "Ob'"),
                     Unique.ID=NA,
                     Latitude=c(68.75,66.77,67.45,69.38,61.93,66.63),
                     Longitude=c(161.30,127.37,-133.74,86.15,-162.88,66.60))

GRO.site=data.frame(LTER="GRO",
                    Country=NA,
                    Biome.Type=NA,
                    mean.annual.temp=NA,
                    mean.annual.precip=NA,
                    Site.Stream=c("Kolyma", "Lena", "Mackenzie", "Yenisey", "Yukon", "Ob'")
)
colnames(GRO.site)=site.names
site.all=rbind(site.all,GRO.site)

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
sites=rbind(sites,hja.basin)
sites=rbind(sites,sage.basin)
sites=rbind(sites,umr.basin)
sites=rbind(sites,tango.basin)
sites=rbind(sites,HBR.basin)
colnames(sites)<-c("LTER","Site.Stream","Unique.ID","Latitude","Longitude")
sites=rbind(sites,GRO.basin)

sites$Longitude=as.numeric(sites$Longitude)

## sites files from Keira
new.sites=read.csv(paste0(data.path,"NewSites_Sept2022.csv"))
colnames(new.sites)=c("Site", "Name", "Source", "Lat", "Long", "State", "Flow.Interpolation", 
                      "Proportion.of.Record.Needing.Interpolation", "Q.downloaded", 
                      "Si.Downloaded", "INFO.created", "UTM.zone", "Potential.Contacts..USGS.", 
                      "Email")
tmp=sapply(strsplit(new.sites$Lat,"Â"),"[",2)
new.sites$DMS=ifelse(is.na(tmp)==F,1,0)

##
utm.convert.fun=function(data,crs,con.crs){
  new.sites.utm1=SpatialPointsDataFrame(data[,c("Lat","Long")],
                                        data=data,
                                        proj4string = crs)
  new.sites.utm1=spTransform(new.sites.utm1,con.crs)
  new.sites.utm1$Long.DD=coordinates(new.sites.utm1)[,1]
  new.sites.utm1$Lat.DD=coordinates(new.sites.utm1)[,2]
  return(new.sites.utm1)
  
}

vars=c("Site","Name","Lat","Long","State","UTM.zone")
new.sites.utm=subset(new.sites,is.na(UTM.zone)==F)[,vars]
new.sites.utm$Lat=as.numeric(new.sites.utm$Lat)
new.sites.utm$Long=as.numeric(new.sites.utm$Long)

tmp=SpatialPointsDataFrame(new.sites.utm[,c("Lat","Long")],
                           data=new.sites.utm[,vars],
                           proj4string = WGSutm32)
coordinates(tmp)[,
                 1]
plot(tmp)


new.sites.utm=bind(
  utm.convert.fun(subset(new.sites.utm,UTM.zone==32),WGSutm32,wgs84),
  utm.convert.fun(subset(new.sites.utm,UTM.zone==33),WGSutm33,wgs84),
  utm.convert.fun(subset(new.sites.utm,UTM.zone==34),WGSutm34,wgs84),
  utm.convert.fun(subset(new.sites.utm,UTM.zone==35),WGSutm35,wgs84),
  utm.convert.fun(subset(new.sites.utm,UTM.zone==36),WGSutm36,wgs84)
)

##
vars=c("Site","Name","Lat","Long","State")
new.sites.DMS=subset(new.sites, DMS==1)[,vars]

lat.split=strsplit(new.sites.DMS$Lat,"\\Â°|\\'")
long.split=strsplit(new.sites.DMS$Long,"\\Â°|\\'")

DMS.sep=data.frame(lat.deg=sapply(lat.split,"[",1),
           lat.min=as.numeric(sapply(lat.split,"[",2)),
           lat.sec=as.numeric(sapply(strsplit(sapply(lat.split,"[",3),'\"'),"[",1)),
           long.deg=sapply(long.split,"[",1),
           long.min=as.numeric(sapply(long.split,"[",2)),
           long.sec=as.numeric(sapply(strsplit(sapply(long.split,"[",3),'\"'),"[",1)))
DMS.sep$Lat.DD=with(DMS.sep,as.numeric(lat.deg) + (as.numeric(lat.min) / 60) + (as.numeric(lat.sec) / 3600))
DMS.sep$Long.DD=with(DMS.sep,as.numeric(long.deg) + (as.numeric(long.min) / 60) + (as.numeric(long.sec) / 3600))*-1

new.sites.DMS$Lat.DD=DMS.sep$Lat.DD
new.sites.DMS$Long.DD=DMS.sep$Long.DD

new.sites.DMS=SpatialPointsDataFrame(new.sites.DMS[,c("Long.DD","Lat.DD")],
                                     data=new.sites.DMS,
                                     proj4string = wgs84)


##
new.sites.other=subset(new.sites, DMS!=1&is.na(UTM.zone)==T)[,vars]
new.sites.other$Lat=as.numeric(new.sites.other$Lat)
new.sites.other$Long=as.numeric(new.sites.other$Long)
new.sites.other=subset(new.sites.other,is.na(Lat)==F)
new.sites.other=SpatialPointsDataFrame(new.sites.other[,c("Long","Lat")],
                                     data=new.sites.other,
                                     proj4string = wgs84)
new.sites.other$Long.DD=coordinates(new.sites.other)[,1]
new.sites.other$Lat.DD=coordinates(new.sites.other)[,2]

vars=c("Site", "Name", "Long.DD", "Lat.DD","State")
new.sites2.shp=bind(
  new.sites.utm[,vars],
  new.sites.DMS[,vars],
  new.sites.other[,vars]
)

sites2=new.sites2.shp@data
sites2$Unique.ID=NA
sites2$Site=with(sites2,ifelse(State=="Finland","Finland",Site))
sites2=sites2[,c("Site","Name","Unique.ID", "Long.DD","Lat.DD")]
colnames(sites2)=c("LTER","Site.Stream","Unique.ID","Long.DD", "Lat.DD")

sites2=SpatialPointsDataFrame(sites2[,c("Long.DD","Lat.DD")],
                              sites2,proj4string = wgs84)



names(sites)
sites.orginal=sites[,c("LTER","Site.Stream","Unique.ID", "Latitude", "Longitude")]
colnames(sites.orginal)=c("LTER","Site.Stream","Unique.ID", "Long.DD", "Lat.DD")

sites.orginal=subset(sites.orginal,is.na(Long.DD)==F)
sites.orginal=subset(sites.orginal,is.na(Lat.DD)==F)
sites.orginal=SpatialPointsDataFrame(sites.orginal[,c("Lat.DD","Long.DD")],
                                     sites.orginal,proj4string = wgs84)


sites.all=bind(sites.orginal,sites2)
sites.all=subset(sites.all,is.na(Long.DD)==F)
sites.all=subset(sites.all,is.na(Lat.DD)==F)
# sites.all.sph=SpatialPointsDataFrame(sites.all[,c("Lat.DD","Long.DD")],
#                                      sites.all,proj4string = wgs84)
unique(sites.all$LTER)

LTER2=data.frame(LTER=sort(unique(sites.all$LTER)),
                 LTER.alias=c(rep("USGS",17), "AND", 
                              "ARC", "BcCZO", "BNZ", "Catalina Jemez", "Coal Crk", "ERSFA", 
                              "Finland", "GRO", "HBR", "Ipswich", "KNZ", "KRR", "Krycklan", 
                              "LMP", "LUQ", "MCM", "NIVA", "NWT", "PIE", "Sagehen", "Tanguro", 
                              "UMR", "Walker Branch"))
sites.all=merge(sites.all,LTER2,"LTER")
LTERs.fact=c("USGS","AND", 
             "ARC", "BcCZO", "BNZ", "Catalina Jemez", "Coal Crk", "ERSFA", 
             "Finland", "GRO", "HBR", "Ipswich", "KNZ", "KRR", "Krycklan", 
             "LMP", "LUQ", "MCM", "NIVA", "NWT", "PIE", "Sagehen", "Tanguro", 
             "UMR", "Walker Branch")

sites.all$LTER.alias=as.factor(sites.all$LTER.alias)
sites.all$LTER.alias=factor(sites.all$LTER.alias,levels=LTERs.fact)

# write.csv(sites.all@data,paste0(export.path,"site_latlong.csv"),row.names = F)

### 

library(rnaturalearth)
world <- ne_countries(scale = "small", returnclass = "sp")
world=spTransform(world,wgs84)

cols=c(adjustcolor("forestgreen",0.5),viridis::plasma(length(LTERs.fact)-1,alpha=0.5))
cols=c(adjustcolor("forestgreen",0.5),rainbow(length(LTERs.fact)-1,alpha=0.5))
site.cols=cols[sites.all$LTER.alias]

# png(filename=paste0(plot.path,"Sites_all.png"),width=5.75,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.25,0.25,0.25,0.25),mar=c(0.3,0.1,0.1,0.1),xpd=F)
layout(matrix(1:2,2,1,byrow=T),heights=c(1,0.5))

AOI=raster::extent(world)
AOI.poly=as(AOI,"SpatialPolygons")
proj4string(AOI.poly)=wgs84

bbox.lims=bbox(world)
asp.val=mapasp(world,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(world,col="grey80",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)#,asp=asp.val)
plot(sites.all,add=T,pch=21,bg=site.cols,col=adjustcolor("white",0.5),lwd=0.01,cex=0.75)
# box(lwd=1)
llgridlines(world,
            llcrs=as.character(crs(world)),
            col="grey50",xpd=NA,offset=0.25,lwd=0.75,plotLabels =F)
llgridlines(world,
            llcrs=as.character(crs(world)),
            col="black",xpd=NA,offset=0.25,plotLabels =T,plotLines=F)
mapmisc::scaleBar(wgs84,"bottom",bty="n",cex=1,seg.len=4,outer=F)
plot(AOI.poly,add=T,lwd=0.75)

plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=LTERs.fact,
       pt.bg=cols,pch=21,lty=0,lwd=0.1,col="white",
       pt.cex=1.5,ncol=5,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " LTER/Site/Data Provider")
dev.off()