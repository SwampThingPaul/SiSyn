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
# subset(sites,is.na(Longitude)==T)
sites=subset(sites,is.na(Longitude)==F)


sites=SpatialPointsDataFrame(coords=sites[,c("Longitude","Latitude")],
                             data=sites,
                             proj4string = wgs84)

site.climate=ddply(site.all,c("LTER"),summarise,
                   mean.temp=mean(mean.annual.temp,na.rm=T),
                   mean.precip=mean(mean.annual.precip,na.rm=T))
site.climate$mean.temp=with(site.climate,ifelse(LTER=="Ipswich",10,mean.temp))# based on notes, Near PIE
site.climate$mean.precip=with(site.climate,ifelse(LTER=="Ipswich",1188,mean.precip))# based on notes, Near PIE
site.climate$LTER=with(site.climate,ifelse(LTER=="DOE SFA East River","Coal Crk",LTER))
site.climate$LTER=as.factor(site.climate$LTER)
# write.csv(site.climate,paste0(export.path,"site_climate.csv"),row.names = F)
## Updated file
site.climate=read.csv(paste0(data.path,"MAP_MAT_byLTER.csv"))
site.climate$mean.precip=with(site.climate,ifelse(LTER=="Coal Crk",1163,mean.precip))
site.climate$LTER=as.factor(site.climate$LTER)
site.climate$mean.precip=site.climate$mean.precip*0.1

library(rnaturalearth)
library(ggplot2)
world <- ne_countries(scale = "small", returnclass = "sp")
class(world)

sort.LTERs=ddply(sites@data,"LTER",summarise,mean.lat=mean(Latitude))
sort.LTERs=sort.LTERs[order(-sort.LTERs$mean.lat),]
sort.LTERs$LTER=factor(sort.LTERs$LTER,levels=sort.LTERs$LTER)
sort.LTERs=c("ARC", "GRO", "BNZ", "AND", "HBR", 
             "LMP", "PIE", "Ipswich", "UMR", "NWT", "BcCZO", "Sagehen", "KNZ", 
             "Coal Crk", "KRR", "LUQ", "Tanguro", "MCM")

sites$LTER=as.factor(sites$LTER)
sites$LTER=factor(sites$LTER,levels=sort.LTERs)

site.climate$LTER=factor(site.climate$LTER,levels=sort.LTERs)

cols=wesanderson::wes_palette("Zissou1",length(unique(sites$LTER)),"continuous")
# cols=wesanderson::wes_palette("Darjeeling1",length(unique(sites$LTER)),"continuous")

site.cols.climate=cols[site.climate$LTER]
site.cols=cols[sites$LTER]
# png(filename=paste0(plot.path,"Map_biome_all.png"),width=5.25,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.25,0.25,0.25,0.25),mar=c(0.1,0.1,0.1,0.1),xpd=F)
layout(matrix(c(1,1,2,3),2,2,byrow=T),widths=c(1,0.3),heights=c(0.75,1))

bbox.lims=bbox(sites)
#plot(world,col="cornsilk",bg="lightblue")
plot(world,col="grey80",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
plot(sites,add=T,pch=21,bg=site.cols,col="white",lwd=0.1,cex=1.25)
box(lwd=1)
mapmisc::scaleBar(wgs84,"bottom",bty="n",cex=1,seg.len=4,outer=F)
legend("bottomleft",legend=sort.LTERs,
       pt.bg=cols,pch=21,lty=0,lwd=0.1,col="white",
       pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " LTER")

# devtools::install_github("valentinitnelav/plotbiomes")
library(plotbiomes)
data(Whittaker_biomes)
biome.list=data.frame(biome_id=1:9,
                      biome=c("Tropical seasonal forest/savanna", 
                        "Subtropical desert", "Temperate rain forest", "Tropical rain forest", 
                        "Woodland/shrubland", "Tundra", "Boreal forest", "Temperate grassland/desert", 
                        "Temperate seasonal forest"),
                      Ricklefs.cols=c("#A09700","#DCBB50","#75A95E", "#317A22","#D16E3F","#C1E1DD","#A5C790",
                                      "#FCD57A","#97B669"))
cols2=wesanderson::wes_palette("Zissou1",9,"continuous")
par(mar=c(2.5,3.5,0.5,1),xpd=F)
ylim.val=c(0,500);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(-20,30);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
txt.cex=0.75
plot(precp_cm~temp_c,Whittaker_biomes,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
for(i in 1:9){
  with(subset(Whittaker_biomes,biome_id==i),
       polygon(temp_c,precp_cm,col=adjustcolor(biome.list$Ricklefs.cols[i],0.5),lwd=0.5))
  # with(subset(Whittaker_biomes,biome_id==i),text(mean(temp_c),mean(precp_cm),biome.list$biome[i],cex=0.75))
}
with(site.climate,points(mean.precip~mean.temp,pch=21,bg=site.cols.climate,cex=1.5))
# with(site.climate,text(mean.temp,mean.precip,LTER,pos=3))
with(subset(site.climate,LTER=="Sagehen"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
with(subset(site.climate,LTER=="BcCZO"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
with(subset(site.climate,LTER=="PIE"),text(mean.temp,mean.precip,LTER,pos=4,cex=txt.cex))
with(subset(site.climate,LTER=="UMR"),text(mean.temp,mean.precip,LTER,pos=1,cex=txt.cex))
with(subset(site.climate,LTER=="LMP"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
with(subset(site.climate,LTER=="ARC"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
with(subset(site.climate,!(LTER%in%c("Sagehen","BcCZO","PIE","UMR","LMP","ARC"))),
     text(mean.temp,mean.precip,LTER,pos=3,cex=txt.cex))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Tempature (\u00B0C)")
mtext(side=2,line=2.5,"Precipitation (cm)")

plot(0:1,0:1,type="n",ann=F,axes=F)
biome.lab=c("Tropical seasonal\nforest/savanna", 
        "Subtropical\ndesert", "Temperate\nrain forest", "Tropical\nrain forest", 
        "Woodland/shrubland", "Tundra", "Boreal forest", "Temperate\ngrassland/desert", 
        "Temperate\nseasonal forest")
legend(0.1,0.5,legend=biome.lab,
       pch=22,lwd=0.1,lty=0,
       pt.bg=adjustcolor(biome.list$Ricklefs.cols,0.5),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " Biome")
dev.off()



unique(sites$Site.Stream)
sites2=read.csv(paste0(data.path,"LongTermWatersheds_LatLong.csv"))
sites2$Longitude=with(sites2,ifelse(LTER=="Sagehen",Longitude*-1,Longitude))
unique(sites2$LTER)

sites2=SpatialPointsDataFrame(coords=sites2[,c("Longitude","Latitude")],
                             data=sites2,
                             proj4string = wgs84)
sort.LTERs=c("ARC", "GRO", "BNZ", "AND", "HBR", 
             "LMP", "PIE", "Ipswich", "UMR", "NWT", "BcCZO", "Sagehen", "KNZ", 
             "Coal Crk", "KRR", "LUQ", "Tanguro", "MCM")
sort.LTERs=sort.LTERs[sort.LTERs%in%unique(sites2$LTER)]

# sites2$LTER=as.factor(sites2$LTER)
sites2$LTER=factor(sites2$LTER,levels=sort.LTERs)

site.climate2=subset(site.climate,LTER%in%unique(sites2$LTER))
site.climate2$LTER=factor(site.climate2$LTER,levels=sort.LTERs)
cols=wesanderson::wes_palette("Zissou1",length(unique(sites2$LTER)),"continuous")
site.cols.climate=cols[site.climate2$LTER]
site.cols=cols[sites2$LTER]
# png(filename=paste0(plot.path,"Map_biome_LongTerm.png"),width=5.25,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.25,0.25,0.25,0.25),mar=c(0.1,0.1,0.1,0.1),xpd=F)
layout(matrix(c(1,1,2,3),2,2,byrow=T),widths=c(1,0.3),heights=c(0.75,1))

bbox.lims=bbox(sites)
#plot(world,col="cornsilk",bg="lightblue")
plot(world,col="grey80",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
plot(sites2,add=T,pch=21,bg=site.cols,col="white",lwd=0.1,cex=1.25)
box(lwd=1)
mapmisc::scaleBar(wgs84,"bottom",bty="n",cex=1,seg.len=4,outer=F)
legend("bottomleft",legend=sort.LTERs,
       pt.bg=cols,pch=21,lty=0,lwd=0.1,col="white",
       pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " LTER")

# devtools::install_github("valentinitnelav/plotbiomes")
library(plotbiomes)
data(Whittaker_biomes)
biome.list=data.frame(biome_id=1:9,
                      biome=c("Tropical seasonal forest/savanna", 
                              "Subtropical desert", "Temperate rain forest", "Tropical rain forest", 
                              "Woodland/shrubland", "Tundra", "Boreal forest", "Temperate grassland/desert", 
                              "Temperate seasonal forest"),
                      Ricklefs.cols=c("#A09700","#DCBB50","#75A95E", "#317A22","#D16E3F","#C1E1DD","#A5C790",
                                      "#FCD57A","#97B669"))
cols2=wesanderson::wes_palette("Zissou1",9,"continuous")
par(mar=c(2.5,3.5,0.5,1),xpd=F)
ylim.val=c(0,500);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(-20,30);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
txt.cex=0.75
plot(precp_cm~temp_c,Whittaker_biomes,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
for(i in 1:9){
  with(subset(Whittaker_biomes,biome_id==i),
       polygon(temp_c,precp_cm,col=adjustcolor(biome.list$Ricklefs.cols[i],0.5),lwd=0.5))
  # with(subset(Whittaker_biomes,biome_id==i),text(mean(temp_c),mean(precp_cm),biome.list$biome[i],cex=0.75))
}
with(site.climate2,points(mean.precip~mean.temp,pch=21,bg=site.cols.climate,cex=1.5,lwd=0.01))
# with(site.climate,text(mean.temp,mean.precip,LTER,pos=3))
with(subset(site.climate2,LTER=="Sagehen"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
# with(subset(site.climate2,LTER=="BcCZO"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
# with(subset(site.climate2,LTER=="PIE"),text(mean.temp,mean.precip,LTER,pos=4,cex=txt.cex))
with(subset(site.climate2,LTER=="UMR"),text(mean.temp,mean.precip,LTER,pos=1,cex=txt.cex))
with(subset(site.climate2,LTER=="LMP"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
with(subset(site.climate2,LTER=="ARC"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
with(subset(site.climate2,!(LTER%in%c("Sagehen","BcCZO","PIE","UMR","LMP","ARC"))),
     text(mean.temp,mean.precip,LTER,pos=3,cex=txt.cex))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Temperature (\u00B0C)")
mtext(side=2,line=2.5,"Precipitation (cm)")

plot(0:1,0:1,type="n",ann=F,axes=F)
biome.lab=c("Tropical seasonal\nforest/savanna", 
            "Subtropical\ndesert", "Temperate\nrain forest", "Tropical\nrain forest", 
            "Woodland/shrubland", "Tundra", "Boreal forest", "Temperate\ngrassland/desert", 
            "Temperate\nseasonal forest")
legend(0.1,0.5,legend=biome.lab,
       pch=22,lwd=0.1,lty=0,
       pt.bg=adjustcolor(biome.list$Ricklefs.cols,0.5),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " Biome")
dev.off()

# Polar plots
# https://khufkens.com/2017/01/18/r-polar-plots/
library(maps)
maps2sp = function(xlim, ylim, l.out = 100, clip = TRUE) {
  stopifnot(require(maps))
  m = map(xlim = xlim, ylim = ylim, plot = FALSE, fill = TRUE)
  p = rbind(cbind(xlim[1], seq(ylim[1],ylim[2],length.out = l.out)),
            cbind(seq(xlim[1],xlim[2],length.out = l.out),ylim[2]),
            cbind(xlim[2],seq(ylim[2],ylim[1],length.out = l.out)),
            cbind(seq(xlim[2],xlim[1],length.out = l.out),ylim[1]))
  LL = CRS("+init=epsg:4326")
  bb = SpatialPolygons(list(Polygons(list(Polygon(list(p))),"bb")), proj4string = LL)
  IDs <- sapply(strsplit(m$names, ":"), function(x) x[1])
  stopifnot(require(maptools))
  m <- map2SpatialPolygons(m, IDs=IDs, proj4string = LL)
  if (!clip)
    m
  else {
    stopifnot(require(rgeos))
    gIntersection(m, bb) # cut map slice in WGS84
  }
}
pol.clip=function(dat,xlim, ylim,l.out = 100,CRS.in = CRS("+init=epsg:4326")){
  m = map(xlim = xlim, ylim = ylim, plot = FALSE, fill = TRUE)
  p = rbind(cbind(xlim[1], seq(ylim[1],ylim[2],length.out = l.out)),
            cbind(seq(xlim[1],xlim[2],length.out = l.out),ylim[2]),
            cbind(xlim[2],seq(ylim[2],ylim[1],length.out = l.out)),
            cbind(seq(xlim[2],xlim[1],length.out = l.out),ylim[1]))
  LL = CRS.in
  bb = SpatialPolygons(list(Polygons(list(Polygon(list(p))),"bb")), proj4string = LL)
  gIntersection(dat, bb)
}

Npolar = CRS("+init=epsg:3995")
Spolar = CRS("+init=epsg:3031")

ice.shelf=readOGR("C:/Julian_LaCie/_GISData/NaturalEarthData/50m","ne_50m_antarctic_ice_shelves_polys")
glac.dat=readOGR("C:/Julian_LaCie/_GISData/NaturalEarthData/50m","ne_50m_glaciated_areas")
lake.dat=readOGR("C:/Julian_LaCie/_GISData/NaturalEarthData/50m","ne_50m_lakes")
rivers.dat=readOGR("C:/Julian_LaCie/_GISData/NaturalEarthData/10m","ne_10m_rivers_lake_centerlines")
world <- ne_countries(scale = 50, returnclass = "sp")

# png(filename=paste0(plot.path,"Map_biome_LongTerm_polar.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.25,0.25,0.25,0.25),mar=c(0.1,0.1,0.1,0.1),xpd=F)
layout(matrix(1:4,2,2,byrow=T),widths=c(1.25,0.5),heights=c(0.8,1))

bbox.lims=bbox(sites)
#plot(world,col="cornsilk",bg="lightblue")
plot(world,col="grey80",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
plot(sites2,add=T,pch=21,bg=site.cols,col="white",lwd=0.1,cex=1.25)

box(lwd=1)
mapmisc::scaleBar(wgs84,"bottom",bty="n",cex=1,seg.len=4,outer=F)
legend("bottomleft",legend=sort.LTERs,
       pt.bg=cols,pch=21,lty=0,lwd=0.1,col="white",
       pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " LTER")

m=maps2sp(xlim=c(-180,180),ylim=c(60,120))
gl = gridlines(m, easts = seq(-180,180,20))
plot(spTransform(m,Npolar),lwd=0.01,col="grey90",border="grey")
plot(spTransform(pol.clip(glac.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col=adjustcolor("lightblue1",0.5),border="lightblue1",lwd=0.1,add=T)
plot(spTransform(pol.clip(lake.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col="lightblue",border="dodgerblue1",lwd=0.1,add=T)
plot(spTransform(pol.clip(rivers.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col="skyblue",add=T,lwd=0.8)
# plot(spTransform(ice.shelf,Npolar),col="skyblue",border="blue",add=T,lty=2)
gl.polar = spTransform(gl, Npolar)
lines(gl.polar,lty=2,lwd=0.25,col=adjustcolor("grey50",0.5))
# raster::text(spTransform(tmp,Npolar),"Site.Stream",pos=1,halo=T)
plot(spTransform(subset(sites2,LTER%in%c("ARC",'GRO')),Npolar),pch=21,bg=cols[subset(sites2,LTER%in%c("ARC",'GRO'))$LTER],cex=1.25,lwd=0.1,add=T)
raster::text(spTransform(subset(sites2,Stream.Site=="TW Weir"),Npolar),"LTER",halo=T,pos=2,cex=0.75)
raster::text(spTransform(subset(sites2,LTER%in%c('GRO')),Npolar),"LTER",halo=T,pos=2,cex=0.75)
box(lwd=1)
mapmisc::scaleBar(Npolar,"bottomleft",bty="n",cex=1,seg.len=4,outer=T)

cols2=wesanderson::wes_palette("Zissou1",9,"continuous")
par(mar=c(2.5,3.5,0.5,1),xpd=F)
ylim.val=c(0,500);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(-20,30);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
txt.cex=0.75
plot(precp_cm~temp_c,Whittaker_biomes,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
for(i in 1:9){
  with(subset(Whittaker_biomes,biome_id==i),
       polygon(temp_c,precp_cm,col=adjustcolor(biome.list$Ricklefs.cols[i],0.5),lwd=0.5))
  # with(subset(Whittaker_biomes,biome_id==i),text(mean(temp_c),mean(precp_cm),biome.list$biome[i],cex=0.75))
}
with(site.climate2,points(mean.precip~mean.temp,pch=21,bg=site.cols.climate,cex=1.5,lwd=0.01))
# with(site.climate,text(mean.temp,mean.precip,LTER,pos=3))
with(subset(site.climate2,LTER=="Sagehen"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
# with(subset(site.climate2,LTER=="BcCZO"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
# with(subset(site.climate2,LTER=="PIE"),text(mean.temp,mean.precip,LTER,pos=4,cex=txt.cex))
with(subset(site.climate2,LTER=="UMR"),text(mean.temp,mean.precip,LTER,pos=1,cex=txt.cex))
with(subset(site.climate2,LTER=="LMP"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
with(subset(site.climate2,LTER=="ARC"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
with(subset(site.climate2,!(LTER%in%c("Sagehen","BcCZO","PIE","UMR","LMP","ARC"))),
     text(mean.temp,mean.precip,LTER,pos=3,cex=txt.cex))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Temperature (\u00B0C)")
mtext(side=2,line=2.5,"Precipitation (cm)")

plot(0:1,0:1,type="n",ann=F,axes=F)
biome.lab=biome.list$biome# c("Tropical seasonal\nforest/savanna", 
            # "Subtropical\ndesert", "Temperate\nrain forest", "Tropical\nrain forest", 
            # "Woodland/shrubland", "Tundra", "Boreal forest", "Temperate\ngrassland/desert", 
            # "Temperate\nseasonal forest")
legend(0.1,0.5,legend=biome.lab,
       pch=22,lwd=0.1,lty=0,
       pt.bg=adjustcolor(biome.list$Ricklefs.cols,0.5),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " Biome")
dev.off()


si.dat=read.csv(paste0(export.path,"20210907_masterdata.csv"))
sort.LTERs.df=data.frame(LTER=c("ARC", "GRO","AND","HBR","LMP(Wymore)",
                  "UMR(Jankowski)","NWT","Sagehen(Sullivan)",
                  "KRR(Julian)","LUQ","MCM"),
  LTER2=sort.LTERs)
si.dat=subset(si.dat,LTER%in%sort.LTERs.df$LTER)
si.dat=merge(si.dat,sort.LTERs.df,"LTER",all.x=T)
si.dat$LTER=factor(si.dat$LTER,levels=sort.LTERs)

boxplot(value~LTER2,subset(si.dat,variable=="DSi"),outline=F)

mean.dat=ddply(subset(si.dat,variable=="DSi"),"LTER2",summarise,mean.val=mean(value,na.rm=T))
mean.dat$pch.val=log(mean.dat$mean.val)/max(log(mean.dat$mean.val))*2
mean.dat$pch.val2=1+scale(mean.dat$mean.val)
# png(filename=paste0(plot.path,"Map_biome_LongTerm_polar_v2.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.25,0.25,0.25,0.25),mar=c(0.1,0.1,0.1,0.1),xpd=F)
# layout(matrix(c(1,1,2:5),2,3,byrow=T),widths=c(1.25,0.4,0.4),heights=c(0.8,1))
# layout(matrix(1:4,2,2,byrow=T),widths=c(1.25,0.5),heights=c(0.8,1))
layout(matrix(1:4,2,2,byrow=T),widths=c(1.25,0.65),heights=c(0.8,1))

bbox.lims=bbox(sites)
#plot(world,col="cornsilk",bg="lightblue")
plot(world,col="grey80",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
plot(sites2,add=T,pch=21,bg=site.cols,col="white",lwd=0.1,cex=1.25)

box(lwd=1)
mapmisc::scaleBar(wgs84,"bottom",bty="n",cex=1,seg.len=4,outer=F)
legend("bottomleft",legend=sort.LTERs,
       pt.bg=cols,pch=21,lty=0,lwd=0.1,col="white",
       pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " LTER")

m=maps2sp(xlim=c(-180,180),ylim=c(60,120))
gl = gridlines(m, easts = seq(-180,180,20))
plot(spTransform(m,Npolar),lwd=0.01,col="grey90",border="grey")
plot(spTransform(pol.clip(glac.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col=adjustcolor("lightblue1",0.5),border="lightblue1",lwd=0.1,add=T)
plot(spTransform(pol.clip(lake.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col="lightblue",border="dodgerblue1",lwd=0.1,add=T)
plot(spTransform(pol.clip(rivers.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col="skyblue",add=T,lwd=0.8)
# plot(spTransform(ice.shelf,Npolar),col="skyblue",border="blue",add=T,lty=2)
gl.polar = spTransform(gl, Npolar)
lines(gl.polar,lty=2,lwd=0.25,col=adjustcolor("grey50",0.5))
# raster::text(spTransform(tmp,Npolar),"Site.Stream",pos=1,halo=T)
plot(spTransform(subset(sites2,LTER%in%c("ARC",'GRO')),Npolar),pch=21,
     bg=cols[subset(sites2,LTER%in%c("ARC",'GRO'))$LTER],cex=1.25,lwd=0.1,add=T)
raster::text(spTransform(subset(sites2,Stream.Site=="TW Weir"),Npolar),"LTER",halo=T,pos=2,cex=0.75)
raster::text(spTransform(subset(sites2,LTER%in%c('GRO')),Npolar),"LTER",halo=T,pos=2,cex=0.75)
box(lwd=1)
mapmisc::scaleBar(Npolar,"bottomleft",bty="n",cex=1,seg.len=4,outer=T)

cols2=wesanderson::wes_palette("Zissou1",9,"continuous")
par(mar=c(2.5,3.5,0.5,1),xpd=F)
ylim.val=c(0,500);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(-20,30);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
txt.cex=0.75
plot(precp_cm~temp_c,Whittaker_biomes,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
for(i in 1:9){
  with(subset(Whittaker_biomes,biome_id==i),
       polygon(temp_c,precp_cm,col=adjustcolor(biome.list$Ricklefs.cols[i],0.5),lwd=0.5))
  # with(subset(Whittaker_biomes,biome_id==i),text(mean(temp_c),mean(precp_cm),biome.list$biome[i],cex=0.75))
}
with(site.climate2,points(mean.precip~mean.temp,pch=21,bg=site.cols.climate,cex=mean.dat$pch.val,lwd=0.01))
# with(site.climate2,points(mean.precip~mean.temp,pch=21,bg=site.cols.climate,cex=mean.dat$pch.val2,lwd=0.01))
# with(site.climate,text(mean.temp,mean.precip,LTER,pos=3))
with(subset(site.climate2,LTER=="Sagehen"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
# with(subset(site.climate2,LTER=="BcCZO"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
# with(subset(site.climate2,LTER=="PIE"),text(mean.temp,mean.precip,LTER,pos=4,cex=txt.cex))
with(subset(site.climate2,LTER=="UMR"),text(mean.temp,mean.precip,LTER,pos=1,cex=txt.cex))
with(subset(site.climate2,LTER=="LMP"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
with(subset(site.climate2,LTER=="ARC"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
with(subset(site.climate2,!(LTER%in%c("Sagehen","BcCZO","PIE","UMR","LMP","ARC"))),
     text(mean.temp,mean.precip,LTER,pos=3,cex=txt.cex))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Temperature (\u00B0C)")
mtext(side=2,line=2.5,"Precipitation (cm)")

plot(0:1,0:1,type="n",ann=F,axes=F)
biome.lab=biome.list$biome# c("Tropical seasonal\nforest/savanna", 
# "Subtropical\ndesert", "Temperate\nrain forest", "Tropical\nrain forest", 
# "Woodland/shrubland", "Tundra", "Boreal forest", "Temperate\ngrassland/desert", 
# "Temperate\nseasonal forest")
legend(0.1,0.5,legend=biome.lab,
       pch=22,lwd=0.1,lty=0,
       pt.bg=adjustcolor(biome.list$Ricklefs.cols,0.5),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " Biome")

# plot(0:1,0:1,type="n",ann=F,axes=F)
round(subset(mean.dat,pch.val==max(pch.val))$mean.val,0)

legend(0.8,0.5,legend=c("High",NA,"Low"),
       pch=c(21,NA,21),lwd=0.1,lty=0,
       pt.bg="grey",
       pt.cex=c(2,NA,1),ncol=1,cex=0.75,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " Relative DSi\nConcentration")

dev.off()



biome.rename1=read.csv(paste0(data.path,"Biome.csv"))
colnames(biome.rename1)=c("LTER",'SITE',"Biome_new")

biome.rename=ddply(biome.rename1, c("LTER","Biome_new"),summarise,N.val=N.obs(LTER))
unique(biome.rename$LTER)
biome.rename$LTER=factor(biome.rename$LTER,levels=sort.LTERs)

biome.rename.sort=c(
  "Tropical rainforest","Tropical savanna",
  "Temperate coniferous forest","Temperate deciduous forest",
  "Temperate grassland","Alpine tundra", "Arctic tundra",
  "Boreal forest","Polar desert"
)
biome.cols=c("lawngreen", "goldenrod1", 
             "darkgreen", "darkorange2", 
             "firebrick1", "mediumpurple3", 
              "dodgerblue4","lightsteelblue3","ivory3")
  
biome.rename$Biome_new=factor(biome.rename$Biome_new,levels=biome.rename.sort)

# biome.cols=adjustcolor(biome.cols,0.5)
site.climate2[site.climate2$LTER=="ARC","mean.precip"]=35

sites.climate3=merge(site.climate2,biome.rename,by="LTER")
sites.climate3$Biome_new

# png(filename=paste0(plot.path,"Map_biome_LongTerm_polar_v3.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.25,0.25,0.25,0.25),mar=c(0.1,0.1,0.1,0.1),xpd=F)
layout(matrix(1:4,2,2,byrow=T),widths=c(1.25,0.65),heights=c(0.8,1))

bbox.lims=bbox(sites)
plot(world,col="grey80",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
plot(sites2,add=T,pch=21,bg=site.cols,col="white",lwd=0.1,cex=1.25)

box(lwd=1)
mapmisc::scaleBar(wgs84,"bottom",bty="n",cex=1,seg.len=4,outer=F)
legend("bottomleft",legend=sort.LTERs,
       pt.bg=cols,pch=21,lty=0,lwd=0.1,col="white",
       pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " Site")

m=maps2sp(xlim=c(-180,180),ylim=c(60,120))
gl = gridlines(m, easts = seq(-180,180,20))
plot(spTransform(m,Npolar),lwd=0.01,col="grey90",border="grey")
plot(spTransform(pol.clip(glac.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col=adjustcolor("lightblue1",0.5),border="lightblue1",lwd=0.1,add=T)
plot(spTransform(pol.clip(lake.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col="lightblue",border="dodgerblue1",lwd=0.1,add=T)
plot(spTransform(pol.clip(rivers.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col="skyblue",add=T,lwd=0.8)
# plot(spTransform(ice.shelf,Npolar),col="skyblue",border="blue",add=T,lty=2)
gl.polar = spTransform(gl, Npolar)
lines(gl.polar,lty=2,lwd=0.25,col=adjustcolor("grey50",0.5))
# raster::text(spTransform(tmp,Npolar),"Site.Stream",pos=1,halo=T)
plot(spTransform(subset(sites2,LTER%in%c("ARC",'GRO')),Npolar),pch=21,
     bg=cols[subset(sites2,LTER%in%c("ARC",'GRO'))$LTER],cex=1.25,lwd=0.1,add=T)
raster::text(spTransform(subset(sites2,Stream.Site=="TW Weir"),Npolar),"LTER",halo=T,pos=2,cex=0.75)
raster::text(spTransform(subset(sites2,LTER%in%c('GRO')),Npolar),"LTER",halo=T,pos=2,cex=0.75)
box(lwd=1)
mapmisc::scaleBar(Npolar,"bottomleft",bty="n",cex=1,seg.len=4,outer=T)


par(mar=c(2.5,3.5,0.5,1),xpd=F)
ylim.val=c(0,400);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(-20,30);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
txt.cex=0.75
plot(precp_cm~temp_c,Whittaker_biomes,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(v=0,h=0)
with(subset(sites.climate3,LTER!="UMR"),points(mean.temp,mean.precip,pch=21,
       bg=biome.cols[Biome_new],cex=1.5,lwd=0.01))
with(subset(sites.climate3,LTER=="UMR"&Biome_new=="Temperate deciduous forest"),
     points(mean.temp+0.5,mean.precip-5,pch=21,
            bg=biome.cols[Biome_new],cex=1.5,lwd=0.01))
with(subset(sites.climate3,LTER=="UMR"&Biome_new=="Temperate grassland"),
     points(mean.temp-0.5,mean.precip+5,pch=21,
            bg=biome.cols[Biome_new],cex=1.5,lwd=0.01))
with(subset(site.climate2,LTER=="Sagehen"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
with(subset(site.climate2,LTER=="UMR"),text(mean.temp,mean.precip,LTER,pos=4,cex=txt.cex))
with(subset(site.climate2,LTER=="LMP"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
with(subset(site.climate2,LTER=="ARC"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
with(subset(site.climate2,!(LTER%in%c("Sagehen","BcCZO","PIE","UMR","LMP","ARC"))),
     text(mean.temp,mean.precip,LTER,pos=3,cex=txt.cex))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Temperature (\u00B0C)")
mtext(side=2,line=2.5,"Precipitation (cm)")

plot(0:1,0:1,type="n",ann=F,axes=F)
legend("center",legend=biome.rename.sort,
       pch=22,lwd=0.1,lty=0,
       pt.bg=biome.cols,
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " Biome")
dev.off()



sites2$Stream.Site
subset(sites2,LTER=="MCM")@data
subset(biome.rename1,LTER=="MCM")
subset(sites3,LTER=="MCM")

biome.rename1$SITE
sites3=merge(sites2,biome.rename1,by.x=c("Stream.Site","LTER"),by.y=c("SITE","LTER"))
sites3$Biome_new=factor(sites3$Biome_new,levels=biome.rename.sort)

# png(filename=paste0(plot.path,"Map_biome_LongTerm_polar_v4.png"),width=5.25,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.25,0.25,0.25,0.25),mar=c(0.1,0.1,0.1,0.1),xpd=F)
layout(matrix(c(1,1:3),2,2,byrow=T),widths=c(1,0.65),heights=c(0.75,1))

bbox.lims=bbox(sites)
plot(world,col="grey90",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
plot(sites3,add=T,pch=21,bg=biome.cols[sites3$Biome_new],col="black",lwd=0.1,cex=1.25)
box(lwd=1)
mapmisc::scaleBar(wgs84,"bottom",bty="n",cex=1,seg.len=4,outer=F)

par(mar=c(2.5,3.5,0.5,0.1),xpd=F)
ylim.val=c(0,400);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(-20,30);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
txt.cex=0.75
plot(precp_cm~temp_c,Whittaker_biomes,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(v=0,h=0)
with(subset(sites.climate3,LTER!="UMR"),points(mean.temp,mean.precip,pch=21,
                                               bg=biome.cols[Biome_new],cex=1.5,lwd=0.01))
xval=0.3;yval=5
with(subset(sites.climate3,LTER=="UMR"&Biome_new=="Temperate deciduous forest"),
     points(mean.temp+xval,mean.precip-yval,pch=21,
            bg=biome.cols[Biome_new],cex=1.5,lwd=0.01))
with(subset(sites.climate3,LTER=="UMR"&Biome_new=="Temperate deciduous forest"),
     text(mean.temp+xval,mean.precip-yval,"UMR",pos=1,cex=txt.cex,offset=0.25))
with(subset(sites.climate3,LTER=="UMR"&Biome_new=="Temperate grassland"),
     points(mean.temp-xval,mean.precip+yval,pch=21,
            bg=biome.cols[Biome_new],cex=1.5,lwd=0.01))
with(subset(sites.climate3,LTER=="UMR"&Biome_new=="Temperate deciduous forest"),
     text(mean.temp-xval,mean.precip+yval,"UMR",pos=4,cex=txt.cex))
with(subset(site.climate2,LTER=="Sagehen"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
# with(subset(site.climate2,LTER=="UMR"),text(mean.temp,mean.precip,LTER,pos=4,cex=txt.cex))
with(subset(site.climate2,LTER=="LMP"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
with(subset(site.climate2,LTER=="ARC"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
with(subset(site.climate2,!(LTER%in%c("Sagehen","BcCZO","PIE","UMR","LMP","ARC"))),
     text(mean.temp,mean.precip,LTER,pos=3,cex=txt.cex))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Temperature (\u00B0C)")
mtext(side=2,line=2.5,"Precipitation (cm)")

plot(0:1,0:1,type="n",ann=F,axes=F)
legend(0.25,0.5,legend=biome.rename.sort,
       pch=22,lwd=0.1,lty=0,
       pt.bg=biome.cols,
       pt.cex=2,ncol=1,cex=0.8,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " Biome")
dev.off()


