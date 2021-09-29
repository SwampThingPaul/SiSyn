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
umr.basin$LTER="WMR"

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

site.climate$mean.precip=site.climate$mean.precip*0.1

library(rnaturalearth)
library(ggplot2)
world <- ne_countries(scale = "small", returnclass = "sp")
class(world)

sort.LTERs=ddply(sites@data,"LTER",summarise,mean.lat=mean(Latitude))
sort.LTERs=sort.LTERs[order(-sort.LTERs$mean.lat),]
sort.LTERs$LTER=factor(sort.LTERs$LTER,levels=sort.LTERs$LTER)
sort.LTERs=c("ARC", "GRO", "BNZ", "AND", "HBR", 
             "LMP", "PIE", "Ipswich", "WMR", "NWT", "BcCZO", "Sagehen", "KNZ", 
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
par(mar=c(2.5,3.5,0.5,1),xpd=F)
ylim.val=c(0,500);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(-20,30);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

plot(precp_cm~temp_c,Whittaker_biomes,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
for(i in 1:9){
  with(subset(Whittaker_biomes,biome_id==i),polygon(temp_c,precp_cm,col=adjustcolor(biome.list$Ricklefs.cols[i],0.5),lwd=0.5))
  # with(subset(Whittaker_biomes,biome_id==i),text(mean(temp_c),mean(precp_cm),biome.list$biome[i],cex=0.75))
}
with(site.climate,points(mean.precip~mean.temp,pch=21,bg=site.cols.climate,cex=1.5))
with(site.climate,text(mean.temp,mean.precip,LTER,pos=3))
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
