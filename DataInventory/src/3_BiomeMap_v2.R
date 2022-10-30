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

library(maps)

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

world <- ne_countries(scale = 50, returnclass = "sp")

## SITE LTER lat/long
sites2=read.csv(paste0(data.path,"LongTermWatersheds_LatLong.csv"))
sites2$Longitude=with(sites2,ifelse(LTER=="Sagehen",Longitude*-1,Longitude))
unique(sites2$LTER)

sites2=SpatialPointsDataFrame(coords=sites2[,c("Longitude","Latitude")],
                              data=sites2,
                              proj4string = wgs84)

## SITE climate information
site.climate=read.csv(paste0(data.path,"MAP_MAT_byLTER.csv"))
site.climate$mean.precip=with(site.climate,ifelse(LTER=="Coal Crk",1163,mean.precip))
site.climate$LTER=as.factor(site.climate$LTER)
site.climate$mean.precip=site.climate$mean.precip*0.1

## SITE and LTER biome designation
biome.rename1=read.csv(paste0(data.path,"Biome.csv"))
colnames(biome.rename1)=c("LTER",'SITE',"Biome_new")

sort.LTERs=c("ARC", "GRO", "BNZ", "AND", "HBR", 
             "LMP", "PIE", "Ipswich", "UMR", "NWT", "BcCZO", "Sagehen", "KNZ", 
             "Coal Crk", "KRR", "LUQ", "Tanguro", "MCM")


biome.rename=ddply(biome.rename1, c("LTER","Biome_new"),summarise,N.val=N.obs(LTER))
biome.rename$LTER=factor(biome.rename$LTER,levels=sort.LTERs)

biome.rename.sort=c(
  "Tropical rainforest","Tropical savanna",
  "Temperate coniferous forest","Temperate deciduous forest",
  "Temperate grassland","Alpine tundra", "Arctic tundra",
  "Boreal forest","Polar desert"
)

biome.cols <- function(pal){
  require(RColorBrewer)
  brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
}
cols <- biome.cols("Set2")

cols_final<-c(cols, "#A46B5C")
col_values<-data.frame(biome=c("Boreal forest","Temperate deciduous forest",
              "Alpine tundra","Arctic tundra","Tropical rainforest",
              "Tropical savanna","Temperate grassland","Polar desert",
              "Temperate coniferous forest"),
              cols=cols_final)
col_values=col_values[match(biome.rename.sort,col_values$biome),]

biome.cols=col_values$cols

# biome.cols=c("lawngreen", "goldenrod1", 
#              "darkgreen", "darkorange2", 
#              "firebrick1", "mediumpurple3", 
#              "dodgerblue4","lightsteelblue3","ivory3")
biome.rename$Biome_new=factor(biome.rename$Biome_new,levels=biome.rename.sort)

plot(1:9,rep(1,9),pch=21,bg=col_values$cols,cex=2)
text(1:9,rep(1,9),col_values$biome,srt=45,pos=3)


sites2$Stream.Site
subset(sites2,LTER=="MCM")@data
subset(biome.rename1,LTER=="MCM")

biome.rename1$SITE
sites3=merge(sites2,biome.rename1,by.x=c("Stream.Site","LTER"),by.y=c("SITE","LTER"))
subset(sites3,LTER=="MCM")

subset(sites3,is.na(Biome_new))@data
sites3$Biome_new=with(sites3@data,ifelse(is.na(Biome_new)&LTER=="ARC","Arctic tundra",as.character(Biome_new)))
sites3$Biome_new=with(sites3@data,ifelse(is.na(Biome_new)&LTER=="LUQ","Tropical rainforest",as.character(Biome_new)))

sites3$Biome_new=factor(sites3$Biome_new,levels=biome.rename.sort)

## 
sites.climate2=subset(site.climate,LTER%in%unique(sites2$LTER))
sites.climate2=merge(sites.climate2,biome.rename,by="LTER")
sites.climate2$Biome_new

# tiff(filename=paste0(plot.path,"Fig2_SiteCharacterization.tiff"),width=5.75,height=4.5,units="in",res=200,type="windows",compression="lzw",bg="white")
# png(filename=paste0(plot.path,"Fig2_SiteCharacterization.png"),width=5.75,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.25,0.25,0.25,0.25),mar=c(0.3,0.1,0.1,0.1),xpd=F)
layout(matrix(c(1,1:3),2,2,byrow=T),widths=c(1,0.65),heights=c(0.8,1))

AOI=raster::extent(world)
AOI.poly=as(AOI,"SpatialPolygons")
proj4string(AOI.poly)=wgs84

bbox.lims=bbox(world)
asp.val=mapasp(world,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)])
plot(world,col="grey90",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)#,asp=asp.val)
plot(sites3,add=T,pch=21,bg=biome.cols[sites3$Biome_new],col="black",lwd=0.01,cex=1.25)
# box(lwd=1)
llgridlines(world,
            llcrs=as.character(crs(world)),
            col="grey50",xpd=NA,offset=0.25,lwd=0.75,plotLabels =F)
llgridlines(world,
            llcrs=as.character(crs(world)),
            col="black",xpd=NA,offset=0.25,plotLabels =T,plotLines=F)
mapmisc::scaleBar(wgs84,"bottom",bty="n",cex=1,seg.len=4,outer=F)
plot(AOI.poly,add=T,lwd=0.75)

par(mar=c(3,3.5,0.5,0.1),xpd=F)
ylim.val=c(0,400);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(-20,30);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
txt.cex=0.75
plot(precp_cm~temp_c,Whittaker_biomes,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
# abline(h=ymaj,v=xmaj,lty=3,col=adjustcolor("grey",0.5),lwd=0.5)
# abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5),lwd=0.5)
abline(v=0,h=0)
with(subset(sites.climate2,LTER!="UMR"),points(mean.temp,mean.precip,pch=21,
                                               bg=biome.cols[Biome_new],cex=1.5,lwd=0.01))
xval=0.3;yval=5
with(subset(sites.climate2,LTER=="UMR"&Biome_new=="Temperate deciduous forest"),
     points(mean.temp+xval,mean.precip-yval,pch=21,
            bg=biome.cols[Biome_new],cex=1.5,lwd=0.01))
with(subset(sites.climate2,LTER=="UMR"&Biome_new=="Temperate deciduous forest"),
     text(mean.temp+xval,mean.precip-yval,"UMR",pos=1,cex=txt.cex,offset=0.25))
with(subset(sites.climate2,LTER=="UMR"&Biome_new=="Temperate grassland"),
     points(mean.temp-xval,mean.precip+yval,pch=21,
            bg=biome.cols[Biome_new],cex=1.5,lwd=0.01))
with(subset(sites.climate2,LTER=="UMR"&Biome_new=="Temperate deciduous forest"),
     text(mean.temp-xval,mean.precip+yval,"UMR",pos=4,cex=txt.cex))
with(subset(sites.climate2,LTER=="Sagehen"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
# with(subset(site.climate2,LTER=="UMR"),text(mean.temp,mean.precip,LTER,pos=4,cex=txt.cex))
with(subset(sites.climate2,LTER=="LMP"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
with(subset(sites.climate2,LTER=="ARC"),text(mean.temp,mean.precip,LTER,pos=2,cex=txt.cex))
with(subset(sites.climate2,!(LTER%in%c("Sagehen","BcCZO","PIE","UMR","LMP","ARC"))),
     text(mean.temp,mean.precip,LTER,pos=3,cex=txt.cex))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Temperature (\u00B0C)")
mtext(side=2,line=2.5,"Precipitation (cm)")

plot(0:1,0:1,type="n",ann=F,axes=F)
legend(0.25,0.5,legend=biome.rename.sort,
       pch=21,lwd=0.1,lty=0,
       pt.bg=biome.cols,
       pt.cex=2,ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " Biome")
dev.off()