## 
## SiSyn
## Compare data prepared by K. Peach to template data on GoogleDrive
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

#Paths
wd="C:/Julian_LaCie/_GitHub/SiSyn"

#https://www.r-bloggers.com/structuring-r-projects/
#https://nicercode.github.io/blog/2013-04-05-projects/

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src/"))
#Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

## Google Drive Data variables
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


# EDI Data ----------------------------------------------------------------
EDI.param.vars=c("NH4", "DOC","pH", "Na", "K", "Ca", "Mg", "SO4", "Cl","Conductivity", 
                 "alkalinity", "PO4", "ANC", "DSi", "Spec.Cond", "DIC", "Temp.C", 
                 "TSS", "NOx", "SRP", "Suspended.Chl", "TN", "TP", "TOC")

EDI.dat=read.xlsx(paste0(data.path,"EDI_data/20200721_ALL_watershed Raw Data.xlsx"))
EDI.dat$Sampling.Date=date.fun(EDI.dat$Sampling.Date)

## some fields are chr 


unique(EDI.dat$LTER)
# ARC data
EDI.dat.arc=subset(EDI.dat,LTER=="ARC")

range(EDI.dat.arc$Sampling.Date)
unique(EDI.dat.arc$`Site/Stream.Name`)
length(unique(EDI.dat.arc$`Site/Stream.Name`))

EDI.dat.arc=EDI.dat.arc[,names(EDI.dat.arc)[names(EDI.dat.arc)!="Time"]]
names(EDI.dat.arc)
EDI.dat.arc$site=EDI.dat.arc$'Site/Stream.Name'

EDI.dat.arc.melt=melt(EDI.dat.arc[,c(idvars,EDI.param.vars)],id.vars=idvars)
# warning message because some values are character
EDI.dat.arc.melt=merge(EDI.dat.arc.melt,param.list)
EDI.dat.arc.melt$site=EDI.dat.arc.melt$'Site/Stream.Name'

EDI.arc.data.inv=ddply(EDI.dat.arc.melt,c("Sampling.Date","variable","plot.val"),summarise,N.val=N.obs(site))
EDI.arc.site.inv=ddply(EDI.dat.arc,c("Sampling.Date","site"),summarise,N.val=N.obs(DSi))
EDI.arc.site.inv=merge(EDI.arc.site.inv,data.frame(site=unique(EDI.arc.site.inv$site),plot.val=1:length(unique(EDI.arc.site.inv$site))))


tiff(filename=paste0(plot.path,"EDI_ARC_inv.tiff"),width=8.5,height=10,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(1,8,0.75,0.5),oma=c(2,1,0.5,0.5));
layout(matrix(c(1,1,2,3),2,2,byrow=F))
xlim.val=range(EDI.arc.data.inv$Sampling.Date,na.rm=T);xmaj=seq(xlim.val[1],xlim.val[2],"10 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")

ylim.val=c(1,length(unique(EDI.arc.site.inv$site)));ymaj=seq(ylim.val[1],ylim.val[2],1)
plot(xlim.val,ylim.val,type="n",ylim=c(0.5-ylim.val[1],ylim.val[2]+0.5),xlim=xlim.val,axes=F,ylab=NA,xlab=NA,yaxs="i")
abline(v=xmin,lty=2,col="grey80",lwd=0.75)
with(EDI.arc.site.inv,points(Sampling.Date,plot.val,pch="|",col=adjustcolor("red",0.5)))
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj,"%m-%Y"))
axis_fun(2,ymaj,ymaj,unique(EDI.arc.site.inv$site),cex=0.5)
box(lwd=1)
mtext(side=3,"Arctic LTER (EDI) - DSi data",cex=0.8,adj=0)
mtext(side=1,line=1.75,"Date (Month-Year)")

ylim.val=c(1,nrow(param.list));ymaj=seq(ylim.val[1],ylim.val[2],1)
plot(xlim.val,ylim.val,type="n",ylim=c(0.5-ylim.val[1],ylim.val[2]+0.5),xlim=xlim.val,axes=F,ylab=NA,xlab=NA,yaxs="i")
abline(v=xmin,h=ymaj,lty=2,col="grey80",lwd=0.75)
with(EDI.arc.data.inv,points(Sampling.Date,plot.val,pch="|",col=adjustcolor("red",0.5)))
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj,"%m-%Y"))
axis_fun(2,ymaj,ymaj,param.list$variable,cex=0.8)
box(lwd=1)
mtext(side=3,"Arctic LTER (EDI)",cex=0.8,adj=0)
mtext(side=1,line=1.75,"Date (Month-Year)")

plot(0:1,0:1,type="n",ann=F,axes=F)

dev.off()



# ARC Data - GoogleDrive

## ARCLTER
arc.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_ARCLTER_05012020.xlsx"),sheet=1,startRow=2,na.strings = "NA")
arc.dat$Sampling.Date=convertToDate(arc.dat$Sampling.Date)
names(arc.dat)
arc.note.Look=arc.dat[,c("Site/Stream.Name","Notes")]

#fix header
arc.dat=rename(arc.dat,c("Dsi"="DSi"))
arc.dat[,c("TDN","TDP","NO3","NO2","DON")]=as.numeric(NA); #Add empty columns for uniformity

length(unique(arc.dat$`Site/Stream.Name`))
range(arc.dat$Sampling.Date)
##
arc.dat.melt=melt(arc.dat[,c(idvars,param.vars)],id.vars=idvars)
arc.dat.melt=subset(arc.dat.melt,is.na(value)==F)

arc.dat.melt=merge(arc.dat.melt,param.list,"variable")
arc.dat.melt$site=arc.dat.melt$'Site/Stream.Name'
arc.dat.melt$variable=as.character(arc.dat.melt$variable)

arc.dat.inv=ddply(arc.dat.melt,c("Sampling.Date","variable","plot.val"),summarise,N.val=N.obs(site))

arc.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_ARCLTER_05012020.xlsx"),sheet=4,startRow=4,na.strings = "NA")
arc.basin=arc.basin[,basin.vars]

