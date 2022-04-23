## 
## SiSyn - Master Dataset
##
##
## Code was compiled by Paul Julian
## contact info: pjulian@ufl.edu
## contact info: pauljulianphd@gmail.com

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

#Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);

library(plyr)
library(reshape)
library(openxlsx)
library(lubridate)

library(PeriodicTable)
#Paths
wd="C:/Julian_LaCie/_GitHub/SiSyn/DataInventory"

#https://www.r-bloggers.com/structuring-r-projects/
#https://nicercode.github.io/blog/2013-04-05-projects/

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src/"))
#Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paste0(dirname(wd),"/Data/")


# Helper functions
# molecular weight
N.mw=14.0067
P.mw=30.973762
C.mw=12.0107
Si.mw=28.0855
H.mw=1.00784
O.mw=15.999

Na.mw=mass("Na")
K.mw=mass("K")
Ca.mw=mass("Ca")
Mg.mw=mass("Mg")
S.mw=mass("S")
Cl.mw=mass("Cl")

# Data --------------------------------------------------------------------
# param.list=data.frame(variable=c("Daily.Avg.Q.(Discharge)", "Instantaneous.Q.(Discharge)", "Stage.Height", 
#                                  "Temp.C", "Conductivity", "Spec.Cond", "Turbidity", "TSS", "VSS", 
#                                  "DSi", "TN","TDN", "DIN", "NOx","NO3","NO2", "NH4","DON", "TP","TDP", "PO4", "SRP", "DOC", 
#                                  "TOC", "alkalinity", "ANC", "pH", "DIC", "Suspended.Chl", "Benthic.Chl", 
#                                  "Na", "K", "Ca", "Mg", "SO4", "Cl"),
#                       plot.val=1:36)

idvars=c("LTER","Site/Stream.Name","Sampling.Date")
param.vars=c("Daily.Avg.Q.(Discharge)", "Instantaneous.Q.(Discharge)", "Stage.Height", 
             "Temp.C", "Conductivity", "Spec.Cond", "Turbidity", "TSS", "VSS", 
             "DSi", "TN","TDN", "DIN", "NOx","NO3","NO2", "NH4","DON", "TP","TDP", "PO4", "SRP", "DOC", 
             "TOC", "alkalinity", "ANC", "pH", "DIC", "Suspended.Chl", "Benthic.Chl", 
             "Na", "K", "Ca", "Mg", "SO4", "Cl")
basin.vars=c("LTER","Site/Stream","Unique.ID","Latitude","Longitude")


# UnitConvert
arc.unit=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_ARCLTER_05012020.xlsx"),sheet=2,na.strings = "NA")
arc.unit$data.set="ARC"
bczo.unit=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_BcCZO.xlsx"),sheet=2,startRow=1,na.strings = "NA")
bczo.unit$data.set="BcCZO"
carey.unit=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Carey_6.12.20.xlsx"),sheet=2,startRow=1,na.strings = "NA")
carey.unit$data.set="carey"
coal.unit=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Coal_Creek.xlsx"),sheet=2,startRow=1,na.strings = "NA")
coal.unit$data.set="coal"
cpcrw.unit=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_CPCRW.xlsx"),sheet=2,startRow=1,na.strings = "NA")
cpcrw.unit$data.set="cpcrw"
konza.unit=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Konza.xlsx"),sheet=2,startRow=1,na.strings = "NA")
konza.unit$data.set="konza"
KRR.unit=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_KRR.xlsx"),sheet=2,startRow=1,na.strings = "NA")
KRR.unit$data.set="KRR"
MCM.unit=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_MCM.xlsx"),sheet=2,startRow=1,na.strings = "NA")
MCM.unit=MCM.unit[,1:2]
MCM.unit$data.set="MCM"
NWT.unit=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_NWT.xlsx"),sheet=2,startRow=1,na.strings = "NA")
NWT.unit$data.set="NWT"
LMP.unit=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_LMP.xlsx"),sheet=2,startRow=1,na.strings = "NA")
LMP.unit$data.set="LMP"
LUQ.unit=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_LUQ.xlsx"),sheet=2,startRow=1,na.strings = "NA")
LUQ.unit=LUQ.unit[,1:2]
LUQ.unit$data.set="LUQ"
pie.unit=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_PIEWatersheds.xlsx"),sheet=2,startRow=1,na.strings = "NA")
pie.unit$data.set="pie"
hja.unit=read.xlsx(paste0(data.path,"HJAndrewsSiSyn.xlsx"),sheet=2,startRow=1,na.strings = "NA")
hja.unit$data.set="hja"
sage.unit=read.xlsx(paste0(data.path,"SagehenSiSyn_UPDATED_03032021.xlsx"),sheet=2,startRow=1,na.strings = "NA")
sage.unit$data.set="sage"
umr.unit=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_UMR.xlsx"),sheet=2,startRow=1,na.strings = "NA")
umr.unit$data.set="umr"
tang.unit=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Tanguro.xlsx"),sheet=2,startRow=1,na.strings = "NA")
tang.unit$data.set="tanguro"

HBR.unit=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_HBR.xlsx"),sheet=2,startRow=1,na.strings = "NA")
HBR.unit$data.set="HBR"

gro.unit=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_GRO_090321.xlsx"),sheet=2,startRow=1,na.strings = "NA")
gro.unit=gro.unit[,1:2]
gro.unit$data.set="GRO"
gro.unit[gro.unit$Measurement=="Alkalinity","Unit"]="mg CaCO3/L"

# sage.unit=read.xlsx(paste0(export.path,"sagehencreek_params_edited.xlsx"))
# sage.unit=subset(sage.unit,keep==1)[,c("Measurement","Unit")]
# sage.unit$data.set="sage"

unit.all=rbind(arc.unit,bczo.unit,carey.unit,coal.unit,cpcrw.unit,konza.unit,
               KRR.unit,MCM.unit,NWT.unit,LMP.unit,LUQ.unit,pie.unit,hja.unit,
               sage.unit,umr.unit,tang.unit,HBR.unit,gro.unit)

# https://ocean.ices.dk/tools/unitconversion.aspx
# http://www.salinitymanagement.org/Salinity%20Management%20Guide/ls/ls_3c.html
#
SiO2.mw=(Si.mw+(O.mw*2))
NO3.mw=(N.mw+(O.mw*3))
NO2.mw=(N.mw+(O.mw*2))
NH4.mw=(N.mw+(H.mw*4))
PO4.mw=P.mw+(O.mw*4)
SO4.mw=(S.mw+O.mw*4)
NOX.val=1;# NOx valence
PO4.val=3;# PO4 valence

unique(unit.all$Measurement)
subset(unit.all,Measurement=="Si")
Si.CF=data.frame(Measurement="Si",
                 Unit=c("mg Si/L","mg SiO2/L","ug Si/L"),
                 CF=c((1/Si.mw)*1000,(1/SiO2.mw)*1000,1/Si.mw))
N.cf=data.frame(Measurement=c("TN","DIN","DIN","TN"),Unit=c(rep("mg N/L",2),"uM N","uM N"),CF=c(rep((1/N.mw)*1000,2),1,1))
NOX.cf=data.frame(Measurement="NOX",
                  Unit=c("uM NO3","mg NO3-N/L","ug NO3-N/L","ueq/L","uM NO3-N","mg NO3/L"),
                  CF=c(1,(1/N.mw)*1000,1/N.mw,1,1,(1/NO3.mw)*1000))
NO3.cf=data.frame(Measurement="NO3",Unit=c("ug/L"),CF=c(1/NO3.mw))
NO2.cf=data.frame(Measurement="NO2",Unit=c("ug/L"),CF=c(1/NO2.mw))
NH4.cf=data.frame(Measurement="NH4",
                  Unit=c("ueq/L","ug NH4/L","mg NH4-N/L","ug NH4-N/L","uM NH4-N","uM NH4"),
                  CF=c(1,((1/NH4.mw)),(1/N.mw)*1000,(1/N.mw),1,1))
other.N=data.frame(Measurement=c(rep("TDN",3),"DON","DTN"),
                   Unit=c("mg N/L","mg/L","uM N","ug/L","ug/L"),
                   CF=c(rep((1/N.mw)*1000,2),1,rep(1/N.mw,2)))
TP.cf=data.frame(Measurement=c("TP"),Unit=c("mg P/L","uM P"),CF=c((1/P.mw)*1000,1))

PO4.cf=data.frame(Measurement=c("PO4"),
                  Unit=c("ueq/L","ug PO4-P/L","mg PO4-P/L","mg PO4/L","uM PO4-P","uM PO4"),
                  CF=c((1/PO4.val),(1/P.mw),(1/P.mw)*1000,((1/PO4.mw))*1000,1,1))
SRP.cf=data.frame(Measurement=c("SRP"),
                  Unit=c("ug P/L","mg P/L","uM P"),
                  CF=c(1/P.mw,(1/P.mw)*1000,1))
C.cf=data.frame(Measurement=c(rep("DOC",2),rep("TOC",2),rep("DIC",2)),Unit=c("mg C/L","uM C"),CF=c((1/C.mw)*1000,1))
Q.cf=data.frame(Measurement=c("Instantaneous Q","Daily Avg Q"),
                  Unit=c("cfs"),
                  CF=0.0283168)

subset(unit.all,Measurement=="Cl")
subset(unit.all,Measurement=="Alkalinity")
## 1 meq/L Alk = 50 mg/L CaCO3
alk.cf=data.frame(Measurement=c("Alkalinity"),
                  Unit=c("ueq/L","meq/L","mg HCO3-C","mg CaCO3/L"),
                  CF=c(1,1000,(1/(H.mw+C.mw+(O.mw*3)))*1000,(1/(Ca.mw+C.mw+(O.mw*3)))*1000))
Na.cf=data.frame(Measurement=c("Na"),Unit=c("ueq/L","uM","mg/L"),CF=c(1,1,(1/Na.mw)*1000))
K.cf=data.frame(Measurement=c("K"),Unit=c("ueq/L","uM","mg/L"),CF=c(1,1,(1/K.mw)*1000))
Ca.cf=data.frame(Measurement=c("Ca"),Unit=c("ueq/L","uM","mg/L"),CF=c(1/2,1,(1/Ca.mw)*1000))
Mg.cf=data.frame(Measurement=c("Mg"),Unit=c("ueq/L","uM","mg/L"),CF=c(1/2,1,(1/Mg.mw)*1000))
SO4.cf=data.frame(Measurement=c("SO4"),Unit=c("ueq/L","uM","umols SO4","umols SO4-S","mg SO4/L","mg SO4-S/L","mg/L"),
                  CF=c(1/2,1,1,1,(1/SO4.mw)*1000,(1/S.mw)*1000,(1/SO4.mw)*1000))
Cl.cf=data.frame(Measurement=c("Cl"),Unit=c("ueq/L","uM","mg/L"),CF=c(1,1,(1/Cl.mw)*1000))

subset(unit.all,Measurement=="VSS")
subset(unit.all,Measurement=="TSS")
subset(unit.all,Measurement=="Chl a (suspended)")
subset(unit.all,Measurement=="Chl a (benthic)")
subset(unit.all,data.set=="umr")

cf.all=rbind(Si.CF,N.cf,NOX.cf,NO3.cf,NO2.cf,NH4.cf,other.N,TP.cf,PO4.cf,SRP.cf,C.cf,Q.cf,
             alk.cf,Na.cf,K.cf,Ca.cf,Mg.cf,SO4.cf,Cl.cf)

unit.all=merge(unit.all,cf.all,all.x=T)
#unit.all$CF=with(unit.all,ifelse(Unit=="mM",1000,CF))
unit.all$CF=with(unit.all,ifelse(Unit=="uM",1,CF))
unit.all$CF=with(unit.all,ifelse(Unit=="cms",1,CF))
unit.all$CF=with(unit.all,ifelse(Measurement=="pH",1,CF))
unit.all$CF=with(unit.all,ifelse(Measurement=="TDP"&Unit=="mM P",1000,CF))

meas.var=data.frame(Measurement=c("Alkalinity", "ANC", "Ca", "Chl a (benthic)", "Chl a (suspended)", 
                         "Cl", "Conductivity", "Daily Avg Q", "DIC", "DIN", "DOC", "DON", 
                         "DTN", "Instantaneous Q", "K", "Mg", "Na", "NH4", "NO3", "NOX", 
                         "pH", "PO4", "Si", "SO4", "Specific Conductance", "SRP", "Stage Height", 
                         "TDN", "TDP", "Temp", "TN", "TOC", "TP", "TSS", "Turbidity", 
                         "VSS"),
           variable=c("alkalinity", "ANC", "Ca", "Benthic.Chl", "Suspended.Chl", 
                      "Cl", "Conductivity", "Daily.Avg.Q.(Discharge)", "DIC", "DIN", "DOC", "DON", 
                      "DTN", "Instantaneous.Q.(Discharge)", "K", "Mg", "Na", "NH4", "NO3", "NOx", 
                      "pH", "PO4", "DSi", "SO4", "Spec.Cond", "SRP", "Stage.Height", 
                      "TDN", "TDP", "Temp.C", "TN", "TOC", "TP", "TSS", "Turbidity", 
                      "VSS"))
meas.var2=data.frame(data.set=sort(rep(unique(unit.all$data.set),nrow(meas.var))),
           Measurement=rep(meas.var$Measurement,length(unique(unit.all$data.set))),
           variable=rep(meas.var$variable,length(unique(unit.all$data.set))))


unit.all=merge(unit.all,meas.var2,c("Measurement","data.set"),all.y=T)
head(unit.all)

subset(unit.all,data.set=="ARC")
other.vars=c("Temp","Conductivity","Specific Conductance","Turbidity","TSS","VSS", "Chl a (benthic)", "Chl a (suspended)")
unit.all$CF=with(unit.all,ifelse(Measurement%in%other.vars,1,CF))

subset(unit.all,Measurement=="Instantaneous Q")
subset(unit.all,Measurement=="Daily Avg Q")

subset(unit.all,Measurement=="Alkalinity")

unit.all2=unit.all[,c("variable","data.set","CF")]
##
data.filelist=list.files(data.path)

## EDI and Data template merged (by Dr Kristen Peach, peach@nceas.ucsb.edu)
## Data from ARC, NWT and HBR sites (unknown units and site lat/long for HBR)
# edi.merge.dat=read.xlsx(paste0(data.path,"SiSyn_DataEDIMerge_102920.xlsx"),sheet=1,startRow=1,na.strings = "NA")

## ARCLTER
arc.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_ARCLTER_05012020.xlsx"),sheet=1,startRow=2,na.strings = "NA")
arc.dat$Sampling.Date=convertToDate(arc.dat$Sampling.Date)
arc.note.Look=arc.dat[,c("Site/Stream.Name","Notes")]
arc.dat$site=arc.dat$'Site/Stream.Name'

#fix header
arc.dat=rename(arc.dat,c("Dsi"="DSi"))
arc.dat[,c("TDN","TDP","NO3","NO2","DON")]=as.numeric(NA); #Add empty columns for uniformity
# write.csv(arc.dat[,c(idvars,param.vars)],paste0(export.path,"/20200911_GD_ARC.csv"),row.names=F)

##
arc.dat.melt=melt(arc.dat[,c(idvars,param.vars)],id.vars=c(idvars))
arc.dat.melt=subset(arc.dat.melt,is.na(value)==F)
# arc.dat.melt=merge(arc.dat.melt,param.list,"variable")
arc.dat.melt$site=arc.dat.melt$'Site/Stream.Name'
arc.dat.melt$variable=as.character(arc.dat.melt$variable)

arc.dat.melt=merge(arc.dat.melt,subset(unit.all2,data.set=="ARC"))
subset(arc.dat.melt,value<0)
subset(arc.dat.melt,value==0)
arc.dat.melt$value[arc.dat.melt$value==0]=NA
arc.dat.melt$value=with(arc.dat.melt, ifelse(value<0,abs(value),value))
arc.dat.melt$value=with(arc.dat.melt,value*CF)
arc.dat.melt$LTER="ARC"
## BcZO
bczo.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_BcCZO.xlsx"),sheet=1,startRow=2,na.strings = "NA")
bczo.dat$Sampling.Date=convertToDate(bczo.dat$Sampling.Date)
names(bczo.dat)
bczo.dat[,c("TN","TP","NO3","NO2","DON")]=as.numeric(NA);
bczo.dat=subset(bczo.dat,is.na('Site/Stream.Name')==F)

bczo.dat.melt=melt(bczo.dat[,c(idvars,param.vars)],id.vars=idvars)
bczo.dat.melt=subset(bczo.dat.melt,is.na(value)==F)
bczo.dat.melt$site=bczo.dat.melt$'Site/Stream.Name'
bczo.dat.melt$variable=as.character(bczo.dat.melt$variable)

bczo.dat.melt=merge(bczo.dat.melt,subset(unit.all2,data.set=="BcCZO"))
# charge blaance difference >10%,NP and DL in "value" field for some of the values.
subset(bczo.dat.melt,value<0)
bczo.dat.melt$value=as.numeric(bczo.dat.melt$value)
bczo.dat.melt$value=with(bczo.dat.melt, ifelse(value<0,abs(value),value))
bczo.dat.melt$value=with(bczo.dat.melt,as.numeric(value)*CF)
bczo.dat.melt$LTER="BcCZO"
## Carey
carey.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Carey_6.12.20.xlsx"),sheet=1,startRow=2,na.strings = "NA")
carey.dat$Sampling.Date=convertToDate(carey.dat$Sampling.Date)
carey.dat=carey.dat[2:nrow(carey.dat),]
carey.dat[,6:ncol(carey.dat)]=sapply(carey.dat[,6:ncol(carey.dat)],as.numeric)
carey.dat=rename(carey.dat,c("NO3.(uM)"="NO3","NO2.(uM)"="NO2"))
carey.dat[,c("TDN","TDP","DON")]=as.numeric(NA)

carey.dat.melt=melt(carey.dat[,c(idvars,param.vars)],id.vars=idvars)
carey.dat.melt=subset(carey.dat.melt,is.na(value)==F)
carey.dat.melt$site=carey.dat.melt$'Site/Stream.Name'
carey.dat.melt$variable=as.character(carey.dat.melt$variable)

carey.dat.melt=merge(carey.dat.melt,subset(unit.all2,data.set=="carey"))
subset(carey.dat.melt,value<0)
carey.dat.melt$value=with(carey.dat.melt, ifelse(value<0,abs(value),value))
carey.dat.melt$value=with(carey.dat.melt,value*CF)
carey.dat.melt$LTER="Ipswitch(Carey)"

## Coal Creek
coal.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Coal_Creek.xlsx"),sheet=1,startRow=2,na.strings = "NA")
coal.dat$Sampling.Date=convertToDate(coal.dat$Sampling.Date)
names(coal.dat)
coal.dat[,c("TDN","TDP","NO3","NO2")]=as.numeric(NA);

coal.dat.melt=melt(coal.dat[,c(idvars,param.vars)],id.vars=idvars)
coal.dat.melt=subset(coal.dat.melt,is.na(value)==F)
coal.dat.melt$site=coal.dat.melt$'Site/Stream.Name'
coal.dat.melt$variable=as.character(coal.dat.melt$variable)

coal.dat.melt=merge(coal.dat.melt,subset(unit.all2,data.set=="coal"))
subset(coal.dat.melt,value<0)
coal.dat.melt$value=with(coal.dat.melt,value*CF)

## CPCRW
cpcrw.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_CPCRW.xlsx"),sheet=1,startRow=2,na.strings = "NA")
cpcrw.dat$Sampling.Date=convertToDate(cpcrw.dat$Sampling.Date)
names(cpcrw.dat)
cpcrw.dat[,c("TDN","TDP","NO3","NO2","DON")]=as.numeric(NA);

cpcrw.dat.melt=melt(cpcrw.dat[,c(idvars,param.vars)],id.vars=idvars)
cpcrw.dat.melt=subset(cpcrw.dat.melt,is.na(value)==F)
cpcrw.dat.melt$site=cpcrw.dat.melt$'Site/Stream.Name'
cpcrw.dat.melt$variable=as.character(cpcrw.dat.melt$variable)

cpcrw.dat.melt=merge(cpcrw.dat.melt,subset(unit.all2,data.set=="cpcrw"))
subset(cpcrw.dat.melt,value<0)
cpcrw.dat.melt$value=with(cpcrw.dat.melt, ifelse(value<0,abs(value),value))
cpcrw.dat.melt$value=with(cpcrw.dat.melt,value*CF)

#Knonza
konza.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Konza.xlsx"),sheet=1,startRow=2,na.strings = "NA")
konza.dat$Sampling.Date=convertToDate(konza.dat$Sampling.Date)
names(konza.dat)
konza.dat[,c("TDN","TDP","NO2","DON")]=as.numeric(NA);
konza.dat=rename(konza.dat,c("no3"="NO3"))

konza.dat.melt=melt(konza.dat[,c(idvars,param.vars)],id.vars=idvars)
konza.dat.melt=subset(konza.dat.melt,is.na(value)==F)
konza.dat.melt$site=konza.dat.melt$'Site/Stream.Name'
konza.dat.melt$variable=as.character(konza.dat.melt$variable)

konza.dat.melt=merge(konza.dat.melt,subset(unit.all2,data.set=="konza"))
subset(konza.dat.melt,value<0)
konza.dat.melt$value=with(konza.dat.melt, ifelse(value<0,abs(value),value))
konza.dat.melt$value=with(konza.dat.melt,value*CF)

## KRR
KRR.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_KRR.xlsx"),sheet=1,startRow=2,na.strings = "NA")
KRR.dat$Sampling.Date=convertToDate(KRR.dat$Sampling.Date)
names(KRR.dat)
KRR.dat[,c("TDN","TDP","NO3","NO2","DON")]=as.numeric(NA);

KRR.dat.melt=melt(KRR.dat[,c(idvars,param.vars)],id.vars=idvars)
KRR.dat.melt=subset(KRR.dat.melt,is.na(value)==F)
KRR.dat.melt$site=KRR.dat.melt$'Site/Stream.Name'
KRR.dat.melt$variable=as.character(KRR.dat.melt$variable)

KRR.dat.melt=merge(KRR.dat.melt,subset(unit.all2,data.set=="KRR"))
subset(KRR.dat.melt,value<0)
KRR.dat.melt$value=with(KRR.dat.melt,value*CF)
KRR.dat.melt$LTER="KRR(Julian)"

subset(KRR.dat.melt,variable=="Suspended.Chl")

## MCM
MCM.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_MCM.xlsx"),sheet=1,startRow=2,na.strings = "NA")
MCM.dat$Sampling.Date=convertToDate(MCM.dat$Sampling.Date)
names(MCM.dat)
MCM.dat[,c("TDN","TDP","NO3","NO2","DON")]=as.numeric(NA);

MCM.dat.melt=melt(MCM.dat[,c(idvars,param.vars)],id.vars=idvars)
MCM.dat.melt=subset(MCM.dat.melt,is.na(value)==F)
MCM.dat.melt$site=MCM.dat.melt$'Site/Stream.Name'
MCM.dat.melt$variable=as.character(MCM.dat.melt$variable)

MCM.dat.melt=merge(MCM.dat.melt,subset(unit.all2,data.set=="MCM"))
subset(MCM.dat.melt,value<0)
MCM.dat.melt$value=with(MCM.dat.melt, ifelse(value<0,abs(value),value))
MCM.dat.melt$value=with(MCM.dat.melt,value*CF)

## NWT
NWT.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_NWT.xlsx"),sheet=1,startRow=2,na.strings = "NA")
NWT.dat$Sampling.Date=convertToDate(NWT.dat$Sampling.Date)
names(NWT.dat)
NWT.dat[,c("TDN","TDP","NO3","NO2","DON")]=as.numeric(NA);
NWT.dat=rename(NWT.dat,c("InstantaneoNAs.Q.(Discharge)"="Instantaneous.Q.(Discharge)",
                         "CondNActivity"="Conductivity",
                         "TNArbidity"="Turbidity",
                         "SNAspended.Chl"="Suspended.Chl"))
NWT2.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_NWT_Arikaree.xlsx"),sheet=1,startRow=2,na.strings = "NA")
NWT2.dat$Sampling.Date=convertToDate(NWT2.dat$Sampling.Date)
names(NWT2.dat)
NWT2.dat[,c("TDN","TDP","NO3","NO2","DON")]=as.numeric(NA);
NWT2.dat=rename(NWT2.dat,c("InstantaneoNAs.Q.(Discharge)"="Instantaneous.Q.(Discharge)",
                         "CondNActivity"="Conductivity",
                         "TNArbidity"="Turbidity",
                         "SNAspended.Chl"="Suspended.Chl"))
NWT.dat=rbind(NWT.dat,NWT2.dat)
# write.csv(NWT.dat[,c(idvars,param.vars)],paste0(export.path,"20200911_GD_NWT.csv"),row.names = F)

NWT.dat.melt=melt(NWT.dat[,c(idvars,param.vars)],id.vars=idvars)
NWT.dat.melt=subset(NWT.dat.melt,is.na(value)==F)
NWT.dat.melt$site=NWT.dat.melt$'Site/Stream.Name'
NWT.dat.melt$variable=as.character(NWT.dat.melt$variable)

NWT.dat.melt=merge(NWT.dat.melt,subset(unit.all2,data.set=="NWT"))
# View(NWT.dat.melt[is.na(as.numeric(NWT.dat.melt$value)),])
# values reported less than MDL set to MDL
NWT.dat.melt$value=with(NWT.dat.melt,ifelse(is.na(as.numeric(value)),as.numeric(substr(value,2,10)),as.numeric(value)))
subset(NWT.dat.melt,value<0)
NWT.dat.melt$value=with(NWT.dat.melt, ifelse(value<0,abs(value),value))
NWT.dat.melt$value=with(NWT.dat.melt,value*CF)

## LMP
LMP.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_LMP.xlsx"),sheet=1,startRow=2,na.strings = "NA")
LMP.dat$Sampling.Date=convertToDate(LMP.dat$Sampling.Date)
names(LMP.dat)
LMP.dat[,c("TDP","NO3","NO2","DON")]=as.numeric(NA);

LMP.dat.melt=melt(LMP.dat[,c(idvars,param.vars)],id.vars=idvars)
LMP.dat.melt=subset(LMP.dat.melt,is.na(value)==F)
LMP.dat.melt$site=LMP.dat.melt$'Site/Stream.Name'
LMP.dat.melt$variable=as.character(LMP.dat.melt$variable)

LMP.dat.melt=merge(LMP.dat.melt,subset(unit.all2,data.set=="LMP"))
subset(LMP.dat.melt,value<0)
LMP.dat.melt$value[LMP.dat.melt$value==-9999]=NA
LMP.dat.melt$value[LMP.dat.melt$value==-888]=NA
LMP.dat.melt$value=with(LMP.dat.melt, ifelse(value<0,abs(value),value))
LMP.dat.melt$value=with(LMP.dat.melt,value*CF)
LMP.dat.melt$LTER="LMP(Wymore)"

## LUQ
LUQ.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_LUQ.xlsx"),sheet=1,startRow=2,na.strings = "NA")
LUQ.dat$Sampling.Date=convertToDate(LUQ.dat$Sampling.Date)
names(LUQ.dat)
LUQ.dat[,c("TDP","NO3","NO2","DON")]=as.numeric(NA);

LUQ.dat.melt=melt(LUQ.dat[,c(idvars,param.vars)],id.vars=idvars)
LUQ.dat.melt=subset(LUQ.dat.melt,is.na(value)==F)
LUQ.dat.melt$site=LUQ.dat.melt$'Site/Stream.Name'
LUQ.dat.melt$variable=as.character(LUQ.dat.melt$variable)

LUQ.dat.melt=merge(LUQ.dat.melt,subset(unit.all2,data.set=="LUQ"))
subset(LUQ.dat.melt,value<0)
LUQ.dat.melt$value=with(LUQ.dat.melt, ifelse(value<0,abs(value),value))
LUQ.dat.melt$value=with(LUQ.dat.melt,value*CF)

## PIE
pie.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_V1_PIEWatersheds.xlsx"),sheet=1,startRow=2,na.strings = "NA")
pie.dat$Sampling.Date=convertToDate(pie.dat$Sampling.Date)
names(pie.dat)
pie.dat[,c("TN","TDP","NO3","NO2","DON")]=as.numeric(NA);

pie.dat.melt=melt(pie.dat[,c(idvars,param.vars)],id.vars=idvars)
pie.dat.melt=subset(pie.dat.melt,is.na(value)==F)
pie.dat.melt$site=pie.dat.melt$'Site/Stream.Name'
pie.dat.melt$variable=as.character(pie.dat.melt$variable)

pie.dat.melt=merge(pie.dat.melt,subset(unit.all2,data.set=="pie"))
subset(pie.dat.melt,value<0)
pie.dat.melt$value=with(pie.dat.melt, ifelse(value<0,abs(value),value))
pie.dat.melt$value=with(pie.dat.melt,value*CF)

## HJ Andrews
hja.dat=read.xlsx(paste0(data.path,"HJAndrewsSiSyn.xlsx"),sheet=1,startRow=2,na.strings = "NA")
hja.dat$Sampling.Date=convertToDate(hja.dat$Sampling.Date)
names(hja.dat)
hja.dat=rename(hja.dat,c("Si"="DSi"))

hja.dat[,c("TDN","TDP","NO3","NO2","DON","NH4")]=as.numeric(NA);
hja.dat$NH4=hja.dat$NH3

hja.dat.melt=melt(hja.dat[,c(idvars,param.vars)],id.vars=idvars)
hja.dat.melt=subset(hja.dat.melt,is.na(value)==F)
hja.dat.melt$site=hja.dat.melt$'Site/Stream.Name'
hja.dat.melt$variable=as.character(hja.dat.melt$variable)

hja.dat.melt=merge(hja.dat.melt,subset(unit.all2,data.set=="hja"))
subset(hja.dat.melt,value<0)
hja.dat.melt$value=with(hja.dat.melt,value*CF)

# Sagehen
sage.dat=read.xlsx(paste0(data.path,"SagehenSiSyn.xlsx"),sheet=1,startRow=2,na.strings = "NA")
sage.dat$Sampling.Date=convertToDate(sage.dat$Sampling.Date)
names(sage.dat)

sage.dat[,c("TDN","TDP","NO3","NO2","DON")]=as.numeric(NA);

sage.dat.melt=melt(sage.dat[,c(idvars,param.vars)],id.vars=idvars)
sage.dat.melt=subset(sage.dat.melt,is.na(value)==F)
sage.dat.melt$site=sage.dat.melt$'Site/Stream.Name'
sage.dat.melt$variable=as.character(sage.dat.melt$variable)

spl=strsplit(as.character(sage.dat.melt$value)," ")
tmp=data.frame(V1=sapply(spl,"[",1),V2=sapply(spl,"[",2),V3=sapply(spl,"[",3))
tmp[is.na(as.numeric(tmp$V1)),]
tmp[is.na(as.numeric(tmp$V2)),]
tmp[is.na(as.numeric(tmp$V3)),]
tail(unique(tmp$V1))
unique(tmp$V2)
unique(tmp$V3)

tmp$value=with(tmp,ifelse(is.na(V1),as.character(V2),as.character(V1)))
fatal.qual=c("+c","bn","@nc","@c","@rc","@","b","@bc")
tmp$value=with(tmp,ifelse(V3%in%fatal.qual,NA,as.numeric(value)))
sage.dat.melt$value=tmp$value
subset(sage.dat.melt,value<0)

sage.dat.melt=merge(sage.dat.melt,subset(unit.all2,data.set=="sage"))
sage.dat.melt$value=with(sage.dat.melt,value*CF)
sage.dat.melt$LTER="Sagehen(Sullivan)"

# ## Sagehen from USGS
# library(dataRetrieval)
# sage.dat=readWQPdata(siteid="USGS-10343500")
# head(sage.dat)
# 
# unique(sage.dat$CharacteristicName)
# unique(sage.dat$ResultMeasure.MeasureUnitCode)
# unique(sage.dat$ResultSampleFractionText)
# sage.dat.params=ddply(sage.dat,c("CharacteristicName","ResultSampleFractionText","ResultMeasure.MeasureUnitCode"),summarise,N.val=N.obs(ResultMeasureValue))
# # write.csv(sage.dat.params,paste0(export.path,"sagehencreek_params.csv"),row.names = F)
# sage.unit=read.xlsx(paste0(export.path,"sagehencreek_params_edited.xlsx"))
# sage.unit=subset(sage.unit,keep==1)[,c("CharacteristicName","ResultSampleFractionText","Param")]
# 
# sage.dat=merge(sage.dat,sage.unit,c("CharacteristicName","ResultSampleFractionText"))
# sage.dat2=data.frame(cast(data.frame(sage.dat),MonitoringLocationIdentifier+ActivityStartDate~Param,value='ResultMeasureValue',mean))
# sage.dat2$NOx=with(sage.dat2,NO2+NO3)
# sage.dat2
# 
# sage.dat2[,c("Conductivity","TSS","VSS","TN","SRP","TDN","TDP","DON","ANC","TOC","Benthic.Chl")]=as.numeric(NA);
# sage.dat2$LTER="Sagehen"
# sage.dat2$'Site/Stream.Name'="Sagehen"
# sage.dat2$Sampling.Date=sage.dat2$ActivityStartDate
# sage.dat2$Treatment=NA
# sage.dat2$Time=NA



# UMR
umr.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_UMR.xlsx"),sheet=1,startRow=2,na.strings = "NA")
umr.dat$Sampling.Date=convertToDate(umr.dat$Sampling.Date)
names(umr.dat)

umr.dat[,c("TDN","TDP","NO3","NO2","DON")]=as.numeric(NA);
# umr.dat[,c(idvars,param.vars)]

umr.dat.melt=melt(umr.dat[,c(idvars,param.vars)],id.vars=idvars)
umr.dat.melt=subset(umr.dat.melt,is.na(value)==F)
umr.dat.melt$site=umr.dat.melt$'Site/Stream.Name'
umr.dat.melt$variable=as.character(umr.dat.melt$variable)

umr.dat.melt=merge(umr.dat.melt,subset(unit.all2,data.set=="umr"))
subset(umr.dat.melt,value<0)
umr.dat.melt$value=with(umr.dat.melt, ifelse(value<0,abs(value),value))
umr.dat.melt$value=with(umr.dat.melt,value*CF)
umr.dat.melt$LTER="UMR(Jankowski)"

# nique(umr.dat.melt$variable)
# plot(value~Sampling.Date,subset(umr.dat.melt,variable=="Suspended.Chl"))
# Tanguro
tango.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Tanguro.xlsx"),sheet=1,startRow=2,na.strings = "NA")
tango.dat$Sampling.Date=convertToDate(tango.dat$Sampling.Date)
names(tango.dat)
tango.dat[,c("TDN","TDP","NO3","NO2","DON")]=as.numeric(NA);

tango.dat.melt=melt(tango.dat[,c(idvars,param.vars)],id.vars=idvars)
tango.dat.melt=subset(tango.dat.melt,is.na(value)==F)
tango.dat.melt$site=tango.dat.melt$'Site/Stream.Name'
tango.dat.melt$variable=as.character(tango.dat.melt$variable)
tango.dat.melt$value=as.numeric(tango.dat.melt$value)

tango.dat.melt=merge(tango.dat.melt,subset(unit.all2,data.set=="tanguro"))
subset(tango.dat.melt,value<0)
tango.dat.melt$value=with(tango.dat.melt, ifelse(value<0,abs(value),value))
tango.dat.melt$value=with(tango.dat.melt,value*CF)
tango.dat.melt$LTER="Tanguro(Jankowski)"

umr.basin=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_Tanguro.xlsx"),sheet=4,startRow=4,na.strings = "NA")
umr.basin=umr.basin[,basin.vars]

# HBR
hbr.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_HBR.xlsx"),sheet=1,startRow=2,na.strings = "NA")
hbr.dat$Sampling.Date=convertToDate(hbr.dat$Sampling.Date)
names(hbr.dat)
hbr.dat[,c("TDN","TDP","NO3","NO2","DON")]=as.numeric(NA);

hbr.dat.melt=melt(hbr.dat[,c(idvars,param.vars)],id.vars=idvars)
hbr.dat.melt=subset(hbr.dat.melt,is.na(value)==F)
hbr.dat.melt$site=hbr.dat.melt$'Site/Stream.Name'
hbr.dat.melt$variable=as.character(hbr.dat.melt$variable)
hbr.dat.melt$value=as.numeric(hbr.dat.melt$value)

hbr.dat.melt=merge(hbr.dat.melt,subset(unit.all2,data.set=="HBR"))
subset(hbr.dat.melt,value<0)
hbr.dat.melt$value=with(hbr.dat.melt, ifelse(value<0,abs(value),value))
hbr.dat.melt$value=with(hbr.dat.melt,value*CF)
hbr.dat.melt$LTER="HBR"

# GRO
GRO.dat=read.xlsx(paste0(data.path,"SiSyn_DataTemplate_GRO_090321.xlsx"),sheet=1,startRow=2,na.strings = "NA")
GRO.dat$Sampling.Date=convertToDate(GRO.dat$Sampling.Date)
names(GRO.dat)
names(hbr.dat)%in%names(GRO.dat)
ncol(GRO.dat)
ncol(hbr.dat)
GRO.dat[,c("TDN","TDP","NO3","NO2","DON")]=as.numeric(NA);

GRO.dat.melt=melt(GRO.dat[,c(idvars,param.vars)],id.vars=idvars)
GRO.dat.melt=subset(GRO.dat.melt,is.na(value)==F)
GRO.dat.melt$site=GRO.dat.melt$'Site/Stream.Name'
GRO.dat.melt$variable=as.character(GRO.dat.melt$variable)

head(sort(unique(GRO.dat.melt$value)))
tail(sort(unique(GRO.dat.melt$value)))
subset(GRO.dat.melt,value=="-0.1")
subset(GRO.dat.melt,value=="-0.2")
subset(GRO.dat.melt,value=="-0.3")
subset(GRO.dat.melt,value=="-0.46650000000000003")
subset(GRO.dat.melt,value=="-0.5")
subset(GRO.dat.melt,value=="-1")
subset(GRO.dat.melt,value=="BD")
GRO.dat.melt$value=as.numeric(GRO.dat.melt$value)

GRO.dat.melt=merge(GRO.dat.melt,subset(unit.all2,data.set=="GRO"))
subset(GRO.dat.melt,value<0)
head(subset(GRO.dat.melt,variable=="alkalinity"))
subset(GRO.dat.melt,value<0)
GRO.dat.melt$value=with(GRO.dat.melt, ifelse(variable!="Temp.C"&value<0,abs(value),value))
GRO.dat.melt$value=with(GRO.dat.melt,value*CF)
GRO.dat.melt$LTER="GRO"

# Combine all data 
master.dat=rbind(arc.dat.melt,bczo.dat.melt,carey.dat.melt,coal.dat.melt,cpcrw.dat.melt,
                 konza.dat.melt,KRR.dat.melt,MCM.dat.melt,NWT.dat.melt,LMP.dat.melt,
                 LUQ.dat.melt,pie.dat.melt,hja.dat.melt,sage.dat.melt,umr.dat.melt,
                 tango.dat.melt,hbr.dat.melt,GRO.dat.melt)
master.dat=master.dat[,c( "LTER", "Site/Stream.Name","site", "Sampling.Date","variable","value")]
master.dat$value[master.dat$value==0]=NA
master.dat=subset(master.dat,is.na(value)==F)
subset(master.dat, value==0)

summary(master.dat)
subset(master.dat, value<0)
# write.csv(master.dat,paste0(export.path,"20201015_masterdata.csv"),row.names=F)

# Chlorophyll data should be in master data
# write.csv(master.dat,paste0(export.path,"20201111_masterdata.csv"),row.names=F)

# Added HBR and corrected Coal Creek 
# write.csv(master.dat,paste0(export.path,"20210224_masterdata.csv"),row.names=F)

# updated Sagehen dataset
# write.csv(master.dat,paste0(export.path,"20210304_masterdata.csv"),row.names=F)

# fixed HBR NOx data
# write.csv(master.dat,paste0(export.path,"20210421_masterdata.csv"),row.names=F)

# fixed NWT NOx and SRP data
# write.csv(master.dat,paste0(export.path,"20210524_masterdata.csv"),row.names=F)

# Added GRO
# write.csv(master.dat,paste0(export.path,"20210804_masterdata.csv"),row.names=F)

# Added GRO higher frequency data
# write.csv(master.dat,paste0(export.path,"20210907_masterdata.csv"),row.names=F)

# Fixed Conversion factors
# write.csv(master.dat,paste0(export.path,"20220419_masterdata.csv"),row.names=F)


boxplot(value~site,subset(master.dat,variable=="DSi"),log="y",col="grey",ylab="DSi (uM)")

# tiff(filename=paste0(plot.path,"site_DSi_boxplot.tiff"),width=7,height=4,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
# png(filename=paste0(plot.path,"site_DSi_boxplot.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(6,4,0.75,0.5),oma=c(2,1,0.5,0.5));
ylim.val=c(0.01,1e3);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
x=boxplot(value~LTER,subset(master.dat,variable=="DSi"),log="y",col="grey",pch=21,cex=0.5,bg=adjustcolor("grey",0.25),axes=F,ann=F,ylim=ylim.val)
axis_fun(2,ymaj,ymin,format(ymaj,scientific = F),0.8,maj.tcl=-0.5,min.tcl=-0.25,line=-0.4)
axis_fun(1,1:length(x$names),1:length(x$names),NA);box(lwd=1)
text(1:length(x$names),rep(0.0015,length(x$names)),x$names,srt=90,xpd=NA,adj=1,cex=0.8)
mtext(side=2,line=3.5,"DSi (\u03BCM)")
dev.off()

range(subset(master.dat,variable=="DSi")$value,na.rm=T)
subset(master.dat,variable=="DSi"&value<0)
subset(master.dat,variable=="DSi"&site==1)


# tiff(filename=paste0(plot.path,"site_Chla_boxplot.tiff"),width=5,height=4.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
# png(filename=paste0(plot.path,"site_Chla_boxplot.png"),width=5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(6,4,0.75,0.5),oma=c(2,1,0.5,0.5));
ylim.val=c(0.01,400);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
x=boxplot(value~LTER,subset(master.dat,variable%in%c("Suspended.Chl","Benthic.Chl")),log="y",col="grey",pch=21,cex=0.5,bg=adjustcolor("grey",0.25),axes=F,ann=F,ylim=ylim.val)
axis_fun(2,ymaj,ymin,format(ymaj,scientific = F),0.8,maj.tcl=-0.5,min.tcl=-0.25,line=-0.4)
axis_fun(1,1:length(x$names),1:length(x$names),NA);box(lwd=1)
text(1:length(x$names),rep(0.0025,length(x$names)),x$names,srt=90,xpd=NA,adj=1,cex=0.8)
mtext(side=2,line=3.5,"Chlorophyll (\u03BCg L\u207B\u00B9)")
dev.off()
