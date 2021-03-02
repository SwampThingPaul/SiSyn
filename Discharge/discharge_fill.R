## 
## SiSyn
## Discharge data fill
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

library(mgcv)
library(gratia)

#Paths
wd="C:/Julian_LaCie/_GitHub/SiSyn/Discharge"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src/"))
#Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paste0(dirname(wd),"/Data/")



# -------------------------------------------------------------------------
dat=read.csv(paste0(data.path,"discharge/NWT_alb_Q.csv"))

dat$Date=as.Date(as.character(dat$Date))
dat$DOY=as.numeric(format(dat$Date,"%j"))
dat$Yr=as.numeric(format(dat$Date,"%Y"))


plot(Discharge~Date,dat)
DOY.k=20
YR.k=32
mod=gam(Discharge~Yr+
          s(DOY,k=DOY.k,bs="cc")+
          s(Yr,k=YR.k)+
          ti(Yr,DOY,bs=c("tp","cc"),k=c(DOY.k,DOY.k)),
          knots=list(DOY=c(1,366)),data=dat)


nvar=3;layout(matrix(1:nvar,1,nvar))
plot(mod,residuals=T,pch=21)

nvar=4;layout(matrix(1:nvar,1,nvar))
gam.check(mod)

summary(mod)

pdata=dat[,c("Date","DOY","Yr","Discharge")]
pred=predict(mod,pdata)
pdata=cbind(pdata,pred)
pdata$pred=exp(pdata$pred)

dev.off()
plot(Discharge~Date,pdata)
with(pdata,lines(exp(pred),Date,col="red"))
