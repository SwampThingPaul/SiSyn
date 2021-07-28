## 
## SiSyn
## C-Q
##
## Code was compiled by Paul Julian
## contact info: pjulian@ufl.edu/pauljulianphd@gmail.com/pjulian@sccf.org

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape2)
library(zoo)

#Paths
wd="C:/Julian_LaCie/_GitHub/SiSyn/ConcentrationDischarge"

#https://www.r-bloggers.com/structuring-r-projects/
#https://nicercode.github.io/blog/2013-04-05-projects/

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src/"))
#Folder.Maker(paths[c(1,2,4)]);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paste0(dirname(wd),"/Data/")


# -------------------------------------------------------------------------
# missing data in larger file
# dat=read.csv(paste0(data.path,"Conc_Discharge_Master/WRTDS_data_filtered_longterm.csv"))
# dat$Date=as.Date(dat$Date)
# summary(dat)
# unique(dat$site.name)
# unique(subset(dat,is.na(Si)==F)$site.name)

dat.Q=read.csv(paste0(data.path,"Conc_Discharge_Master/M786.2C_Q_WRTDS.csv"))
dat.Q$Date=as.Date(dat.Q$Date)
dat.C=read.csv(paste0(data.path,"Conc_Discharge_Master/M786.2C_Si_WRTDS.csv"))
dat.C$Date=as.Date(dat.C$Date)
dat.CQ=merge(dat.Q,dat.C,"Date")

plot(Q~Date,dat.CQ)
plot(Si~Date,dat.CQ)

# plot(Si~Q,dat.CQ,log="xy")


library(segmented)

# Power Law
logC=log(dat.CQ$Si)
logQ=log(dat.CQ$Q)

plot(logC~logQ)

loglog.mod=lm(logC~logQ)
loglog.mod.sum=summary(loglog.mod)

confint(loglog.mod)
loglog.mod.sum$coefficients[2,2]


test=gvlma::gvlma(loglog.mod)
test$GlobalTest

as.character(ifelse(test$GlobalTest$GlobalStat4$pvalue<0.05,"No","Yes"))

# variabled of interest
loglog.mod.sum$r.squared
loglog.mod.sum$adj.r.squared
loglog.mod.sum$sigma
loglog.beta=as.numeric(coef(loglog.mod)[2])
loglog.alpha=as.numeric(coef(loglog.mod)[1])

loglog.assumpt=

LL.rslt=data.frame(LL.R2=loglog.mod.sum$r.squared,
                   LL.R2.adj=loglog.mod.sum$adj.r.squared,
                   LL.RMSE=loglog.mod.sum$sigma,
                   LL.beta=loglog.beta,
                   LL.alpha=loglog.alpha)

# Segmented
# assumes 1 breakpoint
loglog.mod.seg=segmented(loglog.mod,seg.Z=~logQ)
loglog.mod.seg.sum=summary(loglog.mod.seg)

loglog.mod.seg$psi[1,c(2,3)]

loglog.mod.seg.sum$r.squared
loglog.mod.seg.sum$adj.r.squared
loglog.mod.seg.sum$sigma

seg.rslt=data.frame(seg.bk.est=exp(loglog.mod.seg$psi[1,c(2)]),
                    seg.bk.SE=exp(loglog.mod.seg$psi[1,c(3)]),
                    seg.R2=loglog.mod.seg.sum$r.squared,
                    seg.R2.adj=loglog.mod.seg.sum$adj.r.squared,
                    seg.RMSE=loglog.mod.seg.sum$sigma)

# Moatar
Q50=median(dat.Q$Q,na.rm=T)

logQ=log(dat.CQ$Q[dat.CQ$Q<Q50])
logC=log(dat.CQ$Si[dat.CQ$Q<Q50])
loglog.mod.inf=lm(logC~logQ)
loglog.mod.inf.sum=summary(loglog.mod.inf)
CQ50.inf=predict(loglog.mod.inf,data.frame(logQ=log(Q50)))

logQ=log(dat.CQ$Q[dat.CQ$Q>Q50])
logC=log(dat.CQ$Si[dat.CQ$Q>Q50])
loglog.mod.sup=lm(logC~logQ)
loglog.mod.sup.sum=summary(loglog.mod.sup)
CQ50.sup=predict(loglog.mod.sup,data.frame(logQ=log(Q50)))

moatar.rslt=data.frame(Q50=Q50,
                       moatar.beta.inf=as.numeric(coef(loglog.mod.inf)[2]),
                       moatar.R2.inf=loglog.mod.inf.sum$r.squared,
                       moatar.R2.adj.inf=loglog.mod.inf.sum$adj.r.squared,
                       moatar.RMSE.inf=loglog.mod.inf.sum$sigma,
                       moatar.beta.sup=as.numeric(coef(loglog.mod.sup)[2]),
                       moatar.R2.sup=loglog.mod.sup.sum$r.squared,
                       moatar.R2.adj.sup=loglog.mod.sup.sum$adj.r.squared,
                       moatar.RMSE.sup=loglog.mod.sup.sum$sigma,
                       moatar.C_Q50=mean(c(CQ50.inf,CQ50.sup)))

rslt.all=cbind(LL.rslt,seg.rslt,moatar.rslt)

plot(dat.CQ$Si~dat.CQ$Q,log="xy")
x.val=seq(min(log(dat.CQ$Q),na.rm=T),max(log(dat.CQ$Q),na.rm=T),length.out=50)
LL.pred=predict(loglog.mod,data.frame(logQ=x.val),interval="confidence")
lines(exp(x.val),exp(LL.pred[,1]),col="red")
lines(exp(x.val),exp(LL.pred[,2]),col="red",lty=2)
lines(exp(x.val),exp(LL.pred[,3]),col="red",lty=2)

seg.pred=predict(loglog.mod.seg,data.frame(logQ=x.val),interval="confidence",level=0.95)
lines(exp(x.val),exp(seg.pred[,1]),col="blue")
lines(exp(x.val),exp(seg.pred[,2]),col="blue",lty=2)
lines(exp(x.val),exp(seg.pred[,3]),col="blue",lty=2)

x.val=seq(min(log(dat.CQ$Q),na.rm=T),log(Q50),length.out=50)
LL.inf=predict(loglog.mod.inf,data.frame(logQ=x.val),interval="confidence")          
lines(exp(x.val),exp(LL.inf[,1]),col="forestgreen")
lines(exp(x.val),exp(LL.inf[,2]),col="forestgreen",lty=2)
lines(exp(x.val),exp(LL.inf[,3]),col="forestgreen",lty=2)

x.val=seq(log(Q50),max(log(dat.CQ$Q),na.rm=T),length.out=50)
LL.sup=predict(loglog.mod.sup,data.frame(logQ=x.val),interval="confidence")          
lines(exp(x.val),exp(LL.sup[,1]),col="forestgreen")
lines(exp(x.val),exp(LL.sup[,2]),col="forestgreen",lty=2)
lines(exp(x.val),exp(LL.sup[,3]),col="forestgreen",lty=2)

# ,plot=T,models=c("linear","segmented","moatar")


mod.col[match(models,"log-linear")]

CQ_fun=function(logQ,logC,Q50,plot=T,models=c("log-linear","segmented","moatar"),plot.CI=T,plot.lwd=1.5,legend.pos="topleft",...){
  
  # Power Law
  loglog.mod=lm(logC~logQ)
  loglog.mod.sum=summary(loglog.mod)
  
  # variabled of interest
  loglog.beta=as.numeric(coef(loglog.mod)[2])
  loglog.alpha=as.numeric(coef(loglog.mod)[1])
  
  LL.rslt=data.frame(LL.R2=loglog.mod.sum$r.squared,
                     LL.R2.adj=loglog.mod.sum$adj.r.squared,
                     LL.RMSE=loglog.mod.sum$sigma,
                     LL.beta=loglog.beta,
                     LL.alpha=loglog.alpha)
  
  # Segmented
  # Picks 1st breakpoint
  loglog.mod.seg=segmented(loglog.mod,seg.Z=~logQ)
  loglog.mod.seg.sum=summary(loglog.mod.seg)
  
  if(nrow(loglog.mod.seg$psi)==0){
    seg.psi.est=NA
    seg.psi.SE=NA
  }else{
    seg.psi.est=loglog.mod.seg$psi[1,c(2)]
    seg.psi.SE=seg.psi=loglog.mod.seg$psi[1,c(3)]
  }
  
  seg.rslt=data.frame(seg.bk.est=exp(seg.psi.est),
                      seg.bk.SE=exp(seg.psi.SE),
                      seg.R2=loglog.mod.seg.sum$r.squared,
                      seg.R2.adj=loglog.mod.seg.sum$adj.r.squared,
                      seg.RMSE=loglog.mod.seg.sum$sigma)
  
  # Moatar
  logQ.m=logQ[logQ<log(Q50)]
  logC.m=logC[logQ<log(Q50)]
  loglog.mod.inf=lm(logC.m~logQ.m)
  loglog.mod.inf.sum=summary(loglog.mod.inf)
  CQ50.inf=predict(loglog.mod.inf,data.frame(logQ.m=log(Q50)))

  logQ.m=logQ[logQ>log(Q50)]
  logC.m=logC[logQ>log(Q50)]
  loglog.mod.sup=lm(logC.m~logQ.m)
  loglog.mod.sup.sum=summary(loglog.mod.sup)
  CQ50.sup=predict(loglog.mod.sup,data.frame(logQ.m=log(Q50)))

  moatar.rslt=data.frame(Q50=Q50,
                         moatar.beta.inf=as.numeric(coef(loglog.mod.inf)[2]),
                         moatar.R2.inf=loglog.mod.inf.sum$r.squared,
                         moatar.R2.adj.inf=loglog.mod.inf.sum$adj.r.squared,
                         moatar.RMSE.inf=loglog.mod.inf.sum$sigma,
                         moatar.beta.sup=as.numeric(coef(loglog.mod.sup)[2]),
                         moatar.R2.sup=loglog.mod.sup.sum$r.squared,
                         moatar.R2.adj.sup=loglog.mod.sup.sum$adj.r.squared,
                         moatar.RMSE.sup=loglog.mod.sup.sum$sigma,
                         moatar.C_Q50=mean(c(CQ50.inf,CQ50.sup)))
  
  rslt.all=cbind(LL.rslt,seg.rslt,moatar.rslt)
  
  if(plot==T){
    plot(dat.CQ$Si~dat.CQ$Q,log="xy",...)
    
    if(sum(match(models,"log-linear"),na.rm=T)==1){
    x.val=seq(min(logQ,na.rm=T),max(logQ,na.rm=T),length.out=50)
    LL.pred=predict(loglog.mod,data.frame(logQ=x.val),interval="confidence")
    lines(exp(x.val),exp(LL.pred[,1]),col="red",lwd=plot.lwd)
    if(plot.CI==T){
    lines(exp(x.val),exp(LL.pred[,2]),col="red",lty=2)
    lines(exp(x.val),exp(LL.pred[,3]),col="red",lty=2)
    }
    }
    
    if(sum(match(models,"segmented"),na.rm=T)==1){
    seg.pred=predict(loglog.mod.seg,data.frame(logQ=x.val),interval="confidence")
    lines(exp(x.val),exp(seg.pred[,1]),col="blue",lwd=plot.lwd)
    if(plot.CI==T){
    lines(exp(x.val),exp(seg.pred[,2]),col="blue",lty=2)
    lines(exp(x.val),exp(seg.pred[,3]),col="blue",lty=2)
    }
    }
    
    if(sum(match(models,"moatar"),na.rm=T)==1){
      abline(v=Q50)
      x.val=seq(min(logQ,na.rm=T),log(Q50),length.out=50)
      LL.inf=predict(loglog.mod.inf,data.frame(logQ.m=x.val),interval="confidence")          
      lines(exp(x.val),exp(LL.inf[,1]),col="forestgreen",lwd=plot.lwd)
      if(plot.CI==T){
      lines(exp(x.val),exp(LL.inf[,2]),col="forestgreen",lty=2)
      lines(exp(x.val),exp(LL.inf[,3]),col="forestgreen",lty=2)
      }
      
      x.val=seq(log(Q50),max(logQ,na.rm=T),length.out=50)
      LL.sup=predict(loglog.mod.sup,data.frame(logQ.m=x.val),interval="confidence")          
      lines(exp(x.val),exp(LL.sup[,1]),col="forestgreen")
      if(plot.CI==T){
      lines(exp(x.val),exp(LL.sup[,2]),col="forestgreen",lty=2)
      lines(exp(x.val),exp(LL.sup[,3]),col="forestgreen",lty=2)
      }
    }
    
    mod.col=c("red","blue","forestgreen")
    mod.col=mod.col[match(models,c("log-linear","segmented","moatar"))]
    legend.pos=if(is.na(legend.pos)==T){"topleft"}else(legend.pos)
    legend(legend.pos,
           legend=models,
           lty=1,
           col=mod.col,
           ncol=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,title="Model",title.adj = 0)
  
  }
  return(rslt.all)
}

CQ_fun(log(dat.CQ$Q),log(dat.CQ$Si),median(dat.Q$Q,na.rm=T),plot.CI=T,pch=19,ylim=c(1,10),legend.pos = "bottomright",models="moatar")



dat.Q=read.csv(paste0(data.path,"Conc_Discharge_Master/M786.2C_Q_WRTDS.csv"))
dat.Q$Date=as.Date(dat.Q$Date)
dat.Q$CY=as.numeric(format(dat.Q$Date,"%Y"))
dat.C=read.csv(paste0(data.path,"Conc_Discharge_Master/M786.2C_Si_WRTDS.csv"))
dat.C$Date=as.Date(dat.C$Date)
dat.CQ=merge(dat.Q,dat.C,"Date")
dat.CQ$CY=as.numeric(format(dat.CQ$Date,"%Y"))
ddply(dat.CQ,"CY",summarise,N.val=N.obs(Q))

yrs=c(seq(1997,2002,1),seq(2004,2019,1))
rslt.CQ=data.frame()
for(i in 1:length(yrs)){
  tmp=subset(dat.CQ,CY==yrs[i])
  tmp2=subset(dat.Q,CY==yrs[i])
  rslt=CQ_fun(log(tmp$Q),log(tmp$Si),median(tmp2$Q,na.rm=T),plot=F)
  rslt$CY=yrs[i]
  rslt.CQ=rbind(rslt.CQ,rslt)
  print(i)
}

plot(LL.R2~CY,rslt.CQ)
plot(LL.beta~CY,rslt.CQ,type="b")
abline(h=0)

plot(seg.bk.est~CY,rslt.CQ,type="b")
plot(Q50~CY,rslt.CQ,type="b")
plot(moatar.C_Q50~CY,rslt.CQ,type="b")

plot(Q50~seg.bk.est,rslt.CQ)
