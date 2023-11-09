install.packages("loadflex")
library(loadflex)

#subset UMR site for loadflex modeling
#estimation data: daily discharge data
CH00.1M_Q = subset(WRTDS_discharge_allsites, WRTDS_discharge_allsites$site.name=="CH00.1M")
head(CH00.1M_Q)

#interpolation data: DSi grad sample data
CH00.1M_Chem = subset(X20201111_masterdata, X20201111_masterdata$site=="CH00.1M")
unique(CH00.1M_Si$variable)
CH00.1M_Si = subset(CH00.1M_Chem, CH00.1M_Chem$variable=="DSi")
names(CH00.1M_Si)[names(CH00.1M_Si)=="Sampling.Date"] = "Date"

CH00.1M_intdat = merge(CH00.1M_Q, CH00.1M_Si, by="Date")
head(CH00.1M_intdat)
#keep only columns that are needed for loadflex
CH00.1M_intdat = data.frame("Date"=CH00.1M_intdat$Date,
                            "Q"=CH00.1M_intdat$Q,
                            "DSi"=CH00.1M_intdat$value)

#metadata description of the dataset and desired output
meta = metadata(constituent="DSi", flow="Q", dates="Date",
                conc.units="mg L^-1", flow.units="cfs", load.units="kg",
                load.rate.units="kg d^-1", site.name="CH00.1M",
                consti.name="Dissolved SiO2")
                
#loadflex doesn't recognize uM concentration units - convert back to mg/L
CH00.1M_intdat$DSi = CH00.1M_intdat$DSi * 0.06

#run composite model using rloadest package
library(rloadest)
CH00.1M_lr = loadReg2(loadReg(DSi~Q, data=CH00.1M_intdat,
                              flow="Q", dates="Date", conc.units="mg/L", load.units="kg"))
CH00.1M_lc = loadComp(reg.model=CH00.1M_lr, interp.format="conc", interp.data=CH00.1M_intdat)

getFittedModel(CH00.1M_lc)
ggplot2::qplot(x=Date, y=Resid, data=getResiduals(CH00.1M_lc, newdata=CH00.1M_intdat))

#generate point predictions from daily Q values
preds_lc = predictSolute(CH00.1M_lc,"flux",CH00.1M_Q,se.pred=T,date=T)
head(preds_lc)

#aggregate point predictions for daily load estimate
CH00.1M_aggs_lc = aggregateSolute(preds_lc,meta,"flux rate","day")
#double check daily flux units here: kg/day?
#confirmed with sample value load calculation

#plot daily load estimates
library(ggplot2)
ggplot(data=aggs_lc, aes(x=as.Date(Day),y=Flux_Rate))+
  geom_point()+
  scale_x_date(date_labels="%Y")

#AND site
#prep GSMACK site discharge and DSi data
GSMACK_Q = subset(WRTDS_discharge_allsites, WRTDS_discharge_allsites$site.name=="GSMACK")
head(GSMACK_Q)
GSMACK_Si = subset(X20201111_masterdata, X20201111_masterdata$site=="GSMACK" & X20201111_masterdata$variable=="DSi")
head(GSMACK_Si)
names(GSMACK_Si)[names(GSMACK_Si)=="Sampling.Date"] = "Date"
GSMACK_intdat = merge(GSMACK_Q,GSMACK_Si,by="Date")
GSMACK_intdat = data.frame("Date"=GSMACK_intdat$Date,
                           "Q"=GSMACK_intdat$Q,
                           "DSi"=GSMACK_intdat$value*0.06)
#run composite model
GSMACK_meta = metadata(constituent="DSi", flow="Q", dates="Date",
                       conc.units="mg L^-1", flow.units="cfs", load.units="kg",
                       load.rate.units="kg d^-1", site.name="GSMACK",
                       consti.name="Dissolved SiO2")

GSMACK_lr = loadReg2(loadReg(DSi~Q, data=GSMACK_intdat,
                              flow="Q", dates="Date", conc.units="mg/L", load.units="kg"))
GSMACK_lc = loadComp(reg.model=GSMACK_lr, interp.format="conc", interp.data=GSMACK_intdat)
#point predictions and daily load estimates
GSMACK_preds_lc = predictSolute(GSMACK_lc,"flux",GSMACK_Q,se.pred=T,date=T)
GSMACK_aggs_lc = aggregateSolute(GSMACK_preds_lc,GSMACK_meta,"flux rate","day")
#plot daily load estimates
ggplot(data=GSMACK_aggs_lc, aes(x=as.Date(Day),y=Flux_Rate))+
  geom_point()+
  scale_x_date(date_labels="%Y")

#LUQ site
#prep QS site discharge and DSi data
QS_Q = subset(WRTDS_discharge_allsites, WRTDS_discharge_allsites$site.name=="QS")
head(QS_Q)
QS_Si = subset(X20201111_masterdata, X20201111_masterdata$site=="QS" & X20201111_masterdata$variable=="DSi")
names(QS_Si)[names(QS_Si)=="Sampling.Date"] = "Date"
QS_intdat = merge(QS_Q,QS_Si,by="Date")
QS_intdat = data.frame("Date"=QS_intdat$Date,
                           "Q"=QS_intdat$Q,
                           "DSi"=QS_intdat$value*0.06)
#Loadflex composite model
QS_meta = metadata(constituent="DSi", flow="Q", dates="Date",
                       conc.units="mg L^-1", flow.units="cfs", load.units="kg",
                       load.rate.units="kg d^-1", site.name="QS",
                       consti.name="Dissolved SiO2")

#QS_lr = loadReg2(loadReg(DSi~Q, data=QS_intdat,
#                             flow="Q", dates="Date", conc.units="mg/L", load.units="kg"))
#there are  multiple Q measurements for certain dates at this site, we can only have one obs per day in intdat
QS_intdat_nodup = QS_intdat[!duplicated(QS_intdat$Date),]
QS_lr = loadReg2(loadReg(DSi~Q, data=QS_intdat_nodup,
                         flow="Q", dates="Date", conc.units="mg/L", load.units="kg"))
QS_lc = loadComp(reg.model=QS_lr, interp.format="conc", interp.data=QS_intdat)
#point predictions and daily load estimates
QS_preds_lc = predictSolute(QS_lc,"flux",QS_Q,se.pred=T,date=T)
QS_aggs_lc = aggregateSolute(QS_preds_lc,QS_meta,"flux rate","day")
#plot daily load estimates
ggplot(data=QS_aggs_lc, aes(x=as.Date(Day),y=Flux_Rate))+
  geom_point()+
  scale_x_date(date_labels="%Y")