library(loadflex)
library(lubridate)
library(ggplot2)
library(dplyr)

#run single ARC site (Imnavait_Weir) using WRTDS prep files

#create interpolation data frame with Si concentration and corresponding Q
#merge Q and Si dataframes
head(Imnavait_Weir_Q)
names(Imnavait_Weir_Q)[names(Imnavait_Weir_Q)=="date"] = "Date"
names(Imnavait_Weir_Q)[names(Imnavait_Weir_Q)=="Q"] = "Q_cms"
head(Imnavait_Weir_Si)
Imnavait_int = merge(Imnavait_Weir_Q, Imnavait_Weir_Si)
head(Imnavait_int)

#duplicated days and negative/0 values not permitted for Loadflex
#use average daily Si for interpolation data
Imnavait_int = 
  Imnavait_int %>%
  group_by(Date) %>%
  summarize(
    Q_cms = mean(Q),
    Si = mean(Si)
  )
head(Imnavait_int)

ggplot(Imnavait_int, aes(x=Q_cms, y=Si))+
  geom_point()+
  scale_x_continuous(trans="log10")+
  theme_bw(base_size=24)

#metadata description of the dataset and desired output
meta = metadata(constituent="Si", flow="Q_cms", dates="Date",
                conc.units="mg L^-1", flow.units="cms", load.units="kg",
                load.rate.units="kg d^-1", site.name="Imnavait_Weir",
                consti.name="Dissolved SiO2")

#run composite model using rloadest package
library(rloadest)
Imnavait_lr = loadReg2(loadReg(Si~Q_cms, data=Imnavait_int,
                              flow="Q_cms", dates="Date", conc.units="mg/L", load.units="kg"))
Imnavait_lc = loadComp(reg.model=Imnavait_lr, interp.format="conc", interp.data=Imnavait_int)

getFittedModel(Imnavait_lc)
ggplot2::qplot(x=Date, y=Resid, data=getResiduals(Imnavait_lc, newdata=Imnavait_int))

#generate point predictions from daily Q values
preds_lc = predictSolute(Imnavait_lc,"flux",Imnavait_Weir_Q,se.pred=T,date=T)
head(preds_lc)

names(preds_lc)[names(preds_lc)=="fit"] = "Si_load_kg.day"

#add month and year
preds_lc$day.month = format(as.Date(preds_lc$date), "%m-%d")
preds_lc$month = month(as.Date(preds_lc$date))
preds_lc$year = year(as.Date(preds_lc$date))

#merge daily flow data
head(preds_lc)
head(Imnavait_Weir_Q)
names(preds_lc)[names(preds_lc)=="date"] = "Date"

Imnavait_loads = merge(preds_lc, Imnavait_Weir_Q, by=c("Date"))
head(Imnavait_loads)

#plot by date, each line is year
ggplot(preds_lc)+
  geom_line(aes(x=Date, y=Si_load_kg.day))

#write csv
write.csv(Imnavait_loads, file="Imnavait_dailySi_Loadflex.csv")

#get monthly averages of loads
Imnavait_avgs = 
Imnavait_loads %>%
  group_by(month, year) %>%
  summarize(
    count = n(),
    meanSiLoad = mean(Si_load_kg.day),
    seSiLoad = sd(Si_load_kg.day)/sqrt(count)
  )

ggplot(Imnavait_avgs, aes(x=month, y=meanSiLoad, color=year))+
  geom_point()
