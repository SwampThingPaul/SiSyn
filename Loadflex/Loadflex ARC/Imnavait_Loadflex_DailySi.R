library(loadflex)
library(lubridate)
library(ggplot2)
library(dplyr)

#run single ARC site (Imnavait_Weir) using WRTDS prep files

#create interpolation data frame with Si concentration and corresponding Q
#merge Q and Si dataframes
head(Imnavait_Weir_Q)
head(Imnavait_Weir_Si)
Imnavait_int = merge(Imnavait_Weir_Q, Imnavait_Weir_Si)
head(Imnavait_int)

#duplicated days and negative/0 values not permitted for Loadflex
Imnavait_int = Imnavait_int[!duplicated(Imnavait_int$Date),]
Imnavait_int[Imnavait_int <= 0] = NA
Imnavait_int = na.omit(Imnavait_int)

#metadata description of the dataset and desired output
meta = metadata(constituent="Si", flow="Q", dates="Date",
                conc.units="mg L^-1", flow.units="cfs", load.units="kg",
                load.rate.units="kg d^-1", site.name="Imnavait_Weir",
                consti.name="Dissolved SiO2")

#run composite model using rloadest package
library(rloadest)
Imnavait_lr = loadReg2(loadReg(Si~Q, data=Imnavait_int,
                              flow="Q", dates="Date", conc.units="mg/L", load.units="kg"))
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
names(Imnavait_Weir_Q)[names(Imnavait_Weir_Q)=="Date"] = "date"
Imnavait_loads = merge(preds_lc, Imnavait_Weir_Q, by=c("date"))
head(Imnavait_loads)

#plot by date, each line is year
ggplot(preds_lc)+
  geom_line(aes(x=day.month, y=Si_load_kg.day, color=year))

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
