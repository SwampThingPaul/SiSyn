library(dplyr)
library(lubridate)
library(lfstat)
library(ggplot2)

#create lfobj for TW_Weir
#need c("flow","day","month","year")
TW_Weir_lfdf = data.frame(flow=TW_Weir_missingQ$Q,
                          day=day(TW_Weir_missingQ$Date),
                          month=TW_Weir_missingQ$month,
                          year=year(TW_Weir_missingQ$Date))
TW_Weir_lfobj = createlfobj(TW_Weir_lfdf)

#straight line interpolations
TW_Weir_lf = lfnainterpolate(TW_Weir_lfobj)
#add date
TW_Weir_lf$date = as.Date(paste(TW_Weir_lf$year, TW_Weir_lf$month, TW_Weir_lf$day, sep="-"))
#restrict interpolated data to summer period; May-August
TW_Weir_filledQ = subset(TW_Weir_lf, TW_Weir_lf$month>=5 & TW_Weir_lf$month<=8)

#plot filled Q by date; facet by year
ggplot(TW_Weir_filledQ, aes(x=date, y=flow))+
  geom_line()+
  facet_wrap(~year, scales="free")

#export interpolated Q
write.csv(TW_Weir_filledQ, file="TW_Weir_filledQ.csv")
