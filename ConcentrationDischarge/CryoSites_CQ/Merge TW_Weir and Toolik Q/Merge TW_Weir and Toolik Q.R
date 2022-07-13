library(dplyr)

#check which dates are in merged DSi-Q file and individual ARC sites
ARC_WRTDS__Q = subset(merged_DSi_Q_WRTDSlongterm_3Jun22, merged_DSi_Q_WRTDSlongterm_3Jun22$site.name=="TW Weir" |
                        merged_DSi_Q_WRTDSlongterm_3Jun22$site.name=="Toolik Inlet")
TW_Discharge_Kling$Date = as.Date(TW_Discharge_Kling$DateTime,format="%Y-%m-%d")
Toolik_Inlet_Kling$Date = as.Date(Toolik_Inlet_Kling$Date_Time,format="%Y-%m-%d")
#get mean daily discharge
TW_MeanDailyQ = TW_Discharge_Kling %>%
  group_by(Date) %>%
  summarize(
    MeanDailyQ_Lsec = mean(Q_Lsec,na.rm=T)
  )
Toolik_Inlet_MeanDailyQ = Toolik_Inlet_Kling %>%
  group_by(Date) %>%
  summarize(
    MeanDailyQ_m3sec = mean(Q_m3sec,na.rm=T)
  )
