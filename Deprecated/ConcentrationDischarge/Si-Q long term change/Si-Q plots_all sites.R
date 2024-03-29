library(ggplot2)
library(scales)
library(lubridate)

#merge all site Q and Si data
Si_Q_allsites = merge(WRTDS_discharge_allsites, WRTDS_DSi_mergedsites, by=c("site.name","Date"))
write.csv(Si_Q_allsites,file="Si_Q_WRTDS_sites.csv")

#plot Si v Q; facet by site; color by year
Si_Q_allsites$year = year(Si_Q_allsites$Date)

ggplot(Si_Q_allsites, aes(x=Q, y=Si, color=year)) +
  geom_point(alpha=0.25)+
  scale_color_gradientn(colors=rainbow(7))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(.x)))+
  labs(x=expression("Q (10"^x*")"))+
  facet_wrap(~site.name, scales="free")+
  theme_bw(base_size=12)

#histogram of Si and Q data by year; facet by site
WRTDS_discharge_allsites$Year = year(WRTDS_discharge_allsites$Date)
ggplot(WRTDS_discharge_allsites, aes(x=Year))+
  geom_histogram()+
  labs(y="Q data count")+
  facet_wrap(~site.name)
WRTDS_DSi_allsites$Year = year(WRTDS_DSi_allsites$Date)
ggplot(WRTDS_DSi_allsites, aes(x=Year))+
  geom_histogram()+
  labs(y="Q data count")+
  facet_wrap(~site.name)
