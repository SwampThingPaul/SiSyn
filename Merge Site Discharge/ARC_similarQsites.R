#get list of sites in concatenated discharge file
summary(all_discharge)
unique(all_discharge$site)

all_discharge_NAomit = na.omit(all_discharge)
unique(all_discharge_NAomit$site.name)

#plot range of Q by site
library(ggplot2)
ggplot(all_discharge_NAomit, aes(x=site.name, y=Q))+
  geom_boxplot(outlier.shape=NA)+
  scale_y_continuous(limits=c(0,0.02))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))

median(all_discharge_NAomit$Q[all_discharge_NAomit$site.name=="Imnavait Weir"])

#subset sites with median 0 discharge values
library(dplyr)
mutate(as.Date(as.numeric(all_discharge_NAomit$Date), origin = "1970-01-01"))
#date is giving error; remove for subsetting
discharge_nodate = data.frame(site.name=all_discharge_NAomit$site.name,
                              Q=all_discharge_NAomit$Q)
median_zero_Q = 
  discharge_nodate %>%
  group_by(site.name) %>%
  summarize(
    medianQ = median(Q)
  )

median_zero_Q = subset(median_zero_Q, median_zero_Q$medianQ==0)
zero_Q_sites = c(median_zero_Q$site.name)

#subset sites from all_dsicharge files with 0 median Q
median_zero_sites = all_discharge_NAomit[all_discharge_NAomit$site.name %in% zero_Q_sites,]
length(unique(median_zero_sites$site.name))

#plot annual hydrograph for 14 sites
library(lubridate)
median_zero_sites$year = year(median_zero_sites$Date)
median_zero_sites$yday = yday(median_zero_sites$Date)
median_zero_sites = subset(median_zero_sites, select=-c(Date))

annual_Q_median_0 =
  median_zero_sites %>%
  group_by(site.name, yday) %>%
  summarize(
    meanQ = mean(Q)
  )

ggplot(annual_Q_median_0, aes(x=yday, y=meanQ))+
  geom_line()+
  xlab("Day of year")+
  ylab("Mean daily discharge (cms)")+
  facet_wrap(~site.name, scales="free")+
  theme_bw()
