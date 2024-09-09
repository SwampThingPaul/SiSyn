
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

wrtds_out<-read.csv("WRTDS_GFN_MonthlyResults_AllSites_062822.csv")

unique(wrtds_out$site)

wrtds_out_one<-subset(wrtds_out, wrtds_out$site=="GSWS10")

wrtds_out_one$month_year<-paste0(wrtds_out_one$Month, "-", wrtds_out_one$Year)
wrtds_out_one$num_obs<-seq(1,nrow(wrtds_out_one),1)

wrtds_out_one_cropped<-subset(wrtds_out_one, wrtds_out_one$Year > 1994)

p1<-ggplot(wrtds_out_one_cropped, aes(num_obs, Conc))+geom_line()+theme_classic()+
  labs(x="Time", y="[DSi] (mg/L)", tag = "a")+
  scale_x_continuous(breaks = c(148, 208, 268, 328, 388), labels = c(1995, 2000, 2005, 2010, 2015))+
  theme(text = element_text(size = 15))

p1

wrtds_one_agg<-wrtds_out_one_cropped %>%
  group_by(Month) %>%
  summarise(mean_conc=mean(Conc))


p2<-ggplot(wrtds_one_agg, aes(Month, mean_conc))+geom_smooth(se=F, col="black")+theme_classic()+
  labs(x="Month", y="[DSi] (mg/L)", tag = "b")+
  ggtitle("Average Curve")+scale_x_continuous(breaks = seq(1,12,1), labels = seq(1,12,1))+
  theme(text = element_text(size = 15))

p3<-ggplot(wrtds_out_one_cropped, aes(Month, Conc, col=Year))+geom_line(aes(group=Year))+theme_classic()+
  labs(x="Month", y="[DSi] (mg/L)", tag = "c")+
  ggtitle("Yearly Curve")+scale_x_continuous(breaks = seq(1,12,1), labels = seq(1,12,1))+
  scale_color_gradient(low = "grey80", high = "black")+theme(text = element_text(size = 15))

ggarrange(p1, ggarrange(p2, p3, widths = c(0.45, 0.55)), nrow = 2)


