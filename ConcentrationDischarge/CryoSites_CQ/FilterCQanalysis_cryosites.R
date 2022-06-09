library(dplyr)
library(ggplot2)

#check if all cryo sites are in CQ MBLM analysis file
cryo_sites = unique(ObsDSi_sumstats_6_2_22$site)
length(intersect(cryo_sites,AnnualCQslopes_mlbmresults$site.name))
setdiff(cryo_sites,AnnualCQslopes_mlbmresults$site.name)

#filter mblm results to cryo sites alone
cryo_mblm = subset(AnnualCQslopes_mlbmresults,AnnualCQslopes_mlbmresults$site.name%in%cryo_sites)

#plot annual CQ slope for each cryo site
head(cryo_mblm,aes(x=year, y=slope, color=sig))
ggplot(cryo_mblm)+
  geom_point()+
  scale_color_manual(values=c("gray","black"))+
  geom_abline(yintercept=mblm_intercept,slope=mblm_slope)+
  scale_x_continuous(limits=c(1980,2020))+
  geom_hline(yintercept=0, lty="dashed")+
  facet_wrap(~site.name,scales="free_y")+
  theme_bw()
