library(dplyr)
library(ggplot2)

#check if all cryo sites are in CQ MBLM analysis file
cryo_sites = unique(ObsDSi_sumstats_6_2_22$site)
length(intersect(cryo_sites,LTERsites_CQmblm_9Jun22$site.name))
setdiff(cryo_sites,LTERsites_CQmblm_9Jun22$site.name)

#filter mblm results to cryo sites alone
cryo_mblm = subset(LTERsites_CQmblm_9Jun22,LTERsites_CQmblm_9Jun22$site.name%in%cryo_sites)

#repeat for annual CQ slope data
cryo_annualCQ = subset(LongTerm_annualCQ_slopes,LongTerm_annualCQ_slopes$site.name%in%cryo_sites)
#remove NA p values?
cryo_annualCQ = na.omit(cryo_annualCQ)

#get stats on number of significant/non-significant annual relationships
cryo_annualCQ_stats =
  cryo_annualCQ %>%
  group_by(site.name,sig)%>%
  summarize(
    n=n()
  )

#plot annual CQ slope for each cryo site
head(cryo_annualCQ)
ggplot(cryo_annualCQ, aes(x=year, y=slope, color=sig))+
  geom_point()+
  scale_color_manual(values=c("gray","black"))+
  scale_x_continuous(limits=c(1980,2020))+
  geom_hline(yintercept=0, lty="dashed")+
  facet_wrap(~site.name,scales="free_y")+
  theme_bw()
