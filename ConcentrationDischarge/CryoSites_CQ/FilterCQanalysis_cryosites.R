library(dplyr)
library(ggplot2)

#check if all cryo sites are in CQ MBLM analysis file
cryo_sites = unique(ObsDSi_sumstats_6_2_22$site)
<<<<<<< Updated upstream
length(intersect(cryo_sites,AnnualCQslopes_mlbmresults$site.name))
setdiff(cryo_sites,AnnualCQslopes_mlbmresults$site.name)
=======
#add Toolik and TW Weir
length(intersect(cryo_sites,LTERsites_CQmblm_9Jun22$site.name))
setdiff(cryo_sites,LTERsites_CQmblm_9Jun22$site.name)
>>>>>>> Stashed changes

#filter mblm results to cryo sites alone
cryo_annualCQ_mblm = subset(AnnualCQslopes_mlbmresults,AnnualCQslopes_mlbmresults$site.name%in%cryo_sites)
cryo_mblm = subset(LTERsites_CQmblm_9Jun22, LTERsites_CQmblm_9Jun22$site.name%in%cryo_sites)
cryo_annualCQ = subset(LongTerm_annualCQ_slopes_9Jun22,LongTerm_annualCQ_slopes_9Jun22$site.name%in%cryo_sites)

#plot annual CQ slope for each cryo site
head(cryo_mblm)
ggplot()+
  geom_point(data=cryo_annualCQ_mblm, aes(x=year, y=slope))+
  geom_abline(data=cryo_mblm,aes(intercept=mblm_intercept,slope=mblm_slope,color=mblm_sig,lty=mblm_sig))+
  scale_color_manual(values=c("gray","black"))+
  scale_linetype_manual(values=c("dashed","solid"))+
  scale_x_continuous(limits=c(1980,2020))+
  geom_hline(yintercept=0, lty="dashed")+
  facet_wrap(~site.name,scales="free_y")+
  theme_bw()

write.csv(cryo_annualCQ,file="cryo_annualCQ_results.csv")
write.csv(cryo_mblm,file="cryo_mblm_results.csv")
