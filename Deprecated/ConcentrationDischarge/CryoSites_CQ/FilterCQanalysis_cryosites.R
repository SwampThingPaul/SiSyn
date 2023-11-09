library(dplyr)
library(ggplot2)

#check if all cryo sites are in CQ MBLM analysis file
cryo_sites = unique(ObsDSi_sumstats_10_22_22$site)

length(intersect(cryo_sites,AnnualCQslopes_mlbmresults_1Nov22$site.name))
setdiff(cryo_sites,AnnualCQslopes_mlbmresults_1Nov22$site.name)
#these sites are missing from the "all sites" mblm results:
# "ARIKAREE"      "Bohner Stream" "\xd8STEGLO"    "Toolik Inlet"  "TW Weir"       

#filter mblm results to cryo sites alone
cryo_annualCQ_mblm = subset(AnnualCQslopes_mlbmresults_1Nov22,AnnualCQslopes_mlbmresults_1Nov22$site.name%in%cryo_sites)
cryo_mblm = subset(allsites_CQmblm_1Nov22, allsites_CQmblm_1Nov22$site.name%in%cryo_sites)
cryo_mblm$mblm_sig = ifelse(cryo_mblm$mblm_pvalue<=0.1,"sig","not sig")
cryo_annualCQ = subset(annualCQ_slopes_1Nov22,annualCQ_slopes_1Nov22$site.name%in%cryo_sites)

#plot annual CQ slope for each cryo site
head(cryo_annualCQ)
head(cryo_mblm)
ggplot()+
  geom_point(data=cryo_annualCQ, aes(x=year, y=sig_slopes, shape=sig, fill=sig))+
  geom_abline(data=cryo_mblm,aes(intercept=mblm_intercept,slope=mblm_slope,color=mblm_sig, lty=mblm_sig))+
  scale_shape_manual(values=c(1,21))+
  scale_fill_manual(values=c("white","black"))+
  scale_color_manual(values=c("gray","black"))+
  scale_linetype_manual(values=c("dashed","solid"))+
  scale_x_continuous(limits=c(1980,2020))+
  #geom_hline(yintercept=0, lty="dashed")+
  facet_wrap(~site.name,scales="free_y")+
  theme_bw()

write.csv(cryo_annualCQ,file="cryo_annualCQ_results_1Nov22.csv")
write.csv(cryo_mblm,file="cryo_mblm_results_1Nov22.csv")
