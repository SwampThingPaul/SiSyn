library(dplyr)

#check if all cryo sites are in CQ MBLM analysis file
cryo_sites = unique(ObsDSi_sumstats_6_2_22$site)
length(intersect(cryo_sites,LTERsites_CQmblm_3Jun22$site.name))

#filter mblm results to cryo sites alone
cryo_mblm = subset(LTERsites_CQmblm_3Jun22,LTERsites_CQmblm_3Jun22$site.name%in%cryo_sites)
