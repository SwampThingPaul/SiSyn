##compare monthly slopes to annual slopes

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

concM<-read.csv("FN_Conc_Slope.csv")
names(concM)[1]<-"site"

for (i in 2:13) {
  
  concM[,i]<-str_remove(concM[,i], "\\*")
  
}

fluxM<-read.csv("FN_Flux_Slope.csv")
names(fluxM)[1]<-"site"

for (i in 2:13) {
  
  fluxM[,i]<-str_remove(fluxM[,i], "\\*")
  
}

annual<-read.csv("Si_EGRETCi_Trends_slopes.csv")

annual_conc<-annual[,c(1,2,13)]
annual_flux<-annual[,c(1,2,14)]

#set up code to remove months not Jan/Dec for MCM sites

MCM_months<-c("January", "December")

`%!in%` <- Negate(`%in%`)

annual_conc<-merge(annual_conc, concM, by=c("site"))

for (i in 4:15) {
  
  annual_conc[,i]<-ifelse(annual_conc$LTER=="MCM" & colnames(annual_conc[i]) %!in% MCM_months, 
                          NA, annual_conc[,i]) 
  
}

annual_flux<-merge(annual_flux, fluxM, by=c("site"))

for (i in 4:15) {
  
  annual_flux[,i]<-ifelse(annual_flux$LTER=="MCM" & colnames(annual_flux[i]) %!in% MCM_months, 
                          NA, annual_flux[,i]) 
  
}

LTER_list<-unique(annual_conc$LTER)

pdf("Conc_Comparison_Monthly_Annual_Slopes.pdf")

for (i in 1:length(LTER_list)) {
  
  one_site<-annual_conc[annual_conc$LTER==LTER_list[i],]
  
  one_site_melt<-melt(one_site, id=c("site", "LTER", "slopeC"))
  
  p1<-ggplot(one_site_melt, aes(x=as.factor(variable), y=as.numeric(value)))+geom_boxplot()+
    theme(axis.text.x = element_text(angle = 45))+facet_wrap(~site)+
    geom_hline(data = one_site_melt, aes(yintercept = slopeC), linetype="dashed")+xlab("Month")+
    ylab("Concentration Slope")+ggtitle(paste(LTER_list[i]))
  
  print(p1)
}

dev.off()

pdf("Flux_Comparison_Monthly_Annual_Slopes.pdf")

for (i in 1:length(LTER_list)) {
  
  one_site<-annual_flux[annual_flux$LTER==LTER_list[i],]
  
  one_site_melt<-melt(one_site, id=c("site", "LTER", "slopeF"))
  
  p1<-ggplot(one_site_melt, aes(x=as.factor(variable), y=as.numeric(value)))+geom_boxplot()+
    theme(axis.text.x = element_text(angle = 45))+facet_wrap(~site)+
    geom_hline(data = one_site_melt, aes(yintercept = slopeF), linetype="dashed")+xlab("Month")+
    ylab("Flux Slope")+ggtitle(paste(LTER_list[i]))
  
  print(p1)
}

dev.off()

       