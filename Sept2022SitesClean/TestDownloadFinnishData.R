##test finish Q and Si data - download Si and Q individual files

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

finn<-read.csv("Finnish_DischargeData_08112022.csv")

names(finn)<-c("Id", "Date","Q")

finn_chem<-read.csv("Finn_Si.csv")

finn_chem<-finn_chem[,c(1,2,3,12)]

names(finn_chem)<-c("Id","Station","Date","SiO2")

sites<-unique(finn$Id)

finn$Date<-as.Date(finn$Date, "%d.%m.%Y")

finn_chem$Date<-as.Date(finn_chem$Date, "%m/%d/%y")

#missing<-list()

pdf("Finn_Si_Q.pdf")

for (i in 1:length(sites)) {

  finn_site<-subset(finn, finn$Id==sites[i])
  
  finn_si_site<-subset(finn_chem, finn_chem$Id==sites[i])
  
  fact<-max(finn_si_site$SiO2, na.rm = TRUE)/max(finn_site$Q, na.rm = TRUE)
  
  p1<-ggplot()+geom_line(finn_site, mapping=aes(Date, Q))+geom_point(finn_si_site, mapping = aes(Date, SiO2/fact), col="red")+
    theme_bw()+scale_y_continuous(sec.axis = sec_axis(trans = ~.*fact))+ggtitle(sites[i])
  
  print(p1)
  
  # date_range <- seq(min(finn_site$Date), max(finn_site$Date), by = 1) 
  # num_missing_days<-length(date_range[!date_range %in% finn_site$Date])
  # missing_days_prop<-num_missing_days/length(date_range)
  # 
  # missing[[i]]<-c(num_missing_days, missing_days_prop)
  
  
    
}

dev.off()

missing_df<-as.data.frame(do.call(rbind, missing))

missing_df$site<-sites

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NewSites_Sept2022")

for (i in 1:length(sites)) {
  
  finn_site<-subset(finn, finn$Id==sites[i])
  
  finn_si_site<-subset(finn_chem, finn_chem$Id==sites[i])
  
  write.csv(finn_site, paste0("Site", sites[i], "_Q.csv"))
  
  write.csv(finn_si_site, paste0("Site", sites[i], "_Si.csv"))
  
}



