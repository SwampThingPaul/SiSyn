##test and download individual files for NIVA data##

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NIVA river data_v2")

niva_q<-read.csv("NIVA_Q.csv")

niva_si<-read.csv("NIVA_Si.csv")

names(niva_q)<-c("ID", "Site", "Stream", "Date", "Q")

niva_q$Date<-as.Date(niva_q$Date, "%m/%d/%y")

niva_si<-niva_si[,c(1:3, 17)]

names(niva_si)<-c("ID", "Site", "Date", "Si")

niva_si$Si<-as.numeric(niva_si$Si)

niva_si$Date<-as.Date(niva_si$Date, "%m/%d/%y")

streams<-unique(niva_q$Site)

#pdf("NIVA_Q_Si_Check.pdf")

missing<-list()

for (i in 1:length(streams)) {
  
  q<-subset(niva_q, niva_q$Site==streams[i])
  
  date_range <- seq(min(q$Date), max(q$Date), by = 1) 
  num_missing_days<-length(date_range[!date_range %in% q$Date])
  missing_days_prop<-num_missing_days/length(date_range)
  
  missing[[i]]<-c(num_missing_days, missing_days_prop)
  
  # si<-subset(niva_si, niva_si$Site==streams[i])
  # 
  # fact<-max(si$Si, na.rm = TRUE)/max(q$Q, na.rm = TRUE)
  # 
  # p1<-ggplot()+geom_line(q, mapping = aes(Date, Q))+geom_point(si, mapping = aes(Date, Si/fact), col="red")+
  #   theme_bw()+scale_y_continuous(sec.axis = sec_axis(trans = ~.*fact))+ggtitle(paste(streams[i]))
  # 
  # plot(p1)
  
}

missing_df<-as.data.frame(do.call(rbind, missing))

missing_df$site<-streams

#dev.off()

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NewSites_Sept2022")

for (i in 1:length(streams)) {
  
  q<-subset(niva_q, niva_q$Site==streams[i])
  
  si<-subset(niva_si, niva_si$Site==streams[i])
  
  write.csv(q, paste0(streams[i], "_Q.csv"))
  
  write.csv(si, paste0(streams[i], "_Si.csv"))
  
}




