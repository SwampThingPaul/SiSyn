require(dplyr)
require(dataRetrieval)
require(EGRET)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

camels<-read.csv("CAMELSchem.csv", na.strings="(null)")

camels$sample_start_dt<-as.Date(camels$sample_start_dt)


table(camels_si$gauge_id)

camels_si<-camels %>%
  select(gauge_id, sample_start_dt, si) %>%
  na.omit() %>%
  group_by(gauge_id) %>%
  filter(n() > 50)

camels_si_dates<-camels_si %>%
  group_by(gauge_id) %>%
  summarise(mindate = min(sample_start_dt),
            maxdate = max(sample_start_dt))

camels_si_dates<-camels_si_dates %>%
  filter(year(maxdate) > 2010)

LTid<-camels_si_dates$gauge_id

camels_si<-camels_si[c(camels_si$gauge_id %in% LTid),]

Q<-list()

for (i in 41:length(LTid)) {
  
  dat<-readNWISDaily(LTid[i], "00060", "","")
  
  dat$gauge_id<-LTid[i]
  
  Q[[i]]<-dat
  
}

Qmaster<-do.call(rbind, Q)

Qmaster$gauge_id<-as.integer(Qmaster$gauge_id)

pdf("Q_Si_CAMELS_Chem.pdf")

for (i in 1:length(LTid)) {
  
  si_site<-subset(camels_si, camels_si$gauge_id==LTid[i])
  
  LTid_Q<-paste(LTid[i])
  
  Q_site<-subset(Qmaster, Qmaster$gauge_id==LTid_Q)
  
  fact<-max(si_site$si, na.rm = TRUE)/max(Q_site$Q, na.rm = TRUE)
  
  p1<-ggplot()+
    geom_line(Q_site, mapping = aes(Date, Q))+
    geom_point(si_site, mapping=aes(sample_start_dt, si/fact), col="red")+
    theme_bw()+scale_y_continuous(sec.axis = sec_axis(trans = ~.*fact))+
    ggtitle(LTid[i])
  
  print(p1)
  
}

dev.off()

camels_Q<-c("01434025","01440000","01491000","02096846","05412500","06468250","09306242",
"09352900","11264500","07060710","01054200","01170100","01632900","02327100","08079600","08086290",
"08195000","09505800")

camels_names<-scan(text="Biscuit Brook,Flat Brook,Choptank River,Cane Creek,Turkey River,James River,Corral Gulch,Vallecito Creek,Merced River,North Slyamore Creek,Wild River,Green River,Smith Creek,Sopchoppy River,DMF Brazos River,Big Sandy Creek,Frio River,West Clear Creek", what="", sep=",")

camels_names_nospace<- gsub(" ", "_", camels_names)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NewSites_Sept2022")

for (i in 1:length(camels_Q)) {
  
  dat<-readNWISDaily(camels_Q[i], "00060", "","")
  
  write.csv(dat, file = paste0(camels_names_nospace[i], "_Q.csv"))

  
}

camels_si<-camels[, c(1,2,3,16)]

for (i in 1:length(camels_Q)) {
  
  site<-ifelse(substr(camels_Q[i], 1, 1)=="0", substr(camels_Q[i], 2, 8), camels_Q[i])
  
  dat<-subset(camels_si, camels_si$gauge_id==site)
  
  write.csv(dat, file = paste0(camels_names_nospace[i], "_Si.csv"))
  
}


new_sites_files<-list.files(path = ".", pattern = ".csv")

pdf("CamelsKeptSites.pdf")

for (i in 1:length(camels_names_nospace)) {
  
  filesQ<-grep(camels_names_nospace[i], new_sites_files)[1]
  
  filesSi<-grep(camels_names_nospace[i], new_sites_files)[2]
  
  Q<-read.csv(new_sites_files[filesQ])
  
  Q$Date<-as.Date(Q$Date)
  
  Si<-read.csv(new_sites_files[filesSi])
  
  Si$sample_start_dt<-as.Date(Si$sample_start_dt)
  
  Si$si<-as.numeric(Si$si)
  
  p1<-ggplot()+geom_line(Q, mapping=aes(Date, Q))+
    geom_point(Si, mapping=aes(sample_start_dt, si), col="red")+
    ggtitle(paste(camels_names_nospace[i]))
  
  print(p1)
  
}

dev.off()





