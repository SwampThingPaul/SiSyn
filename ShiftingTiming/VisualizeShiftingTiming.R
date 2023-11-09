##plot change over time for each site
#set wd
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#link to monthly WRTDS data
link<-"https://drive.google.com/file/d/1qSwukNdNMgOnpk_fd7RTHDEnvaFvXv_r/view?usp=share_link"

#get file
file_get<-drive_get(as_id(link))

#download
drive_download(file_get$drive_resource,  overwrite=T)

#read in monthly results
monthly_results<-read.csv("Full_Results_Monthly_GFN_WRTDS.csv")

chem_count<-monthly_results %>%
  group_by(stream) %>%
  count(chemical)

#filter to only include Si data
#monthly_results<-subset(monthly_results, monthly_results$chemical=="DSi")

site<-monthly_results %>%
  dplyr::filter(stream=="YUKON RIVER")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NewSites_Sept2022")

interp<-read.csv("interp_final.csv")

colnames(interp)[5]<-"stream"

interp_monthly<-merge(monthly_results, interp, by=c("stream", "Month", "Year"))

interp_monthly_summary<-interp_monthly %>%
  group_by(stream) %>%
  count()

write.csv(interp_monthly, "interp_monthly.csv")

write.csv(interp_monthly_summary, "interp_monthly_summary.csv")


dfyear<-monthly_results %>%
  filter(stream=="PICEANCE CREEK AT WHITE RIVER") %>%
  summarise(unique(Year))

monthly_results %>%
  filter(stream=="YAMPA RIVER AT DEERLODGE PARK") %>%
  ggplot(aes(x=Month, y=Conc_mgL, col=Year))+geom_point()

#remove sites without continuous flow
remove_site<-c("SADDLE STREAM 007", "MARTINELLI")

monthly_results<-monthly_results[!monthly_results$stream %in% remove_site,]

monthly_results<-monthly_results[!monthly_results$LTER=="MCM",]

#set WY
monthly_results$WY<-ifelse(monthly_results$Month > 9, monthly_results$Year+1, monthly_results$Year)

#choose columns we need
month_conc<-monthly_results[,c("stream", "Month", "WY", "Conc_mgL", "chemical")]

WY_count<-month_conc %>% group_by(stream) %>%
  summarise(n_distinct(WY))

WY_20<-subset(WY_count, WY_count$`n_distinct(WY)` > 19)

streams<-unique(WY_20$stream)

pdf("AllSites_Shifting_MultiChem.pdf")

for (i in 1:length(streams)) {
  
  one_site<-subset(month_conc, month_conc$stream==streams[i])
  
  p1<-ggplot(one_site, aes(Month, Conc_mgL))+geom_line(aes(col=WY, group=WY))+theme_bw()+ggtitle(streams[i])+
    facet_wrap(~chemical, scales = "free")
  
  print(p1)
  
}

dev.off()



