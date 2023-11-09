setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/Manuscripts/Silica/Cluster/SiteMetadata_for_Sub")

info<-read.csv("Site_Information_All.csv")

info$cv_q<-info$SD.Q/info$Mean.Q

ggplot(info, aes(cv_q, Overall.Stability))+geom_point()


setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

monthly_results<-read.csv("Full_Results_Monthly_GFN_WRTDS.csv")

monthly_results<-subset(monthly_results, monthly_results$chemical=="DSi")

range_si<-monthly_results %>%
  group_by(stream) %>%
  summarise(si_range=max(Conc_mgL)-min(Conc_mgL))

colnames(range_si)[1]<-"Site"

info_range<-merge(range_si, info, by="Site")

pdf("Ranges.pdf", width = 5, height = 5, family="Times")

ggplot(info_range, aes(x=Cluster, y=si_range))+geom_boxplot()+
  lims(y=c(0,23))+theme_classic()+
  theme(text = element_text(size=20))+
  labs(x="", y="Range in Si Concentration (mg/L)")+
  scale_x_discrete(labels=c("Fall Peak" = "FP", "Fall Trough" = "FT", "Spring Trough" = "ST", "Spring Trough, Fall Peak" = "STFP",
                            "Spring Trough, Variable Summer" = "STVS"))

dev.off()
