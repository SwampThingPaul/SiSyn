require(googledrive)
require(ggplot2)
require(dplyr)
require(rcartocolor)
require(reshape2)
require(stringr)
require(comperes)
require(EflowStats)
require(tibble)
require(rcartocolor)
require(ggpubr)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

SiStab<-read.csv("StabilitySiClusters.csv")
SiStab<-SiStab[,c(2,7)]
colnames(SiStab)[2]<-"SiStab"

#read in cluster data for average cluster, modal cluster, number of clusters
si_clust<-read.csv("MonthClustersNov2022.csv")

si_clust_mode<-read.csv("SiClustersMode.csv")

siteyear_clust_num<-read.csv("SiNumClusters.csv")

#merge and clean
si_clist_mode_mean<-merge(si_clust, si_clust_mode, by="Site")

si_clist_mode_mean<-si_clist_mode_mean[,-c(2,18)]

colnames(si_clist_mode_mean)[2:13]<-seq(1,12,1)

si_clust_mode_mean_num<-merge(si_clist_mode_mean, siteyear_clust_num, by="Site")

si_clust_mode_mean_num<-si_clust_mode_mean_num[,-18]

si_clust_mode_mean_num<-merge(si_clust_mode_mean_num, SiStab, by="Site")

#calculate clster stability
si_clust_mode_mean_num$clust_stab<-1+((5-si_clust_mode_mean_num$num_clusts)/5)

#calculate overall stability
si_clust_mode_mean_num$overall_stab<-si_clust_mode_mean_num$SiStab/si_clust_mode_mean_num$clust_stab

#merge with tot which includes si mean, sd, q mean, sd, cvc/cvq
tot<-si_clust_mode_mean_num

#create df of centroid names
centroid_names<-as.data.frame(t(tibble("1"="Spring Trough",
                                       "2"="Fall Trough",
                                       "3"="Fall Peak",
                                       "4"="Spring Trough, Fall Peak",
                                       "5"="Spring Trough, Variable Summer")))
centroid_names$mode<-c(1:5)

colnames(centroid_names)<-c("Centroid_Name", "mode")

centroid_names$w_abb<-c("Spring Trough (ST)", 
                        "Fall Trough (FT)",
                        "Fall Peak (FP)",
                        "Spring Trough, Fall Peak (STFP)",
                        "Spring Trough, Variable Summer (STVS)")

#merge
tot<-merge(tot, centroid_names, by="mode")

#order
tot$w_abb<-factor(tot$w_abb,
                          levels = c("Fall Peak (FP)", "Fall Trough (FT)", "Spring Trough (ST)",
                                     "Spring Trough, Fall Peak (STFP)", "Spring Trough, Variable Summer (STVS)"))

#remove duplicates
tot<-tot[-which(duplicated(tot$Site)),]

write.csv(tot, "ClusterAllMetadata.csv")

#metadata_table<-tot[,c(2,16,17,21,22)]

#colnames(metadata_table)<-c("Site", "Data Source", "Climate Zone", "Stability (S)", "Cluster")

#write.csv(metadata_table, "SiSiteMetadata.csv")

Clim<-data.frame(unique(tot$Name))
colnames(Clim)[1]<-"Name"
unique(Clim$Name)
#Clim$Clim_abb<-c("Arid (A)", "Humid Continental (HC)", "Humid Subtropical (HS)", "Humid Temperate (HTem)", 
#                 "Humid Tropical (HTro)", "Mediterranean (M)", "SemiArid (SA)", "Subarctic (Sub)")

Clim$Clim_abb<-c("Humid Subtropical (HS)","Subarctic (Sub)", "Humid Continental (HC)", "SemiArid (SA)",
                 "Mediterranean (M)", "Humid Temperate (HTem)", "Arid (A)", "Humid Tropical (HTro)") 

Clim$Clim_abb<-factor(Clim$Clim_abb,
                      levels= c("Humid Tropical (HTro)","Arid (A)", "SemiArid (SA)", "Humid Subtropical (HS)",
                 "Mediterranean (M)", "Humid Continental (HC)", "Humid Temperate (HTem)", "Subarctic (Sub)")) 

colnames(Clim)[1]<-"Name"

tot<-merge(tot, Clim, by="Name")

#melt to plot seasonality lines
si_clust_melt<-melt(si_clust_mode_mean_num, id.vars=c("Site", "Cluster", "Name", "LTER", "mode", "num_clusts", 
                                                      "SiStab", "clust_stab", "overall_stab"))

#merge with cluster names
si_clust_melt<-merge(si_clust_melt, centroid_names, by="mode")

tiff("AvgSiClusters.tiff", width=15.5, height = 8, units = "in", res=300, family = "Times")

#plot si seasonality curves, colored by climate
ggplot(si_clust_melt, aes(variable, value))+geom_line(aes(group=Site, col=Name), size=1)+theme_classic()+
  facet_wrap(~w_abb)+labs(x="Month", y="Normalized Si Concentration", col="Climate Zone")+
  scale_color_manual(values = carto_pal(n=8, "Bold"))+theme(text = element_text(size = 19, family = "Times"))

dev.off()

tiff("ClusterCentroids.tiff", width=7, height = 20, units = "in", res=500, family = "Times")

#plot si seasonality curves, colored by climate
ggplot(si_clust_melt, aes(variable, value))+theme_classic()+
  geom_smooth(method = "loess", aes(group=Centroid_Name), col="black", size=1.3, se=TRUE, span=0.5, level=0.999)+
  facet_wrap(~Centroid_Name, ncol = 1)+labs(x="Month", y="Normalized Si Concentration", col="Climate Zone")+
  theme(text = element_text(size=20, family = "Times"))

dev.off()

tiff("ClusterMembershipBarplot.tiff", width = 4, height = 4, units = "in", res=300)

#barplot of clusters, colored by climate
ggplot(tot, aes(Centroid_Name))+geom_bar(position = "stack", aes(fill=Name))+
  theme_classic()+labs(x="", y="Count", col="Climate Zone")+
  scale_fill_manual(values = carto_pal(n=8, "Bold"))+
  theme(text = element_text(size = 20, family = "Times"), legend.position = "null",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_x_discrete(labels=c("FP", "FT", "ST", "STFP", "STVS"))

dev.off()

#histogram of overall stability colored by climate zone
ggplot(tot, aes(x=1-overall_stab))+geom_histogram(aes(fill=Name), bins = 50)+
  theme_classic()+labs(x="Si Stability", y="Count", fill="Climate Zone")+
  scale_fill_manual(values = carto_pal(n=8, "Bold"))+
  theme(text = element_text(size=20))

#boxplot of overall stability by cluster
p1<-ggplot(tot, aes(x=w_abb, y=1-overall_stab))+
  geom_jitter(aes(col=w_abb))+
  geom_boxplot(aes(fill=w_abb), alpha=0.5, outlier.size = 0)+theme_classic()+
  scale_fill_manual(values=carto_pal(n=5, "Bold"))+
  scale_color_manual(values=carto_pal(n=5, "Bold"))+
  labs(x="", y="Stability (S)", fill="Cluster", col="Cluster", tag="a")+
  theme(text = element_text(size = 20, family = "Times"), axis.text.x = element_blank())

p1

#boxplot of overall stability by climate zone
p2<-ggplot(tot, aes(x=Clim_abb, y=1-overall_stab))+
  geom_jitter(aes(col=Clim_abb))+
  geom_boxplot(aes(fill=Clim_abb), alpha=0.5, outlier.size = 0)+theme_classic()+
  scale_fill_manual(values=carto_pal(n=8, "Bold"))+
  scale_color_manual(values=carto_pal(n=8, "Bold"))+
  labs(x="", y="Stability (S)", fill="Climate Zone", col="Climate Zone", tag="c")+
  theme(text = element_text(size = 20, family="Times"), axis.text.x = element_blank())
p2

#these next four plots are looking at the relationship between different stability metrics
ggplot(si_clust_mode_mean_num, aes(1-SiStab,clust_stab))+geom_point(size=5, alpha=0.5)+
  geom_jitter(aes(col=as.character(Cluster)), size=3)+
  labs(x="Time Stability", y="Cluster Stability")+theme_classic()

ggplot(si_clust_mode_mean_num, aes(clust_stab,1-overall_stab))+geom_point(size=5, alpha=0.5)+
  geom_jitter(aes(col=as.character(Cluster)), size=3)+
  labs(x="Cluster Stability", y="Overall Stability")+theme_classic()

ggplot(si_clust_mode_mean_num, aes(1-SiStab,1-overall_stab))+geom_point(size=5, alpha=0.5)+
  geom_jitter(aes(col=as.character(Cluster)), size=3)+
  labs(x="Cluster Stability", y="Overall Stability")+theme_classic()

#function to give number of observations in group, use for plotting below
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

#overall stability by cluster, face wrapped by climate zone
ggplot(tot, aes(x=Centroid_Name, y=1-overall_stab))+
  geom_boxplot(aes(fill=Centroid_Name))+theme_classic()+
  scale_fill_manual(values=carto_pal(n=5, "Bold"))+labs(x="", y="Stability", fill="Cluster")+
  theme(text = element_text(size = 20), axis.text.x = element_blank())+
  facet_wrap(~Name)+stat_summary(fun.data = give.n, geom="text", size=4, vjust=-3)+
  ylim(0.4, 1.15)


##perform ANOVA to test significance of difference in overall stability between clusters and climate zones
#cluster
model<-aov(overall_stab~Centroid_Name, data=tot)
summary(model)

centroid_tukey<-TukeyHSD(model, conf.level = 0.95)
plot(centroid_tukey)

#convert to df
tukey_df<-as.data.frame(centroid_tukey$Centroid_Name)
tukey_df<-rownames_to_column(tukey_df)
tukey_df$test1<-str_extract(tukey_df$rowname, "[^-]+")
tukey_df$test2<-str_replace(tukey_df$rowname, '.+-(.+)', '\\1')

colnames(tukey_df)[5]<-"p_val"

p_mat<-tukey_df %>% long_to_mat("test1", "test2", "p_val")

mat_long<-melt(p_mat)
mat_long$sig5<-ifelse(mat_long$value <= 0.05, "yes", "no")

p3<-ggplot(mat_long, aes(Var1, Var2))+geom_raster(aes(fill=sig5))+theme_classic()+
  scale_fill_manual(values = c("yes" = "#008080", "no" = "#ca562c"), na.value = "white")+
  labs(x="", y="", fill="Significant", tag="b")+theme(text = element_text(size = 20, family="Times"))+
  scale_x_discrete(labels=c("FT", "ST", "STFP", "STVS"))+
  scale_y_discrete(labels=c("FP", "FT", "ST", "STFP"))
p3
##perform ANOVA on climate
tot[which(tot$Name=="Semi-Arid"),"Name"]<-"SemiArid"

tot$Name<-factor(tot$Name, levels = c("Humid Tropical","Arid", "SemiArid", "Humid Subtropical",
                                       "Mediterranean", "Humid Continental", "Humid Temperate", "Subarctic"))

model<-aov(overall_stab~Name, data=tot)
summary(model)

centroid_tukey<-TukeyHSD(model, conf.level = 0.95)
plot(centroid_tukey)

#convert to df
tukey_df<-as.data.frame(centroid_tukey$Name)
tukey_df<-rownames_to_column(tukey_df)
tukey_df$test1<-str_extract(tukey_df$rowname, "[^-]+")
tukey_df$test2<-str_replace(tukey_df$rowname, '.+-(.+)', '\\1')

colnames(tukey_df)[5]<-"p_val"

p_mat<-tukey_df %>% long_to_mat("test1", "test2", "p_val")

mat_long<-melt(p_mat)
mat_long$sig5<-ifelse(mat_long$value <= 0.05, "yes", "no")

mat_long$Var1<-factor(mat_long$Var1, levels = c("Humid Tropical","Arid", "SemiArid", "Humid Subtropical",
                                      "Mediterranean", "Humid Continental", "Humid Temperate", "Subarctic"))

mat_long$Var2<-factor(mat_long$Var2, levels = c("Humid Tropical","Arid", "SemiArid", "Humid Subtropical",
                                                "Mediterranean", "Humid Continental", "Humid Temperate", "Subarctic"))


p4<-ggplot(mat_long, aes(Var1, Var2))+geom_raster(aes(fill=sig5))+theme_classic()+
  scale_fill_manual(values = c("yes" = "#008080", "no" = "#ca562c"), na.value = "white")+
  labs(x="", y="", fill="Significant", tag="d")+theme(text = element_text(size = 20, family="Times"), 
                                             axis.text.x = element_text(angle=45, hjust=1))+
  scale_y_discrete(labels=c("HTro", "A", "SA", "HS", "M", "HC", "HTem"))+
  scale_x_discrete(labels=c("A", "SA", "HS", "M", "HC", "HTem", "Sub"))
p4

tiff("ClusterClimateStabBoxplotsSig.tiff", width = 15, height = 8, units = "in", res = 300)

ggarrange(p1, p3, p2, p4, widths = c(0.63, 0.37, 0.63, 0.37))

dev.off()

tot %>%
  group_by(Clim_abb) %>%
  summarise(mean_stab = mean(1-overall_stab))

tot %>%
  group_by(Centroid_Name) %>%
  summarise(mean_stab = mean(1-overall_stab))

sum(tot$overall_stab > 0.50)/202


