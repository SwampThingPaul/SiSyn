##drivers, curve metrics RDA
install.packages("vegan")
require(stringr)
require(vegan)
require(ggplot2)
require(ggrepel)
require(dplyr)
require(tibble)
require(reshape2)

#read in, clean metrics
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

metrics<-read.csv("Curve_Metrics.csv")

metrics<-metrics[!metrics$stream=="Imnavait Weir",]

metadata<-read.csv("AllSiClusterData.csv")

meta_crop<-metadata[,c("Site", "slope", "cvc_cvq", "Name.x", "SiStab", "Centroid_Name")]

colnames(meta_crop)<-c("stream", "CQ", "cvc_cvq", "Climate", "Stability", "Cluster")

metrics<-merge(metrics, meta_crop, by="stream")

metrics_scale<-metrics

metrics_scale[,c(3,5,9,10,11,14,15)]<-data.frame(sapply(metrics_scale[,c(3,5,9,10,11,14,15)], scale))

#read in, clean drivers
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

drivers<-read.csv("AllDrivers_Harmonized.csv")

drivers<-drivers[,!colnames(drivers) %in% c("X.1", "X", "LTER", "Stream_Name", "rndCoord.lon", "rndCoord.lat", "Name", "cycle1")]
drivers<-drivers[!duplicated(drivers$Stream_ID),]

#crop to only relevant drivers
drivers_cropped<-drivers[,c("Stream_ID","med_si","CV_C","med_q","CV_Q", "cvc_cvq","slope","ClimateZ","Latitude","Longitude",
                            "drainSqKm", "num_days","prop_area","precip", "evapotrans", "temp", "npp",
                            "cycle0", "elevation_mean_m", colnames(drivers[30:58]))]

#replace NA with 0 in proportion of land/lithology/soils
drivers_cropped[,c(21:48)]<-replace(drivers_cropped[,c(21:48)], is.na(drivers_cropped[,c(21:48)]), 0)

drivers_cropped$stream<-str_split(drivers_cropped$Stream_ID, '__', simplify = TRUE)[,2]

drivers_cropped<-drivers_cropped[complete.cases(drivers_cropped),]

drivers_scale<-drivers_cropped

remove_these<-c(setdiff(drivers_scale$stream, metrics_scale$stream), setdiff(metrics_scale$stream, drivers_scale$stream))

drivers_scale<-drivers_scale[-which(drivers_scale$stream %in% remove_these),]

drivers_scale<-drivers_scale[,-which(colnames(drivers_scale) %like% "major")]

metrics_scale<-metrics_scale[-which(metrics_scale$stream %in% remove_these),]

#run rda
my.rda<-rda(metrics_scale[,c(3,5,9,10,11,14,15)], drivers_scale[,c(2:7,9:45)])

#summarize RDA
rda_sum<-summary(my.rda)

#extract data from rda summary - from example code from kjo
st=as.data.frame(rda_sum$sites[,1:2])
st$Cluster<-metrics_scale$Cluster
sp=as.data.frame(rda_sum$species[,1:2])*2
yz=as.data.frame(rda_sum$biplot[,1:2])

#test for significance in yz - change numbers in ifelse depending on output of ttest
t.test(yz$RDA1, conf.level = 0.99)
yz$RDA1_sig<-ifelse(yz$RDA1 < -0.05584158, "sig",
                    ifelse(yz$RDA1 > 0.06280446, "sig", "not sig"))
t.test(yz$RDA2, conf.level = 0.99)
yz$RDA2_sig<-ifelse(yz$RDA2 < -0.11314155, "sig",
                    ifelse(yz$RDA2 > 0.07662962, "sig", "not sig"))

#create column that says significance of each variable
yz$sig_col<-ifelse(yz$RDA1_sig=="sig"&yz$RDA2_sig=="sig", "both",
                   ifelse(yz$RDA1_sig=="sig", "1",
                          ifelse(yz$RDA2_sig=="sig", "2", "")))
yz[,c(1,2)]<-yz[,c(1,2)]*4

#remove insignificant columns
yz <- yz %>% 
  rownames_to_column()

yz$sig_col[yz$sig_col == ""] <- NA

yz<-yz[complete.cases(yz$sig_col),]

#set up df for loadings plot
rda_loadings<-rda_sum$biplot

rda_loadings<-rda_loadings[rownames(rda_loadings) %in% yz$rowname,]

write.csv(rda_loadings, "RDA_Loadings_Percent_99.csv")

#melt
rda_loadings_melt<-melt(rda_loadings)
names(rda_loadings_melt)<-c("Variable", "RDA_axis", "Eigenvalue")
#only keep RDA 1 and 2
rda_loadings_melt<-subset(rda_loadings_melt, rda_loadings_melt$RDA_axis=="RDA1"|rda_loadings_melt$RDA_axis=="RDA2")

ggplot(rda_loadings_melt, aes(x=RDA_axis, y=Eigenvalue))+
  geom_point(aes(color=Eigenvalue), size=10)+
  geom_text_repel(aes(label = Variable), max.overlaps = 50, box.padding = unit(2, 'lines'))+
  scale_color_gradient2(low = "red", mid = "white", high = "blue")


ggplot() +
  geom_point(data = st,aes(RDA1,RDA2,color=Cluster),size=4)+
  theme_bw()+
  theme(legend.position = "right", text = element_text(size=20))+
  geom_segment(data = sp,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "red")+
  geom_text_repel(data = sp,aes(RDA1,RDA2,label=row.names(sp)),colour="red")+
  geom_segment(data = yz,aes(x = 0, y = 0, xend = RDA1, yend = RDA2, lty=sig_col), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"), size=0.6)+
  geom_text_repel(data = yz,aes(RDA1,RDA2,label=rowname))+
  scale_color_manual(values = carto_pal(n=5, "Bold"))






