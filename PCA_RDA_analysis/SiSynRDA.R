#install.packages("vegan")
require(vegan)
require(ggplot2)
#install.packages("ggrepel")
require(ggrepel)
require(dplyr)
require(tibble)
require(reshape2)

##rda for SiSyn Data - updated with new data on 1/18/22
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#read in trend files
#WRTDS_trends<-read.csv("Si_EGRETCi_Trends_slopes_090721.csv")
MK_trends<-read.csv("All_MK_Trends_011722.csv")

##read in site characteristics
site_chars<-read.csv("SiteCharacteristics_PCA_WRTDS_03082022.csv")
site_chars$Latitude<-abs(site_chars$Latitude)

#read in climate and change names to match site chars for merge
climate<-read.csv("MAP_MAT_byStream.csv")
names(climate)[c(1,2)]<-c("LTER.x", "SITE")

#merge climate and site chars dataframes
site_chars<-merge(site_chars, climate, by=c("LTER.x", "SITE"))

#assign to "trends tot"
trends_tot<-MK_trends

#keep important columns - this will change depending on if keeping yield or not 
#keep/remove col 12 and 14
trends_tot<-trends_tot[,c(1,2,4,6,8,10,16)]

#set up dataframe for centered and scaled data
final_center<-trends_tot

#scale data - this will change if including yield (2:6 vs 2:8)
final_center[c(2:6)]<-data.frame(sapply(final_center[c(2:6)], scale))

#keep only columns to run in RDA
final_trends<-final_center

#remove Sagehen - missing other data
final_trends<-final_trends[-46,]

#unique column for later PCA formatting
site_chars$unique<-paste0(site_chars$LTER.x, "_", site_chars$SITE)

#remove rows with missing data
site_chars<-site_chars[complete.cases(site_chars),]

#center and scale
final_center<-site_chars

#remove Q2, not included in MK trends
final_center<-final_center[-31,]

#center and scale - this will not change depending on yield
final_center[c(6:14,18,19)]<-data.frame(sapply(final_center[c(6:14,18,19)], scale))

final_chars<-final_center

#run rda - this will change with yield data
my.rda<-rda(final_trends[,c(2:6)], final_chars[,c(6:14,18,19)])

#summarize RDA
rda_sum<-summary(my.rda)

#extract data from rda summary - from example code from kjo
st=as.data.frame(rda_sum$sites[,1:2])
st$Biome<-final_center$Biome2
st$Lithology<-final_center$major_rock
sp=as.data.frame(rda_sum$species[,1:2])*2
yz=as.data.frame(rda_sum$biplot[,1:2])

#test for significance in yz - change numbers in ifelse depending on output of ttest
t.test(yz$RDA1)
yz$RDA1_sig<-ifelse(yz$RDA1 < -0.1111761, "sig",
                    ifelse(yz$RDA1 > 0.3260022, "sig", "not sig"))
t.test(yz$RDA2)
yz$RDA2_sig<-ifelse(yz$RDA2 < -0.2509118, "sig",
                    ifelse(yz$RDA2 > 0.2123280, "sig", "not sig"))

#create column that says significance of each variable
yz$sig_col<-ifelse(yz$RDA1_sig=="sig"&yz$RDA2_sig=="sig", "both",
                   ifelse(yz$RDA1_sig=="sig", "1",
                          ifelse(yz$RDA2_sig=="sig", "2", "")))
yz[,c(1,2)]<-yz[,c(1,2)]*2

#remove insignificant columns
yz <- yz %>% 
  rownames_to_column() %>%
  mutate_all(na_if,"")
yz<-yz[complete.cases(yz),]

#set up df for loadings plot
rda_loadings<-rda_sum$biplot
#melt
rda_loadings_melt<-melt(rda_loadings)
names(rda_loadings_melt)<-c("Variable", "RDA_axis", "Eigenvalue")
#only keep RDA 1 and 2
rda_loadings_melt<-subset(rda_loadings_melt, rda_loadings_melt$RDA_axis=="RDA1"|rda_loadings_melt$RDA_axis=="RDA2")

#make yield plot
pdf("RDA_Loadings_V3_NoYields.pdf")

ggplot(rda_loadings_melt, aes(x=RDA_axis, y=Eigenvalue))+
  geom_point(aes(color=Eigenvalue), size=10)+
  geom_text_repel(aes(label = Variable), max.overlaps = 50, box.padding = unit(2, 'lines'))+
  scale_color_gradient2(low = "red", mid = "white", high = "blue")

dev.off()

#plot RDA
pdf("SiSynRDA_Lithology.pdf", height = 8, width = 12)

#plot
ggplot() +
  geom_point(data = st,aes(RDA1,RDA2,color=Lithology),size=4)+
  theme_bw()+
  theme(legend.position = "right")+
  geom_segment(data = sp,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
  arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
  type = "closed"),linetype=1, size=0.6,colour = "grey")+
  geom_text_repel(data = sp,aes(RDA1,RDA2,label=row.names(sp)),colour="grey")+
  geom_segment(data = yz,aes(x = 0, y = 0, xend = RDA1, yend = RDA2, lty=sig_col), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"), size=0.6)+
  geom_text_repel(data = yz,aes(RDA1,RDA2,label=rowname))

dev.off()



