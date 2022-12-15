#install.packages("vegan")
require(vegan)
require(ggplot2)
require(PCAtools)
require(ggpubr)
require(ggforce)
#install.packages("ggrepel")
require(ggrepel)
require(dplyr)
require(tibble)
require(reshape2)

##rda for SiSyn Data - updated with new data on 1/18/22
#setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Data/PCA_RDA")

new_RDA<-read.csv("RDA_data_120122.csv")
new_RDA <- new_RDA[,-10]

# #read in trend files
# #WRTDS_trends<-read.csv("Si_EGRETCi_Trends_slopes_090721.csv")
# MK_trends<-read.csv("All_MK_Trends_011722.csv")
# 
# ##read in site characteristics
# site_chars<-read.csv("SiteCharacteristics_PCA_WRTDS_03082022.csv")
# site_chars$Latitude<-abs(site_chars$Latitude)
# 
# #read in climate and change names to match site chars for merge
# climate<-read.csv("MAP_MAT_byStream.csv")
# names(climate)[c(1,2)]<-c("LTER.x", "SITE")
# 
# #merge climate and site chars dataframes
# site_chars<-merge(site_chars, climate, by=c("LTER.x", "SITE"))

#assign to "trends tot"
trends_tot<-new_RDA

##select conc, yield, and Q - will change for percent vs absolute
trends_tot<-trends_tot[,c(1,2,3,5,7,8)]

#set up dataframe for centered and scaled data
final_center<-trends_tot

site_chars<-new_RDA[,c(1:3,9:27)]

#scale data - this will change if including yield (2:6 vs 2:8)
final_center[c(4:6)]<-data.frame(sapply(final_center[c(4:6)], scale))

#keep only columns to run in RDA
final_trends<-final_center

#remove Sagehen - missing other data
#final_trends<-final_trends[-46,]

#unique column for later PCA formatting
site_chars$unique<-paste0(site_chars$LTER, "_", site_chars$site)

#remove rows with missing data
site_chars<-site_chars[complete.cases(site_chars),]

#center and scale
final_center<-site_chars

#remove Q2, not included in MK trends
#final_center<-final_center[-31,]

#center and scale - this will not change depending on yield
final_center[c(4:22)]<-data.frame(sapply(final_center[c(4:22)], scale))

final_chars<-final_center

#run rda - this will change with yield data
my.rda<-rda(final_trends[,c(4:6)], final_chars[,c(4:22)])

#summarize RDA
rda_sum<-summary(my.rda)

#extract data from rda summary - from example code from kjo
st=as.data.frame(rda_sum$sites[,1:2])
st$Biome<-final_center$Biome
st$Lithology<-final_center$major_rock
sp=as.data.frame(rda_sum$species[,1:2])*2
yz=as.data.frame(rda_sum$biplot[,1:2])

#test for significance in yz - change numbers in ifelse depending on output of ttest
t.test(yz$RDA1, conf.level = 0.99)
yz$RDA1_sig<-ifelse(yz$RDA1 < -0.1886656, "sig",
                    ifelse(yz$RDA1 > 0.1546657, "sig", "not sig"))
t.test(yz$RDA2, conf.level = 0.99)
yz$RDA2_sig<-ifelse(yz$RDA2 < -0.0806054, "sig",
                    ifelse(yz$RDA2 > 0.1378437, "sig", "not sig"))

#create column that says significance of each variable
yz$sig_col<-ifelse(yz$RDA1_sig=="sig"&yz$RDA2_sig=="sig", "both",
                   ifelse(yz$RDA1_sig=="sig", "1",
                          ifelse(yz$RDA2_sig=="sig", "2", "")))
yz[,c(1,2)]<-yz[,c(1,2)]*4

#remove insignificant columns
yz <- yz %>% 
  rownames_to_column() %>%
  mutate_all(na_if,"")
yz<-yz[complete.cases(yz),]

setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/RDA")
write.csv(yz, "RDA_Loadings_Significance.csv", row.names=FALSE)


#set up df for loadings plot
rda_loadings<-rda_sum$biplot

rda_loadings<-rda_loadings[rownames(rda_loadings) %in% yz$rowname,]


setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/RDA")

write.csv(rda_loadings, "RDA_Loadings_Absolute_99.csv")

#melt
rda_loadings_melt<-melt(rda_loadings)
names(rda_loadings_melt)<-c("Variable", "RDA_axis", "Eigenvalue")
#only keep RDA 1 and 2
rda_loadings_melt<-subset(rda_loadings_melt, rda_loadings_melt$RDA_axis=="RDA1"|rda_loadings_melt$RDA_axis=="RDA2")

#make yield plot
pdf("RDA_Loadings_Absolute_99CI.pdf")

ggplot(rda_loadings_melt, aes(x=RDA_axis, y=Eigenvalue))+
  geom_point(aes(color=Eigenvalue), size=10)+
  geom_text_repel(aes(label = Variable), max.overlaps = 50, box.padding = unit(2, 'lines'))+
  scale_color_gradient2(low = "red", mid = "white", high = "blue")

dev.off()

#reorder the names of Biome so that they plot in this order on axis and legens
st$Biome<-factor(st$Biome, levels = c("Tropical rainforest", 
                                                 "Tropical savanna",
                                                 "Temperate grassland",
                                                 "Temperate deciduous forest",
                                                 "Temperate coniferous forest",
                                                 "Boreal forest",
                                                 "Alpine tundra", 
                                                 "Arctic tundra",
                                                 "Polar desert"))

#plot RDA

f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
cols <- f("Set2")

cols_final<-c(cols, "#A46B5C")
#show_col(cols_final)

#assign colors
col_values<-c("Boreal forest" = cols_final[1],
              "Temperate deciduous forest" = cols_final[2],
              "Alpine tundra" = cols_final[3],
              "Arctic tundra" = cols_final[4],
              "Tropical rainforest" = cols_final[5],
              "Tropical savanna" = cols_final[6],
              "Temperate grassland" = cols_final[7],
              "Polar desert" = cols_final[8],
              "Temperate coniferous forest" = cols_final[9])

col_pal<-list(scale_color_manual(values = col_values),scale_fill_manual(values = col_values))



#plot
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Manuscripts/SiSynthesis/Figures")
pdf("Figure 3b RDA.pdf", height=5, width=7)

ggplot() +
  geom_mark_ellipse(st, mapping = aes(RDA1,RDA2, fill=Biome, col=Biome))+
  geom_point(data = st,aes(RDA1,RDA2,color=Biome),size=4)+
  theme_bw()+
  theme(legend.position = "right")+
  theme_classic()+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12))+
  geom_segment(data = sp,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
  arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
  type = "closed"),linetype=1, size=0.6,colour = "darkgreen")+
  geom_text_repel(data = sp,aes(RDA1,RDA2,label=row.names(sp)),colour="darkgreen")+
  geom_segment(data = yz,aes(x = 0, y = 0, xend = RDA1, yend = RDA2, lty=sig_col), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"), size=0.6)+
  geom_text_repel(data = yz,aes(RDA1,RDA2,label=rowname))+
  ylim(-4,4)+xlim(-4,2)+
  col_pal+
  ggtitle("3b")+
  xlab("RDA1, 28% variation")+ylab("RDA2, 10% variation")

ggsave("Figure 3b_RDA_Absolute_99CI.png", height = 8, width = 12)

dev.off()



