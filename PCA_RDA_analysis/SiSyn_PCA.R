require(ggalt)
require(PCAtools)
require(ggpubr)
#install.packages("ggforce")
require(ggforce)
require(ggplot2)
require(viridis)
require(RColorBrewer)

##PCA on silica trends
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#read in trend files
WRTDS_trends<-read.csv("Si_EGRETCi_Trends_slopes_090721.csv")
MK_trends<-read.csv("All_MK_Trends_090821.csv")

#merge two dataframes
trends_tot<-merge(WRTDS_trends, MK_trends, by=c("LTER", "site"))

#keep important columns
trends_tot<-trends_tot[,c(1,2,13,14,19,21,23)]

#create "unique" column - important for setting up pca
trends_tot$unique<-paste0(trends_tot$LTER, "_", trends_tot$site)

#read in site characterists, climate, and landcover files (we should just consolidate these...)
site_chars<-read.csv("SiteCharacteristics_PCA_WRTDS_03082022.csv")
site_chars$Latitude<-abs(site_chars$Latitude)
names(site_chars)[c(1,2)]<-c("LTER", "site")

climate<-read.csv("MAP_MAT_byStream.csv")

names(climate)[2]<-c("site")

landcover<-read.csv("SilicaSites_litho_and_landcover.csv")

#set all landcover NA to 0, otherwise rows will be removed by complete cases below
landcover[is.na(landcover)]<-0
landcover$site<-gsub("-", " ", landcover$site)

#merge
trends_tot<-merge(trends_tot, site_chars, by=c("LTER", "site"))

trends_tot<-merge(trends_tot, climate, by=c("LTER", "site"))

trends_tot<-merge(trends_tot, landcover, by=c("LTER", "site"))

#remove rows with missing data
trends_tot<-trends_tot[complete.cases(trends_tot),]

trends_tot_csv<-trends_tot[,c(1,2,10,14,18:20,23,24,25, 37:47)]

write.csv(trends_tot_csv, "SiteChars_06212022.csv")

#set up dataframe for centered and scaled data
final_center<-trends_tot

#scale data - only keep columns using in PCA
final_center[c(14,18:20,24,25, 37:47)]<-data.frame(sapply(final_center[c(14,18:20,24,25, 37:47)], scale))

#examine ranges of centered and scaled data
ranges = vapply(final_center[,c(14,18:20,24,25, 37:47)], FUN=function(col) c(min(col, na.rm = TRUE), max(col, na.rm = TRUE)), numeric(2))
rownames(ranges) = c("vmin", "vmax") 
rframe = as.data.frame(t(ranges))  # make ymin/ymax the columns
rframe$varName = rownames(rframe)

#plot ranges
ggplot()+geom_dumbbell(data=rframe, aes(y=varName, x=vmin, xend=vmax))+
  xlab("Centered/Scaled Value")+ylab("Variable")

#keep only columns to run in PCA
final_mat<-final_center[c(14,18:20,24,25, 37:47)]

colnames(final_mat)[c(1,9,15)]<-c("drainage area", "carbonate & evaporite", "snow & ice")

#transpose and rename columns
final_mat_t<-data.frame(t(final_mat))
colnames(final_mat_t)<-final_center$unique

#set metadata columns
metadata<-final_center[,c(1,2,3:7,8:11, 21, 22,23)]
rownames(metadata)<-final_center$unique

#run PCA
pca<-pca(final_mat_t, metadata = metadata)

#screeplot (shows % variance explained by each PC)
screeplot(pca)

#plot 1 PC against another - from PCA tools
biplot(pca, x="PC1", y="PC2", showLoadings = TRUE, colby = "Biome2.x", legendPosition = "right", lab=NULL,
       ntopLoadings = 30, ylim=c(-8,8), xlim = c(-10,10), encircle = TRUE, encircleFill = TRUE)


pairsplot(pca, components = c("PC1", "PC2", "PC3", "PC4"))

####THIS CODE TO CREATE CUSTOM PCA PLOT WITH SIGNIFICANCE OF EIGENVECTORS###

#use this to test significance of PC1 and PC2 eigenvectors
loadings<-pca$loadings[,c(1,2)]

#t test - copy and past min and max of 95% CI into ifelse statement below
#PC1
t.test(loadings$PC1, conf.level = 0.99)

loadings$PC1_sig<-ifelse(loadings$PC1 < -0.1593077, "sig",
       ifelse(loadings$PC1 > 0.1939830, "sig", "not sig"))

#PC2 t test
t.test(loadings$PC2, conf.level = 0.99)

loadings$PC2_sig<-ifelse(loadings$PC2 < -0.2690913, "sig",
                         ifelse(loadings$PC2 > 0.0450696, "sig", "not sig"))

#create final significance column
loadings$sig<-ifelse(loadings$PC1_sig=="sig"&loadings$PC2_sig=="sig", "both",
                     ifelse(loadings$PC1_sig=="sig", "1",
                            ifelse(loadings$PC2_sig=="sig", "2", NA)))


loadings<-loadings[complete.cases(loadings$sig),]
#write.csv(loadings, "PCA_trends_sig.csv")

#create dataframe of loadings
pc_loadings<-pca$rotated[,c(1:3)]

#add biome column to loadings df
pc_loadings$Biome<-final_center$Biome2.x
loadings$rowname<-rownames(loadings)

pc_loadings$Biome<-factor(pc_loadings$Biome, levels = c("Tropical rainforest","Tropical savanna",
                                                        "Temperate coniferous forest","Temperate deciduous forest", 
                                                         "Temperate grassland", "Alpine tundra",
                                                         "Boreal forest","Arctic tundra", "Polar desert"))

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

#plot final PCA figure with siginifcance eigenvctors
pdf("FinalPCA_052522.pdf", width = 10, height = 7)

ggplot()+geom_segment(loadings, mapping=aes(x=0, y=0, xend=PC1*10, yend=PC2*10, lty=sig),size=0.6)+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=PC1*10, yend=PC2*10), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"), lty="blank", size=0.6)+
  geom_point(pc_loadings, mapping = aes(PC1, PC2, col=Biome), size=4)+theme_classic()+
  geom_text_repel(data = loadings,aes(PC1*10,PC2*10,label=rowname))+
  geom_mark_ellipse(pc_loadings, mapping = aes(PC1, PC2, fill=Biome, col=Biome))+
  labs(x= "PC1, 30% variation", y="PC2, 19% variation", col="Biome", lty="PC Significance")+
  scale_linetype_manual(values=c("dotted", "dashed", "solid"))+col_pal+xlim(-5,5)


dev.off()
