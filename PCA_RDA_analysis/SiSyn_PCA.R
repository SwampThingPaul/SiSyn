require(ggalt)
require(PCAtools)
require(ggpubr)

##PCA on silica trends
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#read in files
WRTDS_trends<-read.csv("Si_EGRETCi_Trends_slopes_090721.csv")
MK_trends<-read.csv("All_MK_Trends_090821.csv")

#merge two dataframes
trends_tot<-merge(WRTDS_trends, MK_trends, by=c("LTER", "site"))

#keep important columns
trends_tot<-trends_tot[,c(1,2,13,14,19,21,23)]

#create "unique" column - important for setting up pca
trends_tot$unique<-paste0(trends_tot$LTER, "_", trends_tot$site)

site_chars<-read.csv("SiteCharacteristics_PCA_WRTDS_03082022.csv")
site_chars$Latitude<-abs(site_chars$Latitude)

names(site_chars)[c(1,2)]<-c("LTER", "site")

climate<-read.csv("MAP_MAT_byStream.csv")

names(climate)[2]<-c("site")

trends_tot<-merge(trends_tot, site_chars, by=c("LTER", "site"))

trends_tot<-merge(trends_tot, climate, by=c("LTER", "site"))

trends_tot<-trends_tot[complete.cases(trends_tot),]

#set up dataframe for centered and scaled data
final_center<-trends_tot

#scale data
final_center[c(3:7,12:20,24,25)]<-data.frame(sapply(final_center[c(3:7,12:20,24,25)], scale))

#examine ranges of centered and scaled data
ranges = vapply(final_center[,c(3:7,12:20,24,25)], FUN=function(col) c(min(col, na.rm = TRUE), max(col, na.rm = TRUE)), numeric(2))
rownames(ranges) = c("vmin", "vmax") 
rframe = as.data.frame(t(ranges))  # make ymin/ymax the columns
rframe$varName = rownames(rframe)

#plot ranges
ggplot()+geom_dumbbell(data=rframe, aes(y=varName, x=vmin, xend=vmax))+
  xlab("Centered/Scaled Value")+ylab("Variable")

#keep only columns to run in PCA
final_mat<-final_center[c(3:7,12:20,24,25)]

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

loadings<-pca$loadings[,c(1,2,3)]

t.test(loadings$PC1)

loadings$PC1_sig<-ifelse(loadings$PC1 < -0.20677010, "sig",
       ifelse(loadings$PC1 > 0.05550425, "sig", "not sig"))

t.test(loadings$PC2)

loadings$PC2_sig<-ifelse(loadings$PC2 < -0.09683561, "sig",
                         ifelse(loadings$PC2 > 0.17495415, "sig", "not sig"))

t.test(loadings$PC3)

loadings$PC3_sig<-ifelse(loadings$PC3 < -0.09656431, "sig",
                         ifelse(loadings$PC3 > 0.17518187, "sig", "not sig"))

write.csv(loadings, "PCA_trends_sig.csv")

pc_loadings<-pca$rotated[,c(1:3)]

pdf("Trends_PCA_Lithology.pdf", width = 12, height = 10)

#plot 1 PC against another
biplot(pca, x="PC1", y="PC2", showLoadings = TRUE, colby = "major_rock", legendPosition = "right", lab=NULL,
       ntopLoadings = 30, ylim=c(-8,8), xlim = c(-10,10))

biplot(pca, x="PC1", y="PC3", showLoadings = TRUE, colby = "major_rock", legendPosition = "right", lab=NULL,
       ntopLoadings = 30, ylim=c(-8,8), xlim = c(-10,10))

biplot(pca, x="PC2", y="PC3", showLoadings = TRUE, colby = "major_rock", legendPosition = "right", lab=NULL,
       ntopLoadings = 30, ylim=c(-8,8), xlim = c(-10,10))

#plot loadings of each PC
plotloadings(pca)

dev.off()



pairsplot(pca, components = c("PC1", "PC2", "PC3"), colby = "Biome2", legendPosition = "left")

##PCA on site characteristics
site_chars<-read.csv("SiteCharPCA.csv")

site_chars<-read.csv("PCA_WRTDS_093021.csv")

#unique column for later PCA formatting
site_chars$unique<-paste0(site_chars$LTER.x, "_", site_chars$SITE)

#remove rows with missing data
site_chars<-site_chars[complete.cases(site_chars),]

#center and scale
final_center<-site_chars

final_center[c(4:8, 11:16)]<-data.frame(sapply(final_center[c(4:8, 11:16)], scale))

#examine ranges of centered and scaled data
ranges = vapply(final_center[,c(4:8, 11:16)], FUN=function(col) c(min(col, na.rm = TRUE), max(col, na.rm = TRUE)), numeric(2))
rownames(ranges) = c("vmin", "vmax") 
rframe = as.data.frame(t(ranges))  # make ymin/ymax the columns
rframe$varName = rownames(rframe)


#plot ranges
ggplot()+geom_dumbbell(data=rframe, aes(y=varName, x=vmin, xend=vmax))+
  xlab("Centered/Scaled Value")+ylab("Variable")

final_mat<-final_center[c(4:8, 11:16)]
final_mat_t<-data.frame(t(final_mat))
colnames(final_mat_t)<-final_center$unique

metadata<-final_center[,c(1,2,3,9,10,17)]
rownames(metadata)<-final_center$unique

pca<-pca(final_mat_t, metadata = metadata)

screeplot(pca)

pdf("PCA_with_pChange.pdf", width = 12, height = 10)

biplot(pca, x="PC1", y="PC2", showLoadings = TRUE, colby = "LTER.x", legendPosition = "right", lab=NULL,
       ntopLoadings = 20)

dev.off()

pairsplot(pca, colby = "LTER.x", components = getComponents(pca, seq_len(3)))

plotloadings(pca)

#correlate PC with trends

trends_tot$unique<-paste0(trends_tot$LTER, "_", trends_tot$site)

pc_loadings<-tibble::rownames_to_column(pc_loadings)
names(pc_loadings)[1]<-"unique"

pc_trends<-merge(pc_loadings, trends_tot, by="unique")

pdf("PCA_trends_plots.pdf")

for (i in 2:4) {
  
  for (k in 7:11) {
    
    PC<-pc_trends[i]
    trend<-pc_trends[k]
    
    pc_graph<-cbind(PC, trend, pc_trends$LTER)
    names(pc_graph)<-c("PC", "trend", "LTER")
    
    p1<-ggplot(pc_graph, aes(PC, trend, col=LTER))+geom_point()+
      xlab(colnames(pc_trends[i]))+ylab(colnames(pc_trends[k]))
    
    print(p1)
  }
  
}

dev.off()

