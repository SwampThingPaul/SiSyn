require(PCAtools)
require(ggalt)
require(ggplot2)
require(GGally)

###curve metrics clusyering test

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

metrics<-read.csv("Curve_Metrics.csv")

metrics<-metrics[!metrics$stream=="Imnavait Weir",]

metadata<-read.csv("AllSiClusterData.csv")

meta_crop<-metadata[,c("Site", "slope", "cvc_cvq", "Name.x", "SiStab", "Centroid_Name")]

colnames(meta_crop)<-c("stream", "CQ", "cvc_cvq", "Climate", "Stability", "Cluster")

metrics<-merge(metrics, meta_crop, by="stream")

final_center<-metrics

#scale data - only keep columns using in PCA
final_center[c(3,5,9,10,11,14,15)]<-data.frame(sapply(final_center[c(3,5,9,10,11,14,15)], scale))

#examine ranges of centered and scaled data
ranges = vapply(final_center[,c(3,5,9,10,11,14,15)], FUN=function(col) c(min(col, na.rm = TRUE), max(col, na.rm = TRUE)), numeric(2))
rownames(ranges) = c("vmin", "vmax") 
rframe = as.data.frame(t(ranges))  # make ymin/ymax the columns
rframe$varName = rownames(rframe)

#plot ranges
ggplot()+geom_dumbbell(data=rframe, aes(y=varName, x=vmin, xend=vmax))+
  xlab("Centered/Scaled Value")+ylab("Variable")

#keep only columns to run in PCA
final_mat<-final_center[c(3,5,9,10,11,14,15)]

#transpose and rename columns
final_mat_t<-data.frame(t(final_mat))
colnames(final_mat_t)<-final_center$stream

#set metadata columns
metadata_pca<-final_center[,c(1,16:20)]
rownames(metadata_pca)<-final_center$stream

#run PCA
pca<-pca(final_mat_t, metadata = metadata_pca)

#screeplot (shows % variance explained by each PC)
screeplot(pca)

plotloadings(pca, components = c("PC1", "PC2", "PC3", "PC4"), labSize =5)

#plot 1 PC against another - from PCA tools
biplot(pca, x="PC1", y="PC2", showLoadings = TRUE, colby = "Cluster", legendPosition = "right", lab=NULL, ntopLoadings = 10,
       xlim = c(-8,8), ylim = c(-8,8))

biplot(pca, x="PC1", y="PC3", showLoadings = TRUE, colby = "Cluster", legendPosition = "right", lab=NULL, ntopLoadings = 10,
       xlim = c(-8,8), ylim = c(-8,8))

biplot(pca, x="PC2", y="PC3", showLoadings = TRUE, colby = "Cluster", legendPosition = "right", lab=NULL, ntopLoadings = 10,
       xlim = c(-8,8), ylim = c(-8,8))

biplot(pca, x="PC2", y="PC4", showLoadings = TRUE, colby = "Cluster", legendPosition = "right", lab=NULL, ntopLoadings = 10,
       xlim = c(-8,8), ylim = c(-8,8))

pairsplot(pca, colby = "Cluster", components = c("PC1","PC2", "PC3", "PC4"), pointSize = 2)

cor<-cor(final_center[c(3,5,9,10,11,14,15)])

corrplot::corrplot(cor)



