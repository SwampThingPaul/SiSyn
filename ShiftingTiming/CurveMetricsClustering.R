
require(ggalt)
require(ggplot2)
require(GGally)
require(factoextra)
require(cluster)
require(rcartocolor)

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
input_data<-data.frame(sapply(final_center[,c(3,5,9,10,11,14,15)], scale))

fviz_nbclust(input_data, FUN = hcut, method = "silhouette", k.max = 40)

dist_mat<-dist(input_data, method = "euclidean")

clusters<-hclust(dist_mat, method = "ward.D2")

plot(clusters)
rect.hclust(clusters, k=3, border="red")
rect.hclust(clusters, k=9, border="blue")
rect.hclust(clusters, k=25, border="forestgreen")

sil_cl <- silhouette(cutree(clusters, k=25) ,dist(input_data), title=title(main = 'Good'))
plot(sil_cl)

sub_grp <- cutree(clusters, k = 9)

fviz_cluster(list(data = input_data, cluster = sub_grp))+theme_bw()+theme(text = element_text(size = 20))

final_center_groups<-cbind(final_center, sub_grp)

ggplot(final_center_groups, aes(x=sub_grp))+geom_bar(aes(fill=Cluster))+theme_bw()+
  scale_fill_manual(values = carto_pal(n=5, "Bold"))+scale_x_continuous(breaks = seq(1,9,1))+theme(text = element_text(size = 20))

final_center_melt<-melt(final_center_groups[,c(3,5,9,10,11,14,15,20,21)], id.vars = c("Cluster", "sub_grp"))

ggplot(final_center_melt, aes(sub_grp, value))+geom_boxplot(aes(group=sub_grp))+facet_wrap(~variable, scales = "free")+theme_bw()+
  theme(text = element_text(size=20), legend.position = "none")+
  scale_x_continuous(breaks = seq(1,9,1))+geom_jitter(aes(col=Cluster))+
  scale_color_manual(values = carto_pal(n=5, "Bold"))

ggplot(final_center_melt, aes(x=value, fill=as.character(sub_grp)))+geom_density(alpha=0.5)+facet_wrap(~variable, scales = "free")+theme_bw()+
  theme(text = element_text(size=20))

ggplot(final_center_melt, aes(x=value, fill=Cluster))+geom_density(alpha=0.5)+facet_wrap(~variable, scales = "free")+theme_bw()+
  theme(text = element_text(size=20))







