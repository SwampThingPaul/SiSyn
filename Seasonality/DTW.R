##dynamic time warping
install.packages("dtw")
require("dtw")
require(plot.matrix)
require(factoextra)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

monthly_results<-read.csv("WRTDS_GFN_MonthlyResults_AllSites_050422.csv")

month_agg<-aggregate(monthly_results, by=list(monthly_results$site, monthly_results$Month), 
                     FUN=mean, na.rm=TRUE)

names(month_agg)[c(1,2)]<-c("Site", "Month")

month_conc<-month_agg[,c("Site", "Month", "Conc")]

month_cast<-dcast(month_conc, formula = Month~Site)

month_norm<-month_cast

month_norm[2:61]<-as.data.frame(scale(month_cast[2:61]))

month_melt<-melt(month_norm, id="Month")

names(month_melt)[2]<-"site"

month_melt<-merge(month_melt, biomes, by="site")

month_melt$Biome2<-factor(month_melt$Biome_new, levels = c("Tropical rainforest","Temperate coniferous forest",
                                                           "Temperate grassland","Boreal forest",
                                                           "Temperate deciduous forest", 
                                                           "Tropical savanna","Alpine tundra", 
                                                           "Polar desert", "Arctic tundra"))



month_t<-as.matrix(t(month_norm[2:61]))

ggplot(month_melt, aes(Month, value))+geom_path(aes(col=Biome2, group=site))+
  theme_classic()+scale_color_manual(values = col_values)

dtw_matrix<-matrix(nrow = 60, ncol = 60)

for (i in 1:nrow(month_t)) {
  
  print(i)
  
  for (k in 1:nrow(month_t)) {
    
    d<-dtw(month_t[i,], month_t[k,], distance.only = TRUE)
    
    dtw_matrix[i,k]<-d$distance
    
  }
  
}

plot(dtw_matrix, col=topo.colors)

fviz_nbclust(dtw_matrix, kmeans, method = "wss")

clusters<-kmeans(dtw_matrix, 4)

cluster_nums<-as.data.frame(clusters$cluster)
cluster_nums$variable<-colnames(month_norm[,2:61])
colnames(cluster_nums)<-c("cluster_num", "site")

month_clusters<-merge(month_melt, cluster_nums, by="site")

biomes<-read.csv("Biome.csv")

#reorder the names of Biome so that they plot in this order on axis and legens
month_clusters$Biome2<-factor(month_clusters$Biome_new, levels = c("Tropical rainforest","Temperate coniferous forest",
                                                     "Temperate grassland","Boreal forest",
                                                     "Temperate deciduous forest", 
                                                     "Tropical savanna","Alpine tundra", 
                                                     "Polar desert", "Arctic tundra"))

#set color palette
#color_pal<-c("lawngreen", "goldenrod1", "darkgreen", "darkorange2", "firebrick1", "mediumpurple3", 
#"royalblue3", "lightsteelblue3", "ivory3")

#assign color and fill (need to be different for points and boxplots) to one variable
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


ggplot(month_clusters, aes(Month, value))+geom_path(aes(col=Biome_new, group=site))+
  theme_bw()+facet_wrap(~cluster_num, nrow = 2)+scale_color_manual(values = col_values)

#unique(month_clusters[which(month_clusters$cluster_num==1),]$variable)

fviz_cluster(clusters, data = dtw_matrix)+theme_classic()




