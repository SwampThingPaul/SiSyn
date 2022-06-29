##dynamic time warping
install.packages("dtw")
require("dtw")
require(plot.matrix)
require(factoextra)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

monthly_results<-read.csv("WRTDS_GFN_MonthlyResults_AllSites_050422.csv")

monthly_results$WY<-ifelse(monthly_results$Month > 9, monthly_results$Year+1, monthly_results$Year)

month_conc<-monthly_results[,c("site", "Month", "WY", "Conc")]

month_cast<-dcast(month_conc, formula = Month~site+WY)

month_norm<-month_cast

month_norm[,c(2:1617)]<-as.data.frame(scale(month_norm[, c(2:1617)]))

month_melt<-melt(month_norm, id="Month")

month_melt$site<-gsub('.{5}$', '', month_melt$variable)

month_melt<-merge(month_melt, biomes, by="site")

month_melt$Biome2<-factor(month_melt$Biome_new, levels = c("Tropical rainforest","Temperate coniferous forest",
                                                                   "Temperate grassland","Boreal forest",
                                                                   "Temperate deciduous forest", 
                                                                   "Tropical savanna","Alpine tundra", 
                                                                   "Polar desert", "Arctic tundra"))


month_t<-as.matrix(t(month_norm[,c(2:1617)]))

month_t<-month_t[complete.cases(month_t),]

ggplot(month_melt, aes(Month, value))+geom_path(aes(col=Biome2, group=variable))+
  theme_classic()+
  scale_color_manual(values = col_values)

dtw_matrix<-matrix(nrow = nrow(month_t), ncol = nrow(month_t))

for (i in 1:nrow(month_t)) {
  
  print(i)
  
  for (k in 1:nrow(month_t)) {
    
    d<-dtw(month_t[i,], month_t[k,], distance.only = TRUE)
    
    dtw_matrix[i,k]<-d$distance
    
  }
  
}

pdf("DTW_ANNUAL.pdf", width=15, height=15)

plot(dtw_matrix, col=topo.colors)

dev.off()

fviz_nbclust(dtw_matrix, kmeans, method = "wss")

clusters<-kmeans(dtw_matrix, 5)

cluster_nums<-as.data.frame(clusters$cluster)
colnames(cluster_nums)[1]<-"cluster_num"
cluster_nums$variable<-rownames(month_t)

month_clusters<-merge(month_melt, cluster_nums, by="variable")

biomes<-read.csv("Biome.csv")
names(biomes)[2]<-"site"

month_clusters<-merge(month_clusters, biomes, by="site")

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


ggplot(month_clusters, aes(Month, value))+geom_line(aes(col=Biome_new, group=variable))+
  theme_bw()+facet_wrap(~cluster_num, nrow = 2)+
  scale_color_manual(values = col_values)

#unique(month_clusters[which(month_clusters$cluster_num==1),]$variable)

fviz_cluster(clusters, data = dtw_matrix)+theme_classic()

cluster_sum<-month_clusters %>% 
  group_by(site) %>%
  summarise(count = n_distinct(cluster_num))
  

cluster_sum <- aggregate(data = month_clusters,                # Applying aggregate
                         cluster_num ~ site,
                          function(x) length(unique(x)))

cluster_sum<-merge(cluster_sum, biomes, by="site")

cluster_sum$Biome2<-factor(cluster_sum$Biome_new, levels = c("Tropical rainforest","Temperate coniferous forest",
                                                                   "Temperate grassland","Boreal forest",
                                                                   "Temperate deciduous forest", 
                                                                   "Tropical savanna","Alpine tundra", 
                                                                   "Polar desert", "Arctic tundra"))


ggplot(cluster_sum)+geom_bar(aes(cluster_num, fill=Biome2))+
  scale_fill_manual(values = col_values)+theme_classic()+labs(x="Cluster Number",
                                                              y="Count",
                                                              fill="Biome")


