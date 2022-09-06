##dynamic time warping
# install.packages("dtw")
# install.packages("dtwclust")
install.packages("cluster")
require("dtw")
require(plot.matrix)
require(factoextra)
require(dtwclust)
require(reshape2)
require(RColorBrewer)
require(dtwclust)
require(cluster)
require(purrr)
require(tibble)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

monthly_results<-read.csv("WRTDS_GFN_MonthlyResults_AllSites_050422.csv")

month_agg<-aggregate(monthly_results, by=list(monthly_results$site, monthly_results$Month), 
                     FUN=mean, na.rm=TRUE)

names(month_agg)[c(1,2)]<-c("Site", "Month")

month_conc<-month_agg[,c("Site", "Month", "Conc")]

month_cast<-dcast(month_conc, formula = Month~Site)

month_norm<-month_cast

month_norm[2:61]<-as.data.frame(scale(month_cast[2:61]))

month_norm_t<-as.data.frame(t(month_norm[2:61]))

# m <- c( "average", "single", "complete", "ward")
# names(m) <- c( "average", "single", "complete", "ward")
# 
# # function to compute coefficient
# ac <- function(x) {
#   agnes(month_norm_t, method = x)$ac
# }
# 
# map_dbl(m, ac)
# 
# hc3 <- agnes(month_norm_t, method = "single")
# pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 
# 
# fviz_nbclust(month_norm_t, FUN = hcut, method = "silhouette")
# 
# fviz_nbclust(month_norm_t, FUN = hcut, method = "wss")
# 
# sub_grp <- cutree(hc3, k = 4)
# table(sub_grp)
# 
# pltree(hc3, cex = 0.6)
# rect.hclust(hc3, k = 4, border = 2:5)
# 
# fviz_cluster(list(data = month_norm_t, cluster = sub_grp))

# clust.pam <- tsclust(month_norm_t, type="partitional", k=2L:12L, distance="dtw", centroid="pam")
# 
# cvi_df<-lapply(clust.pam, cvi)
# 
# cluster_stats<-do.call(rbind, cvi_df)
# 
# cluster_stats_melt<-melt(cluster_stats)
# 
# ggplot(cluster_stats_melt, aes(Var1, value))+geom_line()+facet_wrap(~Var2, scales = "free")

pdf("CVI_Cluster_Eval.pdf")

for (i in 1:6) {
  
  # clust.tadpole <- tsclust(month_norm_t, type="tadpole", seed = 8,
  #                          control = tadpole_control(dc = 1.5,
  #                                                    window.size = 2L), k=i)
  # 
  # #plot(clust.tadpole, type = "sc")
  # 
  # clust.pam <- tsclust(month_norm_t, type="partitional", centroid = "pam", distance = "dtw",
  #                      window.size=2L, k=i, seed = 8)
  
  #plot(clust.pam, type = "sc")
  
  clust.dba <- tsclust(month_norm_t, type="partitional", centroid = "dba", distance = "dtw",
                       window.size=i, k=6L, seed = 8)
  
  #plot(clust.dba, type = "sc")
  
  cvi_df<-lapply(list(PAM = clust.pam, TADPole = clust.tadpole, DBA = clust.dba),
                 cvi, type = "internal")
  
  cvi_tot<-as.data.frame(t(do.call(rbind,cvi_df)))
  
  cvi_tot <- rownames_to_column(cvi_tot, "index")
  
  index<-c("Sil","D","COP","DB","DBstar","CH","SF")
  cvi_ideal<-c("max","max","min","min","min","max","max")
  
  cvi_index<-data.frame(index, cvi_ideal)
  
  cvi_tot_melt<-melt(cvi_tot)
  
  cvi_tot_melt<-merge(cvi_tot_melt, cvi_index, by="index")
  
  cvi_tot_melt$cvi_goal<-paste0(cvi_tot_melt$index, "-", cvi_tot_melt$cvi_ideal)
  
  p1<-ggplot(cvi_tot_melt, aes(variable, value))+geom_boxplot()+facet_wrap(~cvi_goal, scales = "free")+
    theme_classic()+ggtitle(paste("Clusters =", i))
  
  print(p1)
  
}

clust.dba <- tsclust(month_norm_t, type="partitional", centroid = "dba", distance = "dtw",
                     window.size=2L, k=3L:10L, seed = 8)

cvi_df<-lapply(clust.dba, cvi)

cluster_stats<-do.call(rbind, cvi_df)

cluster_stats_melt<-melt(cluster_stats)

colnames(cluster_stats_melt)[2]<-"index"

cluster_stats_melt<-merge(cluster_stats_melt, cvi_index, by="index")

cluster_stats_melt$cvi_goal<-paste0(cluster_stats_melt$index, "-", cluster_stats_melt$cvi_ideal)

ggplot(cluster_stats_melt, aes(Var1, value))+geom_line()+facet_wrap(~cvi_goal, scales = "free")+
  theme_classic()
  

dev.off()

clust.tadpole <- tsclust(month_norm_t, type="tadpole", seed = 8,
                     control = tadpole_control(dc = 1.5,
                                               window.size = 2L), k=5L)

#plot(clust.tadpole, type = "sc")

clust.pam <- tsclust(month_norm_t, type="partitional", centroid = "pam", distance = "dtw",
                     window.size=2L, k=5L, seed = 8)

#plot(clust.pam, type = "sc")

clust.dba <- tsclust(month_norm_t, type="partitional", centroid = "dba", distance = "dtw",
                     window.size=5L, k=6L, seed = 8)

plot(clust.dba, type = "sc")

clust.dba

mydata<-clust.dba@datalist

data_df<-t(do.call(cbind, mydata))

fviz_cluster(object = list(data=data_df, cluster = clust.dba@cluster),
             data = month_norm_t, stand = TRUE,
             labelsize = 0, , ellipse.type = "t", ellipse.level = 0.9)+theme_classic()

month_clusters<-as.data.frame(cbind(data_df, clust.dba@cluster))
colnames(month_clusters)<-c(paste(seq(1:12)), "Cluster")
month_clusters<-rownames_to_column(month_clusters, "Site")
#month_clusters$Site<-str_sub(month_clusters$SiteYear,1,nchar(month_clusters$SiteYear)-5)

biomes<-read.csv("Biome.csv")
month_clusters<-merge(month_clusters, biomes, by="Site")

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

month_clusters_melt<-melt(month_clusters, id.vars = c("Site", "Cluster",
                                                      "LTER", "Biome_new", "Biome2"))


ggplot(month_clusters_melt, aes(variable, value))+
  geom_line(aes(col=Biome2, group=Site))+
  theme_bw()+facet_wrap(~Cluster, nrow = 2)+
  scale_color_manual(values = col_values)

#unique(month_clusters[which(month_clusters$cluster_num==1),]$variable)

#fviz_cluster(clusters, data = dtw_matrix)+theme_classic()

# cluster_sum<-month_clusters_melt %>% 
#   group_by(Site) %>%
#   summarise(count = n_distinct(Cluster))
# 
# 
# cluster_sum <- aggregate(data = month_clusters_melt,                # Applying aggregate
#                          Cluster ~ Site,
#                          function(x) length(unique(x)))
# 
# cluster_sum<-merge(cluster_sum, biomes, by="Site")
# 
# cluster_sum$Biome2<-factor(cluster_sum$Biome_new, levels = c("Tropical rainforest","Temperate coniferous forest",
#                                                              "Temperate grassland","Boreal forest",
#                                                              "Temperate deciduous forest", 
#                                                              "Tropical savanna","Alpine tundra", 
#                                                              "Polar desert", "Arctic tundra"))

cluster_avg<-month_clusters_melt %>% 
  group_by(Cluster, variable) %>%
  summarise(avergae_cluster = mean(value))

ggplot(cluster_avg, aes(variable, avergae_cluster))+geom_line(aes(group=Cluster))+
  facet_wrap(~Cluster)+theme_bw()












cvi_df<-lapply(list(PAM = clust.pam, TADPole = clust.tadpole, DBA = clust.dba),
       cvi, type = "internal")

cvi_tot<-as.data.frame(t(do.call(rbind,cvi_df)))

cvi_tot <- rownames_to_column(cvi_tot, "index")

index<-c("Sil","D","COP","DB","DBstar","CH","SF")
cvi_ideal<-c("max","max","min","min","min","max","max")

cvi_index<-data.frame(index, cvi_ideal)

cvi_tot_melt<-melt(cvi_tot)

cvi_tot_melt<-merge(cvi_tot_melt, cvi_index, by="index")

cvi_tot_melt$cvi_goal<-paste0(cvi_tot_melt$index, "-", cvi_tot_melt$cvi_ideal)

ggplot(cvi_tot_melt, aes(variable, value))+geom_boxplot()+facet_wrap(~cvi_goal, scales = "free")+
  theme_classic()

clust_list<-list()

for (i in 1:6) {
  
  clust.dba <- tsclust(month_norm_t, type="partitional", centroid = "dba", distance = "dtw",
                       window.size=i, k=6L, seed = 8)
  
  clust_list[[i]]<-cvi(clust.dba, type = "internal")
  
}

cvi_list<-as.data.frame(do.call(rbind, clust_list))

cvi_list$window_len<-seq(1:6)

cvi_list_melt<-melt(cvi_list, id.vars = c("window_len"))

colnames(cvi_list_melt)[2]<-"index"

cvi_list_melt<-merge(cvi_list_melt, cvi_index, by="index")

cvi_list_melt$cvi_goal<-paste0(cvi_list_melt$index, "-", cvi_list_melt$cvi_ideal)

ggplot(cvi_list_melt, aes(window_len, value))+geom_line()+facet_wrap(~cvi_goal, scales = "free")+
  theme_classic()

plot(clust.dba, type = "sc")





month_melt<-melt(month_norm, id="Month")

names(month_melt)[2]<-"site"

biomes<-read.csv("Biome.csv")
colnames(biomes)[2]<-"site"

month_melt<-merge(month_melt, biomes, by="site")

month_melt$Biome2<-factor(month_melt$Biome_new, levels = c("Tropical rainforest","Temperate coniferous forest",
                                                           "Temperate grassland","Boreal forest",
                                                           "Temperate deciduous forest", 
                                                           "Tropical savanna","Alpine tundra", 
                                                           "Polar desert", "Arctic tundra"))


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


month_t<-as.matrix(t(month_norm[2:61]))

ts_months<-tslist(month_t)

clusters<-compare_clusterings(ts_months, types = "h", configs = compare_clusterings_configs(types))

ggplot(month_melt, aes(Month, value))+geom_path(aes(col=Biome2, group=site))+
  theme_classic()+scale_color_manual(values = col_values)

dtw_matrix<-matrix(nrow = 60, ncol = 60)

#dtwWindow.plot(sakoeChibaWindow, window.size=10)

for (i in 1:nrow(month_t)) {
  
  print(i)
  
  for (k in 1:nrow(month_t)) {
    
    d<-dtw(month_t[i,], month_t[k,], distance.only = TRUE,
           window.type = "sakoechiba", window.size=0.05)
    
    #d<-dtw(month_t[i,], month_t[k,], distance.only = TRUE)
    
    dtw_matrix[i,k]<-d$distance
    
  }
  
}

#plot(dtw_matrix, col=topo.colors)

fviz_nbclust(dtw_matrix, kmeans, method = "wss")

clusters<-kmeans(dtw_matrix, 5)

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



##using dtwclust package

dtwclust(month_t, type="h", )




