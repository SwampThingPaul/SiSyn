##dynamic time warping
# install.packages("dtw")
# install.packages("dtwclust")
#install.packages("cluster")
require(dtw)
require(plot.matrix)
require(factoextra)
require(dtwclust)
require(reshape2)
require(RColorBrewer)
require(dtwclust)
require(cluster)
require(purrr)
require(tibble)
require(zoo)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#read in interpolated monthly concentrations
monthly_results<-read.csv("Monthly_Results_PA_Interp.csv")

#filter to only include DSi
monthly_results$chemical<-ifelse(is.na(monthly_results$chemical), "DSi", monthly_results$chemical)
monthly_results<-subset(monthly_results, monthly_results$chemical=="DSi")

#remove first column
monthly_results<-monthly_results[,-1]

month_conc<-monthly_results[,c("stream", "Month", "Conc")]

#normalize data (z-score scaled)
month_conc_norm <- month_conc %>%
  group_by(stream) %>%
  mutate(norm_conc=scale(Conc))

#aggregate by month and stream to get one value for every month and stream pair
month_norm_agg<-aggregate(norm_conc ~ Month+stream, data = month_conc_norm, FUN=mean)

colnames(month_norm_agg)[3]<-"norm_conc"

#read in biomes data
biomes<-read.csv("Koeppen_Geiger.csv")
biomes<-biomes[,-1]
names(biomes)[2]<-"stream"
month_conc<-merge(month_norm_agg, biomes, by="stream")

month_conc$unique<-paste0(month_conc$stream, month_conc$Month)

month_conc<-month_conc[!duplicated(month_conc$unique),]

#remove unique column
month_conc<-month_conc[,-11]

#remove sites with non-continuous flow - thought I would include them but they make the clusters weird
remove_site<-c("SADDLE STREAM 007", "MARTINELLI")

month_conc<-month_conc[!month_conc$stream %in% remove_site,]

month_conc<-month_conc[!month_conc$LTER=="MCM",]

# LTER_unique<-unique(month_conc$LTER)
# 
# pdf("LTER_NonNormal_Si_Concentrations.pdf", width = 10, height = 8)
# 
# ID<-seq(1,12,1)
# 
# for (i in 1:length(LTER_unique)) {
# 
#    clusters_LTER<-subset(month_conc, month_conc$LTER==LTER_unique[i])
# 
#    p2<-ggplot(clusters_LTER)+geom_line(mapping=aes(Month, Conc, group=Site, col=Site), size=2)+
#      theme_bw()+
#      theme(text = element_text(size = 20))+labs(x="Flow Period", y="Si Concentration (mg/L)")+
#      ggtitle(LTER_unique[i])+
#      scale_x_continuous(breaks = ID, labels = as.character(ID))
# 
#    print(p2)
# 
#  }
# 
#  dev.off()

# clusters_LTER<-subset(month_clusters_melt, month_clusters_melt$LTER==LTER_unique[i])
# 
# p2<-ggplot(month_agg)+geom_line(mapping=aes(variable, value, group=Site, col=Site), size=2)+
#   theme_bw()+
#   theme(text = element_text(size = 20))+labs(x="Flow Period", y="Normalized Si Concentration")+
#   ggtitle(LTER_unique[i])
# 
# print(p2)

# month_conc$unique<-paste0(month_conc$stream, month_conc$Month)
# 
# month_conc<-month_conc[!duplicated(month_conc$unique),]

#cast (make wide) to get in proper format for DTW, should be 1-12 columns and sites as rows
month_cast<-dcast(month_conc, formula = Month~stream, value.var = "norm_conc")
month_norm<-month_cast
month_norm_t<-as.data.frame(t(month_norm[2:ncol(month_norm)]))

#### Cluster Validity Indices ####
#set up index "best" avlues - i.e. for "Sil" CVI, higher values (max) is better
index<-c("Sil","D","COP","DB","DBstar","CH","SF")
cvi_ideal<-c("max","max","min","min","min","max","max")
cvi_index<-data.frame(index, cvi_ideal)

#test CVIs for 2-15 clusters
clust.dba <- tsclust(month_norm_t, type="partitional", centroid = "dba", distance = "dtw",
                     window.size=1L, k=2L:15L, seed = 8)

#put CVI output into list
cvi_df<-lapply(clust.dba, cvi)

#put into df and melt
cluster_stats<-do.call(rbind, cvi_df)
cluster_stats_melt<-melt(cluster_stats)

#change column to reflect number of cluster (currently starts at 1, need to start at 2)
cluster_stats_melt$Var1<-cluster_stats_melt$Var1+1

#rename column
colnames(cluster_stats_melt)[2]<-"index"

#merge index df with CVI df
cluster_stats_melt<-merge(cluster_stats_melt, cvi_index, by="index")
colnames(cluster_stats_melt)[2]<-"number_of_clusters"
cluster_stats_melt$cvi_goal<-paste0(cluster_stats_melt$index, "-", cluster_stats_melt$cvi_ideal)

#plot to evaluate CVIs
ggplot(cluster_stats_melt, aes(number_of_clusters, value))+geom_line()+facet_wrap(~cvi_goal, scales = "free")+
  theme_classic()


#### now we DTW with the decided number of clusters, some basic visualization ####
#cluster
clust.dba <- tsclust(month_norm_t, type="partitional", centroid = "dba", distance = "dtw",
                     window.size=1, k=5L, seed = 8)

#plot the lines and centroids
plot(clust.dba, type = "sc")

#plot just the centroids
plot(clust.dba, type = "centroid")

#pull out centroids, save as csv
centroids_list<-clust.dba@centroids
centroids_df<-as.data.frame(do.call(rbind, centroids_list))
centroids_df$cluster<-seq(1,5)
write.csv(centroids_df, "AverageClusterCentroids.csv")

#plot
colnames(centroids_df)[1:12]<-seq(1,12)
centroids_melt<-melt(centroids_df, id.vars = "cluster")
ggplot(centroids_melt, aes(variable, value))+geom_line(group=1)+theme_bw()+facet_wrap(~cluster)+
  labs(x="Month", y="Normalized Si Concentration")+theme(text = element_text(size=20))

#summary of clustering (intracluster distance, time for analysis etc)
clust.dba

#pull out individual lines
mydata<-clust.dba@datalist

#convert to df
data_df<-t(do.call(cbind, mydata))

#visualize clusters in PC space
fviz_cluster(object = list(data=data_df, cluster = clust.dba@cluster),
             data = month_norm_t, stand = TRUE,
             labelsize = 0, , ellipse.type = "t", ellipse.level = 0.9)+theme_bw()+
  theme(text = element_text(size = 20))+labs(col="Cluster", fill="Cluster", shape="Cluster")

#this saves the data into df of each cluster, site, and month value
month_clusters<-as.data.frame(cbind(data_df, clust.dba@cluster))
colnames(month_clusters)<-c(paste(seq(1:12)), "Cluster")
month_clusters<-rownames_to_column(month_clusters, "Site")
#month_clusters$Site<-str_sub(month_clusters$SiteYear,1,nchar(month_clusters$SiteYear)-5)

#merge biome data inot it
biomes<-read.csv("Koeppen_Geiger.csv")
names(biomes)[3]<-"Site"
month_clusters<-merge(month_clusters, biomes, by="Site")

#remove duplicates and unnecesary columns
month_clusters<-month_clusters[!duplicated(month_clusters$Site),]
month_clusters<-month_clusters[,-c(15:18,20,21)]

write.csv(month_clusters, "MonthClustersNov2022.csv")

#reorder the names of Biome so that they plot in this order on axis and legens
# month_clusters$Biome2<-factor(month_clusters$Biome_new, levels = c("Tropical rainforest","Temperate coniferous forest",
#                                                                    "Temperate grassland","Boreal forest",
#                                                                    "Temperate deciduous forest", 
#                                                                    "Tropical savanna","Alpine tundra", 
#                                                                    "Polar desert", "Arctic tundra"))

#set color palette
#color_pal<-c("lawngreen", "goldenrod1", "darkgreen", "darkorange2", "firebrick1", "mediumpurple3", 
#"royalblue3", "lightsteelblue3", "ivory3")

#### specific visualization for si data ####

#assign color and fill (need to be different for points and boxplots) to one variable
f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
cols <- f("Set2")

cols_final<-c(cols, "#A46B5C")

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
                                                      "LTER", "Name"))

#plot lines by cluster
ggplot(month_clusters_melt, aes(variable, value))+
  geom_line(aes(col=Name, group=Site), size=1)+
  theme_bw()+facet_wrap(~Cluster, nrow = 2)+scale_color_manual(values = carto_pal(n=8, "Bold"))+
  theme(text = element_text(size = 20))+labs(col="Climate Zone", x="Flow Period", y="Normalized Si Concentration")

#for plot showing number of sites in each cluster
#find number of sites per cluster
cluster_sum<-month_clusters_melt %>%
  dplyr::group_by(Cluster) %>%
  dplyr::summarise(count = n_distinct(Site))

cluster_summary<-subset(month_clusters_melt, month_clusters_melt$variable==1)

ID <- 1:5

ggplot(cluster_summary, aes(Cluster))+geom_bar(aes(fill=Name))+scale_fill_manual(values = carto_pal(n=8, "Bold"))+
  scale_x_continuous(labels = as.character(ID), breaks = ID)+
  theme_classic()+labs(col="Climate Zone", x="Cluster Number", y="Count")+
  theme(text = element_text(size = 20), legend.position = "none")

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

#plot average cluster buhavior
cluster_avg<-month_clusters_melt %>% 
  dplyr::group_by(Cluster, variable) %>%
  dplyr::summarise(average_cluster = mean(value))

ggplot(cluster_avg, aes(variable, average_cluster))+geom_line(aes(group=Cluster))+
  facet_wrap(~Cluster, nrow = 2)+theme_bw()+theme(text = element_text(size=20))+labs(x="Flow Period", y="Normalized Si Concentration")


LTER_unique<-unique(month_clusters_melt$LTER)


#### make plots of LTER average curves ####

pdf("LTER_Cluster_SurveyPlots.pdf", width =15, height = 8)

pdf("LTER_SiPlots_Survey.pdf", width = 15, height = 8)

for (i in 1:length(LTER_unique)) {
  
  clusters_LTER<-subset(month_clusters_melt, month_clusters_melt$LTER==LTER_unique[i])

  p1<-ggplot()+
     geom_line(month_clusters_melt, mapping=aes(variable, value, group=Site), size=1)+
     geom_line(clusters_LTER, mapping=aes(variable, value, group=Site, col=Site), size=1)+
     theme_bw()+facet_wrap(~Cluster, nrow = 2)+
     theme(text = element_text(size = 20))+labs(x="Flow Period", y="Normalized Si Concentration")+
     ggtitle(LTER_unique[i])
  
   print(p1)
  
  # p2<-ggplot(clusters_LTER)+geom_line(mapping=aes(variable, value, group=Site, col=Site), size=2)+
  #   theme_bw()+
  #   theme(text = element_text(size = 20))+labs(x="Flow Period", y="Normalized Si Concentration")+
  #   ggtitle(LTER_unique[i])
  # 
  # print(p2)
  
  
}

dev.off()




#### not using code for testing CVIs on different number of clusters AND different prototyping functions ####


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
