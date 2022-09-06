##dynamic time warping
install.packages("dtw")
require("dtw")
require(plot.matrix)
require(factoextra)
require(stringr)
require(dtwclust)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

monthly_results<-read.csv("WRTDS_GFN_MonthlyResults_AllSites_050422.csv")

MCM_months<-c(2:11)

Mart_months<-c(10,11,12,1:4)

Saddle_months<-c(1:4,8:12)
  

monthly_results$Conc<-ifelse(monthly_results$LTER=="MCM"&monthly_results$Month %in% MCM_months, 
                        0, monthly_results$Conc)

monthly_results$Conc<-ifelse(monthly_results$site=="MARTINELLI"&monthly_results$Month %in% Mart_months, 
                             0, monthly_results$Conc)

monthly_results$Conc<-ifelse(monthly_results$site=="SADDLE STREAM 007"&monthly_results$Month %in% Saddle_months, 
                             0, monthly_results$Conc)

month_agg<-aggregate(monthly_results, by=list(monthly_results$site, monthly_results$Month), 
                     FUN=mean, na.rm=TRUE)

names(month_agg)[c(1,2)]<-c("site", "Month")

month_avg<-month_agg[,c("site", "Month", "Conc")]

month_avg$type<-"average"

month_avg$WY<-NA

monthly_results$WY<-ifelse(monthly_results$Month > 9, monthly_results$Year+1, monthly_results$Year)

monthly_results$type<-"annual"

month_conc<-monthly_results[,c("site", "Month", "WY", "Conc", "type")]

month_conc<-rbind(month_avg, month_conc)

month_cast<-dcast(month_conc, formula = Month~site+WY+type, value.var = "Conc")

month_norm<-month_cast

month_norm[,c(2:1677)]<-as.data.frame(scale(month_norm[, c(2:1677)]))

month_norm_t<-as.data.frame(t(month_norm[,c(2:1677)]))

month_norm_t<-month_norm_t[complete.cases(month_norm_t),]

# clust.pam <- tsclust(month_norm_t, type="tadpole", 
#                      control = tadpole_control(dc = 1.5,
#                                                window.size = 5L), k=6L)

clust.dba <- tsclust(month_norm_t, type="partitional", centroid = "dba", distance = "dtw",
                     window.size=1L, k=6L, seed = 8)

clust.dba

plot(clust.dba, type = "sc")

mydata<-clust.dba@datalist

data_df<-t(do.call(cbind, mydata))

fviz_cluster(object = list(data=data_df, cluster = clust.dba@cluster),
             data = month_norm_t, stand = TRUE,
             labelsize = 0, ellipse.type = "convex", ellipse.level = 0.9)+theme_classic()

month_clusters<-as.data.frame(cbind(data_df, clust.dba@cluster))
colnames(month_clusters)<-c(paste(seq(1:12)), "Cluster")
month_clusters<-rownames_to_column(month_clusters, "SiteYear")
month_clusters$Site<-as.character(map(strsplit(month_clusters$SiteYear, "_"),1))
month_clusters$WY<-as.numeric(map(strsplit(month_clusters$SiteYear, "_"),2))
month_clusters$type<-as.character(map(strsplit(month_clusters$SiteYear, "_"),3))


biomes<-read.csv("Biome.csv")
month_clusters<-merge(month_clusters, biomes, by=c("Site"))

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

month_clusters_melt<-melt(month_clusters, id.vars = c("Site", "SiteYear", "Cluster",
                                                      "LTER", "Biome_new", "Biome2", "WY",
                                                      "type"))

colnames(avg_cluster)<-c("Average_Cluster","Site")

month_clusters_avg<-merge(month_clusters, avg_cluster, by="Site")

cluster_comp<-month_clusters_avg %>%
  filter(type=="annual") %>%
  mutate(same_cluster = ifelse(Cluster==Average_Cluster, "yes", "no"))

same_clust_count<-cluster_comp %>%
  group_by(Site) %>%
  count(same_cluster) %>%
  filter(same_cluster=="yes")

num_years<-cluster_comp %>%
  count(Site)

same_clust_count<-merge(same_clust_count, num_years, by="Site")

names(same_clust_count)<-c("Site","Same_Cluster","Yes","Total")

same_clust_count$diff<-same_clust_count$Total-same_clust_count$Yes

same_clust_count$Prop<-same_clust_count$diff/same_clust_count$Total

same_clust_count<-merge(same_clust_count, biomes, by="Site")

#reorder the names of Biome so that they plot in this order on axis and legens
same_clust_count$Biome2<-factor(same_clust_count$Biome_new, levels = c("Tropical rainforest","Temperate coniferous forest",
                                                                   "Temperate grassland","Boreal forest",
                                                                   "Temperate deciduous forest", 
                                                                   "Tropical savanna","Alpine tundra", 
                                                                   "Polar desert", "Arctic tundra"))


ggplot(same_clust_count,aes(Biome2, Prop))+geom_boxplot()+theme_classic()+geom_jitter()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="Biome",y="Proportion of Years Different from Average")

ggplot(month_clusters_melt, aes(variable, value))+
  geom_line(aes(col=Biome2, group=SiteYear))+
  theme_bw()+facet_wrap(~Cluster, nrow = 2)+
  scale_color_manual(values = col_values)

month_clusters_melt %>%
  filter(Site=="MPR") %>%
  ggplot(aes(variable, value))+geom_line(aes(col=SiteYear, group=SiteYear))+
  theme_bw()+facet_wrap(~Cluster, nrow = 2)

month_avg %>%
  group_by(LTER) %>%
  ggplot(aes(Month, Conc))+geom_line(aes(col=site, group=site))+
  theme_bw()+facet_wrap(~LTER, nrow = 2, scales = "free")

month_clusters_melt %>%
  group_by(Site, WY) %>%
  slice_min(value) %>%
  ggplot(aes(WY,value))+geom_line(aes(col=Site, group=Site))+
  theme_bw()+facet_wrap(~LTER, nrow = 2, scales = "free")

month_clusters_melt %>%
  group_by(Site, WY) %>%
  slice_max(value) %>%
  ggplot(aes(WY,value))+geom_line(aes(col=Site, group=Site))+
  theme_bw()+facet_wrap(~LTER, nrow = 2, scales = "free")



#unique(month_clusters[which(month_clusters$cluster_num==1),]$variable)

#fviz_cluster(clusters, data = dtw_matrix)+theme_classic()

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

cluster_sum<-month_clusters %>% 
  group_by(Site) %>%
  summarise(mode=getmode(Cluster))

cluster_membership<-month_clusters %>% 
  filter(type=="annual") %>%
  group_by(Site) %>%
  summarise(num_clusters=n_distinct(Cluster))

cluster_membership<-merge(cluster_membership, biomes, by="Site")

cluster_membership$Biome2<-factor(cluster_membership$Biome_new, levels = c("Tropical rainforest","Temperate coniferous forest",
                                                             "Temperate grassland","Boreal forest",
                                                             "Temperate deciduous forest", 
                                                             "Tropical savanna","Alpine tundra", 
                                                             "Polar desert", "Arctic tundra"))

ggplot(cluster_membership)+geom_bar(aes(num_clusters, fill=Biome2))+
  scale_fill_manual(values = col_values)+theme_classic()+labs(x="Number of Different Clusters",
                                                              y="Count",
                                                              fill="Biome")




cluster_avg<-grep("average", month_clusters$SiteYear)
site_avg_cluster<-month_clusters[cluster_avg,c("Site","Cluster")]

site_avg<-merge(cluster_sum, site_avg_cluster, by="Site")

site_avg<-merge(site_avg, cluster_membership, by="Site")

site_avg$match<-ifelse(site_avg$mode==site_avg$Cluster, "yes", "no")


cluster_avg<-month_clusters_melt %>% 
  group_by(Cluster, variable) %>%
  summarise(avergae_cluster = mean(value))

month_clusters_melt %>% 
  filter(type=="annual") %>%
  ggplot(aes(variable, value))+geom_line(aes(group=SiteYear, col=Biome2))+
  facet_wrap(~Cluster)+theme_bw()+labs(x="Month", y="Normalized Si Concentration")+
  scale_color_manual(values = col_values)

month_clusters_melt$type<-factor(month_clusters_melt$type, levels = c("average", "annual"))



month_clusters_melt %>% 
  filter(Site %in% c("MPR"), type=="annual") %>%
  ggplot(aes(variable, value))+geom_line(aes(group=SiteYear, col=WY))+
  theme_bw()+labs(x="Month", y="Normalized Si Concentration")+ggtitle("MPR")
  scale_color_continuous(na.value="black")



avg_cluster<-month_clusters %>% 
  filter(type=="average") %>%
  ggplot(aes(Cluster))+geom_histogram(aes(fill=Biome2), stat = "count")+
  theme_bw()+labs(x="Cluster", y="Count")+
  scale_fill_manual(values = col_values)

ggplot(cluster_avg, aes(variable, avergae_cluster))+geom_line(aes(group=Cluster))+
  facet_wrap(~Cluster)+theme_bw()+labs(x="Month", y="Normalized Si Concentration")


pdf("LTER_Si_Seasonality.pdf")

LTER_list<-unique(month_avg$LTER)

for (i in 1:length(LTER_list)) {
  
  p1<-month_avg %>%
    filter(LTER==LTER_list[i]) %>%
    ggplot(aes(Month, Conc))+geom_line(aes(col=site, group=site))+
    theme_bw()+ggtitle(paste0(LTER_list[i]))
  
  print(p1)
  
}

dev.off()


pdf("LTER_Si_Seasonality_AnnualChange.pdf")

LTER_list<-unique(month_avg$LTER)

for (i in 1:length(LTER_list)) {
  
  p1<-month_clusters_melt %>%
    filter(LTER==LTER_list[i]) %>%
    ggplot(aes(variable, value))+geom_line(aes(col=WY, group=SiteYear))+
    theme_bw()+ggtitle(paste0(LTER_list[i]))+facet_wrap(~Site)+
    scale_color_gradient(low = "red", high = "blue", na.value = "black")
  
  print(p1)
  
}

dev.off()


pdf("LTER_Si_Seasonality_MinChange.pdf")

LTER_list<-unique(month_avg$LTER)

for (i in 1:length(LTER_list)) {
  
  p1<-month_clusters_melt %>%
    filter(LTER==LTER_list[i]) %>%
    group_by(Site, WY) %>%
    slice_min(value) %>%
    ggplot(aes(WY,value))+geom_line()+ggtitle(paste0(LTER_list[i], "- min"))+
    theme_bw()+facet_wrap(~Site, nrow = 2, scales = "free")+
    geom_smooth(method = "lm", se=FALSE)
  
  print(p1)
  
  p1<-month_clusters_melt %>%
    filter(LTER==LTER_list[i]) %>%
    group_by(Site, WY) %>%
    slice_max(value) %>%
    ggplot(aes(WY,value))+geom_line()+ggtitle(paste0(LTER_list[i], "- min"))+
    theme_bw()+facet_wrap(~Site, nrow = 2, scales = "free")+
    geom_smooth(method = "lm", se=FALSE)
  
}

dev.off()


# #clust.pam <- tsclust(month_norm_t, type="partitional", centroid = "pam", distance = "dtw",
#  #                    window.size=0.1, k=5L)
# 
# plot(clust.pam, type = "sc")
# 
# month_melt<-melt(month_norm, id="Month")
# 
# month_melt$site<-gsub('.{5}$', '', month_melt$variable)
# 
# biomes<-read.csv("Biome.csv")
# 
# colnames(biomes)[2]<-"site"
# 
# month_melt<-merge(month_melt, biomes, by="site")
# 
# month_melt$Biome2<-factor(month_melt$Biome_new, levels = c("Tropical rainforest","Temperate coniferous forest",
#                                                                    "Temperate grassland","Boreal forest",
#                                                                    "Temperate deciduous forest", 
#                                                                    "Tropical savanna","Alpine tundra", 
#                                                                    "Polar desert", "Arctic tundra"))
# 
# 
# month_t<-as.matrix(t(month_norm[,c(2:1617)]))
# 
# month_t<-month_t[complete.cases(month_t),]
# 
# # ggplot(month_melt, aes(Month, value))+geom_path(aes(col=Biome2, group=variable))+
# #   theme_classic()+
# #   scale_color_manual(values = col_values)
# 
# dtw_matrix<-matrix(nrow = nrow(month_t), ncol = nrow(month_t))
# 
# for (i in 1:nrow(month_t)) {
#   
#   print(i)
#   
#   for (k in 1:nrow(month_t)) {
#     
#     d<-dtw(month_t[i,], month_t[k,], distance.only = TRUE, 
#            window.type = "sakoechiba", window.size=0.05)
#     
#     dtw_matrix[i,k]<-d$distance
#     
#   }
#   
# }
# 
# pdf("DTW_ANNUAL.pdf", width=15, height=15)
# 
# plot(dtw_matrix, col=topo.colors)
# 
# dev.off()
# 
# TADPole(month_t, k=6, dc=3, window.size = 0.1, trace = TRUE)
# 
# fviz_nbclust(dtw_matrix, hclust, method = "wss", k.max = 30)
# 
# cluster::clusGap(dtw_matrix, kmeans, K.max = 30, B=100)
# 
# clusters<-kmeans(dtw_matrix, 6)
# 
# cluster_nums<-as.data.frame(clusters$cluster)
# colnames(cluster_nums)[1]<-"cluster_num"
# cluster_nums$variable<-rownames(month_t)
# 
# month_clusters<-merge(month_melt, cluster_nums, by="variable")
# 
# biomes<-read.csv("Biome.csv")
# names(biomes)[2]<-"site"
# 
# month_clusters<-merge(month_clusters, biomes, by="site")
# 
# #reorder the names of Biome so that they plot in this order on axis and legens
# month_clusters$Biome2<-factor(month_clusters$Biome_new.x, levels = c("Tropical rainforest","Temperate coniferous forest",
#                                                                    "Temperate grassland","Boreal forest",
#                                                                    "Temperate deciduous forest", 
#                                                                    "Tropical savanna","Alpine tundra", 
#                                                                    "Polar desert", "Arctic tundra"))
# 
# #set color palette
# #color_pal<-c("lawngreen", "goldenrod1", "darkgreen", "darkorange2", "firebrick1", "mediumpurple3", 
# #"royalblue3", "lightsteelblue3", "ivory3")
# 
# #assign color and fill (need to be different for points and boxplots) to one variable
# f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
# cols <- f("Set2")
# 
# cols_final<-c(cols, "#A46B5C")
# #show_col(cols_final)
# 
# #assign colors
# col_values<-c("Boreal forest" = cols_final[1],
#               "Temperate deciduous forest" = cols_final[2],
#               "Alpine tundra" = cols_final[3],
#               "Arctic tundra" = cols_final[4],
#               "Tropical rainforest" = cols_final[5],
#               "Tropical savanna" = cols_final[6],
#               "Temperate grassland" = cols_final[7],
#               "Polar desert" = cols_final[8],
#               "Temperate coniferous forest" = cols_final[9])
# 
# 
# ggplot(month_clusters, aes(Month, value))+geom_line(aes(col=Biome2, group=variable))+
#   theme_bw()+facet_wrap(~cluster_num, nrow = 2)+
#   scale_color_manual(values = col_values)
# 
# #unique(month_clusters[which(month_clusters$cluster_num==1),]$variable)
# 
# fviz_cluster(clusters, data = dtw_matrix)+theme_classic()
# 
# cluster_sum<-month_clusters %>% 
#   group_by(site) %>%
#   summarise(count = n_distinct(cluster_num))
#   
# 
# cluster_sum <- aggregate(data = month_clusters,                # Applying aggregate
#                          cluster_num ~ site,
#                           function(x) length(unique(x)))
# 
# cluster_sum<-merge(cluster_sum, biomes, by="site")
# 
# cluster_sum$Biome2<-factor(cluster_sum$Biome_new, levels = c("Tropical rainforest","Temperate coniferous forest",
#                                                                    "Temperate grassland","Boreal forest",
#                                                                    "Temperate deciduous forest", 
#                                                                    "Tropical savanna","Alpine tundra", 
#                                                                    "Polar desert", "Arctic tundra"))
# 
# 
# ggplot(cluster_sum)+geom_bar(aes(cluster_num, fill=Biome2))+
#   scale_fill_manual(values = col_values)+theme_classic()+labs(x="Cluster Number",
#                                                               y="Count",
#                                                               fill="Biome")
# 
# 
