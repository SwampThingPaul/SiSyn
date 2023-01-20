##dynamic time warping
#install.packages("dtw")
require("dtw")
require(plot.matrix)
require(factoextra)
require(stringr)
require(dtwclust)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

monthly_results<-read.csv("Monthly_Results_PA_Interp.csv")

monthly_results$chemical<-ifelse(is.na(monthly_results$chemical), "DSi", monthly_results$chemical)

monthly_results<-subset(monthly_results, monthly_results$chemical=="DSi")

monthly_results<-monthly_results[,-1]

remove_site<-c("SADDLE STREAM 007", "MARTINELLI")

monthly_results<-monthly_results[!monthly_results$stream %in% remove_site,]

monthly_results<-monthly_results[!is.na(monthly_results$LTER),]

#names(monthly_results)[c(2,4,5)]<-c("Site", "variable", "WY")
# 
# MCM_months<-c(2:11)
# 
# Mart_months<-c(10,11,12,1:4)
# 
# Saddle_months<-c(1:4,8:12)
#   
# 
# monthly_results$Conc<-ifelse(monthly_results$LTER=="MCM"&monthly_results$Month %in% MCM_months, 
#                         0, monthly_results$Conc)
# 
# monthly_results$Conc<-ifelse(monthly_results$site=="MARTINELLI"&monthly_results$Month %in% Mart_months, 
#                              0, monthly_results$Conc)
# 
# monthly_results$Conc<-ifelse(monthly_results$site=="SADDLE STREAM 007"&monthly_results$Month %in% Saddle_months, 
#                              0, monthly_results$Conc)
# 
# month_agg<-aggregate(monthly_results, by=list(monthly_results$stream, monthly_results$Month),
#                      FUN=mean, na.rm=TRUE)
# 
# names(month_agg)[c(1,2)]<-c("site", "Month")
# 
# month_avg<-month_agg[,c("site", "Month", "Conc")]
# 
# month_avg$type<-"average"
# 
# month_avg$WY<-NA

monthly_results$WY<-ifelse(monthly_results$Month > 9, monthly_results$Year+1, monthly_results$Year)

month_conc<-monthly_results[,c("stream", "Month", "WY", "Conc")]

month_conc$unique<-paste0(month_conc$stream, month_conc$WY, month_conc$Month)

dups<-which(duplicated(month_conc$unique))

month_conc<-month_conc[-dups,]

names(month_conc)[1]<-"site"

month_conc<-month_conc[,-5]

month_conc_scale<-month_conc %>%
  group_by(site) %>%
  mutate(scale(Conc))

colnames(month_conc_scale)[5]<-"scaled_conc"

#month_conc$uniqueID<-paste0(month_conc$site, month_conc$WY, month_conc$type)

#length(unique(month_conc$uniqueID))

month_cast<-dcast(month_conc_scale, formula = Month~site+WY, value.var = "scaled_conc")

month_norm<-month_cast

#month_norm[,c(2:ncol(month_norm))]<-as.data.frame(scale(month_norm[, c(2:ncol(month_norm))]))

month_norm_t<-as.data.frame(t(month_norm[,c(2:ncol(month_norm))]))

month_norm_t<-month_norm_t[complete.cases(month_norm_t),]

# clust.pam <- tsclust(month_norm_t, type="tadpole", 
#                      control = tadpole_control(dc = 1.5,
#                                                window.size = 5L), k=6L)


centroids_df<-read.csv("AverageClusterCentroids.csv")

dist_mat<-dtwDist(as.matrix(month_norm_t), as.matrix(centroids_df[,2:13]), 
                  window.type="sakoechiba", window.size=1L)

dist_mat<-as.data.frame(dist_mat)

dist_mat$clust<-apply(dist_mat, 1, which.min)

dist_mat<-rownames_to_column(dist_mat, var = "siteyear")

dist_mat$Site<-substr(dist_mat$siteyear,1,nchar(dist_mat$siteyear)-5)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

siteyear_clust_mode<-dist_mat %>%
  group_by(Site) %>%
  summarise(mode=Mode(clust))

write.csv(siteyear_clust_mode, "SiClustersMode.csv")

avg_clusters<-read.csv("MonthClustersNov2022.csv")
avg_clusters<-avg_clusters[,c(2,15:17)]
colnames(avg_clusters)[2]<-"Avg_Clust"

clust_mean_mode<-merge(siteyear_clust_mode, avg_clusters, by="Site")  

clust_mean_mode$same<-ifelse(clust_mean_mode$mode==clust_mean_mode$Avg_Clust, "yes", "no")

# sage<-grep("Sage", dist_mat$siteyear)
# 
# dist_mat_sage<-dist_mat[sage,]
# 
# norm_sage<-grep("Sage", rownames(month_norm_t))
# 
# month_norm_t_sage<-month_norm_t[norm_sage,]
# 
# month_norm_t_sage<-rownames_to_column(month_norm_t_sage)
# 
# sage_melt<-melt(month_norm_t_sage, id.vars = "rowname")
# 
# ggplot(sage_melt, aes(variable, value))+geom_line(aes(col=rowname, group=rowname))

clust<-dist_mat[,c(1,7)]

month_norm_t<-rownames_to_column(month_norm_t, var = "siteyear")

colnames(month_norm_t)[2:13]<-seq(1,12)

siteyear_clust<-merge(month_norm_t, clust, by="siteyear")

siteyear_clust_melt<-melt(siteyear_clust, id.vars = c("siteyear", "clust"))

ggplot(siteyear_clust_melt, aes(variable, value, group=siteyear))+geom_line(alpha=0.3)+theme_bw()+
  facet_wrap(~clust)+labs(x="Month", y="Normalized Si Concentration")+
  theme(text = element_text(size = 20))

siteyear_count<-siteyear_clust %>%
  count(clust)

ggplot(siteyear_count, aes(x=clust, y=n))+geom_bar(stat="identity", col="black", fill="black")+
  theme_bw()+theme(text = element_text(size = 20))+labs(x="Cluster", y="Count")

avg_clusters<-read.csv("MonthClustersNov2022.csv")
avg_clusters<-avg_clusters[,c(2,15:17)]
colnames(avg_clusters)[2]<-"Avg_Clust"

siteyear_clust$Site<-substr(siteyear_clust$siteyear,1,nchar(siteyear_clust$siteyear)-5)

siteyear_clust_avg<-merge(siteyear_clust, clust_mean_mode, by="Site")

siteyear_clust_avg$same<-ifelse(siteyear_clust_avg$clust==siteyear_clust_avg$mode, "yes", "no")

same_clust_count<-siteyear_clust_avg %>%
  group_by(Site) %>%
  dplyr::count(same) %>%
  filter(same=="yes")

num_years<-siteyear_clust_avg %>%
  dplyr::count(Site)

same_clust_count<-merge(same_clust_count, num_years, by="Site")

names(same_clust_count)<-c("Site","Same_Cluster","Yes","Total")

same_clust_count$diff<-same_clust_count$Total-same_clust_count$Yes

same_clust_count$Prop<-same_clust_count$diff/same_clust_count$Total

same_clust_count<-merge(same_clust_count, biomes, by="Site")

ggplot(same_clust_count,aes(Name, 1-Prop))+geom_boxplot()+theme_classic()+geom_jitter()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="Climate Classification",y="Stability")+
  theme(text = element_text(size = 20))

write.csv(same_clust_count, "StabilitySiClusters.csv")










clust.dba <- tsclust(month_norm_t, type="partitional", centroid = "dba", distance = "dtw",
                     window.size=1L, k=7L, seed = 8, trace = TRUE, 
                     control = partitional_control(iter.max = 300L))

clust_output<-clust.dba@clusinfo

clust_output$cluster<-seq(1,7,1)

ID<-seq(1,7,1)

ggplot(clust_output, aes(x=cluster, y=av_dist))+geom_bar(stat = "identity", fill="black")+
  scale_x_continuous(labels = as.character(ID), breaks = ID)+theme_classic()+
  labs(x="Cluster", y="Average Intra-Cluster Distance")+theme(text = element_text(size = 20))

plot(clust.dba, type = "sc")

mydata<-clust.dba@datalist

data_df<-t(do.call(cbind, mydata))

fviz_cluster(object = list(data=data_df, cluster = clust.dba@cluster),
             data = month_norm_t, stand = TRUE,
             labelsize = 0, ellipse.type = "t", ellipse.level = 0.9)+theme_classic()

month_clusters<-as.data.frame(cbind(data_df, clust.dba@cluster))
colnames(month_clusters)<-c(paste(seq(1:12)), "Cluster")
month_clusters<-rownames_to_column(month_clusters, "SiteYear")
month_clusters$Site<-as.character(map(strsplit(month_clusters$SiteYear, "_"),1))
month_clusters$WY<-as.numeric(map(strsplit(month_clusters$SiteYear, "_"),2))
month_clusters$type<-as.character(map(strsplit(month_clusters$SiteYear, "_"),3))

#write.csv(month_clusters, "Annual_Cluster_DF.csv")

month_clusters<-read.csv("Annual_Cluster_DF.csv")

month_clusters_conc<-merge(month_clusters_melt, monthly_results, by=c("Site", "variable", "WY"))

month_clusters_conc<-month_clusters_conc[,c("Site", "SiteYear", "Cluster", "WY", "variable","value",
                                            "ClimateZ", "Name", "Q", "Conc")]

month_clusters_range<-month_clusters_conc %>%
  dplyr::group_by(Cluster, SiteYear) %>%
  dplyr::summarise(mean(Conc))

colnames(month_clusters_range)<-c("Cluster", "SiteYear", "Conc")

ggplot(month_clusters_range, aes(log(Conc), fill=as.character(Cluster)))+geom_density(alpha=0.3)+
  theme_bw()

colnames(Q_avg)<-c("Cluster", "Month", "Q")

ggplot(Q_avg, aes(Month, Q))+geom_line(group=1)+facet_wrap(~Cluster)


month_clusters<-month_clusters[,-1]

biomes<-read.csv("Koeppen_Geiger.csv")
names(biomes)[2]<-"Site"
month_clusters<-merge(month_clusters, biomes, by="Site")

KG_name<-read.csv("KG_Clim_Name.csv")

month_clusters<-merge(month_clusters, KG_name, by="ClimateZ")

month_clusters_unique<-ddply(month_clusters, "Site", function(z) tail(z,1))

ggplot(month_clusters_unique, aes(x=Name))+geom_bar(aes(fill=LTER))+
  scale_fill_viridis_d(option = "magma")+
  theme_classic()+theme(text = element_text(size = 20))+
  labs(x="Köeppen-Geigen Climate Classification", y="Count")
  

#reorder the names of Biome so that they plot in this order on axis and legens
# month_clusters$Biome2<-factor(month_clusters$Biome_new, levels = c("Tropical rainforest","Temperate coniferous forest",
#                                                                      "Temperate grassland","Boreal forest",
#                                                                      "Temperate deciduous forest", 
#                                                                      "Tropical savanna","Alpine tundra", 
#                                                                      "Polar desert", "Arctic tundra"))

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

month_clusters<-month_clusters[,-c(19,20,21,23,24)]

month_clusters_melt<-melt(month_clusters, id.vars = c("Site", "SiteYear", "Cluster",
                                                      "LTER", "ClimateZ", "Name", "WY",
                                                      "type"))

month_clusters_melt$variable<-gsub("X", "", month_clusters_melt$variable)

#site_list<-unique(month_clusters_melt$Site)

LTER_list<-unique(month_clusters_melt$LTER)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/Plots_for_Seasonality_Survey/Annual_Si_Clusters")

for (k in 1:length(LTER_list)) {
  
  LTER_sites<-subset(month_clusters_melt, month_clusters_melt$LTER==LTER_list[k])
  
  site_list<-unique(LTER_sites$Site)
  
  pdf(paste0(LTER_list[k], "_Annual_Si_Concentration_Clusters.pdf"), width=11)
  
  for (i in 1:length(site_list)) {
    
    cluster_site<-subset(month_clusters_melt, month_clusters_melt$Site==site_list[i])
    
    p0<-ggplot(cluster_site, aes(as.numeric(variable), value, col=as.character(WY), group=SiteYear))+
      geom_line()+theme_bw()+scale_x_continuous(labels = seq(1,12,1), breaks = seq(1,12,1))+
      ggtitle(site_list[i])+
      labs(x="Flow Period", y="Normalized Si Concentration", col="Water Year")
    
    print(p0)
    
    p1<-ggplot(cluster_site, aes(as.numeric(variable), value, col=as.character(WY), group=SiteYear))+
      geom_line()+facet_wrap(~Cluster)+theme_bw()+
      scale_color_discrete(na.value="black")+
      scale_x_continuous(labels = seq(1,12,1), breaks = seq(1,12,1))+
      ggtitle(site_list[i])+labs(x="Flow Period", y="Normalized Si Concentration", col="Water Year")
    
    na_clust<-subset(cluster_site, cluster_site$type=="average")
    
    print(p1)
    
    p2<-ggplot(cluster_site, aes(WY, Cluster))+geom_line()+geom_point()+theme_bw()+
      scale_y_continuous(labels = ID, breaks = ID)+geom_hline(yintercept = na_clust$Cluster,
                                                              col="red")+
      ggtitle(site_list[i])
    
    print(p2)
    
    
  }
  
  dev.off()
  
}




cluster_avg<-grep("average", month_clusters$SiteYear)
site_avg_cluster<-month_clusters[cluster_avg,c("Site","Cluster")]

names(site_avg_cluster)[2]<-"Average_Cluster"

month_clusters_avg<-merge(month_clusters, site_avg_cluster, by="Site")

cluster_comp<-month_clusters_avg %>%
  filter(type=="annual") %>%
  mutate(same_cluster = ifelse(Cluster==Average_Cluster, "yes", "no"))

same_clust_count<-cluster_comp %>%
  group_by(Site) %>%
  dplyr::count(same_cluster) %>%
  filter(same_cluster=="yes")

num_years<-cluster_comp %>%
  dplyr::count(Site)

same_clust_count<-merge(same_clust_count, num_years, by="Site")

names(same_clust_count)<-c("Site","Same_Cluster","Yes","Total")

same_clust_count$diff<-same_clust_count$Total-same_clust_count$Yes

same_clust_count$Prop<-same_clust_count$diff/same_clust_count$Total

same_clust_count<-merge(same_clust_count, biomes, by="Site")

same_clust_count<-merge(same_clust_count, KG_name, by="ClimateZ")

#reorder the names of Biome so that they plot in this order on axis and legens
same_clust_count$Biome2<-factor(same_clust_count$Biome_new, levels = c("Tropical rainforest","Temperate coniferous forest",
                                                                   "Temperate grassland","Boreal forest",
                                                                   "Temperate deciduous forest", 
                                                                   "Tropical savanna","Alpine tundra", 
                                                                   "Polar desert", "Arctic tundra"))

same_clust_count$Name<-factor(same_clust_count$Name, levels = c("Humid Monsoon", "Humid Subtropical", "Mediterranean", 
                                        "Humid Temperate", "Humid Continental", "Subarctic",
                                        "Tundra", "Ice Cap"))

month_clusters_melt$Name<-factor(month_clusters_melt$Name, levels = c("Humid Monsoon", "Humid Subtropical", "Mediterranean", 
                                                                "Humid Temperate", "Humid Continental", "Subarctic",
                                                                "Tundra", "Ice Cap"))


ggplot(same_clust_count,aes(Name, 1-Prop))+geom_boxplot()+theme_classic()+geom_jitter()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="Climate Classification",y="Stability")+
  theme(text = element_text(size = 20))

ggplot(month_clusters_melt, aes(variable, value))+
  geom_line(aes(col=Name, group=SiteYear))+
  theme_bw()+facet_wrap(~Cluster, nrow = 2)+
  scale_color_manual(values = carto_pal(n=12, "Bold"))+
  theme(text = element_text(size = 20))+labs(x="Month", y="Normalized Si Concentration")

avg_annual_cluster<-aggregate(value~Cluster+variable, month_clusters_melt, FUN = mean)

ggplot(avg_annual_cluster, aes(variable, value, group=1))+geom_line()+theme_bw()+
  facet_wrap(~Cluster, nrow = 2)+labs(x="Flow Period", y="Normalized Si Concentration")+
  theme(text = element_text(size = 20))

ID<-seq(1,7,1)

ggplot(month_clusters_melt, aes(Cluster, fill=Name))+geom_bar()+theme_bw()+
  scale_fill_manual(values = carto_pal(n=12, "Bold"))+labs(x="Cluster", y="Count")+
  scale_x_continuous(labels = as.character(ID), breaks = ID)+
  theme(text = element_text(size=20))

wanted_sites<-c("MPR", "GSWS08")

month_clusters_melt %>%
  filter(Site %in% wanted_sites) %>%
  ggplot(aes(variable, value))+geom_line(aes(col=Site, group=SiteYear))+
  theme_bw()+facet_wrap(~Cluster, nrow = 2)+labs(x="Month", y="Normalized Si Concentration")+
  theme(text = element_text(size = 20))

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
  dplyr::summarise(num_clusters=n_distinct(Cluster))

cluster_membership<-merge(cluster_membership, biomes, by="Site")

cluster_membership$Biome2<-factor(cluster_membership$Biome_new, levels = c("Tropical rainforest","Temperate coniferous forest",
                                                             "Temperate grassland","Boreal forest",
                                                             "Temperate deciduous forest", 
                                                             "Tropical savanna","Alpine tundra", 
                                                             "Polar desert", "Arctic tundra"))

ggplot(cluster_membership)+geom_bar(aes(num_clusters, fill=ClimateZ))+
  scale_fill_manual(values = carto_pal(n=12, "Bold"))+
  theme_classic()+labs(x="Number of Different Clusters",y="Count",fill="Biome")+
  theme(text = element_text(size=20))




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
  ggplot(aes(variable, value))+geom_line(aes(group=SiteYear, col=))+
  facet_wrap(~Cluster)+theme_bw()+labs(x="Month", y="Normalized Si Concentration")

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
