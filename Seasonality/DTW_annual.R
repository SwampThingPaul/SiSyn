##dynamic time warping
#install.packages("dtw")
require("dtw")
require(plot.matrix)
require(factoextra)
require(stringr)
require(dtwclust)
require(dplyr)
require(reshape2)
require(tibble)

#set wd
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#link to monthly WRTDS data
link<-"https://drive.google.com/file/d/1qSwukNdNMgOnpk_fd7RTHDEnvaFvXv_r/view?usp=share_link"

#get file
file_get<-drive_get(as_id(link))

#download
drive_download(file_get$drive_resource,  overwrite=T)

#read in monthly results
monthly_results<-read.csv("Full_Results_Monthly_GFN_WRTDS.csv")

#filter to only include Si data
monthly_results<-subset(monthly_results, monthly_results$chemical=="DSi")

# ##example plots
# gunnison<-subset(monthly_results, monthly_results$stream=="Delaware River at Trenton")
# 
# gunnison<-gunnison[,c("Month", "Year", "Conc_mgL", "FNConc_mgL")]
# 
# gun_melt<-melt(gunnison, id.vars=c("Month", "Year"))
# 
# ggplot(gun_melt, aes(Month, value))+geom_line(aes(col=Year, group=Year))+facet_wrap(~variable)+theme_bw()+
#   theme(text = element_text(size = 20))+scale_x_continuous(breaks = seq(1,12,1))+
#   labs(y="Concentration (mg/L)")+ggtitle("Delaware River at Trenton")


# setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NewSites_Sept2022")
# 
# interp<-read.csv("interp_final.csv")
# 
# colnames(interp)[5]<-"stream"
# 
# interp_monthly<-merge(monthly_results, interp, by=c("stream", "Month", "Year"))
# 
# interp_monthly_summary<-interp_monthly %>%
#   group_by(stream) %>%
#   count()
# 
# write.csv(interp_monthly, "interp_monthly.csv")
# 
# write.csv(interp_monthly_summary, "interp_monthly_summary.csv")
# 
# 
# dfyear<-monthly_results %>%
#   filter(stream=="PICEANCE CREEK AT WHITE RIVER") %>%
#   summarise(unique(Year))
# 
# monthly_results %>%
#   filter(stream=="YAMPA RIVER AT DEERLODGE PARK") %>%
#   ggplot(aes(x=Month, y=Conc_mgL, col=Year))+geom_point()

#remove sites without continuous flow
remove_site<-c("SADDLE STREAM 007", "MARTINELLI")

monthly_results<-monthly_results[!monthly_results$stream %in% remove_site,]

monthly_results<-monthly_results[!monthly_results$LTER=="MCM",]

#set WY
monthly_results$WY<-ifelse(monthly_results$Month > 9, monthly_results$Year+1, monthly_results$Year)

#choose columns we need
#month_conc<-monthly_results[,c("stream", "Month", "WY", "Conc_mgL")]
month_conc<-monthly_results[,c("stream", "Month", "WY", "FNConc_mgL")]
month_conc<-monthly_results[,c("stream", "Month", "Year", "FNConc_mgL")]

#z score norm
month_conc_scale<-month_conc %>%
  dplyr::group_by(stream) %>%
  dplyr::mutate(scale(FNConc_mgL))

colnames(month_conc_scale)[5]<-"scaled_conc"


#cast to long format - also can use "pivot wide"
#each column is a year-site, each row a month
month_cast<-dcast(month_conc_scale, formula = Month~stream+WY, value.var = "scaled_conc")

#assign to "month_norm"
month_norm<-month_cast

#transpose - now rows are site-year, columns are months
month_norm_t<-as.data.frame(t(month_norm[,c(2:ncol(month_norm))]))

#remove any rows with missing data - could be incomplete years
month_norm_t<-month_norm_t[complete.cases(month_norm_t),]

write.csv(month_norm_t,"AllSiteYearsMonthlyConc.csv")

#read in centroids df from average si clustering
centroids_df<-read.csv("AverageClusterCentroids.csv")

#find distance between each site year and cluster centroid
dist_mat<-dtwDist(as.matrix(month_norm_t), as.matrix(centroids_df[,2:13]), 
                  window.type="sakoechiba", window.size=1L)

dist_mat<-as.data.frame(dist_mat)

#find minimum DTW distance for each site year time series
dist_mat$clust<-apply(dist_mat, 1, which.min)

#convert rownames to columns
dist_mat<-rownames_to_column(dist_mat, var = "siteyear")

#extract just the site
dist_mat$Site<-substr(dist_mat$siteyear,1,nchar(dist_mat$siteyear)-5)
dist_mat$Year<-substr(dist_mat$siteyear,nchar(dist_mat$siteyear)-3, nchar(dist_mat$siteyear))

write.csv(dist_mat, "AllSiteYearClusters.csv")

#write function to get mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#calculate "modal" cluster for each site
siteyear_clust_mode<-dist_mat %>%
  dplyr::group_by(Site) %>%
  dplyr::summarise(mode=Mode(clust))

#get number of different clusters for each site
siteyear_clust_num<-dist_mat %>%
  dplyr::group_by(Site) %>%
  dplyr::summarise(num_clusts=length(unique(clust)))

#write these to csvs
write.csv(siteyear_clust_mode, "SiClustersMode.csv")

write.csv(siteyear_clust_num, "SiNumClusters.csv")

# #read in average cluster behavior
# avg_clusters<-read.csv("MonthClustersNov2022.csv")
# avg_clusters<-avg_clusters[,c(2,15:17)]
# colnames(avg_clusters)[2]<-"Avg_Clust"
# 
# #merge modal behavior with average cluster behavior
# clust_mean_mode<-merge(siteyear_clust_mode, avg_clusters, by="Site")  
# 
# #find if modal and average clusters are the same
# clust_mean_mode$same<-ifelse(clust_mean_mode$mode==clust_mean_mode$Avg_Clust, "yes", "no")

#get cluster number from the distance matrix
clust<-dist_mat[,c(1,7)]

#make siteyear column in month norm
month_norm_t<-rownames_to_column(month_norm_t, var = "siteyear")

#rename columns to represent months
colnames(month_norm_t)[2:13]<-seq(1,12)

#merge month norm with cluster
siteyear_clust<-merge(month_norm_t, clust, by="siteyear")

#melt
siteyear_clust_melt<-melt(siteyear_clust, id.vars = c("siteyear", "clust"))

#count number of sites per cluster
siteyear_count<-siteyear_clust %>%
  dplyr::count(clust)

#plot all seasonality lines
ggplot(siteyear_clust_melt, aes(variable, value, group=siteyear))+geom_line(alpha=0.3)+theme_bw()+
  facet_wrap(~clust)+labs(x="Month", y="Normalized Si Concentration")+
  theme(text = element_text(size = 20))

#plot barplot of number of sites per cluster
ggplot(siteyear_count, aes(x=clust, y=n))+geom_bar(stat="identity", col="black", fill="black")+
  theme_bw()+theme(text = element_text(size = 20))+labs(x="Cluster", y="Count")

#read in KG biomes
biomes<-read.csv("Koeppen_Geiger.csv")
names(biomes)[3]<-"Site"

#get just the site from "site year"
siteyear_clust$Site<-substr(siteyear_clust$siteyear,1,nchar(siteyear_clust$siteyear)-5)

#merge siteyear clust and mode
siteyear_clust_avg<-merge(siteyear_clust, siteyear_clust_mode, by="Site")


#if modal and siteyear cluster are the same, "yes"
siteyear_clust_avg$same<-ifelse(siteyear_clust_avg$clust==siteyear_clust_avg$mode, "yes", "no")

#count number of "yesses"
same_clust_count<-siteyear_clust_avg %>%
  group_by(Site) %>%
  dplyr::count(same) %>%
  filter(same=="yes")

#get number of years of data per site
num_years<-siteyear_clust_avg %>%
  dplyr::count(Site)

#merge and rename
same_clust_count<-merge(same_clust_count, num_years, by="Site")

names(same_clust_count)<-c("Site","Same_Cluster","Yes","Total")

#find difference between total number of years and number of years cluster is same as modal cluster
same_clust_count$diff<-same_clust_count$Total-same_clust_count$Yes

#find proportion of time cluster is same as modal cluster
same_clust_count$Prop<-same_clust_count$diff/same_clust_count$Total

#merge with KG biomes
same_clust_count<-merge(same_clust_count, biomes, by="Site")

#write csv
write.csv(same_clust_count, "StabilitySiClusters.csv")


##example plots
gunnison<-subset(siteyear_clust_avg, siteyear_clust_avg$Site=="GUNNISON RIVER")

gunnison<-gunnison[,c("Month", "Year", "Conc_mgL", "FNConc_mgL")]

gun_melt<-melt(gunnison, id.vars=c("Site", "siteyear", "clust", "mode", "same"))

ggplot(gun_melt, aes(variable, value))+geom_line(aes(group=siteyear))+theme_bw()+
  theme(text = element_text(size = 20))+facet_wrap(~clust)+
  labs(y="Concentration (mg/L)")

