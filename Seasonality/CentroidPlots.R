require(ggplot2)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#plot just cluster centroids
centroids_df<-read.csv("AverageClusterCentroids.csv")

#rename columns to represent months
colnames(centroids_df)[2:13]<-seq(1,12,1)

#melt
centroids_melt<-melt(centroids_df, id.vars = c(1,14))

#make df of centroid names
centroid_names<-as.data.frame(t(tibble("1"="Spring Trough",
                                       "2"="Fall Trough",
                                       "3"="Fall Peak",
                                       "4"="Spring Trough, Fall Peak",
                                       "5"="Spring Trough, Variable Summer")))

centroid_names$cluster<-seq(1,5)
colnames(centroid_names)[1]<-"Centroid_Name"

#merge centroid and centroid names
centroids_melt<-merge(centroids_melt, centroid_names, by="cluster")

#order
centroids_melt$Centroid_Name<-factor(centroids_melt$Centroid_Name,
                                     levels = c("Fall Peak", "Fall Trough", "Spring Trough",
                                                "Spring Trough, Fall Peak", "Spring Trough, Variable Summer"))
#plot
ggplot(centroids_melt, aes(variable, value))+facet_wrap(~Centroid_Name)+
  geom_line(size=1, aes(group=cluster))+
  theme_classic()+theme(text = element_text(size=20))+
  labs(x="Month", y="Normalized Si Concentration")

centroids<-unique(centroids_melt$Centroid_Name)

min_max_month<-list()

for (i in 1:length(centroids)) {
  
  cent_df<-subset(centroids_melt,centroids_melt$Centroid_Name==centroids[i])
  
  min<-cent_df  %>%  
    slice_min(value)
  
  max<-cent_df  %>%  
    slice_max(value)
  
  min_max<-bind_rows(min,max)
  
  min_max_month[[i]]<-as.numeric(min_max$variable)
  
  
}

min_max_df<-as.data.frame(do.call(rbind, min_max_month))
colnames(min_max_df)<-c("min", "max")

min_max_df$centroid<-centroids


