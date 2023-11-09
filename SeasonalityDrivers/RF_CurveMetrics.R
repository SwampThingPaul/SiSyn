#read in, clean metrics
require(randomForest)
require(cowplot)


setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

metrics<-read.csv("Curve_Metrics.csv")

metrics<-metrics[!metrics$stream=="Imnavait Weir",]

metrics<-metrics[,c("stream", "Min_Month", "Max_Month", "num_months_decline", "min_angle", "max_angle", "return_var", "decline_var")]

drivers<-read.csv("AllDrivers_Harmonized.csv")

drivers<-drivers[,!colnames(drivers) %in% c("X.1", "X", "LTER", "Stream_Name", "rndCoord.lon", "rndCoord.lat", "Name", "cycle1")]
drivers<-drivers[!duplicated(drivers$Stream_ID),]

#crop to only relevant drivers
drivers_cropped<-drivers[,c("Stream_ID","med_si","CV_C","med_q","CV_Q", "cvc_cvq","slope","Latitude","Longitude",
                            "drainSqKm", "num_days","prop_area","precip", "evapotrans", "temp", "npp",
                            "cycle0", "elevation_mean_m", colnames(drivers[30:58]))]

drivers_cropped<-drivers_cropped[,-which(colnames(drivers_cropped) %like% "major")]

#replace NA with 0 in proportion of land/lithology/soils
drivers_cropped[,c(19:44)]<-replace(drivers_cropped[,c(19:44)], is.na(drivers_cropped[,c(19:44)]), 0)

drivers_cropped$stream<-str_split(drivers_cropped$Stream_ID, '__', simplify = TRUE)[,2]

drivers_cropped<-drivers_cropped[,!colnames(drivers_cropped) %in% c("Stream_ID")]

tot<-merge(metrics, drivers_cropped, by="stream")

tot<-tot[c(complete.cases(tot)),]

#only include numeric drivers (not min month or max month)
metrics<-colnames(tot)[4:8]

i=1

pdf("CurveMetricsRFPlots_CroppedDrivers.pdf", width = 12, height = 8)

for (i in 1:length(metrics)) {
  
  k<-i+3
  
  new_df<-tot[,c(k,9,11,15,18:39)]
  
  input_vars<-formula(paste(metrics[i], "~", paste(colnames(tot[,c(9,11,15,18:39)]), collapse = "+")))
  
  rf_model<-randomForest(input_vars, 
                         data=new_df, importance=TRUE, proximity=TRUE, ntree=1000, mtry=6)
  
  varImpPlot(rf_model, main = paste(metrics[i]))
  
  predict<-as.data.frame(cbind(as.numeric(rf_model$predicted), new_df[,1]))
  
  colnames(predict)<-c("Modeled", "Observed")
  
  lm<-lm(as.numeric(Modeled)~as.numeric(Observed), data = predict)
  
  p1<-ggplot(predict, aes(Modeled, Observed))+geom_abline(col="grey", lwd=2)+
    geom_point(size=2)+
    ggtitle(paste(metrics[i]), subtitle = summary(lm)$r.squared)+
    theme_bw()+ theme(text = element_text(size=20))
  
  print(p1)
  
}

dev.off()

####partial differential plots for most imporant variables####
#most important vars: prop area, num days, temp, evap, npp, elevation, Latitude

metrics<-colnames(tot)[2:8]

imp_vars<-tot[,c(15,18,19,21:23,25)]

pdf("PartialDependencePlots.pdf", width = 12, height = 8)

for (i in 1:length(metrics)) {
  
  k<-i+1
  
  new_df<-tot[,c(k,9,11,15,18:39)]
  
  input_vars<-formula(paste(metrics[i], "~", paste(colnames(tot[,c(9,11,15,18:39)]), collapse = "+")))
  
  rf_model<-randomForest(input_vars, 
                         data=new_df, importance=TRUE, proximity=TRUE, ntree=1000, mtry=6)
  
  
  par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))
  
  for (j in 1:length(imp_vars)) {
    
    partialPlot(rf_model, new_df, colnames(imp_vars[j]), main=paste(colnames(imp_vars[j])), 
                                xlab=paste(colnames(imp_vars[j])), ylab=paste(colnames(tot[k])))
    
  }
  
  
}

dev.off()


#min month
new_df<-tot[,c(2,9,11,15,18:39)]

rf_model<-randomForest(new_df[,c(2:26)], y=as.factor(new_df$Min_Month), 
                       data=new_df, importance=TRUE, proximity=TRUE, ntree=1000, mtry=6)

varImpPlot(rf_model)

rf_model

predict<-as.data.frame(cbind(as.numeric(rf_model$predicted), as.numeric(new_df$Min_Month)))

colnames(predict)<-c("Modeled", "Observed")

predict$Modeled<-factor(predict$Modeled, levels = c(11,12,1,2,3,4,5,6,7,8,9,10))

predict$Observed<-factor(predict$Observed, levels = c(11,12,1,2,3,4,5,6,7,8,9,10))

lm<-lm(as.numeric(Modeled)~as.numeric(Observed), data = predict)

summary(lm)

df<-as.data.frame(rf_model$confusion)

df_new<-t(apply(df[,c(1:12)],1, function(x) x/sum(x)))

df_melt<-melt(df_new)

df_melt$value<-round(df_melt$value, digits = 2)
                                                                                                                                                                                                                                                                                                                                                                                                                    
df_melt %>% 
  mutate(color = case_when(Var1 == Var2 ~ 'forestgreen', 
                           value == 0 ~ 'white', 
                           TRUE ~ 'deeppink3')) %>%
  ggplot(aes(Var1, Var2, fill = color)) +
  geom_tile(color="grey57") + 
  geom_text(aes(label=value)) +
  scale_fill_identity() + 
  scale_x_continuous(breaks = seq(1,12,1))+
  scale_y_continuous(breaks = seq(1,12,1))+
  theme_classic()+
  theme(text = element_text(size=20))+
  labs(x = "Observed",y = "Prediction")


ggplot(predict, aes(Modeled, Observed))+geom_abline(col="grey", lwd=2)+
  geom_point()+geom_jitter(aes(col=as.factor(Observed)), size=2)+theme_bw()+labs(col="Observed Month")+
  theme(text = element_text(size=20))



#max month
new_df<-tot[,c(3,9,11,15,18:39)]

rf_model<-randomForest(new_df[,c(2:26)], y=as.factor(new_df$Max_Month), 
                       data=new_df, importance=TRUE, proximity=TRUE, ntree=1000, mtry=6)

varImpPlot(rf_model)

rf_model

predict<-as.data.frame(cbind(as.numeric(rf_model$predicted), as.numeric(new_df$Max_Month)))

colnames(predict)<-c("Modeled", "Observed")

predict$Modeled<-factor(predict$Modeled, levels = c(6,7,8,9,10,11,12,1,2,3,4,5))

predict$Observed<-factor(predict$Observed, levels = c(6,7,8,9,10,11,12,1,2,3,4,5))

lm<-lm(as.numeric(Modeled)~as.numeric(Observed), data = predict)

summary(lm)

ggplot(predict, aes(Modeled, Observed))+geom_abline(col="grey", lwd=2, slope = 1, intercept = 1)+
  geom_point()+geom_jitter(aes(col=as.factor(Observed)), size=2)+theme_bw()+labs(col="Observed Month")+
  theme(text = element_text(size=20))

df<-as.data.frame(rf_model$confusion)

df_new<-t(apply(df[,c(1:12)],1, function(x) x/sum(x)))

df_melt<-melt(df_new)

df_melt$value<-round(df_melt$value, digits = 2)

df_melt %>% 
  mutate(color = case_when(Var1 == Var2 ~ 'forestgreen', 
                           value == 0 ~ 'white', 
                           TRUE ~ 'deeppink3')) %>%
  ggplot(aes(Var1, Var2, fill = color)) +
  geom_tile(color="grey57") + 
  geom_text(aes(label=value)) +
  scale_fill_identity() + 
  scale_x_continuous(breaks = seq(1,12,1))+
  scale_y_continuous(breaks = seq(1,12,1))+
  theme_classic()+
  theme(text = element_text(size=20))+
  labs(x = "Observed",y = "Prediction")

#num months decline
new_df<-tot[,c(4,9,11,15,18:39)]

rf_model<-randomForest(new_df[,c(2:26)], y=as.factor(new_df$num_months_decline), 
                       data=new_df, importance=TRUE, proximity=TRUE, ntree=1000, mtry=6)

varImpPlot(rf_model)

rf_model

predict<-as.data.frame(cbind(as.numeric(rf_model$predicted), as.numeric(new_df$num_months_decline)))

colnames(predict)<-c("Modeled", "Observed")

lm<-lm(Modeled~Observed, data = predict)

summary(lm)

ggplot(predict, aes(Modeled, Observed))+geom_abline(col="grey", lwd=2)+
  geom_point()+geom_jitter(aes(col=as.factor(Observed)), size=2)+theme_bw()+labs(col="Observed Month")+
  scale_x_continuous(breaks = seq(1,12,1))+scale_y_continuous(breaks = seq(1,12,1))+
  theme(text = element_text(size=20))

df<-as.data.frame(rf_model$confusion)

df_new<-t(apply(df[,c(1:8)],1, function(x) x/sum(x)))

df_melt<-melt(df_new)

df_melt$value<-round(df_melt$value, digits = 2)

df_melt %>% 
  mutate(color = case_when(Var1 == Var2 ~ 'forestgreen', 
                           value == 0 ~ 'white', 
                           TRUE ~ 'deeppink3')) %>%
  ggplot(aes(Var1, Var2, fill = color)) +
  geom_tile(color="grey57") + 
  geom_text(aes(label=value)) +
  scale_fill_identity() +
  theme_classic()+
  theme(text = element_text(size=20))+
  scale_x_continuous(breaks = seq(2,9,1))+
  scale_y_continuous(breaks = seq(2,9,1))+
  labs(x = "Observed",y = "Prediction")

  

####curve metrics biplots and distributions ####

pdf("CurveMetricsBiPlots.pdf", width = 12, height = 8)

for (i in 1:length(metrics)) {
  
  k<-i+1
  
  new_df<-tot[,c(k,9,11,15,18:39)]
  
  new_df_melt<-melt(new_df, id.vars=metrics[i])
  
  p1<-ggplot(new_df_melt, aes(new_df_melt[,1], value))+geom_point()+facet_wrap(~variable, scales = "free")+
    theme_classic()+labs(x=paste(metrics[i]))+ggtitle(paste(metrics[i]))+geom_smooth(se=F)
  
  print(p1)
  
}

dev.off()

pdf("CurveMetrics_Drivers_Dist.pdf", width = 12, height = 8)

new_df<-tot[,c(metrics)]
new_df$seq<-seq(1,nrow(new_df), 1)

new_df_melt<-melt(new_df, id.vars="seq")

ggplot(new_df_melt, aes(x=value))+geom_histogram()+facet_wrap(~variable, scales = "free")+theme_bw()

new_df<-tot[,c(9,11,15,18:39)]
new_df$seq<-seq(1,nrow(new_df), 1)

new_df_melt<-melt(new_df, id.vars="seq")

ggplot(new_df_melt, aes(x=value))+geom_histogram()+facet_wrap(~variable, scales = "free")+theme_bw()

dev.off()

for (i in 1:length(metrics)) {
  
  k<-i+1
  
  new_df<-tot[,c(k,9,11,15,18:39)]
  
  new_df_melt<-melt(new_df, id.vars=metrics[i])
  
  p1<-ggplot(new_df_melt, aes(new_df_melt[,1], value))+geom_point()+facet_wrap(~variable, scales = "free")+
    theme_classic()+labs(x=paste(metrics[i]))+ggtitle(paste(metrics[i]))+geom_smooth(se=F)
  
  print(p1)
  
}

dev.off()

