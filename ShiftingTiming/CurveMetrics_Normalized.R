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
require(googledrive)
require(dplyr)

#set wd
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#read in monthly results
monthly_results<-read.csv("Full_Results_Monthly_GFN_WRTDS.csv")

#filter to only include DSi
monthly_results<-subset(monthly_results, monthly_results$chemical=="DSi")

#remove sites without continuous flow
remove_site<-c("SADDLE STREAM 007", "MARTINELLI")

monthly_results<-monthly_results[!monthly_results$stream %in% remove_site,]

#remove MCM sites
monthly_results<-monthly_results[!monthly_results$LTER=="MCM",]

#unique(monthly_results$stream)

#month_conc<-monthly_results[,c("stream", "Month", "Conc_mgL")]
month_conc<-monthly_results[,c("stream", "Month", "Conc_mgL")]

#aggregate by month and stream to get one value for every month and stream pair
month_agg_unscaled<-aggregate(Conc_mgL ~ Month+stream, data = month_conc, FUN=mean)

minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# month_conc_norm <- month_conc %>%
#   group_by(stream) %>%
#   mutate(norm_conc=scale(Conc_mgL))

month_conc_norm <- month_agg_unscaled %>%
  group_by(stream) %>%
  mutate(norm_conc=minMax(Conc_mgL))

month_agg<-month_conc_norm[,c(1,2,4)]

colnames(month_agg)[3]<-"conc"

# sites_unique<-unique(month_agg$stream)
# 
# pdf("MinMaxNorm_SiCurves.pdf")
# 
# for (i in 1:length(sites_unique)) {
# 
#   curve<-subset(month_agg, month_agg$stream==sites_unique[i])
# 
#   p1<-ggplot(curve, aes(Month, conc))+geom_line()+theme_bw()+ggtitle(paste(sites_unique[i]))+
#     scale_x_continuous(breaks = seq(1,12,1))
# 
#   print(p1)
# 
# 
# }
# 
# dev.off()

####timing of min and max####

#select row of min conc for each stream
min<-month_agg %>%
  group_by(stream) %>%
  slice(which.min(conc))

#rename columns
colnames(min)<-c("Min_Month", "stream", "Min_Conc")

#select row of max conc for each stream
max<-month_agg %>%
  group_by(stream) %>%
  slice(which.max(conc))

#rename columns
colnames(max)<-c("Max_Month", "stream", "Max_Conc")

#merge
min_max_all<-merge(min, max, by="stream")

####calculate rate of decline/return####

#find the difference between min and max
min_max_all$si_diff<-min_max_all$Max_Conc-min_max_all$Min_Conc

#calculate the number of month between min and max (return)
min_max_all$num_months_return<-ifelse(min_max_all$Max_Month > min_max_all$Min_Month, min_max_all$Max_Month - min_max_all$Min_Month, 
                                      (12-min_max_all$Min_Month) + (min_max_all$Max_Month))

#calculate the number of month between max and min (decline)
min_max_all$num_months_decline<-ifelse(min_max_all$Min_Month > min_max_all$Max_Month, min_max_all$Min_Month - min_max_all$Max_Month, 
                                      (12-min_max_all$Max_Month) + (min_max_all$Min_Month))

#calculate decline rate
min_max_all$rate_decline<-min_max_all$si_diff/min_max_all$num_months_decline

#calculate return rate
min_max_all$rate_return<-min_max_all$si_diff/min_max_all$num_months_return

#merge into one df
month_agg_min_max<-merge(month_agg, min, by="stream")
month_agg_min_max<-merge(month_agg_min_max, max, by="stream")

####angle calculation####

#get month before and after min month
month_agg_min_max$min_minus_one<-ifelse(month_agg_min_max$Min_Month==1, 12, month_agg_min_max$Min_Month-1)
month_agg_min_max$min_plus_one<-ifelse(month_agg_min_max$Min_Month==12, 1, month_agg_min_max$Min_Month+1)

#get month before and after max month
month_agg_min_max$max_minus_one<-ifelse(month_agg_min_max$Max_Month==1, 12, month_agg_min_max$Max_Month-1)
month_agg_min_max$max_plus_one<-ifelse(month_agg_min_max$Max_Month==12, 1, month_agg_min_max$Max_Month+1)


#select rows that are before and after the min
min_angle<-month_agg_min_max %>%
  group_by(stream) %>%
  filter(Month %in% c(min_minus_one, min_plus_one))

#rename columns
min_angle<-min_angle[,c("stream", "Month", "conc", "Min_Conc")] 

#find difference between min and one month before and min and one month after
min_angle$conc_diff<-min_angle$Min_Conc-min_angle$conc  

#multiply every other one by -1 to make only the before-min slope negative
#min_angle$conc_diff[seq(2,nrow(min_angle), 2)]<-min_angle$conc_diff[seq(2,nrow(min_angle), 2)]*-1

min_angle$conc_diff<-min_angle$conc_diff*-1

min_angle$scaled_diff<-min_angle$conc_diff/(1/12)

#make list of streams
stream_list<-unique(min_angle$stream)

#open list to store angle
deg<-list()

#loop through each stream and calculate the angle of the min
#we are doing this by taking the arctan of the concnetration difference between min and one month before
#and the arctan of the oncnetration difference between min and one month after
#we then subract both these angles from 180 to get the angle in between the two lines
for (i in 1:length(stream_list)) {
  
  one_stream<-subset(min_angle, min_angle$stream==stream_list[i])

  slope1<-(atan(abs(one_stream$scaled_diff[1]))*180)/pi
  
  slope2<-(atan(one_stream$scaled_diff[2])*180)/pi
  
  deg[[i]]<-180-slope1-slope2
  
}

#unlist from the list
min_angle_clean<-as.data.frame(unlist(deg))
min_angle_clean$stream<-stream_list

colnames(min_angle_clean)<-c("min_angle", "stream")

#repeat for maximum angle
max_angle<-month_agg_min_max %>%
  group_by(stream) %>%
  filter(Month %in% c(max_minus_one, max_plus_one))

max_angle<-max_angle[,c("stream", "Month", "conc", "Max_Conc")] 

max_angle$conc_diff<-max_angle$Max_Conc-max_angle$conc  

#max_angle$conc_diff[seq(2,nrow(max_angle), 2)]<-max_angle$conc_diff[seq(2,nrow(max_angle), 2)]*-1

max_angle$scaled_diff<-max_angle$conc_diff/(1/12)

deg<-list()

for (i in 1:length(stream_list)) {
  
  one_stream<-subset(max_angle, max_angle$stream==stream_list[i])
  
  slope1<-(atan(one_stream$scaled_diff[1])*180)/pi
  
  slope2<-(atan(abs(one_stream$scaled_diff[2]))*180)/pi
  
  deg[[i]]<-180-slope1-slope2
  
}

max_angle_clean<-as.data.frame(unlist(deg))
max_angle_clean$stream<-stream_list

colnames(max_angle_clean)<-c("max_angle", "stream")

min_max_all<-merge(min_max_all, min_angle_clean, by="stream")

min_max_all<-merge(min_max_all, max_angle_clean, by="stream")

####variability around decline/return rate####

#return variability

diff<-list()

for (i in 1:length(stream_list)) {
  
  #select just one stream
  one_stream<-subset(min_max_all, min_max_all$stream==stream_list[i])
  
  #create a new df that goes from 0 to the number of months of return
  new_df<-as.data.frame(c(0:one_stream$num_months_return))
  
  #make the first row of a new column the minimum concentration
  new_df[1,2]<-one_stream$Min_Conc
  
  #rename columns
  colnames(new_df)<-c("months", "expected_conc")
  
  #create a linear regression between the min and max based on the rate of return
  new_df$expected_conc[c(2:nrow(new_df))]<-one_stream$Min_Conc+(new_df$months[c(2:nrow(new_df))]*one_stream$rate_return)
  
  #pull out the actual monthly concentrations from the monthly agg df
  all_data_one_stream<-subset(month_agg, month_agg$stream==stream_list[i])
  
  #add the number of month of return to the minimum month
  max_month<-one_stream$Min_Month+one_stream$num_months_return
  
  #reset the max month to make 12 the maximum max month
  max_month_crop<-ifelse(max_month > 12, 12, max_month)
  
  #if the max month is past 12, then subtract 12 from max month and use this to inform 
  #the months to pull from at the beginning of the year (i.e., if max month is 17, then it would include 1-5)
  past_12<-ifelse(max_month > 12, max_month-12, 0)
  
  #select the months to subset from the total month agg df
  #if the max month if > 12, then use min month-12, plus 1-(12-max month)
  #if the max month is < 12, then just use min month-max month
  if(past_12 > 0){
    
    use_these_months<-c(one_stream$Min_Month:max_month_crop, 1:past_12)
    
  }else{
    
    use_these_months<-c(one_stream$Min_Month:max_month_crop)
    
  }
  
  #subset all the month agg df to only include the months that are during the return interval
  all_data_one_stream_return<-subset(all_data_one_stream, all_data_one_stream$Month %in% use_these_months)
  
  #arrange them in order - min -> max (factor by use these months)
  all_data_one_stream_return<-all_data_one_stream_return %>% arrange(factor(Month, levels = use_these_months))
  
  #add the actual concentration from the above subsetted df to the new df with expected concentrations
  new_df$actual_conc<-all_data_one_stream_return$conc
  
  #sum the expected concentrations
  sum_expected<-sum(new_df$expected_conc)
  
  #sum actual concentrations
  sum_actual<-sum(new_df$actual_conc)
  
  #find difference between expected and actual
  new_df$diff<-abs(new_df$actual_conc-new_df$expected_conc)
  
  #sum differences between expected and actual, add to list
  diff[[i]]<-sum(new_df$diff)/one_stream$num_months_return
  
}

#unlist, add stream name
return_variability<-as.data.frame(unlist(diff))
return_variability$stream<-stream_list

#rename columns
colnames(return_variability)<-c("return_var", "stream")

#decline variability
diff<-list()

for (i in 1:length(stream_list)) {
  
  #select just one stream
  one_stream<-subset(min_max_all, min_max_all$stream==stream_list[i])
  
  #create a new df that goes from 0 to the number of months of return
  new_df<-as.data.frame(c(0:one_stream$num_months_decline))
  
  #make the first row of a new column the minimum concentration
  new_df[1,2]<-one_stream$Max_Conc
  
  #rename columns
  colnames(new_df)<-c("months", "expected_conc")
  
  #create a linear regression between the min and max based on the rate of return
  new_df$expected_conc[c(2:nrow(new_df))]<-one_stream$Max_Conc-(new_df$months[c(2:nrow(new_df))]*one_stream$rate_decline)
  
  #pull out the actual monthly concentrations from the monthly agg df
  all_data_one_stream<-subset(month_agg, month_agg$stream==stream_list[i])
  
  #add the number of month of return to the minimum month
  min_month<-one_stream$Max_Month+one_stream$num_months_decline
  
  #reset the max month to make 12 the maximum max month
  min_month_crop<-ifelse(min_month > 12, 12, min_month)
  
  #if the max month is past 12, then subtract 12 from max month and use this to inform 
  #the months to pull from at the beginning of the year (i.e., if max month is 17, then it would include 1-5)
  past_12<-ifelse(min_month > 12, min_month-12, 0)
  
  #select the months to subset from the total month agg df
  #if the max month if > 12, then use min month-12, plus 1-(12-max month)
  #if the max month is < 12, then just use min month-max month
  if(past_12 > 0){
    
    use_these_months<-c(one_stream$Max_Month:min_month_crop, 1:past_12)
    
  }else{
    
    use_these_months<-c(one_stream$Max_Month:min_month_crop)
    
  }
  
  #subset all the month agg df to only include the months that are during the return interval
  all_data_one_stream_return<-subset(all_data_one_stream, all_data_one_stream$Month %in% use_these_months)
  
  #arrange them in order - min -> max (factor by use these months)
  all_data_one_stream_return<-all_data_one_stream_return %>% arrange(factor(Month, levels = use_these_months))
  
  #add the actual concentration from the above subsetted df to the new df with expected concentrations
  new_df$actual_conc<-all_data_one_stream_return$conc
  
  #sum the expected concentrations
  sum_expected<-sum(new_df$expected_conc)
  
  #sum actual concentrations
  sum_actual<-sum(new_df$actual_conc)
  
  #find difference between expected and actual
  new_df$diff<-abs(new_df$actual_conc-new_df$expected_conc)
  
  #sum differences between expected and actual, add to list
  diff[[i]]<-sum(new_df$diff)/one_stream$num_months_decline

  
}

#unlist, add stream name
decline_variability<-as.data.frame(unlist(diff))
decline_variability$stream<-stream_list

#rename columns
colnames(decline_variability)<-c("decline_var", "stream")

min_max_all<-merge(min_max_all, return_variability, by="stream")

min_max_all<-merge(min_max_all, decline_variability, by="stream")

write.csv(min_max_all,"Curve_Metrics.csv")






