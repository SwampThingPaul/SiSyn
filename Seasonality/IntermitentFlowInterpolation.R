setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

link<-"https://drive.google.com/drive/folders/1842KSgp48k_DwvNeYbmz-_b4PSH-vrxg?usp=share_link"

folder = drive_get(as_id(link))

#get list of csv files from folder
csv_files = drive_ls(folder, type="csv")

#check working directory where files will be stored locally; separate folder within project folder
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")
#"L:/GitHub/SiSyn/Merge Site Discharge"
#setwd("L:/GitHub/SiSyn/Merge Site Discharge/discharge files")

#download each file to the working directory; files are saved locally
for (i in 1:length(csv_files$drive_resource)) {
  drive_download(csv_files$drive_resource[i],  overwrite=T)
}

#load monthly results - pull out sites that need to be extended (PA list changes)
monthly_results<-read.csv("Full_Results_Monthly_GFN_WRTDS.csv")

#remove
monthly_results_YR<-monthly_results[!c(monthly_results$LTER=="MCM"),]
monthly_results_YR<-monthly_results_YR[!c(monthly_results_YR$stream=="SADDLE STREAM 007"),]
monthly_results_YR<-monthly_results_YR[!c(monthly_results_YR$stream=="MARTINELLI"),]

#read in daily results- this is the file we will use to mimic "months" for streams that dont flow year round
daily_results<-read.csv("Full_Results_GFN_WRTDS.csv")

#subset to Si and columns of interest
daily_results<-subset(daily_results, daily_results$chemical=="DSi")
daily_results<-daily_results[,c("LTER", "stream", "Date", "Month", "waterYear", "ConcDay", "Q")]

#### MCM ####

daily_results_MCM<-subset(daily_results, daily_results$LTER=="MCM")

link<-"https://drive.google.com/drive/folders/1GyJtzGMJt4hu7ekbvKXLVUMaG8r9XW0l"

folder = drive_get(as_id(link))

#get list of csv files from folder
csv_files = drive_ls(folder, type="csv", pattern="Daily_WRTDSK.csv")

#check working directory where files will be stored locally; separate folder within project folder
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")
#"L:/GitHub/SiSyn/Merge Site Discharge"
#setwd("L:/GitHub/SiSyn/Merge Site Discharge/discharge files")

#download each file to the working directory; files are saved locally
for (i in 1:length(csv_files$drive_resource)) {
  drive_download(csv_files$drive_resource[i],  overwrite=T)
}


file_names<-csv_files$name

stream_names<-c("Harnish","Commonwealth","Crescent","Delta")

new_file_list<-list()

for (i in 1:length(csv_files$name)) {
  
  site<-read.csv(file_names[i])
  
  site$LTER<-"MCM"
  
  site$stream<-stream_names[i]
  
  new_file_list[[i]]<-site
  
}


new_sites_df<-do.call(bind_rows, new_file_list)

new_sites_df<-new_sites_df[,c("LTER", "stream", "Date", "Month", "waterYear", "ConcDay", "Q")]

daily_results_MCM<-bind_rows(daily_results_MCM, new_sites_df)

daily_results_MCM<-subset(daily_results_MCM, daily_results_MCM$Month %in% c(12,1))

#make list of streams in MCM LTER
stream_list<-unique(daily_results_MCM$stream)

#all streams have 61 days of data - to get 12 equal periods we want to pull every 5 days from the 61 days
seq_list<-seq(5, 61, 5)

#open lists to append new dfs to
streams_df_Si<-list()
streams_df_Q<-list()
WY_length<-list()

#the outer loop loops through each stream in MCM
for (i in 1:length(stream_list)) {
  
  #pull out one stream
  one_stream<-subset(daily_results_MCM, daily_results_MCM$stream==stream_list[i])
  
  #remove all NA
  one_stream<-one_stream[complete.cases(one_stream$ConcDay),]
  
  #pull out water years
  WY_list<-unique(one_stream$waterYear)
  
  #append to list - this will be used for column names later
  WY_length[[i]]<-WY_list
  
  #create dataframe of "months" to append new data to
  months_df_Si<-as.data.frame(seq(1,12,1))
  
  months_df_Q<-as.data.frame(seq(1,12,1))
  
  #loop through each water year to get rolling average for each "month"
  for (k in 1:length(WY_list)) {
    
    #pull out one year of data
    one_year_stream <-subset(one_stream, one_stream$waterYear==WY_list[k])
    
    #rolling average
    mov_avg_Si<- rollmean(one_year_stream$ConcDay, k=5)
    
    mov_avg_Q<- rollmean(one_year_stream$Q, k=5)
    
    #append NA to front, values dont exist until "k", in this case 5
    mov_avg_Si<-c(NA, NA, NA, NA, mov_avg_Si)
    
    mov_avg_Q<-c(NA, NA, NA, NA, mov_avg_Q)
    
    #pull out "month" days from seq list (every 5 days during 61 day period)
    months_avg_Si<-mov_avg_Si[seq_list]
    
    months_avg_Q<-mov_avg_Q[seq_list]
    
    #bind to dataframe
    months_df_Si<-cbind(months_df_Si, months_avg_Si)
    
    months_df_Q<-cbind(months_df_Q, months_avg_Q)
    
  }
  
  #make list of dfs
  streams_df_Si[[i]]<-months_df_Si
  
  streams_df_Q[[i]]<-months_df_Q
  
}

#open list
MCM_stream_list_Si<-list()

#this loop takes the list of dfs and turns into a melted df similar format to the original monthly df
for (i in 1:length(stream_list)) {
  
  #get stream df
  onestream_df<-streams_df_Si[[i]]
  
  colnames(onestream_df)[1]<-"Period"
  
  onestream_df_melt<-melt(onestream_df, id.vars="Period")
  
  #rename columns
  names(onestream_df)<-c("Period", WY_length[[i]])
  
  #melt
  stream_melt<-melt(onestream_df, id.vars ="Period", value.name="ConcDay")
  
  #add column for stream name
  stream_melt$stream<-stream_list[i]
  
  #add to new list to combine
  MCM_stream_list_Si[[i]]<-stream_melt
  
}

#combine into long, melted df
MCM_allstreams_Si<-bind_rows(MCM_stream_list_Si)

#open list
MCM_stream_list_Q<-list()

#this loop takes the list of dfs and turns into a melted df similar format to the original monthly df
for (i in 1:length(stream_list)) {
  
  #get stream df
  onestream_df<-streams_df_Q[[i]]
  
  colnames(onestream_df)[1]<-"Period"
  
  onestream_df_melt<-melt(onestream_df, id.vars="Period")
  
  #rename columns
  names(onestream_df)<-c("Period", WY_length[[i]])
  
  #melt
  stream_melt<-melt(onestream_df, id.vars ="Period", value.name="Q")
  
  #add column for stream name
  stream_melt$stream<-stream_list[i]
  
  #add to new list to combine
  MCM_stream_list_Q[[i]]<-stream_melt
  
}


#combine into long, melted df
MCM_allstreams_Q<-bind_rows(MCM_stream_list_Q)

MCM_allstreams<-merge(MCM_allstreams_Si, MCM_allstreams_Q, by=c("Period","variable","stream"))


ID=seq(1,12,1)

#check
ggplot(MCM_allstreams, aes(Period, ConcDay))+
  geom_line(aes(col=variable))+facet_wrap(~stream, scales = "free")+theme_classic()+
  scale_x_continuous(breaks = ID)+theme(legend.position = "null")+
  labs(y="Normalized Si Concentration")

write.csv(MCM_allstreams, "MCM_Interpolation.csv")


#### SADDLE STREAM 007 ####

daily_results_SS<-subset(daily_results, daily_results$stream=="SADDLE STREAM 007")
daily_results_SS<-subset(daily_results_SS, daily_results_SS$Month %in% c(5,6,7))

#make list of streams in MCM LTER
stream_list<-unique(daily_results_SS$stream)

#all streams have 61 days of data - to get 12 equal periods we want to pull every 5 days from the 61 days
seq_list<-round(seq(7, 92, 7.5),0)

#open lists to append new dfs to
streams_df<-list()
WY_length<-list()

#the outer loop loops through each stream in MCM
for (i in 1:length(stream_list)) {
  
  #pull out one stream
  one_stream<-subset(daily_results_SS, daily_results_SS$stream==stream_list[i])
  
  #remove all NA
  one_stream<-one_stream[complete.cases(one_stream$ConcDay),]
  
  #pull out water years
  WY_list<-unique(one_stream$waterYear)
  
  #append to list - this will be used for column names later
  WY_length[[i]]<-WY_list
  
  #create dataframe of "months" to append new data to
  months_df<-as.data.frame(seq(1,12,1))
  
  #loop through each water year to get rolling average for each "month"
  for (k in 1:length(WY_list)) {
    
    #pull out one year of data
    one_year_stream <-subset(one_stream, one_stream$waterYear==WY_list[k])
    
    #rolling average
    mov_avg<- rollmean(one_year_stream$ConcDay, k=5)
    
    #append NA to front, values dont exist until "k", in this case 5
    mov_avg<-c(NA, NA, NA, NA, mov_avg)
    
    #pull out "month" days from seq list (every 5 days during 61 day period)
    months_avg<-mov_avg[seq_list]
    
    #bind to dataframe
    months_df<-cbind(months_df, months_avg)
    
  }
  
  #make list of dfs
  streams_df[[i]]<-months_df
  
}

#open list
SS_stream_list<-list()

#this loop takes the list of dfs and turns into a melted df similar format to the original monthly df
for (i in 1:length(stream_list)) {
  
  #get stream df
  onestream_df<-streams_df[[i]]
  
  #rename columns
  names(onestream_df)<-c("Period", WY_length[[i]])
  
  #melt
  stream_melt<-melt(onestream_df, id.vars ="Period", value.name="ConcDay")
  
  #add column for stream name
  stream_melt$stream<-stream_list[i]
  
  #add to new list to combine
  SS_stream_list[[i]]<-stream_melt
  
}

#combine into long, melted df
SS_allstreams<-bind_rows(SS_stream_list)

#check
ggplot(SS_allstreams, aes(Period, ConcDay))+geom_line(aes(col=variable))+facet_wrap(~stream, scales = "free")+theme_classic()


#### MARTINELLI ####

daily_results_MART<-subset(daily_results, daily_results$stream=="MARTINELLI")
daily_results_MART<-subset(daily_results_MART, daily_results_MART$Month %in% c(5,6,7,8,9))

#make list of streams in MCM LTER
stream_list<-unique(daily_results_MART$stream)

#all streams have 61 days of data - to get 12 equal periods we want to pull every 5 days from the 61 days
seq_list<-round(seq(12, 153, 12.75),0)

#open lists to append new dfs to
streams_df<-list()
WY_length<-list()

#the outer loop loops through each stream in MCM
for (i in 1:length(stream_list)) {
  
  #pull out one stream
  one_stream<-subset(daily_results_MART, daily_results_MART$stream==stream_list[i])
  
  #remove all NA
  one_stream<-one_stream[complete.cases(one_stream$ConcDay),]
  
  #pull out water years
  WY_list<-unique(one_stream$waterYear)
  
  #append to list - this will be used for column names later
  WY_length[[i]]<-WY_list
  
  #create dataframe of "months" to append new data to
  months_df<-as.data.frame(seq(1,12,1))
  
  #loop through each water year to get rolling average for each "month"
  for (k in 1:length(WY_list)) {
    
    #pull out one year of data
    one_year_stream <-subset(one_stream, one_stream$waterYear==WY_list[k])
    
    #rolling average
    mov_avg<- rollmean(one_year_stream$ConcDay, k=5)
    
    #append NA to front, values dont exist until "k", in this case 5
    mov_avg<-c(NA, NA, NA, NA, mov_avg)
    
    #pull out "month" days from seq list (every 5 days during 61 day period)
    months_avg<-mov_avg[seq_list]
    
    #bind to dataframe
    months_df<-cbind(months_df, months_avg)
    
  }
  
  #make list of dfs
  streams_df[[i]]<-months_df
  
}

#open list
MART_stream_list<-list()

#this loop takes the list of dfs and turns into a melted df similar format to the original monthly df
for (i in 1:length(stream_list)) {
  
  #get stream df
  onestream_df<-streams_df[[i]]
  
  #rename columns
  names(onestream_df)<-c("Period", WY_length[[i]])
  
  #melt
  stream_melt<-melt(onestream_df, id.vars ="Period", value.name="ConcDay")
  
  #add column for stream name
  stream_melt$stream<-stream_list[i]
  
  #add to new list to combine
  MART_stream_list[[i]]<-stream_melt
  
}

#combine into long, melted df
MART_allstreams<-bind_rows(MART_stream_list)

#check
ggplot(MART_allstreams, aes(Period, ConcDay))+geom_line(aes(col=variable))+facet_wrap(~stream, scales = "free")+theme_classic()


all_PA_streams<-bind_rows(MART_allstreams, MCM_allstreams, SS_allstreams)

ggplot(all_PA_streams, aes(Period, ConcDay))+geom_line(aes(col=variable))+facet_wrap(~stream, scales = "free")+theme_classic()

colnames(all_PA_streams)<-c("Month", "waterYear", "Conc", "stream")

new_monthly_results<-bind_rows(monthly_results_YR, all_PA_streams)

write.csv(new_monthly_results, "Monthly_Results_PA_Interp.csv")


