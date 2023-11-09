# installing the required libraries 
library(readxl)
library(tidyverse)
library(reshape2)

# set the working directory 
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

area<-read.csv("KrycklanArea.csv")

area$Site<-gsub(" ", "", area$Site)

# accessing all the sheets 
sheet = excel_sheets("KrycklanDailydischarge.xlsx")

# applying sheet names to dataframe names
data_frame = lapply(setNames(sheet, sheet), 
                    function(x) read_excel("KrycklanDailydischarge.xlsx", sheet=x))

# attaching all dataframes together
data_frame = bind_rows(data_frame, .id="Sheet")

data<-as.data.frame(data_frame)

data$Date<-as.Date(data$Date)

missing_days_prop<-list()

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NewSites_Sept2022")

#pdf("Krycklan_NewQ.pdf")

for (i in 1:length(sheet)) {
  
  data_site<-data[,c("Date", sheet[i])]
  
  data_site<-data_site[complete.cases(data_site[,2]),]
  
  names(data_site)[2]<-"Q"
  
  site_num<-gsub("C", "Site", sheet[i])
  
  area_site<-subset(area, area$Site==site_num)
  
  area_site$m2<-area_site$Size..km.*1E6
  
  data_site$Q<-(data_site$Q/1000)*area_site$m2
  
  data_site$Q<-data_site$Q/86400
  
  # date_range <- seq(min(data_site$Date), max(data_site$Date), by = 1)
  # num_missing_days<-length(date_range[!date_range %in% data_site$Date])
  # missing_days_prop[[i]]<-num_missing_days/length(date_range)
  # 
  # p1<-ggplot(data_site, aes(Date, data_site[,2]))+geom_line()+ggtitle(sheet[i])+
  #   theme_bw()
  # 
  # print(p1)
  
  write.csv(data_site, paste0(site_num, "_Discharge.csv"))
  
  
  
}

#dev.off()

