### this code pulls compiled driver datasets off google drive and creates exploratory plots for monthly and annual data
require(googledrive)
require(reshape2)
require(ggplot2)

#get link to drivers folder from google drive
link <- "https://drive.google.com/file/d/185M7wOU5z9a370r0OL3M-3StS7yST7cD/view?usp=share_link"

#get file from link
file = drive_get(as_id(link))

#check working directory where files will be stored locally; separate folder within project folder
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#download each file to the working directory; files are saved locally
drive_download(file$drive_resource,  overwrite=T)

#read in data
drivers<-read.csv("all-data_si-extract.csv")

#cluster_sites<-read.csv(XXX)

#divide into categorical, numeric, and elevation categories for function below
drivers_list_cat<-c("soil", "rock", "land")

cat_names<-c("Soil Order", "Lithology", "Land Cover")

drivers_list_quant<-c("num_days", "prop_area", "precip", "evapotrans", "temp")

quant_names<-c("Snow Cover (Days)", "Snow Cover (Proportion of Basin)", "Precipitation (mm/day)",
                      "Evapotranspiration (meters)", "Temperature (C)")

elevation<-c("elevation")

npp<-c("npp")

greenup<-c("cycle0", "cycle1")

#### categorical drivers ####
drive_cols<-grep(drivers_list_cat[1], colnames(drivers))

one_driver<-drivers[,c(1:7, drive_cols)]

ggplot(one_driver, aes(x=one_driver[,8]))+geom_histogram(stat = "count", fill="black")+theme_classic()+
  xlab("")

ggplot(one_driver, aes(x=lat, fill = one_driver[,8]))+geom_histogram(bins = 100)+
  scale_fill_carto_d(palette = 1)+theme_classic()+labs(fill=cat_names[1], x="Latitude")

pdf("SnowPlots.pdf", width=10, height = 9)
dev.off()

#### continuous drivers ####
i<-2

drive_cols<-grep(drivers_list_quant[i], colnames(drivers))

one_driver<-drivers[,c(1:7, drive_cols)]

one_driver_years<-one_driver[,c(1:(ncol(one_driver)-12))]

one_driver_months<-one_driver[,c(1:7,(ncol(one_driver)-11):ncol(one_driver))]

driver_melt<-melt(one_driver_years, id.vars=colnames(one_driver_years[,c(1:7)]))

##yearly plots
driver_melt$variable<-as.numeric(driver_melt$variable)

ggplot(driver_melt, aes(x=as.integer(variable), y=as.numeric(value), group=Stream_Name))+geom_line()+
  theme_classic()+theme(legend.position = "null")+facet_wrap(~LTER, scales = "free")+
  ggtitle(paste(quant_names[i], "- Annual"))+xlab("Year")+ylab(quant_names[i])

##monthly plots
driver_melt<-melt(one_driver_months, id.vars=colnames(one_driver_months[,c(1:7)]))

ggplot(driver_melt, aes(x=as.integer(variable), y=as.numeric(value), group=Stream_Name))+geom_line()+
  theme_classic()+theme(legend.position = "null")+facet_wrap(~LTER, scales = "free")+
  ggtitle(paste(quant_names[i], "- Monthly"))+xlab("Month")+ylab(quant_names[i])

#### elevation ####
drive_cols<-grep(elevation, colnames(drivers))

one_driver<-drivers[,c(1:7, drive_cols)]

ggplot(one_driver, aes(x=elevation_median_m))+geom_histogram(fill="black", bins = 100)+theme_classic()

#### npp ####
drive_cols<-grep(npp, colnames(drivers))

one_driver<-drivers[,c(1:7, drive_cols)]

driver_melt<-melt(one_driver_years, id.vars=colnames(one_driver_years[,c(1:7)]))

driver_melt$variable<-as.numeric(driver_melt$variable)

ggplot(driver_melt, aes(x=as.integer(variable), y=as.numeric(value)/1000, group=Stream_Name))+geom_line()+
  theme_classic()+theme(legend.position = "null")+facet_wrap(~LTER, scales = "free")+
  ggtitle("NPP (kgC/m2/yr)")


#### greenup ####
drive_cols<-grep(greenup[i], colnames(drivers))

one_driver<-drivers[,c(1:7, drive_cols)]

driver_melt<-melt(one_driver, id.vars=colnames(one_driver[,c(1:7)]))

driver_melt$doy<-yday(as.Date(driver_melt$value, "%Y-%m-%d"))

ggplot(driver_melt, aes(x=as.integer(variable), y=doy, group=Stream_Name))+geom_line()+
  theme_classic()+theme(legend.position = "null")+facet_wrap(~LTER, scales = "free")+
  ggtitle("Greenup (Day of Year)")






