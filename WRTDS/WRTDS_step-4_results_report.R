## ---------------------------------------------- ##
           # WRTDS Centralized Workflow
## ---------------------------------------------- ##
# WRTDS = Weighted Regressions on Time, Discharge, and Season
## Nick J Lyon

## ---------------------------------------------- ##
                  # Housekeeping ----
## ---------------------------------------------- ##
# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, NCEAS/scicomptools)

# Clear environment
rm(list = ls())

# If working on server, need to specify correct path
(path <- scicomptools::wd_loc(local = FALSE, remote_path = file.path('/', "home", "shares", "lter-si", "WRTDS")))

# Create a new folder for saving temporary results
dir.create(path = file.path(path, "WRTDS Results"), showWarnings = F)

# Define the GoogleDrive URL to upload to as an object
dest_url <- googledrive::as_id("https://drive.google.com/drive/folders/1842KSgp48k_DwvNeYbmz-_b4PSH-vrxg")

# Check the contents of the folder we'll export the results reports to
googledrive::drive_ls(path = dest_url)
## This is only necessary to ensure that you've authorized the `googledrive` package
## Better to find out now than blow up in the loop

# List all files in "WRTDS Outputs"
wrtds_outs_v0 <- dir(path = file.path(path, "WRTDS Outputs"))

## ---------------------------------------------- ##
          # Identify WRTDS Outputs ----
## ---------------------------------------------- ##

# Do some useful processing of that object
wrtds_outs <- data.frame("file_name" = wrtds_outs_v0) %>%
  # Split LTER off the file name
  tidyr::separate(col = file_name, into = c("LTER", "other_content"),
                  sep = "__", remove = FALSE, fill = "right", extra = "merge") %>%
  # Separate the remaining content further
  tidyr::separate(col = other_content, into = c("stream", "chemical", "data_type"),
                  sep = "_", remove = TRUE, fill = "right", extra = "merge") %>%
  # Recreate the "Stream_Element_ID" column
  dplyr::mutate(Stream_Element_ID = paste0(LTER, "__", stream, "_", chemical)) %>%
  # Remove the PDFs of exploratory graphs
  dplyr::filter(data_type != "WRTDS_GFN_output.pdf")

# Glimpse it
dplyr::glimpse(wrtds_outs)

# Create an empty list
out_list <- list()

# Define the types of output file suffixes that are allowed
(out_types <- unique(wrtds_outs$data_type))

# For each data type...
for(type in out_types){

  # Return processing message
  message("Processing ", type, " outputs.")
  
  # Identify all files of that type
  file_set <- wrtds_outs %>%
    dplyr::filter(data_type == type) %>%
    dplyr::pull(var = file_name)
  
  # Make a counter set to 1
  k <- 1
  
  # Make an empty list
  sub_list <- list()
  
  # Read them all in!
  for(file in file_set){
   
    # Read in CSV and add it to the list
    datum <- read.csv(file = file.path(path, "WRTDS Outputs", file))
    
    # Add it to the list
    sub_list[[paste0(type, "_", k)]] <- datum %>%
      # Add a column for the name of the file
      dplyr::mutate(file_name = file, .before = dplyr::everything())
    
    # Advance counter
    k <- k + 1
  }
  
  # Once all files of that type are retrieved, unlist the sub_list!
  type_df <- sub_list %>%
    # Actual unlisting of the list
    purrr::map_dfr(.f = dplyr::select, dplyr::everything()) %>%
    # Bring in other desired columns
    dplyr::left_join(y = wrtds_outs, by = "file_name") %>%
    # Drop the redundant data_type column
    dplyr::select(-data_type) %>%
    # Relocate other joined columns to front
    dplyr::relocate(Stream_Element_ID, LTER, stream, chemical,
                    .after = file_name)
  
  # Define name for this file
  report_file <- file.path(path, "WRTDS Results", paste0("Full_Results_", type))
  
  # Write this CSV out
  write.csv(x = type_df, na = "", row.names = F, file = report_file)
  
  # Upload that object to GoogleDrive
  googledrive::drive_upload(media = report_file, overwrite = T, path = dest_url)
  
  # Completion message
  message("Completed processing ", type, " outputs.")
}

# Check the structure of the last dataframe
dplyr::glimpse(type_df)


# Identify all files in "WRTDS Outputs" folder
# Read in all of the CSVs of each type
# Unlist them into a dataframe
# Export that dataframe to GoogleDrive here:
#### https://drive.google.com/drive/folders/1842KSgp48k_DwvNeYbmz-_b4PSH-vrxg




### Generates summary data files for WRTDS model run
# For trend, annual, monthly, and daily results this code:
# Reads in csv files for each site, 
# merges all sites into single file and adds site name as a column
# appends LTER name by site
# reads in file with drainage area to calculate yield for each site
# reads out a merged csv for trends, Annual results, monthly results, and daily results

## Load libraries 
library(lubridate)
library(tidyverse)
library(data.table)
library(viridis)
library(reshape2)
library(cowplot)

# load and modify data files for analysis and plotting
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDSResults_N_Updated")

filedir <- setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDSResults_N_Updated")

### Loading data files 
file_names <- dir(filedir)

##########################################################################
### EGRETci Trends
## list of trend result files
trend_files <- file_names[file_names %like% "EGRETCi_GFN_Trend.csv"]
trends <- do.call(rbind, lapply(trend_files, function(x) cbind(read.csv(x), name=strsplit(x,'NOX_NOX_EGRETCi_GFN_Trend.csv')[[1]][1])))
trends=trends[,-1]
colnames(trends) = c(colnames(trends[,1:28]),"site")

## REMOVE HBR site WS1 - added calcium silicate!
trends=filter(trends, site != "ws1")

# calculate yields
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Data/BasinInfo")
site.info=read.csv("SiteCharacteristics_PCA_WRTDS_041222.csv")
colnames(site.info)=c("LTER","site",colnames(site.info[,3:ncol(site.info)]))
trends.join=left_join(trends, site.info,by="site")

trends.yield = trends.join %>% 
  select(site,LTER,Biome,pValC,estC,lowC95,upC95,likeCUp,likeCDown,
         pValF,estF,lowF,upF,lowF95,upF95,likeFUp,likeFDown,
         baseConc,baseFlux,drainSqKm) %>% 
  mutate(FNYield_change_106_kg_y_km2=estF/drainSqKm, 
         FNYield_change_kg_y_km2=(estF/drainSqKm)*(10^6),
         baseYield=baseFlux/drainSqKm,
         #pConc = (estC/baseConc)*100,
         #pFlux = (estF/baseFlux)*100,
         pYield = (FNYield_change_106_kg_y_km2/baseYield)*100)

trends.yield %>% 
  #filter(LTER != "LUQ") %>% 
  ggplot(aes(x=LTER, pYield, fill=site))+
  geom_bar(stat="identity", position="dodge")+
  #geom_errorbar(aes(ymin=percent-(SD/sqrt(N)), ymax=percent+(SD/sqrt(N))), width=.1)+
  theme_classic()+
  theme(legend.position = "none", axis.text.x = element_text(angle=0, size=10))+
  xlab("")+ylab("")+
  scale_fill_grey()+
  scale_x_discrete(labels = scales::wrap_format(10))

## read out combined results
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/Trend Summaries")
write.csv(trends.yield, "WRTDS_EGRETCi_Trends_AllSites_050422.csv", row.names=FALSE)

###########################################################################
## Annual Result Data
#setwd("C:/Users/kjankowski/Desktop/WRTDSResults_Si")
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDS_Si_results_GFN")

#filedir <- setwd("C:/Users/kjankowski/Desktop/WRTDSResults_Si")
filedir <- setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDS_Si_results_GFN")

### Loading data files 
file_names <- dir(filedir)

# list of annual result files
result_files <- file_names[file_names %like% "ResultsTable_GFN_WRTDS.csv"]

# combine all into a dataframe and column with site name
df <- do.call(rbind, lapply(result_files, function(x) cbind(read.csv(x), name=strsplit(x,'_ResultsTable_GFN_WRTDS.csv')[[1]][1])))
df=df[,-1]
colnames(df) = c("Year", "Discharge_cms", "Conc_mgL", "FNConc_mgL", "Flux_106_kg_y", "FNFlux_106_kg_y", "site")

## REMOVE HBR site WS1 
df=filter(df, site != "ws1")

df$LTER = ifelse(df$site %in% c("ALBION","MARTINELLI","SADDLE STREAM 007"),"NWT",
                 ifelse(df$site %in% c("QS","Q1", "Q2","Q3","RI","MPR"), "LUQ", 
                        ifelse(df$site %in% c("ws2","ws3","ws4","ws5","ws6","ws7","ws8","ws9"), "HBR",
                               ifelse(df$site %in% c("S65","S65A","S65D","S65E"),"KRR",
                                      ifelse(df$site %in% c("GSMACK","GSWS02","GSWS06","GSWS07","GSWS08","GSWS09","GSWS10"),"AND",
                                             ifelse(df$site %in% c("LMP73"),"LMP",
                                                    ifelse(df$site %in% c("Kolyma","Lena","Mackenzie","Ob","Yenisey","Yukon"),"GRO",
                                                           ifelse(df$site %in% c("Andersen Creek at H1","Canada Stream at F1",
                                                                                 "Green Creek at F9", "Lawson Creek at B3",
                                                                                 "Onyx River at Lake Vanda Weir", "Onyx River at Lower Wright Weir",
                                                                                 "Priscu Stream at B1","Von Guerard Stream at F6"), "MCM",
                                                                  ifelse(df$site %in% c("Sagehen"),"Sagehen",
                                                                         ifelse(df$site %in% c("BK01.0M","CH00.1M","CN00.1M","CU11.6M",
                                                                                               "I080.2M","LM00.5M","M078.0B","M241.4K",
                                                                                               "M556.4A","M701.1B","M764.3A","M786.2C",
                                                                                               "MQ02.1M", "SG16.2C","WP02.6M"),"UMR",
                                                                                ifelse(df$site %in% c("Imnavait Weir"),"ARC","")))))))))))



## plot results for review
df %>% 
  ggplot(aes(Year,Conc_mgL,col=site))+
  geom_point()+
  geom_line(aes(Year,FNConc_mgL, col=site))+
  facet_wrap(~LTER,scales="free")+
  theme(legend.position="none")

df %>% 
  ggplot(aes(Year,Flux_106_kg_y,col=site))+
  geom_point()+
  geom_line(aes(Year,FNFlux_106_kg_y, col=site))+
  facet_wrap(~LTER,scales="free")+
  theme(legend.position="none")

# calculate yields
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Data/BasinInfo")
site.info=read.csv("SiteCharacteristics_PCA_WRTDS_041222.csv")
colnames(site.info)=c("LTER","site",colnames(site.info[,3:ncol(site.info)]))
df.join=left_join(df, site.info,by=c("LTER","site"))

df.yield = df.join %>% 
  select(LTER,site,Year,Discharge_cms,Conc_mgL,FNConc_mgL,Flux_106_kg_y,
         FNFlux_106_kg_y, drainSqKm) %>% 
  mutate(Yield=Flux_106_kg_y/drainSqKm, 
         FNYield = FNFlux_106_kg_y/drainSqKm)

df.yield %>% 
  ggplot(aes(Year,Yield,col=site))+
  geom_point()+
  geom_line(aes(Year,FNYield, col=site))+
  facet_wrap(~LTER,scales="free")+
  theme(legend.position="none")

median.values = aggregate((Yield*(10^6))~site, FUN=median, na.rm=FALSE, data=df.yield)
median.values$Area_km2 = aggregate(drainSqKm~site, FUN=median, na.rm=TRUE, data=df.yield)[,2]
median.values$Q = aggregate(Discharge_cms~site, FUN=median, na.rm=TRUE, data=df.yield)[,2]
median.values$Conc = aggregate(Conc_mgL~site, FUN=median, na.rm=TRUE, data=df.yield)[,2]
colnames(median.values)=c("Site", "Yield_kg_yr_km2", "Area_km2", "Q_cms", "Conc_mgL")

setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/Trend Summaries")
write.csv(median.values, "Summary_conc_yield_Q.csv", row.names=FALSE)

NWT = df.yield %>% 
  filter(LTER == "HBR") %>% 
  ggplot(aes(Year,FNYield,col=site))+
  geom_line()+
  #geom_line(aes(Year,Yield, col=site))+
  #facet_wrap(~site,scales="free")+
  theme_classic()+
  theme(legend.position="none")

NWT+scale_color_viridis_d(begin=0.2,end=0.4)


## read out combined results
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS")
write.csv(df.yield, "WRTDS_GFN_AnnualResults_AllSites_050422.csv", row.names=FALSE)

#################################3
### Monthly Results

# list of monthly result files
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDS_Si_results_GFN")

#filedir <- setwd("C:/Users/kjankowski/Desktop/WRTDSResults_Si")
filedir <- setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDS_Si_results_GFN")

### Loading data files 
file_names <- dir(filedir)

## select monthly data
month_files <- file_names[file_names %like% "Monthly_GFN_WRTDS.csv"]

# combine all into a dataframe and column with site name
months <- do.call(rbind, lapply(month_files, function(x) cbind(read.csv(x), name=strsplit(x,'_Monthly_GFN_WRTDS.csv')[[1]][1])))
months=months[,-1]
colnames(months) = c("Month","Year", "DecYear", "Q", "Conc", "FNConc", "Flux", "FNFlux","site")

## REMOVE HBR site WS1 
months=filter(months, site != "ws1")

months$LTER = ifelse(months$site %in% c("ALBION","MARTINELLI","SADDLE STREAM 007"),"NWT",
                     ifelse(months$site %in% c("QS","Q1","Q2","Q3","RI","MPR"), "LUQ", 
                            ifelse(months$site %in% c("ws2","ws3","ws4","ws5","ws6","ws7","ws8","ws9"), "HBR",
                                   ifelse(months$site %in% c("S65","S65A","S65D","S65E"),"KRR",
                                          ifelse(months$site %in% c("GSMACK","GSWS02","GSWS06","GSWS07","GSWS08","GSWS09","GSWS10"),"AND",
                                                 ifelse(months$site %in% c("LMP73"),"LMP",
                                                        ifelse(months$site %in% c("Kolyma","Lena","Mackenzie","Ob","Yenisey","Yukon"),"GRO",
                                                               ifelse(months$site %in% c("Andersen Creek at H1","Canada Stream at F1",
                                                                                         "Green Creek at F9", "Lawson Creek at B3",
                                                                                         "Onyx River at Lake Vanda Weir", "Onyx River at Lower Wright Weir",
                                                                                         "Priscu Stream at B1","Von Guerard Stream at F6"), "MCM",
                                                                      ifelse(months$site %in% c("Sagehen"),"Sagehen",
                                                                             ifelse(months$site %in% c("BK01.0M","CH00.1M","CN00.1M","CU11.6M",
                                                                                                       "I080.2M","LM00.5M","M078.0B","M241.4K",
                                                                                                       "M556.4A","M701.1B","M764.3A","M786.2C",
                                                                                                       "MQ02.1M", "SG16.2C","WP02.6M"),"UMR",
                                                                                    ifelse(months$site %in% c("Imnavait Weir"),"ARC","")))))))))))



## plot results for review
colourCount = length(unique(months$Month))
getPalette = colorRampPalette(brewer.pal(11, "Spectral"))

# trend by month across years
months %>% 
  filter(site == "BK01.0M") %>% 
  ggplot(aes(Year,FNConc,col=as.factor(Month)))+
  geom_line()+
  #geom_line(aes(Year,FNConc, col=as.factor(Month)))+
  facet_wrap(~site,scales="free")+
  theme_classic()+
  theme(legend.position="right")+
  scale_fill_manual(values = getPalette(colourCount))

months %>% 
  filter(site == "BK01.0M") %>% 
  ggplot(aes(Month,FNConc,col=as.factor(Year)))+
  geom_line()+
  #geom_line(aes(Year,FNConc, col=as.factor(Month)))+
  #facet_wrap(~site,scales="free")+
  theme_classic()+
  theme(legend.position="right")+
  scale_fill_manual(values = getPalette(colourCount))+
  labs(colour = "Year")+
  ylab("Flow-normalized Concentration (mg/L)")


months %>% 
  filter(LTER=="HBR") %>% 
  ggplot(aes(Year,FNConc,col=as.factor(Month)))+
  geom_line()+
  #geom_line(aes(Year,FNFlux_106_kg_y, col=site))+
  facet_wrap(~site,scales="free")+
  theme(legend.position="right")+
  scale_fill_manual(values = getPalette(colourCount))

## calculate monthly yields
# calculate yields
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Data/BasinInfo")
site.info=read.csv("SiteCharacteristics_PCA_WRTDS_041222.csv")
colnames(site.info)=c("LTER","site",colnames(site.info[,3:ncol(site.info)]))
month.join=left_join(months, site.info,by=c("LTER","site"))

month.yield = month.join %>% 
  select(LTER,site,Month,Year,DecYear,Q,Conc,FNConc,Flux,
         FNFlux,drainSqKm) %>% 
  mutate(Yield=Flux/drainSqKm, 
         FNYield = FNFlux/drainSqKm)

month.yield %>% 
  filter(LTER == "UMR") %>% 
  #filter(Month == 1 | Month == 12) %>% 
  ggplot(aes(Year,FNYield,col=as.factor(Month)))+
  geom_line()+
  facet_wrap(~site,scales="free")+
  theme(legend.position="right")

## read out combined results
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS")
write.csv(month.yield, "WRTDS_GFN_MonthlyResults_AllSites_050422.csv", row.names=FALSE)


############################ Continuous Data
## Continuous Data
# list of monthly result files
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDS_Si_results_GFN")

#filedir <- setwd("C:/Users/kjankowski/Desktop/WRTDSResults_Si")
filedir <- setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS/WRTDS_Si_results_GFN")

### Loading data files 
file_names <- dir(filedir)
# make list of files
cont_files <-file_names[file_names %like% "ContSi_GFN_WRTDS.csv"]

# merge files into one
cont <- do.call(rbind, lapply(cont_files, function(x) cbind(read.csv(x), name=strsplit(x,'_ContSi_GFN_WRTDS.csv')[[1]][1])))
cont=cont[,-1]
cont$Date = as.Date(cont$Date)
cont$year = year(cont$Date)
colnames(cont)=c(colnames(cont[,1:16]),"site","year")

#cont$dates <- as.POSIXct(strptime(df.cont$Date, "%Y-%m-%d"))

cont$LTER = ifelse(cont$site %in% c("ALBION","MARTINELLI","SADDLE STREAM 007"),"NWT",
                   ifelse(cont$site %in% c("QS","Q1","Q2","Q3","RI","MPR"), "LUQ", 
                          ifelse(cont$site %in% c("ws1","ws2","ws3","ws4","ws5","ws6","ws7","ws8","ws9"), "HBR",
                                 ifelse(cont$site %in% c("S65","S65A","S65D","S65E"),"KRR",
                                        ifelse(cont$site %in% c("GSMACK","GSWS02","GSWS06","GSWS07","GSWS08","GSWS09","GSWS10"),"AND",
                                               ifelse(cont$site %in% c("LMP73"),"LMP",
                                                      ifelse(cont$site %in% c("Kolyma","Lena","Mackenzie","Ob","Yenisey","Yukon"),"GRO",
                                                             ifelse(cont$site %in% c("Andersen Creek at H1","Canada Stream at F1",
                                                                                     "Green Creek at F9", "Lawson Creek at B3",
                                                                                     "Onyx River at Lake Vanda Weir", "Onyx River at Lower Wright Weir",
                                                                                     "Priscu Stream at B1","Von Guerard Stream at F6"), "MCM",
                                                                    ifelse(cont$site %in% c("Sagehen"),"Sagehen",
                                                                           ifelse(cont$site %in% c("BK01.0M","CH00.1M","CN00.1M","CU11.6M",
                                                                                                   "I080.2M","LM00.5M","M078.0B","M241.4K",
                                                                                                   "M556.4A","M701.1B","M764.3A","M786.2C",
                                                                                                   "MQ02.1M", "SG16.2C","WP02.6M"),"UMR",
                                                                                  ifelse(cont$site %in% c("Imnavait Weir"),"ARC","")))))))))))




# test
cont %>% 
  filter(site == "WP02.6M") %>% 
  ggplot(aes(year,ConcDay))+geom_point()+
  geom_smooth(method="loess")

# calculate yields
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Data/BasinInfo")
site.info=read.csv("SiteCharacteristics_PCA_WRTDS_122021.csv")
colnames(site.info)=c("LTER","site",colnames(site.info[,3:ncol(site.info)]))

cont.join=left_join(cont, site.info,by=c("LTER","site"))

cont.yield = cont.join %>% 
  select(LTER,site,Date, Month,year, Day,DecYear, Julian,waterYear,i,Q,LogQ,Q7,
         Q30,ConcDay,FluxDay,FNConc,FNFlux,drainSqKm) %>% 
  mutate(Yield=FluxDay/drainSqKm, 
         FNYield = FNFlux/drainSqKm)

# save merged file
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS")
write.csv(cont, "WRTDS_GFN_DailyResults_AllSites_050422.csv", row.names=FALSE)

