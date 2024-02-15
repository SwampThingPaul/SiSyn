## -------------------------------------------------------- ##
# QA/QC of USGS geogenic solute data
## -------------------------------------------------------- ##
# Script Authors: Lienne Sethna

# PURPOSE:
## Check data for outliers and perform QA/QC checks

# Load libraries needed for script
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, purrr, readxl, supportR)

# Clear environment
rm(list = ls())

## ----------------------------------- ##
# Housekeeping ----
## ----------------------------------- ##

# Identify URLs of Drive folders with needed data
masterdata_url <- "https://drive.google.com/drive/u/1/folders/1hbkUsTdo4WAEUnlPReOUuXdeeXm92mg-"

# Identify needed data in the Drive
wanted_files <- googledrive::drive_ls(path = masterdata_url) %>%
  # Filter to only needed files
  dplyr::filter(name %in% c("USGS_geogenic_solutes.csv"))

# Check those files
wanted_files

#check working directory, set to GitHub folder
getwd()
setwd("C:/Users/lsethna/Documents/GitHub/SiSyn/DataCleaning")

# Create a folder to download data into
dir.create(path = file.path("raw_data"), showWarnings = F)

# Download that data
purrr::walk2(.x = wanted_files$name, .y = wanted_files$id,
             .f = ~ googledrive::drive_download(file = .y, overwrite = T,
                                                path = file.path("raw_data", .x)))

## ----------------------------------- ##
# Read in Data ----
## ----------------------------------- ##

# Read in reference table
USGS_dat <-read.csv("raw_data/USGS_geogenic_solutes.csv") %>%
  select(!X) #remove "header" row

# Check the structure of that
dplyr::glimpse(USGS_dat)
length(unique(USGS_dat$Stream_Name)) #number of unique sites
length(unique(USGS_dat$solutes)) #number of solutes

#fix date format
USGS_dat$date = as.Date(USGS_dat$date)

## ----------------------------------- ##
## Check number of solutes per site --
## ----------------------------------- ##

site_summary <- USGS_dat %>%
  group_by(Stream_Name, solutes) %>%
  summarize(n=n())

## ----------------------------------- ##
## Plot solutes to check for outliers  ## 
## ----------------------------------- ##

#create PDF with page for each site
sites <- unique(USGS_dat$Stream_Name)

#pdf size 7" x 6"
pdf('USGS_geogenic_plots.pdf',width=7,height=6)
#loop to create individual pre/post plots
for (i in 1:length(sites)) {
  q <- subset(USGS_dat,USGS_dat$Stream_Name==sites[i])
  
  p <- ggplot(q, aes(x=date,y=value))+
    geom_point()+
    labs(title=sites[i])+
    theme_classic()+
    facet_wrap(~solutes,scales="free_y")
    
  print(p)
}
dev.off()

## ---------------------------------------------- ##
## Look at average values across dataset
## ---------------------------------------------- ##

## ---------------------------------------------- ##
## MDLs?
## ---------------------------------------------- ##

MDL <- USGS_dat %>% filter(remark=="<") %>%
  select(Stream_Name,solutes,value) %>%
  distinct() %>%
  group_by(Stream_Name,solutes) %>%
  summarize(MDL = min(value))

## ---------------------------------------------- ##
## 4x the standard deviation of the mean = outlier
## ---------------------------------------------- ##

solutes <- unique(USGS_dat$solutes)

for (i in 1:length(sites)){
  site_dat <- subset(USGS_dat, USGS_dat$Stream_Name==sites[i])
  for (j in 1: length(solutes)){
    solute_dat <- subset(site_dat,site_dat$solutes==solutes[j])
    
    lwr_bound <- quantile(solute_dat$value,0.005)
    upr_bound <- quantile(solute_dat$value,0.995)
    
    outliers <- solute_dat[which(solute_dat$value>upr_bound|solute_dat$value<lwr_bound),]
    
    solute_dat_wo_outliers <- solute_dat[-which(solute_dat$value>upr_bound|solute_dat$value<lwr_bound),]
  }
  
}

