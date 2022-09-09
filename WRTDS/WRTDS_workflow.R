## ---------------------------------------------- ##
           # WRTDS Centralized Workflow
## ---------------------------------------------- ##
# WRTDS = Weighted Regressions on Time, Discharge, and Season

## ---------------------------------------------- ##
# Housekeeping
## ---------------------------------------------- ##
# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, lubridate, reshape, gtools)

# Clear environment
rm(list = ls())

# Authenticate with GoogleDrive so we can down/upload directly
my_email <- "lyon@nceas.ucsb.edu"
googledrive::drive_auth(email = my_email)

# Create a folder for exporting/importing
dir.create(path = "WRTDS Content", showWarnings = F)

# Identify Google links to relevant folders
disc_folder <- "https://drive.google.com/drive/folders/1HQtpWYoq_YQwj_bDNNbv8D-0swi00o_s"
chem_folder <- "https://drive.google.com/drive/folders/1BAs0y1hHArW8BANUFJrXOXcoMHIk25Pp"

# Identify their IDs
disc_files <- googledrive::drive_ls(path = as_id(disc_folder), type = "csv")
chem_files <- googledrive::drive_ls(path = as_id(chem_folder), type = "csv", pattern = "masterdata")

# Combine all of the wanted files in a single object
needed_files <- rbind(disc_files, chem_files)

# Download these files
for(file in needed_files$name){
  # Find file ID
  file_id <- needed_files$id[needed_files$name == file]
  
  # Download file
  googledrive::drive_download(file = as_id(file_id), 
                              path = file.path("WRTDS Content", file),
                              overwrite = T)
}

# Read in each of these CSV files
disc_main <- read.csv(file = file.path("WRTDS Content", disc_files[1,1]))
disc_log <- read.csv(file = file.path("WRTDS Content", disc_files[2,1]))
chem_main <- read.csv(file = file.path("WRTDS Content", chem_files[1,1]))

# Clean up the environment before continuing
rm(list = setdiff(ls(), c("disc_main", "disc_log", "chem_main")))
## Above line removes any objects *other* than those specified

## ---------------------------------------------- ##
                    # Prep ----
## ---------------------------------------------- ##





## ---------------------------------------------- ##
                      # Run ----
## ---------------------------------------------- ##






## ---------------------------------------------- ##
                    # Results ----
## ---------------------------------------------- ##
                    


# End ----
