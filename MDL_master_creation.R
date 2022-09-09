## ---------------------------------------------- ##
         # Minimum Detection Limit Wrangling
## ---------------------------------------------- ##
# MDL = Minimum Detection Limit

## -------------------------------- ##
          # Housekeeping ----
## -------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Clear environment
rm(list = ls())

# Authenticate with GoogleDrive so we can down/upload directly
my_email <- "lyon@nceas.ucsb.edu"
googledrive::drive_auth(email = my_email)

# Create a folder for exporting/importing
dir.create(path = "MDL_Prep", showWarnings = F)

# Identify Google links to the relevant folder
mdl_folder <- "https://drive.google.com/drive/folders/1BAs0y1hHArW8BANUFJrXOXcoMHIk25Pp"

# Identify fileIDs
mdl_files <- googledrive::drive_ls(path = as_id(mdl_folder), type = "csv", pattern = "Minimum_detection_limit")

# Download these files
for(file in mdl_files$name){
  # Find file ID
  file_id <- mdl_files$id[mdl_files$name == file]
  
  # Download file
  googledrive::drive_download(file = as_id(file_id), 
                              path = file.path("MDL_Prep", file),
                              overwrite = T)
}
# Read in each of these CSV files
mdl_np <- read.csv(file = file.path("MDL_Prep", "Minimum_detection_limit_N_P.csv"))

mdl_nh4 <- read.csv(file = file.path("MDL_Prep", "Minimum_detection_limit_NH4.csv"))

## -------------------------------- ##
       # Wrangle MDL Files ----
## -------------------------------- ##

# We need the MDL files to match to combine them
dplyr::glimpse(mdl_np)
dplyr::glimpse(mdl_nh4)

# Actually, it looks like a join will work as-is!
mdl_full <- mdl_np %>%
  # "Full" join so that sites found in only either of them will all be included
  dplyr::full_join(y = mdl_nh4, by = "site") %>%
  # Rename the NH4 columns
  dplyr::rename(NH4_uM_MDL = MDL..uM.,
                NH4_mgL_MDL = MDL..mg.L.) %>%
  # Group MDL columns with their comment columns
  dplyr::select(LTER, site,
                dplyr::starts_with("P_"),
                dplyr::starts_with("NO3_"),
                dplyr::starts_with("NH4_"))

# Check that out
dplyr::glimpse(mdl_full)

## -------------------------------- ##
              # Export ----
## -------------------------------- ##

# Time stamp a name for this file
date_stamp <- gsub(pattern = "-", replacement = "", x = Sys.Date())
mdl_name <- paste0(date_stamp, "_master_min_det_limit.csv")
mdl_file <- file.path("MDL_Prep", mdl_name)

# Save this object
write.csv(mdl_full, file = mdl_file, row.names = F, na = "")

# Upload it to Google
googledrive::drive_upload(media = mdl_file,
                          path = as_id(mdl_folder),
                          name = mdl_name, type = "csv")

# End ----
