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
# Wrangle the discharge data objects to standardize naming somewhat
disc_v2 <- disc_main %>%
  # Drop row number column
  dplyr::select(-X) %>%
  # Rename site column as it appears in the discharge log file
  dplyr::rename(Stream = site.name)

# Check that out
dplyr::glimpse(disc_v2)

# Clean up the chemistry data
chem_v2 <- chem_main %>%
  # Simplify phosphorous for later
  dplyr::mutate(
    variable_simp = ifelse(variable == "SRP" | variable == "PO4",
                           yes = "P", no = variable)) %>%
  # That done, drop all chemicals other than the core ones we're interested in
  dplyr::filter(variable_simp %in% c("P", "DSi", "NOx", "NH4")) %>%
  # Calculate the mg/L for each of these chemicals
  dplyr::mutate(value_mgL = dplyr::case_when(
    variable_simp == "P" ~ (((value / 10^6) * 30.973762) * 1000),
    variable_simp == "DSi" ~ (((value / 10^6) * 28.0855) * 1000),
    variable_simp == "NOx" ~ (((value / 10^6) * 14.0067) * 1000),
    variable_simp == "NH4" ~ ((value / 10^6) * 14.0067) * 1000 )) %>%
  # Rename date column
  dplyr::rename(Date = Sampling.Date)
  # For Andrews sites, remove pree-1982


dplyr::glimpse(chem_v2)


## ---------------------------------------------- ##
                      # Run ----
## ---------------------------------------------- ##






## ---------------------------------------------- ##
                    # Results ----
## ---------------------------------------------- ##
                    


# End ----
