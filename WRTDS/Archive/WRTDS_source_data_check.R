## ---------------------------------------------- ##
          # WRTDS Centralized Workflow
## ---------------------------------------------- ##
# WRTDS = Weighted Regressions on Time, Discharge, and Season

# NOTE: This script assumes you have run at least the "prep" component of either "WRTDS_workflow.R" or "WRTDS_server_workflow"!!!
## It needs the wrangled input files and this was simpler than redoing that wrangling here

## ---------------------------------------------- ##
                  # Housekeeping ----
## ---------------------------------------------- ##
# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Clear environment
rm(list = ls())

# Create a folder for WRTDS source files
dir.create(path = "WRTDS Source Files", showWarnings = F)

# Identify Google links to relevant folders
disc_folder <- "https://drive.google.com/drive/folders/1HQtpWYoq_YQwj_bDNNbv8D-0swi00o_s"
chem_folder <- "https://drive.google.com/drive/folders/1BAs0y1hHArW8BANUFJrXOXcoMHIk25Pp"
info_folder <- "https://drive.google.com/drive/u/1/folders/1q92ee9nKct_nCJ3NVD2-tm8KCuRBfm2U"

# Identify their IDs
disc_files <- googledrive::drive_ls(path = as_id(disc_folder), type = "csv")
chem_files <- googledrive::drive_ls(path = as_id(chem_folder), type = "csv", pattern = "master")
info_files <- googledrive::drive_ls(path = as_id(info_folder), type = "csv", pattern = "INFO_all_PO4")

# Combine all of the wanted files in a single object
needed_files <- rbind(disc_files, chem_files, info_files)

# Download these files
for(file in needed_files$name){
  # Find file ID
  file_id <- needed_files$id[needed_files$name == file]
  
  # Download file
  googledrive::drive_download(file = as_id(file_id), 
                              path = file.path("WRTDS Source Files", file),
                              overwrite = T)
}

# Read in each of these CSV files
disc <- read.csv(file = file.path("WRTDS Source Files", disc_files[2,1]))
disc_log <- read.csv(file = file.path("WRTDS Source Files", disc_files[1,1]))
info <- read.csv(file = file.path("WRTDS Source Files", info_files[1,1]))
chem <- read.csv(file = file.path("WRTDS Source Files", chem_files[2,1]))

# Clean up the environment before continuing
rm(list = setdiff(ls(), c("disc", "disc_log", "chem", "info")))
## Above line removes anything *other* than objects specified

## ---------------------------------------------- ##
                  # Wrangling ----
## ---------------------------------------------- ##

# Create a reference table
ref_table <- disc_log %>%
  dplyr::rename(Discharge_Stream = DischargeFileName) %>%
  dplyr::select(Discharge_Stream, Stream) %>%
  unique()

head(ref_table)

# Make sure all data include the right columns with matching names
disc_actual <- disc %>%
  dplyr::rename(Discharge_Stream = DischargeFileName) %>%
  dplyr::select(Discharge_Stream) %>%
  # Integrate reference table!
  dplyr::left_join(y = ref_table, by = "Discharge_Stream") %>%
  unique()

head(disc_actual)

# Now chemistry
chem_actual <- chem %>%
  dplyr::rename(Stream = Site.Stream.Name) %>%
  dplyr::select(Stream) %>%
  # Join this by just "Stream"
  dplyr::left_join(y = ref_table, by = "Stream") %>%
  unique()

head(chem_actual)

# Finally, information
info_actual <- info %>%
  dplyr::rename(Stream = station.nm) %>%
  dplyr::select(Stream) %>%
  # Join this also by just "Stream"
  dplyr::left_join(y = ref_table, by = "Stream") %>%
  unique()

head(info_actual)

## ---------------------------------------------- ##
# Integrating ----
## ---------------------------------------------- ##

# Make a huge dataframe by combining these
check_df <- disc_actual %>%
  dplyr::bind_rows(chem_actual) %>%
  dplyr::bind_rows(info_actual) %>%
  # Return only unique values
  unique() %>%
  # Now make columns for each of the data types to determine what has data
  dplyr::mutate(
    discharge = dplyr::case_when(
      is.na(Discharge_Stream) & !is.na(Stream) ~ "missing",
      Discharge_Stream %in% disc_actual$Discharge_Stream ~ "has discharge",
      TRUE ~ "missing"),
    chemistry = dplyr::case_when(
      Discharge_Stream %in% chem_actual$Discharge_Stream ~ "has chemistry",
      Stream %in% chem_actual$Stream ~ "has chemistry",
      TRUE ~ "missing"),
    information = dplyr::case_when(
      Discharge_Stream %in% info_actual$Discharge_Stream ~ "has chemistry",
      Stream %in% info_actual$Stream ~ "has chemistry",
      TRUE ~ "missing")) %>%
  # Now keep only streams that are missing at least one data type
  dplyr::filter(discharge == "missing" |
                  chemistry == "missing" |
                  information == "missing") %>%
  # And (possibly redundant) return only unique values of this as well
  unique()

# Check that out
head(check_df)

# Export!
write.csv(check_df, file.path("WRTDS_data-presence-check.csv"), row.names = F, na = "")

# End ----
