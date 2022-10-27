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

# Download the reference table object
ref_table <- read.csv(file = file.path(path, "WRTDS Source Files", 
                                       "WRTDS_Reference_Table_with_Areas_DO_NOT_EDIT.csv")) %>%
  # Pare down to only needed columns
  dplyr::select(LTER, stream = Stream_Name, drainSqKm)

# Check it out
dplyr::glimpse(ref_table)

# Define the GoogleDrive URL to upload flat results files
dest_url <- googledrive::as_id("https://drive.google.com/drive/folders/1842KSgp48k_DwvNeYbmz-_b4PSH-vrxg")

# Check current contents of this folder
googledrive::drive_ls(path = dest_url)

## ---------------------------------------------- ##
          # Identify WRTDS Outputs ----
## ---------------------------------------------- ##

# List all files in "WRTDS Outputs"
wrtds_outs_v0 <- dir(path = file.path(path, "WRTDS Outputs"))

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
  message("Processing ", type, " outputs")
  
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
  
  # Add this dataframe to the output list
  out_list[[type]] <- type_df
  
  # Completion message
  message("Completed processing ", type, " outputs")
}

# Check the structure of the whole output list
str(out_list)
names(out_list)

# Clear environment of everything but the filepath, destination URL, out_list, & ref_table
rm(list = setdiff(ls(), c("path", "dest_url", "out_list", "ref_table")))

## ---------------------------------------------- ##
          # Necessary Post Processing ----
## ---------------------------------------------- ##

# Handle trends table
trends_table <- out_list[["TrendsTable_GFN_WRTDS.csv"]] %>%
  # Handle different trend table formatting
  dplyr::mutate(
    change_mg_L = dplyr::coalesce(change_mg_L, change.mg.L.),
    slope_mg_L_yr = dplyr::coalesce(slope_mg_L_yr, slope.mg.L.yr.),
    change_percent = dplyr::coalesce(change_percent, change.percent.),
    slope_percent_yr = dplyr::coalesce(change_percent, slope..percent.yr.),
    change_10_3kg_yr = dplyr::coalesce(change_10_3kg_yr, change..10.3kg.yr.),
    slope_10_3kg_yr_yr = dplyr::coalesce(slope_10_3kg_yr_yr, slope..10.3kg.yr.yr.)) %>%
  # Drop unneeded columns
  dplyr::select(-change.mg.L., -slope.mg.L.yr., -change.percent.,
                -slope..percent.yr., -change..10.3kg.yr., -slope..10.3kg.yr.yr.)

# Glimpse this
dplyr::glimpse(trends_table)

# GFN output
gfn <- out_list[["GFN_WRTDS.csv"]] %>%
  # Attach basin area
  dplyr::left_join(y = ref_table, by = c("LTER", "stream")) %>%
  # Calculate some additional columns
  dplyr::mutate(Yield = FluxDay / drainSqKm,
                FNYield = FNFlux / drainSqKm)

# Glimpse
dplyr::glimpse(gfn)

# Error statistics
error_stats <- out_list[["ErrorStats_WRTDS.csv"]]

# Glimpse it
dplyr::glimpse(error_stats)

# Monthly information
monthly <- out_list[["Monthly_GFN_WRTDS.csv"]] %>%
  # Attach basin area
  dplyr::left_join(y = ref_table, by = c("LTER", "stream")) %>%
  # Calculate some additional columns
  dplyr::mutate(Yield = Flux / drainSqKm,
                FNYield = FNFlux / drainSqKm)

# Check it out
dplyr::glimpse(monthly)

# Results table
results_table <- out_list[["ResultsTable_GFN_WRTDS.csv"]] %>%
  # Rename some columns
  dplyr::rename(Discharge_cms = Discharge..cms.,
                Conc_mgL = Conc..mg.L.,
                FNConc_mgL = FN.Conc..mg.L.,
                Flux_10_6kg_yr = Flux..10.6kg.yr.,
                FNFlux_10_6kg_yr = FN.Flux..10.6kg.yr.) %>%
  # Attach basin area
  dplyr::left_join(y = ref_table, by = c("LTER", "stream")) %>%
  # Calculate some additional columns
  dplyr::mutate(Yield = Flux_10_6kg_yr / drainSqKm,
                FNYield = FNFlux_10_6kg_yr / drainSqKm)

# Glimpse this as well
dplyr::glimpse(results_table)

## ---------------------------------------------- ##
                # Export Products ----
## ---------------------------------------------- ##

# Combine files into a list
export_list <- list("TrendsTable_GFN_WRTDS.csv" = trends_table,
                    "GFN_WRTDS.csv" = gfn,
                    "Monthly_GFN_WRTDS.csv" = monthly,
                    "ResultsTable_GFN_WRTDS.csv" = results_table,
                    "ErrorStats_WRTDS.csv" = error_stats)

# Loop across the list to export locally and to GoogleDrive
## Note that the "GFN_WRTDS.csv" file is *huge* so it takes a few seconds to upload
for(name in names(export_list)){
  
  # Rip out that dataframe
  datum <- export_list[[name]]
  
  # Define name for this file
  report_file <- file.path(path, "WRTDS Results", paste0("Full_Results_", name))
  
  # Write this CSV out
  write.csv(x = datum, na = "", row.names = F, file = report_file)
  
  # Upload that object to GoogleDrive
  googledrive::drive_upload(media = report_file, overwrite = T, path = dest_url)
  
}

# End ----
