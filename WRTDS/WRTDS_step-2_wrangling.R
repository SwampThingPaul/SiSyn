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
librarian::shelf(tidyverse, googledrive, lubridate, EGRET, EGRETci, njlyon0/helpR, NCEAS/scicomptools)

# Clear environment
rm(list = ls())

# Need to specify correct path for local versus server work
(path <- scicomptools::wd_loc(local = FALSE, remote_path = file.path('/', "home", "shares", "lter-si", "WRTDS")))

# Create folders for the raw downloaded files (i.e., sources) & WRTDS inputs (created by this script)
dir.create(path = file.path(path, "WRTDS Source Files"), showWarnings = F)
dir.create(path = file.path(path, "WRTDS Inputs"), showWarnings = F)

# Define the names of the Drive files we need
names <- c("WRTDS_Reference_Table_with_Areas_DO_NOT_EDIT.csv", # No.1 Ref table
           "UpdatedAll_Q_master_10272022.csv", # No.2 Main discharge
           "20221030_masterdata_chem.csv") # No.3 Main chemistry

# Find folders for those files
ids <- googledrive::drive_ls(as_id("https://drive.google.com/drive/u/1/folders/1HQtpWYoq_YQwj_bDNNbv8D-0swi00o_s"), pattern = ".csv") %>%
  dplyr::bind_rows(googledrive::drive_ls(as_id("https://drive.google.com/drive/u/1/folders/1BAs0y1hHArW8BANUFJrXOXcoMHIk25Pp"), pattern = ".csv"))

# Check that no file names have changed!
if(!names[1] %in% ids$name | !names[2] %in% ids$name | !names[3] %in% ids$name){
  message("At least one source file name has changed! Update the 'names' vector before proceeding") } else {
    message("All file names found in Google Drive. Please continue") }

# Download the files we want
for(k in 1:length(names)){
  
  # Processing message
  message("Downloading file '", names[k], "'")
  
  # Download files
  ids %>%
    # Filter to desired file
    dplyr::filter(name == names[k]) %>%
    # Download that file!
    googledrive::drive_download(file = as_id(.), overwrite = T,
                                path = file.path(path, "WRTDS Source Files", names[k]))
}

# Read in each of these files
ref_raw <- read.csv(file = file.path(path, "WRTDS Source Files", names[1])) 
disc_raw <- read.csv(file = file.path(path, "WRTDS Source Files", names[2]))
chem_raw <- read.csv(file = file.path(path, "WRTDS Source Files", names[3]))

# Wrangle the reference table file
ref_table <- ref_raw %>%
  # Drop some wrong sites (don't want to delete from ref table in case they are correct for other related datasets)
  dplyr::filter(!Discharge_File_Name %in% c("GRO_Kolyma_Q_fill", "GRO_Mackenzie_Q_fill"))

# Wrangle discharge
disc_main <- disc_raw %>%
  # Fix any broken names (special characters from Scandinavia)
  dplyr::mutate(DischargeFileName = gsub(pattern = "ØSTEGLO_Q", replacement = "OSTEGLO_Q",
                                           x = DischargeFileName))

# Wrangle chemistry as well
chem_main <- chem_raw %>%
  # Fix some Finnish site names that get messed up by some stage of this wrangling
  dplyr::mutate(site = gsub(pattern = "[<]e4[>]", replacement = "a", x = site)) %>%
  dplyr::mutate(site = gsub(pattern = "[<]f6[>]", replacement = "o", x = site)) %>%
  # Need to fix in both 'site name' columns
  dplyr::mutate(Site.Stream.Name = gsub(pattern = "[<]e4[>]", replacement = "a",
                                        x = Site.Stream.Name)) %>%
  dplyr::mutate(Site.Stream.Name = gsub(pattern = "[<]f6[>]", replacement = "o",
                                        x = Site.Stream.Name)) %>%
  # Drop all chemicals other than the core ones we're interested in
  dplyr::filter(variable %in% c("SRP", "PO4", "DSi", "NO3", "NOx", "NH4"))

# Clean up the environment before continuing
rm(list = setdiff(ls(), c("path", "ref_table", "disc_main", "chem_main", "mdl_info")))
## Above line removes anything *other* than objects specified

## ---------------------------------------------- ##
             # Prep Supporting Files ----
## ---------------------------------------------- ##

# Make a lookup table from the reference table for just stream names
name_lkup <- ref_table %>%
  # Pare to only some columns
  dplyr::select(LTER, Discharge_File_Name, Stream_Name)

# Glimpse it
dplyr::glimpse(name_lkup)

# Wrangle a special minimum detection limit object too
mdl_info <- ref_table %>%
  # Drop unneeded columns
  dplyr::select(Stream_Name, dplyr::starts_with("MDL_")) %>%
  # Pivot longer
  tidyr::pivot_longer(cols = dplyr::starts_with("MDL_"),
                      names_to = "variable",
                      values_to = "MDL") %>%
  # Drop any NAs that result from the pivoting
  dplyr::filter(!is.na(MDL)) %>%
  # Clean up variable name
  dplyr::mutate(variable_simp = dplyr::case_when(
    variable == "MDL_P_mgL" ~ "P",
    variable == "MDL_NOx_mgL" ~ "NOx",
    variable == "MDL_NH4_mgL" ~ "NH4",
    variable == "MDL_Si_mgL" ~ "DSi"), .after = variable) %>%
  # Drop duplicate rows
  unique() %>%
  # Drop original variable column
  dplyr::select(-variable)

# Check it
dplyr::glimpse(mdl_info)

## ---------------------------------------------- ##
              # Initial Wrangling ----
## ---------------------------------------------- ##
# Includes:
## Column name standardization
## Removal of unnecessary columns
## Unit standardization (by conversion)
## Removal of streams not in reference table / look-up (that file is an exhaustive list of streams to include)

# Wrangle the discharge data objects to standardize naming somewhat
disc_v2 <- disc_main %>%
  # Rename site column as it appears in the discharge log file
  dplyr::rename(Discharge_File_Name = DischargeFileName) %>%
  # Filter out streams not found in the name look-up 
  ## The lookup table is an exhaustive set of all sites to include
  dplyr::filter(Discharge_File_Name %in% name_lkup$Discharge_File_Name) %>%
  # Convert date to true date format
  dplyr::mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
  # Average through duplicate LTER-stream-date combinations to get rid of them
  dplyr::group_by(LTER, Discharge_File_Name, Date) %>%
  dplyr::summarize(Qcms = mean(Qcms, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Drop any NAs in the discharge or date columns
  dplyr::filter(!is.na(Qcms) & !is.na(Date)) %>%
  # Drop LTER column (it is flawed and not easily salvageable)
  ## Issue is it looks like it is just the first three characters of the stream name
  ## This only works for true LTER sites and GRO sites
  dplyr::select(-LTER)

# Take a look
dplyr::glimpse(disc_v2)

# Check for lost/gained streams
helpR::diff_chk(old = unique(disc_main$DischargeFileName),
                new = unique(disc_v2$Discharge_File_Name))

# Clean up the chemistry data
chem_v2 <- chem_main %>%
  # Calculate the mg/L (from micro moles) for each of these chemicals
  dplyr::mutate(value_mgL = dplyr::case_when(
    ## Phosphorous
    variable == "SRP" ~ (((value / 10^6) * 30.973762) * 1000),
    variable == "PO4" ~ (((value / 10^6) * 30.973762) * 1000),
    variable == "TP" ~ (((value / 10^6) * 30.973762) * 1000),
    ## Silica
    variable == "DSi" ~ (((value / 10^6) * 28.0855) * 1000),
    ## Nitrogen
    variable == "NOx" ~ (((value / 10^6) * 14.0067) * 1000),
    variable == "NO3" ~ (((value / 10^6) * 14.0067) * 1000),
    variable == "NH4" ~ (((value / 10^6) * 14.0067) * 1000),
    variable == "TN" ~ (((value / 10^6) * 14.0067) * 1000))) %>%
  # Drop units, site, and value columns because they're outdated now
  dplyr::select(-units, -site, -value) %>%
  # Rename some columns
  dplyr::rename(Stream_Name = Site.Stream.Name,
                Date = Sampling.Date) %>%
  # Filter out streams not found in the name look-up 
  ## The lookup table is an exhaustive set of all sites to include
  dplyr::filter(Stream_Name %in% name_lkup$Stream_Name) %>%
  # Convert date to true date format
  dplyr::mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
  # Average through duplicate LTER-stream-date-variable combinations to get rid of them
  dplyr::group_by(LTER, Stream_Name, variable, Date) %>%
  dplyr::summarize(value_mgL = mean(value_mgL, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Drop any NAs in the value column
  dplyr::filter(!is.na(value_mgL)) %>%
  # Keep all data from non-Andrews (AND) sites, but drop pre-1983 Andrews data
  dplyr::filter(LTER != "AND" | (LTER == "AND" & lubridate::year(Date) > 1983)) %>%
  # Create a simplified variable column
  dplyr::mutate(variable_simp = dplyr::case_when(
    variable == "SRP" ~ "P",
    variable == "PO4" ~ "P",
    variable == "NO3" ~ "NOx",
    TRUE ~ variable))  %>%
  # Attach the minimum detection limit information where it is known
  dplyr::left_join(y = mdl_info, by = c("Stream_Name", "variable_simp")) %>%
  # Using this, create a "remarks" column that indicates whether a value is below the MDL
  dplyr::mutate(remarks = dplyr::case_when(
    value_mgL < MDL ~ "<",
    value_mgL >= MDL ~ "",
    is.na(MDL) ~ ""),
    .after = Date) %>%
  # Now we can safely drop the MDL information because we have what we need
  dplyr::select(-MDL) %>%
  # Now let's make an "actual" variable column and ditch the others
  dplyr::mutate(variable_actual = ifelse(test = (variable == "SRP" | variable == "PO4"),
                                         yes = "P", no = variable), .after = variable) %>%
  dplyr::select(-variable, -variable_simp) %>%
  dplyr::rename(variable = variable_actual)
  
# Examine that as well
dplyr::glimpse(chem_v2)

# Check for lost/gained streams
helpR::diff_chk(old = unique(chem_main$site),
                new = unique(chem_v2$Stream_Name))

## ---------------------------------------------- ##
           # Match Stream "Aliases" ----
## ---------------------------------------------- ##
# Explanation:
## Discharge streams are identified by the "Discharge_File_Name" column
## Chemistry streams are identified by the "Stream_Name" column
## We need to use the 'name_lkup' object to allow these two dataframes to cross-talk
## One discharge stream can have *multiple* chemisry streams that match

# Wrangle the discharge information
disc_v3 <- disc_v2 %>%
  # Bring in the ref table stream names and LTER names as well
  dplyr::left_join(y = name_lkup, by = c("Discharge_File_Name"))

# Glimpse it
dplyr::glimpse(disc_v3)

# Check for gained/lost streams
helpR::diff_chk(old = unique(disc_v2$Discharge_File_Name),
                new = unique(disc_v3$Discharge_File_Name))

# Wrangle the chemistry data
chem_v3 <- chem_v2 %>%
  # Standardize some LTER names to match the lookup table
  dplyr::mutate(LTER = dplyr::case_when(
    LTER == "KRR(Julian)" ~ "KRR",
    LTER == "LMP(Wymore)" ~ "LMP",
    LTER == "NWQA" ~ "USGS",
    LTER == "Sagehen(Sullivan)" ~ "Sagehen",
    LTER == "UMR(Jankowski)" ~ "UMR",
    TRUE ~ LTER)) %>%
  # Now left join on the name for the stream in the discharge file
  dplyr::left_join(y = name_lkup, by = c("LTER", "Stream_Name"))

# Take a quick look
glimpse(chem_v3)

# Check for gained/lost streams
helpR::diff_chk(old = unique(chem_v2$Stream_Name),
                new = unique(chem_v3$Stream_Name))

## ---------------------------------------------- ##
          # Crop Time Series for WRTDS ----
## ---------------------------------------------- ##
# WRTDS runs best when there are 10 years of discharge data *before* the first chemistry datapoint. Similarly, we can't have more chemistry data than we have discharge data.
# So we need to identify the min/max dates of discharge and chemistry (separately) to be able to use them to crop the actual data as WRTDS requires

# Identify earliest chemical data at each site
disc_lims <- chem_v3 %>%
  # Make a new column of earliest days per stream
  dplyr::group_by(LTER, Stream_Name, Discharge_File_Name) %>%
  dplyr::mutate(min_date = min(Date, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Filter to only those dates
  dplyr::filter(Date == min_date) %>%
  # Pare down columns (drop date now that we have `min_date`)
  dplyr::select(LTER, Stream_Name, Discharge_File_Name, min_date) %>%
  # Subtract 10 years to crop the discharge data to 10 yrs per chemistry data
  dplyr::mutate(disc_start = (min_date - (10 * 365.25)) - 1) %>%
  # Keep only unique rows
  unique()

# Check that
dplyr::glimpse(disc_lims)

# Identify min/max of discharge data
chem_lims <- disc_v3 %>%
  # Group by stream and identify the first and last days of sampling
  dplyr::group_by(LTER, Stream_Name, Discharge_File_Name) %>%
  dplyr::summarize(min_date = min(Date, na.rm = T),
                   max_date = max(Date, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Using the custom function supplied by the Silica team, convert to hydro day
  dplyr::mutate(min_hydro = as.numeric(HERON::hydro_day(cal_date = min_date)),
                max_hydro = as.numeric(HERON::hydro_day(cal_date = max_date))) %>%
  # Find difference between beginning of next water year and end of chem file
  dplyr::mutate(water_year_diff = 365 - max_hydro) %>%
  # Keep only unique rows
  unique()

# Look at that outcome
dplyr::glimpse(chem_lims)

# Crop the discharge file!
disc_v4 <- disc_v3 %>%
  # Left join on the start date from the chemistry data
  dplyr::left_join(y = disc_lims, by = c("LTER", "Discharge_File_Name", "Stream_Name")) %>%
  # Drop any years before the ten year buffer suggested by WRTDS
  dplyr::filter(Date > disc_start) %>%
  # Remove unneeded columns (implicitly)
  dplyr::select(LTER, Discharge_File_Name, Stream_Name, Date, Qcms) %>%
  # Rename the discharge (Q) column without units
  dplyr::rename(Q = Qcms) %>%
  # Make a combo column for LTER and Stream_Name
  dplyr::mutate(Stream_ID = paste0(LTER, "__", Stream_Name),
                .before = dplyr::everything())

# Take another look
dplyr::glimpse(disc_v4)

# Check for gained/lost streams
helpR::diff_chk(old = unique(disc_v3$Discharge_File_Name),
                new = unique(disc_v4$Discharge_File_Name))

# Check for unintentionally lost columns
helpR::diff_chk(old = names(disc_v3), new = names(disc_v4))
## Change to discharge column name is fine
## Added "Stream_ID" column is purposeful

# Now crop chemistry to the min and max dates of discharge
chem_v4 <- chem_v3 %>%
  # Attach important discharge dates
  dplyr::left_join(y = chem_lims, by = c("LTER", "Discharge_File_Name", "Stream_Name")) %>%
  # Use those to crop the dataframe
  dplyr::filter(Date > min_date & Date < max_date) %>%
  # Drop to only needed columns
  dplyr::select(LTER, Discharge_File_Name, Stream_Name,
                variable, Date, remarks, value_mgL) %>%
  # Make a combo column for LTER and Stream_Name
  dplyr::mutate(Stream_ID = paste0(LTER, "__", Stream_Name),
                .before = dplyr::everything())
  
# Glimpse it
dplyr::glimpse(chem_v4)

# Check for gained/lost streams
helpR::diff_chk(old = unique(chem_v3$Stream_Name),
                new = unique(chem_v4$Stream_Name))
## Any streams lost here are lost because somehow *all* chemistry dates are outside of the allowed range defined by the min and max dates found in the discharge data.

# Check for unintentionally lost columns
helpR::diff_chk(old = names(chem_v3), new = names(chem_v4))

# Create the scaffold for what will become the "information" file required by WRTDS
info_v2 <- ref_table %>%
  # Make empty columns to fill later
  dplyr::mutate(param.units = "mg/L",
                shortName = stringr::str_sub(string = Stream_Name, start = 1, end = 8),
                paramShortName = NA,
                constitAbbrev = NA,
                drainSqKm = drainSqKm,
                station.nm = paste0(LTER, "__", Stream_Name)) %>%
  # Drop to only desired columns
  dplyr::select(LTER, Discharge_File_Name, Stream_Name, param.units, shortName,
                paramShortName, constitAbbrev, drainSqKm, station.nm) %>%
  # Make a combo column for LTER and Stream_Name
  dplyr::mutate(Stream_ID = paste0(LTER, "__", Stream_Name),
                .before = dplyr::everything())

# Check that out
dplyr::glimpse(info_v2)

## ---------------------------------------------- ##
          # Final Processing & Export ----
## ---------------------------------------------- ##
# Identify streams in all three datasets (information, chemistry, and discharge)
incl_streams <- intersect(x = intersect(x = disc_v4$Stream_ID, y = chem_v4$Stream_ID),
                          y = info_v2$Stream_ID)

# Filter to only those streams & drop unneeded name columns
discharge <- disc_v4 %>%
  dplyr::filter(Stream_ID %in% incl_streams) %>%
  dplyr::select(-LTER, -Discharge_File_Name, -Stream_Name)

# Final glimpse
dplyr::glimpse(discharge)

# Check for gained/lost streams
helpR::diff_chk(old = unique(disc_v4$Stream_ID),
                new = unique(discharge$Stream_ID))

# Do the same for chemistry
chemistry <- chem_v4 %>%
  dplyr::filter(Stream_ID %in% incl_streams) %>%
  dplyr::select(-LTER, -Discharge_File_Name, -Stream_Name) %>%
  # Make a column for Stream_ID + Chemical
  dplyr::mutate(Stream_Element_ID = paste0(Stream_ID, "_", variable),
                .before = dplyr::everything())

# Check it
dplyr::glimpse(chemistry)

# Check for gained/lost streams
helpR::diff_chk(old = unique(chem_v4$Stream_ID),
                new = unique(chemistry$Stream_ID))

# And finally for information
information <- info_v2 %>%
  dplyr::filter(Stream_ID %in% incl_streams) %>%
  dplyr::select(-LTER, -Discharge_File_Name, -Stream_Name)

# Final glimpse
dplyr::glimpse(information)

# Check for gained/lost streams
helpR::diff_chk(old = unique(info_v2$Stream_ID),
                new = unique(information$Stream_ID))

# Write these final products out for posterity
write.csv(x = discharge, row.names = F, na = "",
          file = file.path(path, "WRTDS Inputs",
                           "WRTDS-input_discharge.csv"))
write.csv(x = chemistry, row.names = F, na = "",
          file = file.path(path, "WRTDS Inputs",
                           "WRTDS-input_chemistry.csv"))
write.csv(x = information, row.names = F, na = "",
          file = file.path(path, "WRTDS Inputs",
                           "WRTDS-input_information.csv"))

# Export them to Google Drive to in case anyone has other uses for them
## Name Drive folder
tidy_dest <- googledrive::as_id("https://drive.google.com/drive/folders/1hOIJRg4XZa3czMU6bskSgAPTlMj25ET-")
## Export to it
googledrive::drive_upload(path = tidy_dest, overwrite = T,
                          media = file.path(path, "WRTDS Inputs",
                                            "WRTDS-input_discharge.csv"))
googledrive::drive_upload(path = tidy_dest, overwrite = T,
                          media = file.path(path, "WRTDS Inputs",
                                            "WRTDS-input_chemistry.csv"))
googledrive::drive_upload(path = tidy_dest, overwrite = T,
                          media = file.path(path, "WRTDS Inputs",
                                            "WRTDS-input_information.csv"))

## ---------------------------------------------- ##
        # Check - Find Dropped Streams ----
## ---------------------------------------------- ##

# We want to be super sure we didn't (somehow) drop any sites in the wrangling steps above.
# Data versions are as follows:
## [disc/chem]_main = "Raw" data (i.e., initial master files)
## [disc/chem]_v2 = Coarse wrangling and averaging within date-stream- combos
## [disc/chem]_v3 = Integration with lookup table to get other data's naming convention
## [disc/chem]_v4 = Cropping by date

# Make a streams only version of each of the discharge objects
d1 <- disc_main %>%
  dplyr::select(Discharge_File_Name = DischargeFileName) %>%
  unique() %>%
  dplyr::mutate(in_d1 = 1)
d2 <- disc_v2 %>%
  dplyr::select(Discharge_File_Name) %>%
  unique() %>%
  dplyr::mutate(in_d2 = 1)
d3 <- disc_v3 %>%
  dplyr::select(LTER, Discharge_File_Name, Stream_Name) %>%
  unique() %>%
  dplyr::mutate(in_d3 = 1)
d4 <- disc_v4 %>%
  dplyr::select(Stream_ID, LTER, Discharge_File_Name, Stream_Name) %>%
  unique() %>%
  dplyr::mutate(in_d4 = 1)
d5 <- discharge %>%
  dplyr::select(Stream_ID) %>%
  unique() %>%
  dplyr::mutate(in_d5 = 1)

# Make one for chemistry as well
c1 <- chem_main %>%
  dplyr::select(Stream_Name = Site.Stream.Name) %>%
  unique() %>%
  dplyr::mutate(in_c1 = 1)
c2 <- chem_v2 %>%
  dplyr::select(Stream_Name) %>%
  unique() %>%
  dplyr::mutate(in_c2 = 1)
c3 <- chem_v3 %>%
  dplyr::select(LTER, Discharge_File_Name, Stream_Name) %>%
  unique() %>%
  dplyr::mutate(in_c3 = 1)
c4 <- chem_v4 %>%
  dplyr::select(Stream_ID, LTER, Discharge_File_Name, Stream_Name) %>%
  unique() %>%
  dplyr::mutate(in_c4 = 1)
c5 <- chemistry %>%
  dplyr::select(Stream_ID) %>%
  unique() %>%
  dplyr::mutate(in_c5 = 1)

# Prep a 'name_check' object by streamlining the ref table
sab_check_v0 <- ref_table %>%
  # Pare down to needed columns only
  dplyr::select(LTER, Discharge_File_Name, Stream_Name) %>%
  # Create a "Stream_ID"
  dplyr::mutate(Stream_ID = paste0(LTER, "__", Stream_Name),
                .before = dplyr::everything()) %>%
  # Bind in version 1 data
  dplyr::full_join(y = d1, by = "Discharge_File_Name") %>%
  dplyr::full_join(y = c1, by = "Stream_Name") %>%
  # Bind in version 2 data
  dplyr::full_join(y = d2, by = "Discharge_File_Name") %>%
  dplyr::full_join(y = c2, by = "Stream_Name") %>%
  # Bind in version 3 data
  dplyr::full_join(y = d3, by = c("LTER", "Discharge_File_Name", "Stream_Name")) %>%
  dplyr::full_join(y = c3, by = c("LTER", "Discharge_File_Name", "Stream_Name")) %>%
  # Version 4
  dplyr::full_join(y = d4, by = c("Stream_ID", "LTER", "Discharge_File_Name", "Stream_Name")) %>%
  dplyr::full_join(y = c4, by = c("Stream_ID", "LTER", "Discharge_File_Name", "Stream_Name")) %>%
  # Version 5
  dplyr::full_join(y = d5, by = "Stream_ID") %>%
  dplyr::full_join(y = c5, by = "Stream_ID")

# Drop any rows without NAs (i.e., those in the data at all stages)
sab_check <- sab_check_v0[ !complete.cases(sab_check_v0), ] %>%
  # Get rowSums to figure out how many versions of data include a given stream
  dplyr::mutate(incl_data_count = rowSums(dplyr::across(dplyr::starts_with("in_")), na.rm = T)) %>%
  # Order by that column
  dplyr::arrange(desc(incl_data_count)) %>%
  # Generate a rough "diagnosis" column from the included data count
  dplyr::mutate(diagnosis = dplyr::case_when(
    incl_data_count == 0 ~ "in reference table but not in either master data file",
    incl_data_count == 1 & is.na(in_c1) ~ "in master discharge but not in master chemistry",
    incl_data_count == 1 & is.na(in_d1) ~ "in master chemistry but not in master discharge",
    incl_data_count == 3 ~ "GUESS: in one dataset but not other so dropped at switch from v3 to v4",
    Stream_Name == "OSTEGLO" ~ "No discharge data (all NAs) so dropped when missing discharge data are filtered out",
    incl_data_count == 4 ~ "Unknown but likely an issue with one master file"
    ), .before = in_d1)

# Take a look!
dplyr::glimpse(sab_check)

# If there are any streams in the sabotage object, export a list for later diagnosis!
if(nrow(sab_check) > 0){
  
  # Make a file name
  (sab_file <- paste0("WRTDS_", Sys.Date(), "_sabotage_check_SITES.csv"))
  
  # Export locally
  write.csv(x = sab_check, na = "", row.names = F,
            file.path(path, "WRTDS Source Files", sab_file))
  
  # Export it to GoogleDrive too
  googledrive::drive_upload(media = file.path(path, "WRTDS Source Files", sab_file),
                            name = "WRTDS_Sabotage_Check_SITES.csv",
                            overwrite = T,
                            path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1HQtpWYoq_YQwj_bDNNbv8D-0swi00o_s"))
}

## ---------------------------------------------- ##
      # Check - Find Dropped Chemicals ----
## ---------------------------------------------- ##
# We also want to be sure that included chemistry sites keep only chemicals
# Above check would (correctly) give green light even if a given chem site lost all but one chemical's data

# Identify stream-element combinations for each data file (except main)
c2_var <- chem_v2 %>%
  # Fix LTER as we do in version 3 of the chem file
  # Standardize some LTER names to match the lookup table
  dplyr::mutate(LTER = dplyr::case_when(
    LTER == "KRR(Julian)" ~ "KRR",
    LTER == "LMP(Wymore)" ~ "LMP",
    LTER == "NWQA" ~ "USGS",
    LTER == "Sagehen(Sullivan)" ~ "Sagehen",
    LTER == "UMR(Jankowski)" ~ "UMR",
    TRUE ~ LTER)) %>%
  dplyr::mutate(Stream_Element_ID = paste0(LTER, "__", Stream_Name, "_", variable)) %>%
  dplyr::select(Stream_Name, Stream_Element_ID) %>%
  unique() %>%
  dplyr::mutate(in_c2 = 1)
c3_var <- chem_v3 %>%
  dplyr::mutate(Stream_Element_ID = paste0(LTER, "__", Stream_Name, "_", variable)) %>%
  dplyr::select(Stream_Element_ID) %>%
  unique() %>%
  dplyr::mutate(in_c3 = 1)
c4_var <- chem_v4 %>%
  dplyr::mutate(Stream_Element_ID = paste0(LTER, "__", Stream_Name, "_", variable)) %>%
  dplyr::select(Stream_Element_ID) %>%
  unique() %>%
  dplyr::mutate(in_c4 = 1)
c5_var <- chemistry %>%
  dplyr::select(Stream_Element_ID) %>%
  unique() %>%
  dplyr::mutate(in_c5 = 1)

# Bind these together to assemble the first pass at this check
var_check_v0 <- c2_var %>%
  dplyr::full_join(y = c3_var, by = "Stream_Element_ID") %>%
  dplyr::full_join(y = c4_var, by = "Stream_Element_ID") %>%
  dplyr::full_join(y = c5_var, by = "Stream_Element_ID")

# Drop any rows that aren't missing in any dataset
var_check <- var_check_v0[ !complete.cases(var_check_v0), ] %>% 
  # Drop any streams that are caught by the "sabotage check" above
  dplyr::filter(!Stream_Name %in% sab_check$Stream_Name) %>%
  # Count how many datasets these streams are included in
  dplyr::mutate(incl_data_count = rowSums(dplyr::across(dplyr::starts_with("in_")), na.rm = T)) %>%
  # Order by that column
  dplyr::arrange(desc(incl_data_count)) %>%
  # Generate a rough "diagnosis" column from the included data count
  dplyr::mutate(diagnosis = dplyr::case_when(
    incl_data_count == 2 ~ "Dropped at date cropping step. Maybe dates are wrong for these elements?",
  ), .before = in_c2)

# Take a look!
dplyr::glimpse(var_check)

# If there are any streams in the sabotage object, export a list for later diagnosis!
if(nrow(var_check) > 0){
  
  # Make a file name
  (var_file <- paste0("WRTDS_", Sys.Date(), "_sabotage_check_CHEMICALS.csv"))
  
  # Export locally
  write.csv(x = var_check, na = "", row.names = F,
            file.path(path, "WRTDS Source Files", var_file))
  
  # Export it to GoogleDrive too
  googledrive::drive_upload(media = file.path(path, "WRTDS Source Files", var_file),
                            name = "WRTDS_Sabotage_Check_CHEMICALS.csv",
                            overwrite = T,
                            path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1HQtpWYoq_YQwj_bDNNbv8D-0swi00o_s"))
}

# End ----