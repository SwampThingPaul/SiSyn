## ---------------------------------------------- ##
           # WRTDS Centralized Workflow
## ---------------------------------------------- ##
# WRTDS = Weighted Regressions on Time, Discharge, and Season

## ---------------------------------------------- ##
                # Housekeeping ----
## ---------------------------------------------- ##
# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, lubridate, gtools)

# Clear environment
# rm(list = ls())

# Create a folder for exporting/importing
dir.create(path = "WRTDS Content", showWarnings = F)

# Identify Google links to relevant folders
disc_folder <- "https://drive.google.com/drive/folders/1HQtpWYoq_YQwj_bDNNbv8D-0swi00o_s"
chem_folder <- "https://drive.google.com/drive/folders/1BAs0y1hHArW8BANUFJrXOXcoMHIk25Pp"

# Identify their IDs
disc_files <- googledrive::drive_ls(path = as_id(disc_folder), type = "csv")
chem_files <- googledrive::drive_ls(path = as_id(chem_folder), type = "csv", pattern = "master")

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
chem_main <- read.csv(file = file.path("WRTDS Content", chem_files[2,1]))
mdl_info <- read.csv(file = file.path("WRTDS Content", chem_files[1,1]))

# Clean up the environment before continuing
# rm(list = setdiff(ls(), c("disc_main", "disc_log", "chem_main", "mdl_info")))
## Above line removes any objects *other* than those specified

# Load in the custom function for converting calendar dates to hydro dates
hydro.day.new = function(x, start.month = 10L){
  start.yr = lubridate::year(x) - (lubridate::month(x) < start.month)
  start.date = lubridate::make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L) }

## ---------------------------------------------- ##
            # General Prep / Tidying ----
## ---------------------------------------------- ##
# Includes:
## Column name standardization
## Removal of unnecessary columns
## Unit standardization (by conversion)

# Wrangle the discharge data objects to standardize naming somewhat
disc_v2 <- disc_main %>%
  # Drop row number column
  dplyr::select(-X) %>%
  # Rename site column as it appears in the discharge log file
  dplyr::rename(Stream = site.name) %>%
  # Convert date to true date format
  dplyr::mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
  # Reorder columns
  dplyr::select(Stream, Date, Qcms)
  
# Check that out
dplyr::glimpse(disc_v2)

# Wrangle the discharge log information as well
ref_table <- disc_log %>%
  # Rename file name columns
  dplyr::rename(files = DischargeFileName) %>%
  # Crop down to only needed columns
  dplyr::select(files, Stream, Units) %>%
  # Keep only streams in the discharge dataset
  dplyr::filter(Stream %in% disc_v2$Stream)

# Check it
dplyr::glimpse(ref_table)

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
    variable_simp == "NH4" ~ (((value / 10^6) * 14.0067) * 1000))) %>%
  # Rename some columns
  dplyr::rename(Stream = Site.Stream.Name, Date = Sampling.Date) %>%
  # Convert date to true date format
  dplyr::mutate(Date = as.Date(Date, "%Y-%m-%d"))

# Examine that as well
dplyr::glimpse(chem_v2)

# Wrangle minimum detection limit file too
mdl_v2 <- mdl_info %>%
  # Drop unneeded columns
  dplyr::select(site, dplyr::ends_with("_MDL"), -NH4_uM_MDL) %>%
  # Rename the stream column and remaining NH4 column
  dplyr::rename(Stream = site, NH4_MDL = NH4_mgL_MDL) %>%
  # Pivot longer
  tidyr::pivot_longer(cols = dplyr::ends_with("MDL"),
                      names_to = "variable",
                      values_to = "MDL") %>%
  # Drop any NAs that result from the pivoting
  dplyr::filter(!is.na(MDL)) %>%
  # Simplify the variable column
  dplyr::mutate(variable_simp = dplyr::case_when(
    variable == "P_MDL" ~ "P",
    variable == "NO3_MDL" ~ "NOx",
    variable == "NH4_MDL" ~ "NH4",
    TRUE ~ variable), .after = variable) %>%
  # And drop the old one
  dplyr::select(-variable)

# Check it
dplyr::glimpse(mdl_v2)  

## ---------------------------------------------- ##
    # Prep - Wrangle Response Dataframes ----
## ---------------------------------------------- ##
# Includes:
## Chem - Incorporation of minimum detection limit (MDL) info (where available)
## Chem - Removal of pre-1982 data at Andrews' sites
## Disc - Averages Qcms if multiple discharge values for a given day/stream

# Identify Andrews (AND) sites pre-1982
early_AND <- chem_v2 %>%
  dplyr::filter(LTER == "AND" & lubridate::year(Date) < 1983)

# Wrangle the chemistry data
chem_v3 <- chem_v2 %>%
  # Remove pre-1982 data at Andrews
  dplyr::anti_join(y = early_AND) %>%
  # Pare down to needed columns (implicitly removes unspecified columns)
  dplyr::select(Stream, variable_simp, Date, value_mgL) %>%
  # Attach the minimum detection limit information where it is known
  dplyr::left_join(y = mdl_v2, by = c("Stream", "variable_simp")) %>%
  # Using this, create a "remarks" column that indicates whether a value is below the MDL
  dplyr::mutate(remarks = ifelse(test = (value_mgL < MDL), yes = "<", no = ""),
                .after = Date) %>%
  # Now we can safely drop the MDL information because we have what we need
  dplyr::select(-MDL)

# Take a quick look
glimpse(chem_v3)

# Wrangle the discharge information
disc_v3 <- disc_v2 %>%
  # Drop any NAs in the discharge column
  dplyr::filter(!is.na(Qcms)) %>%
  # Average discharge if more than one measurement per day/site
  dplyr::group_by(Stream, Date) %>%
  dplyr::summarize(Qcms = mean(Qcms, na.rm = T)) %>%
  dplyr::ungroup()

# Glimpse it
dplyr::glimpse(disc_v3)

## ---------------------------------------------- ##
       # Prep - Identify Years to Exclude ----
## ---------------------------------------------- ##
# WRTDS runs best when there are 10 years of discharge data *before* the first chemistry datapoint. Similarly, we can't have more chemistry data than we have discharge data.
# So we need to identify the min/max dates of discharge and chemistry (separately) to be able to use them to crop the actual data as WRTDS requires

# Identify earliest chemical data at each site
min_chem <- chem_v3 %>%
  # Make a new column of earliest days per stream
  dplyr::group_by(Stream) %>%
  dplyr::mutate(min_date = min(Date, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Filter to only those dates
  dplyr::filter(Date == min_date) %>%
  # Pare down columns (drop date now that we have `min_date`)
  dplyr::select(Stream, min_date) %>%
  # Subtract 10 years to crop the discharge data to 10 yrs per chemistry data
  dplyr::mutate(disc_start = (min_date - (10 * 365.25)) - 1)

# Check that
dplyr::glimpse(min_chem)

# Identify min/max of discharge data
bookends_disc <- disc_v3 %>%
  # Group by stream and identify the first and last days of sampling
  dplyr::group_by(Stream) %>%
  dplyr::summarize(min_date = min(Date, na.rm = T),
                   max_date = max(Date, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Using the custom function supplied by the Silica team, convert to hydro day
  dplyr::mutate(min_hydro = as.numeric(hydro.day.new(x = min_date)),
                max_hydro = as.numeric(hydro.day.new(x = max_date))) %>%
  # Find difference between beginning of next water year and end of chem file
  dplyr::mutate(water_year_diff = 365 - max_hydro)

# Look at that outcome
dplyr::glimpse(bookends_disc)

## ---------------------------------------------- ##
              # Prep - Crop Years ----
## ---------------------------------------------- ##
# Includes:
## Uses the identified start and end dates from the previous section
## Crops discharge and chemistry data to necessary years for WRTDS

# Begin with discharge
discharge <- disc_v3 %>%
  # Left join on the start date from the chemistry data
  dplyr::left_join(y = min_chem, by = "Stream") %>%
  # Identify whether each date is greater than the minimum (per stream!)
  dplyr::group_by(Stream) %>%
  dplyr::mutate(retain = ifelse(Date > disc_start,
                                yes = "keep",
                                no = "drop")) %>%
  dplyr::ungroup() %>%
  # Drop any years before the ten year buffer suggested by WRTDS
  dplyr::filter(retain == "keep") %>%
  # Remove unneeded columns (implicitly)
  dplyr::select(Stream, Date, Qcms)

# Take another look
dplyr::glimpse(discharge)

# Now crop chemistry to the min and max dates of discharge
chemistry <- chem_v3 %>%
  # Attach important discharge dates
  dplyr::left_join(y = bookends_disc, by = "Stream") %>%
  # Use those to crop the dataframe
  dplyr::filter(Date > min_date & Date < max_date) %>%
  # Drop to only needed columns
  dplyr::select(Stream:value_mgL)
  
# Glimpse it
dplyr::glimpse(chemistry)

## ---------------------------------------------- ##
       # Prep - Export Prepared Products ----
## ---------------------------------------------- ##

# Write these final products out for posterity
write.csv(x = discharge, row.names = F, na = "",
          file = file.path("WRTDS Content", "WRTDS-input_discharge.csv"))
write.csv(x = chemistry, row.names = F, na = "",
          file = file.path("WRTDS Content", "WRTDS-input_chemistry.csv"))

# Clean up environment again
rm(list = setdiff(ls(), c("disc_main", "disc_log", "chem_main", "mdl_info",
                          "chemistry", "discharge")))

# Check out mismatched stream IDs
sort(unique(chem_v2$Stream))
sort(unique(disc_v2$Stream))
## Is this a problem?

## ---------------------------------------------- ##
                      # Run ----
## ---------------------------------------------- ##






## ---------------------------------------------- ##
                    # Results ----
## ---------------------------------------------- ##
                    


# End ----
