## ---------------------------------------------- ##
           # WRTDS Centralized Workflow
## ---------------------------------------------- ##
# WRTDS = Weighted Regressions on Time, Discharge, and Season

## ---------------------------------------------- ##
                # Housekeeping ----
## ---------------------------------------------- ##
# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, lubridate, EGRET, EGRETci)

# Clear environment
rm(list = ls())

# Create a folder for (1) source files, (2) direct inputs, (3) site-specific files, (4) outputs, and (5) loop diagnostics
dir.create(path = "WRTDS Source Files", showWarnings = F)
dir.create(path = "WRTDS Inputs", showWarnings = F)
dir.create(path = "WRTDS Temporary Files", showWarnings = F)
dir.create(path = "WRTDS Outputs", showWarnings = F)
dir.create(path = "WRTDS Loop Diagnostic", showWarnings = F)

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
disc_main <- read.csv(file = file.path("WRTDS Source Files", disc_files[3,1]))
disc_log <- read.csv(file = file.path("WRTDS Source Files", disc_files[2,1]))
info_v1 <- read.csv(file = file.path("WRTDS Source Files", info_files[1,1]))
chem_main <- read.csv(file = file.path("WRTDS Source Files", chem_files[2,1]))
mdl_info <- read.csv(file = file.path("WRTDS Source Files", chem_files[1,1]))

# Clean up the environment before continuing
rm(list = setdiff(ls(), c("disc_main", "disc_log", "chem_main", "mdl_info", "info_v1")))
## Above line removes anything *other* than objects specified

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
## Streamlining of information file

# Wrangle the discharge data objects to standardize naming somewhat
disc_v2 <- disc_main %>%
  # Drop row number column
  dplyr::select(-X) %>%
  # Rename site column as it appears in the discharge log file
  dplyr::rename(Discharge_Stream = DischargeFileName) %>%
  # Convert date to true date format
  dplyr::mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
  # Reorder columns
  dplyr::select(Discharge_Stream, Date, Qcms)
  
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
    variable_simp == "NH4" ~ (((value / 10^6) * 14.0067) * 1000))) %>%
  # Rename some columns
  dplyr::rename(Stream = Site.Stream.Name, Date = Sampling.Date) %>%
  # Convert date to true date format
  dplyr::mutate(Date = as.Date(Date, "%Y-%m-%d"))

# Examine that as well
dplyr::glimpse(chem_v2)

# Wrangle the discharge log information as well
ref_table <- disc_log %>%
  # Rename file name columns
  dplyr::rename(Discharge_Stream = DischargeFileName) %>%
  # Crop down to only needed columns
  dplyr::select(Discharge_Stream, Stream)

# Check it
dplyr::glimpse(ref_table)

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

# Wrangle the information dataframe as well
info_v2_p <- info_v1 %>%
  # Make a new column to match with ref_table later
  dplyr::mutate(Stream = station.nm) %>%
  # Pivot longer to build in other compounds
  tidyr::pivot_longer(cols = c(paramShortName, constitAbbrev, param.nm), names_to = "param_col", values_to = "values")

# Cumbersome, but duplicate this dataframe for each chemical
## Silica
info_v2_si <- info_v2_p %>%
  dplyr::mutate(values = dplyr::case_when(
    values == "Phosphate" ~ "Silicon",
    values == "PO4" ~ "DSi")) %>%
  # Once fixed, pivot back to wide format
  tidyr::pivot_wider(names_from = param_col,
                     values_from = values)
## Ammonium
info_v2_nh4 <- info_v2_p %>%
  dplyr::mutate(values = dplyr::case_when(
    values == "Phosphate" ~ "Ammonium",
    values == "PO4" ~ "NH4")) %>%
  tidyr::pivot_wider(names_from = param_col,
                     values_from = values)
## Nitrate
info_v2_nox <- info_v2_p %>%
  dplyr::mutate(values = dplyr::case_when(
    values == "Phosphate" ~ "Nitrate",
    values == "PO4" ~ "NOx")) %>%
  tidyr::pivot_wider(names_from = param_col,
                     values_from = values)
## Pivot phosphorous wider too
info_v2_p_v2 <- info_v2_p %>%
  tidyr::pivot_wider(names_from = param_col,
                     values_from = values)

# Bind all four chemicals together
info_v2 <- info_v2_p_v2 %>%
  dplyr::bind_rows(info_v2_si) %>%
  dplyr::bind_rows(info_v2_nh4) %>%
  dplyr::bind_rows(info_v2_nox) %>%
  # And reorder columns to original order
  dplyr::select(Stream, param.units, shortName, paramShortName, constitAbbrev, drainSqKm, station.nm, param.nm, staAbbrev) %>%
  # Simplify the phosphate entry
  dplyr::mutate(param.nm = ifelse(param.nm == "PO4", yes = "P", no = param.nm))

# Look at it
dplyr::glimpse(info_v2)

## ---------------------------------------------- ##
    # Prep - Wrangle Response Dataframes ----
## ---------------------------------------------- ##
# Includes:
## Chem - Incorporation of minimum detection limit (MDL) info (where available)
## Chem - Removal of pre-1982 data at Andrews' sites
## Disc - Averages Qcms if multiple discharge values for a given day/stream
## Both - Retrieval of matched "stream" name with "discharge stream name" and "chemistry stream name"

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
  dplyr::select(-MDL) %>%
  # Now left join on the name for the stream in the discharge file
  dplyr::left_join(ref_table, by = "Stream") %>%
  # And move the discharge stream name column to the left
  dplyr::select(Discharge_Stream, Stream:value_mgL)

# Take a quick look
glimpse(chem_v3)

# Wrangle the discharge information
disc_v3 <- disc_v2 %>%
  # Drop any NAs in the discharge column
  dplyr::filter(!is.na(Qcms)) %>%
  # Average discharge if more than one measurement per day/site
  dplyr::group_by(Discharge_Stream, Date) %>%
  dplyr::summarize(Qcms = mean(Qcms, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Bring in the ref table stream names as well
  dplyr::left_join(ref_table, by = "Discharge_Stream") %>%
  # And move the column to the right of the Dicharge Stream name
  dplyr::select(Discharge_Stream, Stream, Date, Qcms)

# Glimpse it
dplyr::glimpse(disc_v3)

# Now information
info_v3 <- info_v2 %>%
  # And bring on discharge stream from the chemistry data
  dplyr::mutate(Discharge_Stream = chem_v3$Discharge_Stream[match(Stream, chem_v3$Stream)], .before = Stream)

# Glimpse
dplyr::glimpse(info_v3)

## ---------------------------------------------- ##
       # Prep - Identify Years to Exclude ----
## ---------------------------------------------- ##
# WRTDS runs best when there are 10 years of discharge data *before* the first chemistry datapoint. Similarly, we can't have more chemistry data than we have discharge data.
# So we need to identify the min/max dates of discharge and chemistry (separately) to be able to use them to crop the actual data as WRTDS requires

# Identify earliest chemical data at each site
min_chem <- chem_v3 %>%
  # Make a new column of earliest days per stream
  dplyr::group_by(Discharge_Stream, Stream) %>%
  dplyr::mutate(min_date = min(Date, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Filter to only those dates
  dplyr::filter(Date == min_date) %>%
  # Pare down columns (drop date now that we have `min_date`)
  dplyr::select(Discharge_Stream, Stream, min_date) %>%
  # Subtract 10 years to crop the discharge data to 10 yrs per chemistry data
  dplyr::mutate(disc_start = (min_date - (10 * 365.25)) - 1)

# Check that
dplyr::glimpse(min_chem)

# Identify min/max of discharge data
bookends_disc <- disc_v3 %>%
  # Group by stream and identify the first and last days of sampling
  dplyr::group_by(Discharge_Stream, Stream) %>%
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
  dplyr::left_join(y = min_chem, by = c("Discharge_Stream", "Stream")) %>%
  # Identify whether each date is greater than the minimum (per stream!)
  dplyr::group_by(Discharge_Stream, Stream) %>%
  dplyr::mutate(retain = ifelse(Date > disc_start,
                                yes = "keep",
                                no = "drop")) %>%
  dplyr::ungroup() %>%
  # Drop any years before the ten year buffer suggested by WRTDS
  dplyr::filter(retain == "keep") %>%
  # Remove unneeded columns (implicitly)
  dplyr::select(Discharge_Stream, Stream, Date, Qcms) %>%
  # Rename the discharge (Q) column without units
  dplyr::rename(Q = Qcms)

# Take another look
dplyr::glimpse(discharge)

# Now crop chemistry to the min and max dates of discharge
chemistry <- chem_v3 %>%
  # Attach important discharge dates
  dplyr::left_join(y = bookends_disc, by = c("Discharge_Stream", "Stream")) %>%
  # Use those to crop the dataframe
  ## !!! Note that this removes streams where "Discharge_Stream" is NA !!!
  dplyr::filter(Date > min_date & Date < max_date) %>%
  # Drop to only needed columns
  dplyr::select(Discharge_Stream:value_mgL)
  
# Glimpse it
dplyr::glimpse(chemistry)

# Finally, clean up the information dataframe however is needed
information <- info_v3
## Currently no wrangling needed here

# Take a look
dplyr::glimpse(information)

## ---------------------------------------------- ##
       # Prep - Export Prepared Products ----
## ---------------------------------------------- ##

# Write these final products out for posterity
write.csv(x = discharge, row.names = F, na = "",
          file = file.path("WRTDS Inputs",
                           "WRTDS-input_discharge.csv"))
write.csv(x = chemistry, row.names = F, na = "",
          file = file.path("WRTDS Inputs",
                           "WRTDS-input_chemistry.csv"))
write.csv(x = information, row.names = F, na = "",
          file = file.path("WRTDS Inputs",
                           "WRTDS-input_information.csv"))

# Clean up environment again
rm(list = setdiff(ls(), c("disc_main", "disc_log", "chem_main", "mdl_info", "info_v1", "chemistry", "discharge", "information")))

## ---------------------------------------------- ##
                      # Run ----
## ---------------------------------------------- ##
# "Run" Structure:
## Big for loop that iterates across "Discharge_Stream" names
## Within that loop, a slightly smaller loop that iterates across chemicals that were sampled at that river.

# for(river in unique(discharge$Discharge_Stream)){
# (^^^) Actual loop (uncomment when you are ready)
# (vvv) Test loop for a single site
for(river in "AND_GSWSMC_Q"){
  
  # Subset discharge to correct river
  river_disc <- discharge %>%
    dplyr::filter(Discharge_Stream == river) %>%
    dplyr::select(Date, Q)
  
  # Subset chemistry to the right river (but still all chemicals)
  chem_partial <- chemistry %>%
    dplyr::filter(Discharge_Stream == river)

  # Again, including "actual" and "test" loop heads  
  # for(element in unique(chem_partial$variable_simp)){
  for(element in "DSi"){
    
    if(file.exists(file.path(server_path, "WRTDS Loop Diagnostic", paste0(river, "_", element, "_", "Loop_Diagnostic.csv")))) {
      message("Processing complete for ", river, ", element ", element)
    } else {
      
    # Grab start time for processing
    start <- Sys.time()
    
# Subset chemistry to right river *and* right element
    river_chem <- chem_partial %>%
      dplyr::filter(variable_simp == element) %>%
      dplyr::select(Date, remarks, value_mgL)

# Information also subseted to right river + element
river_info <- information %>%
  dplyr::filter(Discharge_Stream == river & param.nm == element) %>%
  dplyr::select(param.units, shortName, paramShortName, constitAbbrev,
                drainSqKm, station.nm, param.nm, staAbbrev)

# Save these as CSVs with generic names
## This means each iteration of the loop will overwrite them so this folder won't become gigantic
write.csv(x = river_disc, row.names = F, na = "",
          file = file.path("WRTDS Temporary Files", "discharge.csv"))
write.csv(x = river_chem, row.names = F, na = "",
          file = file.path("WRTDS Temporary Files", "chemistry.csv"))
write.csv(x = river_info, row.names = F, na = "",
          file = file.path("WRTDS Temporary Files", "information.csv"))

# Then read them back in with EGRET's special acquisition functions
egret_disc <- EGRET::readUserDaily(filePath = "WRTDS Temporary Files", fileName = "discharge.csv", qUnit = 2, verbose = F)
egret_chem_v1 <- EGRET::readUserSample(filePath = "WRTDS Temporary Files", fileName = "chemistry.csv", verbose = F)
egret_info <- EGRET::readUserInfo(filePath = "WRTDS Temporary Files", fileName = "information.csv", interactive = F)

# Remove any duplicates from chemistry file
egret_chem <- EGRET::removeDuplicates(Sample = egret_chem_v1)

# Create a list of the discharge, chemistry, and information files
egret_list <- EGRET::mergeReport(INFO = egret_info, Daily = egret_disc, Sample = egret_chem, verbose = F)
## Getting a weird "duplicated dates" message/warning...

# Fit original model
egret_estimation <- EGRET::modelEstimation(eList = egret_list, minNumObs = 50, verbose = F)

# Fit "GFN" (?) model
egret_list_out <- EGRET::runSeries(eList = egret_list, windowSide = 11, minNumObs = 50, verbose = F)

# Create a common prefix for all outputs from this run of the loop
out_prefix <- paste0(river, "_", element, "_") 

# Identify error statistics
egret_error <- EGRET::errorStats(eList = egret_estimation)

# Save the error stats out
write.csv(x = egret_error, file = file.path("WRTDS Outputs", paste0(out_prefix, "ErrorStats_WRTDS.csv")), row.names = F, na = "")

# Create PDF report
## Start the PDF
pdf(file = file.path("WRTDS Outputs", paste0(out_prefix, "WRTDS_GFN_output.pdf")))

## Residual plots
EGRET::fluxBiasMulti(eList = egret_estimation)

## Model Fit
EGRET::plotConcTimeDaily(eList = egret_list_out)

## Concentration
EGRET::plotConcHist(eList = egret_list_out) # minYP, maxYP)

## Flux
EGRET::plotFluxHist(eList = egret_list_out) #, minYP, maxYP)

## Data
EGRET::multiPlotDataOverview(eList = egret_list_out)

## Actually create PDF report
dev.off()

# Create annual averages
egret_annual <- EGRET::tableResults(eList = egret_list_out)
## Can't silence this function... >:(

# Export that as a CSV also
write.csv(x = egret_annual, file.path("WRTDS Outputs", paste0(out_prefix, "ResultsTable_GFN_WRTDS.csv")), row.names = F, na = "")

# Identify monthly results
egret_monthly <- EGRET::calculateMonthlyResults(eList = egret_list_out)

# Export that
write.csv(x = egret_monthly, file.path("WRTDS Outputs", paste0(out_prefix, "Monthly_GFN_WRTDS.csv")), row.names = F, na = "")

# Extract daily chemical value from run
egret_concentration <- egret_list_out$Daily

# Export that as well
write.csv(x = egret_concentration, file.path("WRTDS Outputs", paste0(out_prefix, "GFN_WRTDS.csv")), row.names = F, na = "")

# Make a new column for year
egret_concentration$Year <- format(as.Date(egret_concentration$Date), "%Y")

# Find min & max year
min_year <- as.numeric(min(egret_concentration$Year, na.rm = T)) + 1
max_year <- as.numeric(max(egret_concentration$Year, na.rm = T)) - 1

# Set them as a vector
year_points <- c(min_year, max_year)

# Calculate concentration trend
egret_conc_trend_v1 <- EGRET::tableChangeSingle(eList = egret_list_out, fluxUnit = 8, yearPoints = year_points, flux = FALSE)
## Can't silence this function either

# Calculate flux trend
egret_flux_trend <- EGRET::tableChangeSingle(eList = egret_list_out, fluxUnit = 8, yearPoints = year_points, flux = TRUE)
## Can't silence this function either

# Add a column to each of these indicating whether it's flux or conc.
egret_conc_trend_v1$Metric <- "Concentration"
egret_flux_trend$Metric <- "Flux"

# Rename two columns in the concentration dataframe
egret_conc_trend <- egret_conc_trend_v1 %>%
  dplyr::rename(`change[percent]` = `change[%]`,
                `slope [percent/yr]` = `slope [%/yr]`)

# Bind these dataframes together
egret_trends <- egret_conc_trend %>%
  dplyr::bind_rows(egret_flux_trend) %>%
  # Move the metric column before everything else
  dplyr::select(Metric, dplyr::everything())

# Export it!
write.csv(x = egret_trends, file.path("WRTDS Outputs", paste0(out_prefix, "TrendsTable_GFN_WRTDS.csv")), row.names = F, na = "")

# Run trend estimate for GFN method between start/end years
egret_pairs <- EGRET::runPairs(eList = egret_list_out, windowSide = 11, minNumObs = 50, year1 = min_year, year2 = max_year)

# Export those values as well
write.csv(x = egret_pairs, file.path("WRTDS Outputs", paste0(out_prefix, "ListPairs_GFN_WRTDS.csv")), row.names = F, na = "")

# Estimate trend uncertainty
egret_boot <- EGRETci::runPairsBoot(eList = egret_list_out, pairResults = egret_pairs, nBoot = 100, blockLength = 200)

# Strip out key results
egret_boot_results <- data.frame(
  Solute = rep(element, times = length(egret_boot$xConc)),
  xConc = egret_boot$xConc,
  xFlux = egret_boot$xFlux,
  pConc = egret_boot$pConc,
  pFlux = egret_boot$pFlux)

# Export the results
write.csv(x = egret_boot_results, file.path("WRTDS Outputs", paste0(out_prefix, "EGRETCi_GFN_bootstraps.csv")), row.names = F, na = "")

# Also grab the summary information
egret_boot_summary <- as.data.frame(egret_boot$bootOut)

# And export it as well
write.csv(x = egret_boot_summary, file.path("WRTDS Outputs", paste0(out_prefix, "EGRETCi_GFN_Trend.csv")), row.names = F, na = "")

# Grab the end processing time
end <- Sys.time()

# Combine timing into a dataframe
loop_diagnostic <- data.frame("stream" = river,
                              "chemical" = element,
                              "loop_start" = start,
                              "loop_end" = end)

# Export this as well
write.csv(x = loop_diagnostic, file.path("WRTDS Loop Diagnostic", paste0(out_prefix, "Loop_Diagnostic.csv")), row.names = F, na = "")

    } # Close `else` part of whether file exists
    
  } # End "element" loop
  
} # End "river" loop

# End ----
