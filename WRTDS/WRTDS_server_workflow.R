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
librarian::shelf(tidyverse, googledrive, lubridate, EGRET, EGRETci, njlyon0/helpR)

# Clear environment
rm(list = ls())

# If working on server, need to specify correct path
server_path <- file.path('/', "home", "shares", "lter-si", "WRTDS")

# Create a folder for (1) source files, (2) direct inputs, (3) site-specific files, and (4) outputs
dir.create(path = file.path(server_path, "WRTDS Source Files"), showWarnings = F)
dir.create(path = file.path(server_path, "WRTDS Inputs"), showWarnings = F)
dir.create(path = file.path(server_path, "WRTDS Temporary Files"), showWarnings = F)
dir.create(path = file.path(server_path, "WRTDS Outputs"), showWarnings = F)
dir.create(path = file.path(server_path, "WRTDS Loop Diagnostic"), showWarnings = F)

# Define the names of the Drive files we need
names <- c("WRTDS_Reference_Table_with_Areas_DO_NOT_EDIT.csv", # No.1 Ref table
           "UpdatedAll_Q_master_10182022.csv", # No.2 Main discharge
           "20221020_masterdata_chem.csv", # No.3 Main chemistry
           "20220909_master_min_det_limit.csv") # No.4 Minimum detection limit info

# Find folders for those files
ids <- googledrive::drive_ls(as_id("https://drive.google.com/drive/u/1/folders/1HQtpWYoq_YQwj_bDNNbv8D-0swi00o_s")) %>%
  dplyr::bind_rows(googledrive::drive_ls(as_id("https://drive.google.com/drive/u/1/folders/1BAs0y1hHArW8BANUFJrXOXcoMHIk25Pp")))

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
                                path = file.path(server_path, "WRTDS Source Files", names[k]))
}

# Now read in those files!
ref_table <- read.csv(file = file.path(server_path, "WRTDS Source Files", names[1]))
disc_main <- read.csv(file = file.path(server_path, "WRTDS Source Files", names[2]))
chem_main <- read.csv(file = file.path(server_path, "WRTDS Source Files", names[3]))
mdl_info <- read.csv(file = file.path(server_path, "WRTDS Source Files", names[4]))

# Clean up the environment before continuing
rm(list = setdiff(ls(), c("server_path", "ref_table", "disc_main", "chem_main", "mdl_info")))
## Above line removes anything *other* than objects specified

## ---------------------------------------------- ##
          # Prep - Supporting Files ----
## ---------------------------------------------- ##

# Make a lookup table from the reference table for just stream names
name_lkup <- ref_table %>%
  # Pare to only some columns
  dplyr::select(LTER, Discharge_File_Name, Stream_Name)

# Glimpse it
dplyr::glimpse(name_lkup)

# Wrangle minimum detection limit file too
mdl_v2 <- mdl_info %>%
  # Drop unneeded columns
  dplyr::select(site, dplyr::ends_with("_MDL"), -NH4_uM_MDL) %>%
  # Rename the stream column and remaining NH4 column
  dplyr::rename(Stream_Name = site, NH4_MDL = NH4_mgL_MDL) %>%
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
  dplyr::select(-variable) %>%
  # Rename remaining column
  dplyr::rename(variable = variable_simp)

# Check it
dplyr::glimpse(mdl_v2)

## ---------------------------------------------- ##
           # Prep - Initial Wrangling ----
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

# Clean up the chemistry data
chem_v2 <- chem_main %>%
  # Simplify phosphorous for later
  dplyr::mutate(variable_simp = ifelse(variable == "SRP" | variable == "PO4",
                                       yes = "P", no = variable)) %>%
  # That done, drop all chemicals other than the core ones we're interested in
  dplyr::filter(variable_simp %in% c("P", "DSi", "NOx", "NH4")) %>%
  # Calculate the mg/L (from micro moles) for each of these chemicals
  dplyr::mutate(value_mgL = dplyr::case_when(
    variable_simp == "P" ~ (((value / 10^6) * 30.973762) * 1000),
    variable_simp == "DSi" ~ (((value / 10^6) * 28.0855) * 1000),
    variable_simp == "NOx" ~ (((value / 10^6) * 14.0067) * 1000),
    variable_simp == "NH4" ~ (((value / 10^6) * 14.0067) * 1000))) %>%
  # Drop units, site, value, and variable columns because they're outdated now
  dplyr::select(-units, -site, -variable, -value) %>%
  # Rename some columns
  dplyr::rename(Stream_Name = Site.Stream.Name,
                Date = Sampling.Date,
                variable = variable_simp) %>%
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
  # Attach the minimum detection limit information where it is known
  dplyr::left_join(y = mdl_v2, by = c("Stream_Name", "variable")) %>%
  # Using this, create a "remarks" column that indicates whether a value is below the MDL
  dplyr::mutate(remarks = ifelse(test = (value_mgL < MDL), yes = "<", no = ""),
                .after = Date) %>%
  # Now we can safely drop the MDL information because we have what we need
  dplyr::select(-MDL)
  
# Examine that as well
dplyr::glimpse(chem_v2)

## ---------------------------------------------- ##
      # Prep - Match Stream "Aliases" ----
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

## ---------------------------------------------- ##
      # Prep - Crop Time Series for WRTDS ----
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
      # Prep - Final Processing & Export ----
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

# Do the same for chemistry
chemistry <- chem_v4 %>%
  dplyr::filter(Stream_ID %in% incl_streams) %>%
  dplyr::select(-LTER, -Discharge_File_Name, -Stream_Name) %>%
  # Make a column for Stream_ID + Chemical
  dplyr::mutate(Stream_Element_ID = paste0(Stream_ID, "_", variable),
                .before = dplyr::everything())

# Check it
dplyr::glimpse(chemistry)

# And finally for information
information <- info_v2 %>%
  dplyr::filter(Stream_ID %in% incl_streams) %>%
  dplyr::select(-LTER, -Discharge_File_Name, -Stream_Name)

# Final glimpse
dplyr::glimpse(information)

# Write these final products out for posterity
write.csv(x = discharge, row.names = F, na = "",
          file = file.path(server_path, "WRTDS Inputs",
                           "WRTDS-input_discharge.csv"))
write.csv(x = chemistry, row.names = F, na = "",
          file = file.path(server_path, "WRTDS Inputs",
                           "WRTDS-input_chemistry.csv"))
write.csv(x = information, row.names = F, na = "",
          file = file.path(server_path, "WRTDS Inputs",
                           "WRTDS-input_information.csv"))

## ---------------------------------------------- ##
                    # Run WRTDS ----
## ---------------------------------------------- ##
# "Run" Structure:
## Big for loop that iterates across "Stream_Name" names
## Within that loop, a smaller loop iterates across chemicals sampled there

# Remove *everything* from environment (we need the vector memory below)
rm(list = ls())

# Re-define server path
server_path <- file.path('/', "home", "shares", "lter-si", "WRTDS")

# Read in CSVs generated by above steps
discharge <- read.csv(file.path(server_path, "WRTDS Inputs", "WRTDS-input_discharge.csv"))
chemistry <- read.csv(file.path(server_path, "WRTDS Inputs", "WRTDS-input_chemistry.csv"))
information <- read.csv(file.path(server_path, "WRTDS Inputs", "WRTDS-input_information.csv"))

# Drop problem rivers from the loop
bad_rivers <- c(
  # Error in `EGRET::modelEstimation`
  ### "Error in runSurvReg(SampleCrossV$DecYear[i], SampleCrossV$LogQ[i], DecLow,  : 
  ### minNumUncen is greater than total number of samples"
  "AND__GSWS06_NOx", "AND__GSWS07_NOx", "HBR__ws1_P", "HBR__ws2_P", "HBR__ws3_P",
  "HBR__ws4_P", "HBR__ws5_P", "HBR__ws6_P", "HBR__ws7_P", "HBR__ws8_P", "HBR__ws9_P",
  # Warning in `EGRET::mergeReport`
  ### "Some Sample dates do not have corresponding flow data. Not all EGRET functions will work correctly."
  # Warning in ` EGRET::modelEstimation`
  ### "Problems converging"
  # Eventual downstream error message:
  ### Error in if (lastMonth == 2 & (lastYear%%4 == 0) & ((lastYear%%100 !=  : 
  ### missing value where TRUE/FALSE needed
  "LUQ__RI_DSi", "LUQ__RI_NH4", "LUQ__RI_NOx", "LUQ__RI_P"
  
)

# Loop across rivers and elements to run WRTDS workflow!
for(river in setdiff(x = unique(chemistry$Stream_Element_ID), y = bad_rivers)){
# (^^^) Actual loop (uncomment when you are ready)
# (vvv) Test loop for a single site
# for(river in "AND__GSWS02_DSi"){
  
  # Identify corresponding Stream_ID
  stream_id <- chemistry %>%
    dplyr::filter(Stream_Element_ID == river) %>%
    dplyr::select(Stream_ID) %>%
    unique() %>%
    as.character()
  
  # Also element
  element <- chemistry %>%
    dplyr::filter(Stream_Element_ID == river) %>%
    dplyr::select(variable) %>%
    unique() %>%
    as.character()

  # Subset chemistry
  river_chem <- chemistry %>%
    dplyr::filter(Stream_Element_ID == river) %>%
    # Drop unneeded columns
    dplyr::select(-Stream_Element_ID, -Stream_ID, -variable)
  
  # Subset discharge to correct river
  river_disc <- discharge %>%
    dplyr::filter(Stream_ID == stream_id) %>%
    dplyr::select(Date, Q)
  
  # Create a common prefix for all outputs from this run of the loop
  out_prefix <- paste0(stream_id, "_", element, "_") 
  
  # If the file exists
  if(file.exists(file.path(server_path, "WRTDS Loop Diagnostic", paste0(out_prefix, "Loop_Diagnostic.csv"))) == TRUE) {
    message("WRTDS already run for ", element, " at stream '", river, "'")
  } else {
  
  # Message completion of loop
  message("Processing begun for ", element, " at stream '", river, "'")
  
  # Grab start time for processing
  start <- Sys.time()
  
  # Information also subsetted to right river
  river_info <- information %>%
    dplyr::filter(Stream_ID == stream_id) %>%
    # Generate correct information for this element
    dplyr::mutate(constitAbbrev = element) %>%
    dplyr::mutate(paramShortName = dplyr::case_when(
      constitAbbrev == "DSi" ~ "Silicon",
      constitAbbrev == "NOx" ~ "Nitrate",
      constitAbbrev == "P" ~ "Phosphorous",
      constitAbbrev == "NH4" ~ "Ammonium")) %>%
    # Create another needed column
    dplyr::mutate(staAbbrev = shortName) %>%
    # Drop stream ID now that subsetting is complete
    dplyr::select(-Stream_ID)
    
  # Save these as CSVs with generic names
  ## This means each iteration of the loop will overwrite them so this folder won't become gigantic
  write.csv(x = river_disc, row.names = F, na = "",
            file = file.path(server_path, "WRTDS Temporary Files", "discharge.csv"))
  write.csv(x = river_chem, row.names = F, na = "",
            file = file.path(server_path, "WRTDS Temporary Files", "chemistry.csv"))
  write.csv(x = river_info, row.names = F, na = "",
            file = file.path(server_path, "WRTDS Temporary Files", "information.csv"))
  
  # Then read them back in with EGRET's special acquisition functions
  egret_disc <- EGRET::readUserDaily(filePath = file.path(server_path, "WRTDS Temporary Files"), fileName = "discharge.csv", qUnit = 2, verbose = F)
  egret_chem <- EGRET::readUserSample(filePath = file.path(server_path, "WRTDS Temporary Files"), fileName = "chemistry.csv", verbose = F)
  egret_info <- EGRET::readUserInfo(filePath = file.path(server_path, "WRTDS Temporary Files"), fileName = "information.csv", interactive = F)
  
  # Create a list of the discharge, chemistry, and information files
  egret_list <- EGRET::mergeReport(INFO = egret_info, Daily = egret_disc, Sample = egret_chem, verbose = F)

  # Fit original model
  egret_estimation <- EGRET::modelEstimation(eList = egret_list, minNumObs = 50, verbose = F)
  
  # Fit "GFN" model
  egret_list_out <- EGRET::runSeries(eList = egret_list, windowSide = 11, minNumObs = 50, verbose = F)
  
  # Identify error statistics
  egret_error <- EGRET::errorStats(eList = egret_estimation)
  
  # Save the error stats out
  write.csv(x = egret_error, file = file.path(server_path, "WRTDS Outputs", paste0(out_prefix, "ErrorStats_WRTDS.csv")), row.names = F, na = "")
  
  # Create PDF report
  ## Start the PDF
  pdf(file = file.path(server_path, "WRTDS Outputs", paste0(out_prefix, "WRTDS_GFN_output.pdf")))
  
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
  write.csv(x = egret_annual, file.path(server_path, "WRTDS Outputs", paste0(out_prefix, "ResultsTable_GFN_WRTDS.csv")), row.names = F, na = "")
  
  # Identify monthly results
  egret_monthly <- EGRET::calculateMonthlyResults(eList = egret_list_out)
  
  # Export that
  write.csv(x = egret_monthly, file.path(server_path, "WRTDS Outputs", paste0(out_prefix, "Monthly_GFN_WRTDS.csv")), row.names = F, na = "")
  
  # Extract daily chemical value from run
  egret_concentration <- egret_list_out$Daily
  
  # Export that as well
  write.csv(x = egret_concentration, file.path(server_path, "WRTDS Outputs", paste0(out_prefix, "GFN_WRTDS.csv")), row.names = F, na = "")
  
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
  write.csv(x = egret_trends, file.path(server_path, "WRTDS Outputs", paste0(out_prefix, "TrendsTable_GFN_WRTDS.csv")), row.names = F, na = "")
  
  # Grab the end processing time
  end <- Sys.time()
  
  # Combine timing into a dataframe
  loop_diagnostic <- data.frame("stream" = stream_id,
                                "chemical" = element,
                                "loop_start" = start,
                                "loop_end" = end)
  
  # Export this as well
  write.csv(x = loop_diagnostic, file.path(server_path, "WRTDS Loop Diagnostic", paste0(out_prefix, "Loop_Diagnostic.csv")), row.names = F, na = "")
  
  # Message completion of loop
  message("Processing complete for ", element, " at stream '", river, "'")
      
    } # Close `else` part of whether file exists
    
} # End loop

# End ----
