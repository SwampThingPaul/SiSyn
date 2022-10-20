## ------------------------------------------------------- ##
                # WRTDS - Find Watershed Area
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon + ...

# Purpose:
## Find area of drainage basins for sites where that is not known from expert sources

## ---------------------------------------------- ##
                 # Housekeeping ----
## ---------------------------------------------- ##
# Read needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, sf, terra, nngeo)

# Clear environment
rm(list = ls())

# Identify path to location of shared data
(path <- file.path('/', "home", "shares", "lter-si", "si-watershed-extract"))

# Site coordinate retrieval and preparation --------------------

# If you've never used the `googledrive` R package, you'll need to "authorize" it
## 1) Run the below line with your Google email that has access to the needed folders
# googledrive::drive_auth(email = "lyon.nceas.ucsb.edu")
## 1B) Choose which Google account to proceed with
## 2) Check the "see, edit, create, and ..." box
## 3) Click "Continue" at the bottom
## 4) Copy the "authorization code"
## 5) Paste it into the field in the Console


# Identify reference table Google ID
ref_id <- googledrive::drive_ls(as_id("https://drive.google.com/drive/u/1/folders/1HQtpWYoq_YQwj_bDNNbv8D-0swi00o_s")) %>%
  dplyr::filter(name == "WRTDS_Reference_Table")

# Download ref table (overwriting previous downloads)
googledrive::drive_download(file = as_id(ref_id),
                            path = file.path(path, "WRTDS_Reference_Table.xlsx"),
                            overwrite = T)

# Read in reference table of all sites
sites_v0 <- readxl::read_excel(path = file.path(path, "WRTDS_Reference_Table.xlsx"))

# Do some pre-processing to pare down to only desired information
sites <- sites_v0 %>%
  # Filter out sites not used in WRTDS
  dplyr::filter(Use_WRTDS != "No") %>%
  # Drop the WRTDS column now that we've subsetted by it
  dplyr::select(-Use_WRTDS) %>%
  # Make a uniqueID column
  dplyr::mutate(uniqueID = paste0(LTER, "_", Stream_Name), 
                .before = dplyr::everything())

# Glimpse that
dplyr::glimpse(sites)

# Check for missing coordinates
sites %>%
  dplyr::filter(is.na(Latitude) | is.na(Longitude)) %>%
  unique()
## 0 rows means all is good!

# Check for invalid coordinates
## Bad longitudes
dplyr::filter(sites, abs(Longitude) > 180)
dplyr::filter(sites, abs(Latitude) > 90)

# Get an explicitly spatial version
sites_spatial <- sf::st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326)

# Check it out
str(sites_spatial)

## ---------------------------------------------- ##
      # Load HydroSHEDS Basin Delineations ----
## ---------------------------------------------- ##

# See HydroSHEDS website (link below) for download links & tech documentation
## https://www.hydrosheds.org/page/hydrobasins

# Load in relevant files
arctic <- sf::st_read(file.path(path, "hydrosheds-raw", "hybas_ar_lev00_v1c.shp"))
# xmin: -180       ymin: 51.20833   xmax: -61.09936   ymax: 83.21723
asia <- sf::st_read(file.path(path, "hydrosheds-raw", "hybas_as_lev00_v1c.shp"))
# xmin: 57.60833   ymin: 1.166667   xmax: 150.9215    ymax: 55.9375
oceania <- sf::st_read(file.path(path, "hydrosheds-raw", "hybas_au_lev00_v1c.shp"))
# xmin: 94.97022   ymin: -55.11667  xmax: 180.0006    ymax: 24.30053
greenland <- sf::st_read(file.path(path, "hydrosheds-raw", "hybas_gr_lev00_v1c.shp"))
# xmin: -73.00067  ymin: 59.74167   xmax: -11.34932   ymax: 83.62564
north_am <- sf::st_read(file.path(path, "hydrosheds-raw", "hybas_na_lev00_v1c.shp"))
# xmin: -137.9625  ymin: 5.495833   xmax: -52.61605   ymax: 62.74232
south_am <- sf::st_read(file.path(path, "hydrosheds-raw", "hybas_sa_lev00_v1c.shp"))
# xmin: -92.00068  ymin: -55.9875   xmax: -32.37453   ymax: 14.88273
siberia <- sf::st_read(file.path(path, "hydrosheds-raw", "hybas_si_lev00_v1c.shp"))
# xmin: 58.95833   ymin: 45.5625    xmax: 180         ymax: 81.26735
africa <- sf::st_read(file.path(path, "hydrosheds-raw", "hybas_af_lev00_v1c.shp"))
# xmin: -18.1631   ymin: 54.5381    xmax: -34.8370    ymax: 37.5631
europe <- sf::st_read(file.path(path, "hydrosheds-raw", "hybas_eu_lev00_v1c.shp"))
# xmin: -24.5423   ymin: 69.5545    xmax: 12.5913     ymax: 81.8589

# Antarctica is not supported by this product so we just want everything else
# (the minimum latitude is -55.2° for any of the slices)

# Examine structure of one for greater detail
str(arctic)
# page 6 of the technical documentation contains an attribute table that defines these fields
# PFAF_[number] refers to the level of specificity in the basin delineation
## PFAF_1 = separates continents from one another
## PFAF_# + N = progressively finer separation

# Bind our files into a single (admittedly giant) object
all_basins <- rbind(arctic, asia, oceania, greenland, north_am,
                    south_am, siberia, africa, europe)

# For ease of manipulation get just the HYBAS_ID
# These uniquely identify the most specific level so they work upstream too (no pun intended)
basin_simp <- all_basins %>%
  dplyr::select(HYBAS_ID, NEXT_DOWN, NEXT_SINK, SUB_AREA)

# Re-check structure
str(basin_simp)

## ---------------------------------------------- ##
          # Identify Focal Polygon ----
## ---------------------------------------------- ##

# Pre-emptively resolve an error with 'invalid spherical geometry'
sf::sf_use_s2(F)
## s2 processing assumes that two points lie on a sphere
## earlier form of processing assumes two points lie on a plane

# Pull out HYBAS_IDs at site coordinates
sites_actual <- sites_spatial %>%
  dplyr::mutate(
    # Find the interaction points as integers
    ixn = as.integer(st_intersects(geometry, basin_simp)),
    # If a given interaction is not NA (meaning it does overlap)...
    HYBAS_ID = ifelse(test = !is.na(ixn),
                      # ...retain the HYBAS_ID of that interaction...
                      yes = basin_simp$HYBAS_ID[ixn],
                      #...if not, retain nothing
                      no = '') )

# Check it out
sites_actual

# And to make our lives easier, check out which continents we actually need
sort(unique(stringr::str_sub(sites_actual$HYBAS_ID, 1, 1)))
# 1 = Africa; 2 = Europe; 3 = Siberia; 4 = Asia; 5 = Australia; 6 = South America; 7 = North America; 8 = Arctic (North America); 9 = Greenland 

# Prepare only needed HydroSheds 'continents'
basin_needs <- rbind(europe, siberia, north_am, arctic)

# Clean up environment to have less data stored as we move forward
rm(list = setdiff(ls(), c('path', 'sites', 'sites_actual', 'basin_needs')))

## ---------------------------------------------- ##
            # Get Pfafstetter Codes ----
## ---------------------------------------------- ##

# Bring each PFAF code into the sites_actual object by matching with HYBAS_ID
for(i in 1:12) {
  # Processing message
  message("Processing Pfafstetter code level ", i)
  
  # Grab each PFAF code and add it to the "sites_actual" object as a column
  sites_actual[[paste0("PFAF_", i)]] <- basin_needs[[paste0("PFAF_", i)]][match(sites_actual$HYBAS_ID, basin_needs$HYBAS_ID)]}

# Also grab area
sites_actual$SUB_AREA <- basin_needs$SUB_AREA[match(sites_actual$HYBAS_ID, basin_needs$HYBAS_ID)]

# Check the object again
dplyr::glimpse(sites_actual)
# This object has polygons defined at the finest possible level
# We may want to visualize aggregated basins so let's go that direction now

## ---------------------------------------------- ##
      # Load Modified HydroSHEDS Functions ----
## ---------------------------------------------- ##

# These are modified from someone's GitHub functions to accept non-S4 objects
# Link to originals here: https://rdrr.io/github/ECCC-MSC/Basin-Delineation/

# First function finds just the next upstream polygon(s)
find_next_up <- function(HYBAS, HYBAS.ID, ignore.endorheic = F){
  
  # Process sf object into a regular dataframe
  HYBAS_df <- HYBAS %>%
    sf::st_drop_geometry()
  
  # Find next upstream polygon(s) as character
  upstream.ab <- HYBAS_df[HYBAS_df$NEXT_DOWN == HYBAS.ID, c("HYBAS_ID", "ENDO")]
  
  if (ignore.endorheic){
    upstream.ab <- upstream.ab[upstream.ab$ENDO != 2, ]
  }
  return(as.character(upstream.ab$HYBAS_ID))
}

# Second function iteratively runs through the first one to find all of the upstream polygons
find_all_up <- function(HYBAS, HYBAS.ID, ignore.endorheic = F, split = F){
  
  # make containers
  HYBAS.ID <- as.character(HYBAS.ID)
  HYBAS.ID.master <- list()
  
  #Get the possible upstream 'branches'
  direct.upstream <- find_next_up(HYBAS = HYBAS, HYBAS.ID = HYBAS.ID,
                                  ignore.endorheic = ignore.endorheic)
  
  # for each branch iterate upstream until only returning empty results
  for (i in direct.upstream){
    run <- T
    HYBAS.ID.list <- i
    sub.basins <- i # this is the object that gets passed to find_next_up in each iteration
    while (run){
      result.i <- unlist(lapply(sub.basins, find_next_up,
                                HYBAS = HYBAS, ignore.endorheic = ignore.endorheic))
      
      if (length(result.i) == 0){ run <- F } # Stopping criterion
      HYBAS.ID.list <- c(HYBAS.ID.list, result.i)
      sub.basins <- result.i
    }
    HYBAS.ID.master[[i]] <- HYBAS.ID.list
  }
  
  if (!split){ HYBAS.ID.master <- as.character(unlist(HYBAS.ID.master)) }
  
  return(HYBAS.ID.master)
}

## ---------------------------------------------- ##
          # Identify Upstream Polygons ----
## ---------------------------------------------- ##

# Because some sites fall into the same focal HydroSHEDS polygon,
# It makes sense to identify areas / polygons based on that focal polygon
# rather than stream name to save on computation time

# Create an empty list
id_list <- list()

for(focal_poly in unique(sites_actual$HYBAS_ID)){
# for(focal_poly in "7000073120"){
  
  # Create/identify name and path of file
  poly_file <- file.path(path, 'hydrosheds-basin-ids',
                         paste0(focal_poly, '_Upstream_IDs.csv'))
  
  # If we've already found this polygon's upstream polygons:
  if (fs::file_exists(poly_file) == TRUE) {
    
    # Read the CSV
    hydro_df <- read.csv(file = poly_file)
    
    # Add to the list
    id_list[[focal_poly]] <- hydro_df
    
    # Message outcome
    message("Upstream HydroSheds IDs for HYBAS ID '", focal_poly, "' already identified.")
    
    # If we *don't* have the polygon, continue!
  } else {
  
  # Print start-up message
  message( "Processing for HYBAS ID '", focal_poly, "' begun at ", Sys.time())
  
  # Identify all upstream shapes
  fxn_out <- find_all_up(HYBAS = basin_needs, HYBAS.ID = focal_poly)
  
  # Make a dataframe of this
  hydro_df <- data.frame(focal_poly = rep(focal_poly, (length(fxn_out) + 1)),
                         hybas_id = c(focal_poly, fxn_out))
  
  # Save this out
  write.csv(x = hydro_df, file = poly_file, na = '', row.names = F)
  
  # And add it to the list
  id_list[[focal_poly]] <- hydro_df
  
  # Print finishing message
  message( "Processing for HYBAS ID '", focal_poly, "' finished at ", Sys.time())
  
  } # Close `else` clause
} # Close `loop`

# Unlist the list
hydro_out <- id_list %>%
  purrr::map_dfr(dplyr::select, dplyr::everything())

# Check the structure
str(hydro_out)




# HERE NOW ----




## ---------------------------------------------- ##
              # Wrangle Output -----
## ---------------------------------------------- ##



# Pre-emptively resolve an error with 'invalid spherical geometry'
sf::sf_use_s2(F)
## s2 processing assumes that two points lie on a sphere
## earlier form of processing assumes two points lie on a plane

# Strip the polygons that correspond to those IDs
hydro_poly <- hydro_out %>%
  # Make the HydroBasins ID column have an identical name between the df and sf objects
  dplyr::rename(HYBAS_ID = hybas_id) %>%
  # Attach everything in the polygon variant
  ## Necessary because of some polygons are found in >1 uniqueID
  dplyr::left_join(basin_needs, by = 'HYBAS_ID') %>%
  # Within uniqueID...
  dplyr::group_by(uniqueID) %>%
  # ...sum sub-polygon areas and combine sub-polygon geometries
  dplyr::summarise(drainSqKm = sum(SUB_AREA, na.rm = TRUE),
                   geometry = sf::st_union(geometry)) %>%
  # Need to make the class officially sf again before continuing
  sf::st_as_sf() %>%
  # Then eliminate any small gaps within those shapes
  # nngeo::st_remove_holes() %>%
  ## Throws error: `Error in tmp[j][[1]] : subscript out of bounds`
  # Retrieve the domain and stream names
  tidyr::separate(col = uniqueID, into = c("domain", "stream"), sep = "_",
                  remove = F, fill = "right")

# Check structure
str(hydro_poly)

# Write this out for later use (though as a dataframe)
hydro_poly_df <- sf::st_drop_geometry(x = hydro_poly)

# Check it again
str(hydro_poly_df)

# Export this for later use as a CSV
write.csv(hydro_poly_df, file = file.path(path, 'watershed_areas.csv'),
          row.names = F, na = "")

# Now upload this as well to the GoogleDrive
# googledrive::drive_upload(media = file.path(path, 'watershed_areas.csv'),
#                           path = as_id("https://drive.google.com/drive/folders/1HQtpWYoq_YQwj_bDNNbv8D-0swi00o_s"))

# End ----
