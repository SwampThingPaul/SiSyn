# Weighted Regressions on Time, Discharge, and Season (WRTDS) Scripts

- **WRTDS_calc_watershed_area.R**: Calculates drainage basin areas (in square km) for streams where that is not known from other sources. This is necessary for WRTDS because WRTDS requires drainage basin area in the "info" file it demands

- **WRTDS_workflow.R**: Core workflow for doing WRTDS on a local machine. Prepares master chemistry and discharge data and runs through the WRTDS workflow (see `EGRET` R package)

- **WRTDS_server_workflow.R**: Similar to "WRTDS_workflow.R" but file paths are built to read from / export to the "shares" folder of NCEAS' Aurora server

- **WRTDS_source_data_check.R**: The chemistry data, discharge data, and information data have different names for "streams" that do not always match. This file matches the various stream names that can be matched across data types and identifies which streams are missing in at least one of those data types.

- **WRTDS_trends_Results.R**: Legacy script for summarizing / interpreting the old WRTDS workflow outputs (see `git` commit history for the speciifcs of these scripts). When we finish the fundamentals of the "WRTDS_workflow" and "WRTDS_server_workflow" scripts we will create a new version of this script to match those
