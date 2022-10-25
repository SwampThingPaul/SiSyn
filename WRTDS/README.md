# Weighted Regressions on Time, Discharge, and Season (WRTDS) Scripts

The WRTDS workflow includes a substantial amount of code that--while broadly linked--meets relatively disparate needs. For ease of maintenance, the scripts included in this folder are described below and the order they should be run is explicitly included in the file names (e.g,, "...step-1...", etc.). See below for a script-by-script explanation.

- **WRTDS_step-1_find-areas.R**: Accepts the living [GoogleSheet reference table](https://docs.google.com/spreadsheets/d/11Noj6UnAliyA_R96c0a0nrDlukweJAW_O017E88DLBo/edit#gid=1340120887) that connects each stream with its name(s) in the various different data files. Identifies the drainage basin area (in square km) for all sites where that information is not included in the reference table. Uploads the resulting reference table with areas to GoogleDrive

- **WRTDS_step-2_wrangling.R**: Accepts the master discharge, master chemistry, and reference table with areas (see step 1) files and does all pre-WRTDS wrangling. Includes a "sanity check" looking for any sites lost by that wrangling

- **WRTDS_step-3_analysis.R**: Runs WRTDS and exports outputs for later use. Includes separate workflows for different "types" of sites (i.e., those that require modifications away from default `EGRET` function settings)

- **WRTDS_step-4_results-report.R**: Reads in outputs of step 3 and creates one flat file for each data type for ease of downstream use. Uploads these to GoogleDrive.

- **WRTDS_step-5_bootstrap.R**: Separate workflow for handling bootstrapping due to how computationally intensive these `EGRET` functions are. Separating this from step 3 allows quicker retrieval of "main" results while encouraging interested parties to run only this script if those are the data they are interested in.
