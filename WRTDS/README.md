# Weighted Regressions on Time, Discharge, and Season (WRTDS) Scripts

The WRTDS workflow includes a substantial amount of code that--while broadly linked--meets relatively disparate needs. For ease of maintenance, the scripts included in this folder are described below:

## Core WRTDS Scripts

### **WRTDS_step-1_find-areas.R**

 - When to Use: You want to add new rivers to the WRTDS workflow that previously have not been included. Otherwise, skip this script and move to the next!
 - Description: Accepts the living [GoogleSheet reference table](https://docs.google.com/spreadsheets/d/11t9YYTzN_T12VAQhHuY5TpVjGS50ymNmKznJK4rKTIU/edit#gid=357814834) that connects each stream with its name(s) in the chemistry and discharge data files. Identifies the drainage basin area (in square kilometers) for all sites where that information is not included in the reference table. Uploads the resulting reference table with areas to Google Drive.

### **WRTDS_step-2_wrangling.R**

 - When to Use: You just needed to re-run step 1 OR either the master chemistry or discharge files have been updated. Otherwise, skip this script and move to the next!
 - Explanation: Accepts the master discharge, master chemistry, and reference table with areas (see step 1) files and does all pre-WRTDS wrangling. Includes a "sabotage check" looking for any sites dropped by that wrangling.

### **WRTDS_step-3_analysis.R**

 - When to Use: You just needed to re-run step 1 or step 2 OR you've tweaked the actual WRTDS workflow and need to update the results. Otherwise, skip this script and move to the next!
 - Explanation: Runs WRTDS and exports outputs for later use. Includes separate workflows for different "types" of sites (i.e., those that require modifications away from default `EGRET` function settings).
- Step 3 also includes a separate workflow for handling bootstrapping due to how computationally intensive these `EGRET` functions are. Separating this from the rest of the workflow allows for quicker retrieval of "main" results and easier maintenance of both parts.

### **WRTDS_step-4_results-report.R**

 - When to Use: You've just re-run step 3 OR you want to generate new summary results CSVs/PDFs and upload them to the Google Drive.
 - Explanation: Reads in outputs of step 3 and creates one flat file for each data type for ease of downstream use. Uploads these to Google Drive. Also uploads `EGRET` PDFs to the Drive.

## Exploratory Scripts

### **explore_window.R**
 - Explanation: Uses some hand-picked rivers to test various "half windows in seasonal dimension" (see `?EGRET::runSeries` at `windowS` argument). Creates PDF reports with `HERON::egret_report` and uploads to an experimental results Drive folder.
