
# Note:
# Removed from "WRTDS_server_workflow.R" to optimize the non-bootstrapping part of that script
# This almost certainly won't work as-is but I need it out of the other script to work there
# Will return later to repair this and make it stand on its own
# ACTUAL CONTENT TO BE ADDED



# Run trend estimate for GFN method between start/end years
egret_pairs <- EGRET::runPairs(eList = egret_list_out, windowSide = 11, minNumObs = 50,
                               # year1 = min_year,
                               year1 = min(egret_list_out$Sample$waterYear, na.rm = T),
                               # year2 = max_year)
                               year2 = max(egret_list_out$Sample$waterYear, na.rm = T))

# Export those values as well
write.csv(x = egret_pairs, file.path(server_path, "WRTDS Outputs", paste0(out_prefix, "ListPairs_GFN_WRTDS.csv")), row.names = F, na = "")

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
write.csv(x = egret_boot_results, file.path(server_path, "WRTDS Outputs", paste0(out_prefix, "EGRETCi_GFN_bootstraps.csv")), row.names = F, na = "")

# Also grab the summary information
egret_boot_summary <- as.data.frame(egret_boot$bootOut)

# And export it as well
write.csv(x = egret_boot_summary, file.path(server_path, "WRTDS Outputs", paste0(out_prefix, "EGRETCi_GFN_Trend.csv")), row.names = F, na = "")
