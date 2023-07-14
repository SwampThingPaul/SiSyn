
# From poles to tropics: A multi-biome synthesis investigating the controls on river Si exports

- Primary Investigators: Joanna Carey & Kathi Jo Jankowski
- [Project
  Summary](https://lternet.edu/working-groups/river-si-exports/)
- [Participant Information](https://www.nceas.ucsb.edu/projects/12816)

> Riverine exports of silicon (Si) directly influence global carbon (C)
> cycling through the growth of diatoms, ubiquitous autotrophs in marine
> and freshwater systems, which account for ~25% of global primary
> production. Rivers play essential roles in processing and supplying
> the Si necessary for diatom growth, but we have limited knowledge of
> the controls on river Si exports, especially how they vary across
> biomes. Prior work has shown conflicting importance of various
> drivers, such as lithology, riverine productivity, and terrestrial
> vegetation in controlling river Si exports. Capturing a baseline
> understanding of how these factors influence Si exports across biomes
> is essential for understanding freshwater and marine C cycles,
> especially during this period of rapid climatic warming. This
> synthesis will answer three specific research questions related to the
> roles of 1) terrestrial vegetation, 2) river productivity and 3)
> climate warming in controlling river Si exports across biomes. Our
> proposed sites span the globe (e.g., Antarctic, tropical, temperate,
> boreal, alpine, Arctic systems), and present a unique cross-network
> opportunity to connect LTER-based research with that of the Critical
> Zone Observatory and USGS. Together, we will create the first
> data-driven predictive framework of how riverine Si exports will
> respond to global change.

------------------------------------------------------------------------

### Table of Contents

- `./ConcentrationDischarge/` - CQ function and examples (PJ)
- `./DataInventory/` - Biogeochemical inventory data combination (PJ)
- `./Discharge at WRTDS sites/` -
- `./Discharge/` -
- `./Loadflex/` -
- `./Merge Site Discharge/` -
- `./WRTDS/` -
- `./patrick/` -

## Repository Content

See below for a “tree” of this repository’s content:

``` r
supportR::github_tree(repo = "https://github.com/SwampThingPaul/SiSyn",
                      exclude = c("DataInventory", "Data", "Si-Q long term change",
                                  "Loadflex", "Merge Site Discharge", "ConcentrationDischarge",
                                  "Archive"))
```

    ##                                            levelName
    ## 1  .                                                
    ## 2   ¦--.gitattributes                               
    ## 3   ¦--.gitignore                                   
    ## 4   ¦--20210524_masterdata.csv                      
    ## 5   ¦--CQ_Q_Trend.R                                 
    ## 6   ¦--ChangePointSiSyn.R                           
    ## 7   ¦--Conc_Yield_Q_Biome_Figs.R                    
    ## 8   ¦--ConcentrationDischarge                       
    ## 9   ¦   °--75 excluded items                        
    ## 10  ¦--DataInventory                                
    ## 11  ¦   °--105 excluded items                       
    ## 12  ¦--Discharge                                    
    ## 13  ¦   ¦--Discharge.Rproj                          
    ## 14  ¦   ¦--LTER_discharge_summary.R                 
    ## 15  ¦   ¦--MergeSiteQ_AllFiles.R                    
    ## 16  ¦   ¦--NWT_Discharge_Prep.R                     
    ## 17  ¦   ¦--Q_Interpolation.R                        
    ## 18  ¦   ¦--SiSyn_LTER_Qsummary_7June2021.csv        
    ## 19  ¦   ¦--SiSyn_LTER_sitesummary_7June2021.csv     
    ## 20  ¦   ¦--WRTDS_discharge_allsites_21Apr21.csv     
    ## 21  ¦   °--discharge_fill.R                         
    ## 22  ¦--Discharge at WRTDS sites                     
    ## 23  ¦   ¦--Discharge at WRTDS sites.Rproj           
    ## 24  ¦   ¦--DischargePlots_AllSites.R                
    ## 25  ¦   °--WRTDS_discharge_allsites.csv             
    ## 26  ¦--Figure7_SeasonalPlots.R                      
    ## 27  ¦--LICENSE                                      
    ## 28  ¦--LTER- SiSyn.url                              
    ## 29  ¦--Loadflex                                     
    ## 30  ¦   °--113 excluded items                       
    ## 31  ¦--MDL_master_creation.R                        
    ## 32  ¦--Merge Site Discharge                         
    ## 33  ¦   °--39 excluded items                        
    ## 34  ¦--MixedEffectsModel_UPDATED.R                  
    ## 35  ¦--Monthly_Annual_Comparison.R                  
    ## 36  ¦--PCA_RDA_analysis                             
    ## 37  ¦   ¦--Analysis.md                              
    ## 38  ¦   ¦--Figure 3b RDA.R                          
    ## 39  ¦   ¦--RandomForestRegression.R                 
    ## 40  ¦   ¦--SiSynRDA.R                               
    ## 41  ¦   °--SiSyn_PCA.R                              
    ## 42  ¦--README.md                                    
    ## 43  ¦--Seasonality                                  
    ## 44  ¦   ¦--DTW.R                                    
    ## 45  ¦   ¦--DTW_annual.R                             
    ## 46  ¦   ¦--IntermitentFlowInterpolation.R           
    ## 47  ¦   °--PORComparison_Q_Si.R                     
    ## 48  ¦--Sept2022SitesClean                           
    ## 49  ¦   ¦--CAMELSChem_SiQCheck.R                    
    ## 50  ¦   ¦--CreateMacroShedsInfo.R                   
    ## 51  ¦   ¦--DownloadNewKyrcklanDischarge.R           
    ## 52  ¦   ¦--MacroSheds_Download_Si.R                 
    ## 53  ¦   °--ParseComoNEON.R                          
    ## 54  ¦--SiSyn.Rproj                                  
    ## 55  ¦--Si_monthly_graphs.R                          
    ## 56  ¦--Si_monthly_synthesis.R                       
    ## 57  ¦--Site_Information                             
    ## 58  ¦   ¦--KoeppenGeigerClassification.R            
    ## 59  ¦   °--WRTDS_Reference_Table.xlsx               
    ## 60  ¦--StandardizeSiFiles.R                         
    ## 61  ¦--WRTDS                                        
    ## 62  ¦   ¦--Archive                                  
    ## 63  ¦   ¦   °--24 excluded items                    
    ## 64  ¦   ¦--README.md                                
    ## 65  ¦   ¦--WRTDS_step-1_find-areas.R                
    ## 66  ¦   ¦--WRTDS_step-2_wrangling.R                 
    ## 67  ¦   ¦--WRTDS_step-3B_bootstrap.R                
    ## 68  ¦   ¦--WRTDS_step-3_analysis.R                  
    ## 69  ¦   ¦--WRTDS_step-4_results_report.R            
    ## 70  ¦   °--explore_window.R                         
    ## 71  °--patrick                                      
    ## 72      ¦--Analytes by site.Rmd                     
    ## 73      ¦--Analytes-by-site.html                    
    ## 74      ¦--DSi and Si_N_P stoich over time.Rmd      
    ## 75      ¦--DSi-and-Si_N_P-stoich-over-time.html     
    ## 76      ¦--DSi_WRTDS_effect_sizes.Rmd               
    ## 77      ¦--DSi_WRTDS_effect_sizes.html              
    ## 78      ¦--SiSyn data exploration nov 11 data.Rmd   
    ## 79      ¦--SiSyn-data-exploration-nov-11-data.html  
    ## 80      ¦--SiSyn_forestplots_Nov2021.Rmd            
    ## 81      ¦--SiSyn_forestplots_Nov2021.html           
    ## 82      ¦--Si_N_P-stoich-over-time.html             
    ## 83      ¦--Summary analysis for Jan 2021 meeting.Rmd
    ## 84      ¦--index.Rmd                                
    ## 85      ¦--index.html                               
    ## 86      ¦--readme                                   
    ## 87      ¦--seasonality stuff.Rmd                    
    ## 88      ¦--seasonality-stuff.html                   
    ## 89      °--sisync data exploration.Rmd

## Related Repositories

This working group has several repositories. All are linked and
described (briefly) below.

- [lter/**lterwg-silica-data**](https://github.com/lter/lterwg-silica-data) -
  Primary data wrangling / tidying repository for “master” data files
- [SwampThingPaul/**SiSyn**](https://github.com/SwampThingPaul/SiSyn) -
  Original repository for this working group. Performs many functions
  from data wrangling through analysis and figure creation
- [lsethna/**NCEAS_SiSyn_CQ**](https://github.com/lsethna/NCEAS_SiSyn_CQ) -
  Examples concentration (C) and discharge (Q) relationships for a wide
  range of solutes
- [lter/**lterwg-silica-spatial**](https://github.com/lter/lterwg-silica-spatial) -
  Extracts spatial and climatic information from within watershed
  shapefiles
- [njlyon0/**lter_silica-cryosphere**](https://github.com/njlyon0/lter_silica-cryosphere) -
  Performs analysis and visualization for the cryosphere manuscript
