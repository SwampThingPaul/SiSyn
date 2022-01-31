#install.packages("googledrive")
#install.packages("tidyverse")
#install.packages("data.table)
#install.packages("Kendall")
require(tidyverse)
require(googledrive)
require(stringr)
require(lubridate)
require(reshape)
require(gtools)
require(data.table)
require(dataRetrieval)
require(EGRET)
require(EGRETci)
require(ggpubr)
require(Kendall)


#get folder URL from google drive with discharge data
WRTDSfolder_url<-"https://drive.google.com/drive/folders/1s6irhuhH3qTdEi8gHRu5COFuHZyz4T9d?usp=sharing"

#get ID of folder
WRTDSfolder<-drive_get(as_id(WRTDSfolder_url))

#get list of csv files from folder
WRTDScsv_files<-drive_ls(WRTDSfolder, type="csv")

#extract only WRTDS files - there are some regular discharge files in the WRTDScsv_files dataframe
WRTDScsv_files_final<-WRTDScsv_files[WRTDScsv_files$name %like% "WRTDS.csv|INFO.csv",]

#create new column of just the site
WRTDScsv_files_final$files<-word(WRTDScsv_files_final$name, 1, sep = "_")

#setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#read in file of 20 year data
data_20yearQ<-read.csv("Data_years_streams_WRTDS.csv")

#create list of unique sites in this LTER
files20years<-unique(data_20yearQ$Stream.Site)

#filter the original csv file to include only sites from your LTER
RefTable<-subset(WRTDScsv_files_final, WRTDScsv_files_final$files %in% files20years)

#create list of unique files from this LTER
site_files<-unique(RefTable$name)

# for some reason "GSWS09_Si_WRTDS" was listed twice in google drive and wouldn't download, skipped it and did it manually..
site_files = site_files[1:length(site_files)]

#set wd to new folder where these files will be downloaded - WRTDS requires them to be stored locally
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_Prep_Si")

#download each file to the above specified folder
for (i in 2:length(site_files)) {
  
  drive_download(site_files[i], type = "csv", overwrite = TRUE)
  
}


#################################################################################
### start here if not downloading from Google Drive
## set up files for loop
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/SiPrepWRTDS_Updated")

#make list of all files that you just downloaded to local folder
WRTDS_files_List<-list.files(path = "/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/SiPrepWRTDS_Updated")

#remove sites with missing discharge
remove_these<-grep("Toolik Inlet|TW Weir", WRTDS_files_List)

WRTDS_files_List<-WRTDS_files_List[-c(remove_these)]

#make list of only INFO files
WRTDS_files_List_Info<-WRTDS_files_List_Info[WRTDS_files_List_Info %like% "INFO"]

#make list of site names
WRTDS_files<-sub("*_INFO.csv", "", WRTDS_files_List_Info)

WRTDS_files_Q<-sub("*_Q_WRTDS.csv", "", WRTDS_files_List_Q)

remove_these_files<-setdiff(WRTDS_files_Q, WRTDS_files)

matches <- paste(remove_these_files,collapse="|")

#make list of only Q files
WRTDS_files_List_Q<-WRTDS_files_List[WRTDS_files_List %like% "Q_WRTDS"]

WRTDS_files_List_Q<-WRTDS_files_List_Q[!WRTDS_files_List_Q %like% matches]

#make list of only Si files
WRTDS_files_List_Si<-WRTDS_files_List[WRTDS_files_List %like% "Si"]

WRTDS_files_List_Si<-WRTDS_files_List_Si[!WRTDS_files_List_Si %like% matches]
WRTDS_files_List_Si<-WRTDS_files_List_Si[!WRTDS_files_List_Si %like% ".zip"]

WRTDS_files_List_Info<-list.files(path = "/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_Prep_Si")

#make list of only INFO files
WRTDS_files_List_Info<-WRTDS_files_List_Info[WRTDS_files_List_Info %like% "INFO"]

#make list of site names
WRTDS_files<-sub("*_INFO.csv", "", WRTDS_files_List_Info)

#make list of months
month<-seq(1,12,1)

#run WRTDS! output will be saved to the same file
for (i in 2:length(WRTDS_files)) {
  
  #read in Q file
  Daily<-readUserDaily("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/SiPrepWRTDS_Updated", WRTDS_files_List_Q[i],
                       qUnit = 2)
  
  #read in Si file
  Sample<-readUserSample("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/SiPrepWRTDS_Updated", WRTDS_files_List_Si[i])
  
  #read in Info file
  Info<-readUserInfo("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_Prep_Si", WRTDS_files_List_Info[i])
  
  #remove duplicates from sample file
  Sample<-removeDuplicates(Sample)
  
  Sample<-aggregate(Sample, by=list(Sample$Date), mean)
  
  Sample<-Sample[,-c(1)]
  
  #merge into eList
  eList_original<-mergeReport(Info, Daily, Sample)
  
  #save workspace so it can be accessed later
  savePath<-"/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/SiPrepWRTDS_Updated/workspaces/"
  saveResults(savePath, eList_original)
  
  #estimate continuous Si
  eList<-modelEstimation(eList_original, minNumObs=50)
  
  for (k in 1:length(month)) {
    
    # For Sagehen Site
    #eList <- blankTime(eList, startBlank = "1996-01-01", endBlank = "2001-01-01")
    
    #set PA to one month from month list, run for only that month (paLong = 1)
    eList <- setPA(eList, paStart=month[k], paLong=1)
    
    #write output to this folder
    setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/SiPrepWRTDS_Updated/monthly/")
    
    #extract continuous Si file from eList
    ContConc<-eList$Daily
    
    #write csv of continuous Si data
    write.csv(ContConc, paste0(WRTDS_files[i], month[k], "_ContSi_WRTDS.csv"))
    
    #average yearly stats
    Results<-tableResults(eList)
    
    #write csv of results dataframe
    write.csv(Results, paste0(WRTDS_files[i], month[k], "_ResultsTable_WRTDS.csv"))
    
    #make new column for year
    ContConc$Year<-format(as.Date(ContConc$Date), "%Y")
    
    #find min year
    minYP<-as.numeric(min(ContConc$Year))+1
    
    #find max year
    maxYP<-as.numeric(max(ContConc$Year))-1
    
    #set year points for 
    yearPoints<-c(minYP, maxYP)
    
    #calculate concentration trend
    Conc<-tableChangeSingle(eList, fluxUnit = 8, yearPoints)
    
    #calculate flux trend
    Flux<-tableChangeSingle(eList, fluxUnit = 8, yearPoints, flux = TRUE)
    
    #bind into one dataframe
    Trends<-cbind(Conc, Flux)
    
    #write csv of trends dataframe
    write.csv(Trends, paste0(WRTDS_files[i], month[k], "_TrendsTable_WRTDS.csv"))
    
  }
}

#list all "ResultsTable" files from monthly results run in WRTDS
monthly_results<-list.files(path = "/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/SiPrepWRTDS_Updated/monthly",
                            pattern = "ResultsTable_WRTDS.csv")

#remove kolyma and mackenzie from WRTDS_files and monthly_results lists
# kolyma<-grep("Kolyma", monthly_results)
# mackenzie<-grep("Mackenzie", monthly_results)
# 
# kolymaWRTDS<-grep("Kolyma", WRTDS_files)
# 
# mackenzieWRTDS<-grep("Mackenzie", WRTDS_files)
# 
# WRTDS_files<-WRTDS_files[-c(kolymaWRTDS, mackenzieWRTDS)]
# 
# monthly_results<-monthly_results[-c(kolyma, mackenzie)]

#read in sites used for WRTDS
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

sites<-read.csv("Data_years_streams_WRTDS.csv")

#remove sites that did not get run through WRTDS
LTER_remove_these<-grep("Toolik Inlet|TW Weir", sites$Stream.Site)

sites<-sites[-c(LTER_remove_these),]

#create list of LTERs
LTER_list<-unique(sites$LTER)

#set wd for pdf to be saved to
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/WRTDS_Prep_Si/monthly")

for (q in 1:length(LTER_list)) {
  
  sites_LTER<-subset(sites, sites$LTER==LTER_list[[q]])
  
  sites_LTER_list<-sites_LTER$Stream.Site
  
  monthly_result_LTER_nums<-which(monthly_results %like% paste0(sites_LTER_list, collapse = "|"))
  
  monthly_results_LTER<-monthly_results[monthly_result_LTER_nums]
  
  pdf(paste0("MonthlyTrends", LTER_list[q], ".pdf"), width = 20)
  
  #for each site
  for (i in 1:length(sites_LTER_list)) {
    
    #create list of file numbers from one site
    site_data_nums<-grep(sites_LTER_list[i], monthly_results)
    
    #make sure only 12 files (bc 12 months); some sites have names of other sites and therefore included other sites
    site_data_nums<-site_data_nums[1:12]
    
    #files monthly_results list to only include site files wanted
    site_data<-monthly_results[site_data_nums]
    
    #run this ONLY for MCM sites
    site_data<-site_data[c(1,4)]
    
    #open list for concentration and flux plots
    conc<-list()
    plot_list<-list()
    
    #create list of month names for all LTER except MCM
    months<-month.name
    
    #months list for MCM sites
    #months<-c("January", "December")
    
    #for each month
    for (k in 1:length(site_data)) {
      
      #read in data file
      results<-read.csv(site_data[k])
      
      #create concentration graph
      conc<-ggplot(results)+geom_point(aes(Year, Conc..mg.L.), col="blue")+
        geom_line(mapping = aes(Year, FN.Conc..mg.L.), col="blue")+
        ggtitle(paste(months[k], "Concentration"))
      
      #create flux graph
      flux<-ggplot(results)+geom_point(mapping = aes(Year, Flux..10.6kg.yr.), col="red")+
        geom_line(mapping = aes(Year, FN.Flux..10.6kg.yr.), col="red")+ggtitle(paste(months[k], "Flux"))
      
      #use ggarrange to put concentration and flux graph next to each other, assign this dual-plot to list
      plot_list[[k]]<-ggarrange(conc, flux)
      
    }
    
    #use ggarrage to put all conc/flux plots of months for one site on same page
    plot1<-ggarrange(plotlist = plot_list)
    plot1<-annotate_figure(plot1, top = text_grob(paste(sites_LTER_list[i])))
    print(plot1)
    
  }
  
  dev.off()
  
}


#####for one master file of plots

#open PDF
pdf("MonthlyTrendsREVISED.pdf", width = 20)

#for each site
for (i in 1:length(WRTDS_files)) {
  
  #create list of file numbers from one site
  site_data_nums<-grep(WRTDS_files[i], monthly_results)
  
  #make sure only 12 files (bc 12 months); some sites have names of other sites and therefore included other sites
  site_data_nums<-site_data_nums[1:12]
  
  #files monthly_results list to only include site files wanted
  site_data<-monthly_results[site_data_nums]
  
  #open list for concentration and flux plots
  conc<-list()
  plot_list<-list()
  
  #create list of month names
  months<-month.name
  
  #for each month
  for (k in 1:length(site_data)) {
    
    #read in data file
    results<-read.csv(site_data[k])
    
    #create concentration graph
    conc<-ggplot(results)+geom_point(aes(Year, Conc..mg.L.), col="blue")+
      geom_line(mapping = aes(Year, FN.Conc..mg.L.), col="blue")+
      ggtitle(paste(months[k], "Concentration"))
    
    #create flux graph
    flux<-ggplot(results)+geom_point(mapping = aes(Year, Flux..10.6kg.yr.), col="red")+
      geom_line(mapping = aes(Year, FN.Flux..10.6kg.yr.), col="red")+ggtitle(paste(months[k], "Flux"))
    
    #use ggarrange to put concentration and flux graph next to each other, assign this dual-plot to list
    plot_list[[k]]<-ggarrange(conc, flux)
    
  }
  
  #use ggarrage to put all conc/flux plots of months for one site on same page
  plot1<-ggarrange(plotlist = plot_list)
  plot1<-annotate_figure(plot1, top = text_grob(paste(WRTDS_files[i])))
  print(plot1)
  
}

#close pdf
dev.off()