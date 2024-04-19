require(googledrive)
require(readxl)
require(dataRetrieval)
library(EGRET)
library(purrr)
library(data.table)
library(magrittr)

#list parameter codes
#DOC - 00681 - mg/L
#Ca - 00915 - mg/L
#Mg - 00925 - mg/L
#Na - 00930 - mg/L
#K - 00935 - mg/L
#Cl - 00940 - mg/L
#SO4 - 00945 - mg/L

params<-c("00681", "00915", "00925", "00930", "00935", "00940", "00945")

solutes<-c("DOC", "Ca", "Mg", "Na", "K", "Cl", "SO4")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

drivers_url<-"https://docs.google.com/spreadsheets/d/11t9YYTzN_T12VAQhHuY5TpVjGS50ymNmKznJK4rKTIU/edit?usp=sharing"

file_get<-drive_get(as_id(drivers_url))

drive_download(file_get$drive_resource, overwrite = T)

ref_table<-read_xlsx("Site_Reference_Table.xlsx")

ref_table_USGS<-subset(ref_table, ref_table$LTER=="USGS")

site_list<-c(unique(ref_table_USGS$USGSGageNumber))

site_list<-ifelse(nchar(site_list)==8, site_list, paste0("0", site_list))

all_sites<-list()

for (i in 1:length(site_list)) {
  
  print(i)
  
  all_chem<-readNWISqw(site_list[i], parameterCd = params)
  
  all_sites[[i]]<-all_chem
  
}

all_sites_chem<-bind_rows(all_sites)

all_sites<-all_sites[which(lapply(all_sites, nrow) != 0)]

all_sites_chem<-all_sites %>% map_dfr(`[`, c("agency_cd", "site_no", "sample_dt", "parm_cd", "remark_cd", "result_va"))

colnames(all_sites_chem)<-c("agency", "gage_number", "date", "parameter_code", "remark", "value")

sol_par_df<-data.table(params, solutes)
colnames(sol_par_df)[1]<-"parameter_code"

all_sites_chem<-merge(all_sites_chem, sol_par_df, by="parameter_code")

all_sites_chem %>%
  group_by(solutes) %>%
  summarize(count = n_distinct(gage_number))

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

write.csv(all_sites_chem, "USGS_geogenic_solutes.csv")

all_sites_chem$gage_number<-as.numeric(all_sites_chem$gage_number)
colnames(all_sites_chem)[3]<-"USGSGageNumber"

all_sites_chem_names<-merge(ref_table_USGS[,c(3,8)], all_sites_chem, by="USGSGageNumber")

write.csv(all_sites_chem_names, "USGS_geogenic_solutes.csv")
