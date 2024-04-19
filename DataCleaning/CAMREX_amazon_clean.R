#load necessary packages
librarian::shelf(readxl, readr, dplyr, tidyverse, supportR)

#import data
CAMREX_silica_data = read_excel("CAMREX silica data.xlsx", col_names = FALSE)
#import data template
SiSyn_DataTemplate_V1 = read_excel("SiSyn_DataTemplate_V1.xlsx")

#for CAMREX data - need to separate chemistry data from metadata and site data
CAMREX_chem_v1 = CAMREX_silica_data[85:299,]
colnames(CAMREX_chem_v1) = CAMREX_silica_data[84,]

CAMREX_meta = CAMREX_silica_data[16:34,1:4]
colnames(CAMREX_meta) = CAMREX_silica_data[14,1:4]

CAMREX_sites = CAMREX_silica_data[61:78,1:5]
colnames(CAMREX_sites) = CAMREX_silica_data[60,1:5] 

#merge site data with chem data
#first rename site column
CAMREX_chem_v1 = CAMREX_chem_v1 %>% rename(Abbreviation = Station)
CAMREX_chem_v2 = merge(CAMREX_chem_v1, CAMREX_sites, all=T)

#replace all -999 with NA - per CAMREX metadata
CAMREX_chem_v2[CAMREX_chem_v2==-999] = NA
#fix dates
CAMREX_chem_v2$`Sampling Date` = as.Date(as.numeric(CAMREX_chem_v2$jd), origin=as.Date("1982-01-01"))

#import discharge
CAMREX_Q = read_csv("DISCHARGE.csv", col_names = FALSE, 
                    col_types = cols(X2 = col_character(), 
                                     X3 = col_character(), X4 = col_character(), 
                                     X5 = col_character(), X6 = col_character(), 
                                     X7 = col_character(), X8 = col_character(), 
                                     X9 = col_character(), X10 = col_character(), 
                                     X11 = col_character(), X12 = col_character(), 
                                     X13 = col_character(), X14 = col_character()), 
                    trim_ws = FALSE)

#separate discharge from header information
CAMREX_Q_v1 = CAMREX_Q[15:6219,2:14]
colnames(CAMREX_Q_v1) = CAMREX_Q[14,2:14]
colnames(CAMREX_Q_v1)[1] = "Sampling Date"

glimpse(CAMREX_Q_v1)
#change to long format
CAMREX_Q_v2 = pivot_longer(CAMREX_Q_v1, 2:13,names_to="Station Name", values_to="Q_cms")

#fix date
CAMREX_Q_v2$`Sampling Date` = as.Date(CAMREX_Q_v2$`Sampling Date`,format="%Y/%m/%d")

#check site names between chem and Q
diff_check(old=CAMREX_Q_v2$`Station Name`, new=CAMREX_chem_v2$`Station Name`)

#need to change Q names to match chem names
#mostly need to change "R" to "Rio"
CAMREX_Q_v2 = 
  CAMREX_Q_v2 %>%
  mutate(`Station Name` = case_when(`Station Name`=="R Ica"~"Rio Ica",
                                    `Station Name`=="R Japura"~"Rio Japura",
                                    `Station Name`=="R Jurua"~"Rio Jurua",
                                    `Station Name`=="R Jutai"~"Rio Jutai",
                                    `Station Name`=="R Madeira"~"Rio Madeira",
                                    `Station Name`=="R Negro"~"Rio Negro",
                                    `Station Name`=="R Purus"~"Rio Purus",
                                    TRUE~`Station Name`))


#export Q .csv file
glimpse(CAMREX_Q_v2)
write.csv(CAMREX_Q_v2,file="CAMREX_Q.csv")

#create CQ file for all CAMREX data
glimpse(CAMREX_chem_v2)
glimpse(CAMREX_Q_v2)
#Q file has daily discharge, keep all chem, merge available Q
CAMREX_CQ_v1 = merge(CAMREX_chem_v2,CAMREX_Q_v2,all.x=T)
glimpse(CAMREX_CQ_v1)

#get column names from template
template_columns = colnames(SiSyn_DataTemplate_V1)
template_columns
#Make a new df with columns matching template columns
CAMREX_data_template = data.frame(LTER = "Amazon",
                                  `Site/Stream Name` = CAMREX_CQ_v1$`Station Name`,
                                  `Sampling Date` = CAMREX_CQ_v1$`Sampling Date`,
                                  `Daily Avg Q (Discharge)`=CAMREX_CQ_v1$Q_cms,
                                  DSi=CAMREX_CQ_v1$Si,
                                  TN=CAMREX_CQ_v1$TDN, #not the same thing?
                                  NOx=CAMREX_CQ_v1$NO3,
                                  TP=CAMREX_CQ_v1$TDP, #also not the same thing
                                  PO4=CAMREX_CQ_v1$PO4,
                                  DOC=CAMREX_CQ_v1$DOC,
                                  TOC=CAMREX_CQ_v1$CPOC, #coarse and fine particulate available?
                                  alkalinity=CAMREX_CQ_v1$Alk,
                                  pH=CAMREX_CQ_v1$pH,
                                  Na=CAMREX_CQ_v1$Na,
                                  K=CAMREX_CQ_v1$K,
                                  Ca=CAMREX_CQ_v1$Ca,
                                  Mg=CAMREX_CQ_v1$Mg,
                                  Cl=CAMREX_CQ_v1$Cl)

write.csv(CAMREX_data_template, file="CAMREX_filled_template.csv")

#export metadata
glimpse(CAMREX_meta)
CAMREX_meta[20,]=list("Daily Avg Q (Discharge)","Q","cms",NA)
write.csv(CAMREX_meta,file="CAMREX_meta.csv")

#separate out each site to individual chem and Q files
sites = unique(CAMREX_sites$`Station Name`)

site_chem_dat = split(CAMREX_chem_v2, CAMREX_chem_v2$`Station Name`)
site_Q_dat = split(CAMREX_Q_v2, CAMREX_Q_v2$`Station Name`)

#write each element in list to .csv 
mapply(write.csv, site_Q_dat, paste0(names(site_Q_dat), "_Q.csv"))
