

folder_url="https://drive.google.com/drive/folders/1GhLFyyH5LDUQwKINutEAGSKWkR8QWFUA"

#get ID of folder
folder = drive_get(as_id(folder_url))

#get list of csv files from folder
Q_files = drive_ls(folder, type="csv")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/RawData_2023/")

for (i in 1:nrow(Q_files)) {
  
  drive_download(Q_files$drive_resource[i],  overwrite=T)
  
}

file_names<-list.files(path = ".", pattern = "mcmlter")

q_list<-list()

for (i in 1:length(file_names)) {
  
  
  q_list[[i]]<-read.csv(file_names[i])
  
  
}

q_df_all<-bind_rows(q_list)

write.csv(q_df_all, "MCM_Q.csv")

path<-"/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/RawData_2023/2023_MCMdata_allstreamschem.xlsx"

sheets <- readxl::excel_sheets(path)
tibble <- lapply(sheets, function(x) readxl::read_excel(path, sheet = x))

data_frame <- lapply(tibble, as.data.frame)

stnd_colnames<-colnames(data_frame[[1]])

data_frame <- lapply(data_frame, setNames, stnd_colnames)

chem_df<-bind_rows(data_frame)

write.csv(chem_df, "MCM_Chem.csv")

