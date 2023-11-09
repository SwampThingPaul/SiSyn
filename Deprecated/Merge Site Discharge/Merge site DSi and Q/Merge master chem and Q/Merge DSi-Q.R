#import master chemistry and Q csv files (use WRTDS_import files)

#number of unique sites in chemistry and discharge file?
length(unique(WRTDS_input_chemistry$Stream_ID))
length(unique(WRTDS_input_discharge$Stream_ID))
#246

#merge DSi and Q data by site and date
unique(WRTDS_input_chemistry$variable)
WRTDS_input_DSi = subset(WRTDS_input_chemistry, WRTDS_input_chemistry$variable=="DSi")
length(unique(WRTDS_input_DSi$Stream_ID))

colnames(WRTDS_input_DSi)
colnames(WRTDS_input_discharge)

merged_DSi_Q = merge(WRTDS_input_DSi,WRTDS_input_discharge)
colnames(merged_DSi_Q)

#how many sites are in final merged dataframe
length(unique(merged_DSi_Q$Stream_ID))
#224
#some sites are in master chemistry file but don't have DSi

#export merged DSi-Q .csv
write.csv(merged_DSi_Q, file="WRTDS_inputs_mergedDSi_Q_1Nov2022.csv")
