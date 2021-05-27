#run HBR and AND sites with Loadflex, compare with WRTDS output
library(loadflex)
library(lubridate)
library(ggplot2)
library(dplyr)

#merge all Si and Q files from each site
Q_compare = rbind(GSWS09_Q_WRTDS, GSWS10_Q_WRTDS,ws1_Q_WRTDS,ws2_Q_WRTDS,ws3_Q_WRTDS,ws6_Q_WRTDS)
Si_compare = rbind(GSWS09_Si_WRTDS, GSWS10_Si_WRTDS,ws1_Si_WRTDS,ws2_Si_WRTDS,ws3_Si_WRTDS,ws6_Si_WRTDS)

head(Q_compare)
head(Si_compare)

#loop  through loadflex code for each site
