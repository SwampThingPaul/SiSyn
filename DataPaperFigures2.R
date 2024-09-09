setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

drivers<-read.csv("AllDrivers204Sites.csv")

site_list<-read.csv("DataPaperSites_08012024.csv")

site_list <- site_list %>%
  mutate(Stream_Name=case_when(
    Stream_Name=="Porvoonjoki 11,5 \xca6022"~"Porvoonjoki 11,5  6022",
    Stream_Name=="\xcaMustionjoki 4,9 \xca15500"~"Mustionjoki 4,9  15500",
    Stream_Name=="Mustijoki 4,2 \xca6010"~"Mustijoki 4,2  6010",
    Stream_Name=="Vantaa 4,2 \xca6040"~"Vantaa 4,2  6040",
    Stream_Name=="Lestijoki 10800 8tien s"~"Lestijoki 10800 8-tien s",
    Stream_Name=="SIMOJOKI AS 13500"~"SIMOJOKI AS. 13500",
    
    .default = Stream_Name
  ))

drivers_cropped<-drivers[drivers$Stream_Name %in% site_list$Stream_Name,]

colnames(drivers_cropped)<-c("Stream_Name", "Observation_Network", "Latitude (degrees)", "Longitude (degrees)", "Climate Zone", "Major Rock", "Major Land",
                     "Median Elevation (m)", "Log(Drainage Area) (km2)", "Log(Median Q) (cms)", "Maximum Snow Covered Area (proportion)",
                     "Precipitation (mm/year)", "Evapotranspiration (kg/m2)","Mean Annual Temperature (deg C)", "NPP (kgC/m2)", "Green Up Day (day of year)",
                     "Maximum Daylength (hours)", "Median DSi (mg/L)")

missing_sites<-setdiff(site_list$Stream_Name, drivers_cropped$Stream_Name)

drivers2<-read.csv("AllDrivers_Harmonized_20240730_WRTDS_MD_KG.csv")

drivers2_cropped<-drivers2[drivers2$Stream_Name %in% missing_sites,]

drivers2_cropped<-drivers2_cropped[,colnames(drivers2_cropped) %in% keep_these_cols]

keep_these_cols<-c("Stream_Name", "LTER", "Latitude","Longitude","Name", "major_rock","major_land",
                   "elevation_median_m","drainSqKm","prop_area","precip","evapotrans","temp","npp",             
                   "cycle0","Max_Daylength", "med_si","med_q")

colnames(drivers2_cropped)

colnames(drivers2_cropped)<-c("Stream_Name", "Median DSi (mg/L)", "Log(Median Q) (cms)", "Latitude (degrees)", "Longitude (degrees)",
                              "Observation_Network", "Climate Zone", "Log(Drainage Area) (km2)", "Maximum Snow Covered Area (proportion)",
                              "Precipitation (mm/year)", "Evapotranspiration (kg/m2)","Mean Annual Temperature (deg C)", "NPP (kgC/m2)", 
                              "Green Up Day (day of year)", "Major Rock", "Major Land", "Median Elevation (m)", "Maximum Daylength (hours)")

drivers2_cropped$`Precipitation (mm/year)`<-drivers2_cropped$`Precipitation (mm/year)`*365.25

drivers_all<-bind_rows(drivers_cropped, drivers2_cropped)

write.csv(drivers_all, "DataPaperDrivers.csv")

drivers_all$`Log(Drainage Area) (km2)`<-log10(drivers_all$`Log(Drainage Area) (km2)`)
drivers_all$`Log(Median Q) (cms)`<-log10(drivers_all$`Log(Median Q) (cms)`)

drivers_hist<-melt(drivers_all[,c(1,3,4,8,9,11:17)], id.vars=c("Stream_Name"))

drivers_hist <- drivers_hist %>%
  mutate(type=case_when(
    variable=="Latitude (degrees)"~"geographic/topographic",
    variable=="Longitude (degrees)"~"geographic/topographic",
    variable=="Median Elevation (m)"~"geographic/topographic",
    variable=="Log(Drainage Area) (km2)"~"geographic/topographic",
    variable=="Precipitation (mm/year)"~"climate",
    variable=="Maximum Snow Covered Area (proportion)"~"climate",
    variable=="Evapotranspiration (kg/m2)"~"climate",
    variable=="Mean Annual Temperature (deg C)"~"climate",
    variable=="NPP (kgC/m2)"~"productivity",
    variable=="Maximum Daylength (hours)"~"productivity",
    variable=="Green Up Day (day of year)"~"productivity",
    .default = NA
  ))

ggplot(drivers_hist, aes(x=value, fill=type))+geom_histogram()+theme_classic()+facet_wrap(~variable, scales = "free")+
  scale_fill_manual(values = c("geographic/topographic"="sienna4", "climate"="skyblue", "productivity"="forestgreen"))+
  labs(y="Number of Sites", x="Driver Value", fill="Driver Class")+theme(legend.position = "bottom")


p1<-ggplot(drivers_all, aes(x=`Precipitation (mm/year)`, y=`Mean Annual Temperature (deg C)`, col=`Log(Median Q) (cms)`))+
  geom_point(size=3)+
  scale_color_gradientn(colors = c("#b2182b","#ef8a62","#67a9cf", "#2166ac"))+
  theme_classic()+
  theme(text = element_text(size = 10))+
  labs(tag="a")


p2<-ggplot(drivers_all, aes(x=`Evapotranspiration (kg/m2)`, y=`Green Up Day (day of year)`, col=`NPP (kgC/m2)`))+
  geom_point(size=3)+
  scale_color_gradientn(colors = c("#b2182b","#ef8a62","#67a9cf", "#2166ac"))+
  theme_classic()+
  theme(text = element_text(size = 10))+
  labs(tag="c")


p3<-ggplot(drivers_all, aes(x=`Precipitation (mm/year)`, y=`Maximum Snow Covered Area (proportion)`, col=`Mean Annual Temperature (deg C)`))+
  geom_point(size=3)+
  scale_color_gradientn(colors = c("#b2182b","#ef8a62","#67a9cf", "#2166ac"))+
  theme_classic()+
  theme(text = element_text(size = 10))+
  labs(tag="b")

ggarrange(p1, p3, p2, nrow=3, align = "v")


setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

wrtds_input<-read.csv("WRTDS-input_chemistry.csv")

wrtds_input$Stream_Name<-gsub(".*__","",wrtds_input$Stream_ID)

wrtds_input<-wrtds_input %>%
  mutate(Stream_Name=case_when(
    Stream_Name=="Kiiminkij 13010 4tien s"~"Kiiminkij 13010 4-tien s",
    Stream_Name=="Lestijoki 10800 8tien s"~"Lestijoki 10800 8-tien s",
    Stream_Name=="Mustijoki 42  6010"~"Mustijoki 4,2  6010",
    Stream_Name=="Mustionjoki 49  15500"~"Mustionjoki 4,9  15500",
    Stream_Name=="Porvoonjoki 115  6022"~"Porvoonjoki 11,5  6022",
    Stream_Name=="SIMOJOKI AS 13500"~"SIMOJOKI AS. 13500",
    Stream_Name=="Vantaa 42  6040"~"Vantaa 4,2  6040",
    .default = Stream_Name
  ))

input_names<-data.frame(unique(wrtds_input$Stream_Name))

sites<-read.csv("DataPaperSites_08012024.csv")

wrtds_input<-wrtds_input[wrtds_input$Stream_Name %in% sites$Stream_Name,]

setdiff(sites$Stream_Name, unique(wrtds_input$Stream_Name))

unique(wrtds_input$variable)

wrtds_input$year<-year(as.Date(wrtds_input$Date))

#wrtds_input_N<-subset(wrtds_input, wrtds_input$variable %in% c("NO3", "NOx"))

var_count<-wrtds_input %>%
  group_by(variable, year) %>%
  dplyr::summarise(unique_combo=n_distinct(Stream_Name))

var_count$variable<-factor(var_count$variable, levels = c("P","NH4", "NO3", "NOx", "DSi"))

ggplot(var_count, aes(year, variable, col=unique_combo))+geom_line(lwd=15)+theme_classic()+
  labs(x="Year", y="Variable", col="Number of Sites")+scale_color_gradient(low = "grey70", high = "deepskyblue")+
  theme(text = element_text(size = 20))+
  scale_y_discrete(labels=c("P", expression('NH'[4]), expression('NO'[3]), expression('NO'[X]), "DSi"))







