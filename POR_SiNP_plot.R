require(googledrive)
require(ggplot2)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

published_sites<-read.csv("Med_DSi_Map_05142024.csv")

chem_url<-"https://drive.google.com/file/d/13tzAEYRK5KxV5YEjIVWmVn6_L6qbnk6X/view?usp=drive_link"

file_get<-drive_get(as_id(chem_url))

drive_download(file_get$drive_resource, overwrite = T)

master_chem<-read.csv("20240624_masterdata_chem.csv")
#names<-data.frame(unique(master_chem$Stream_Name))

master_chem <- master_chem %>%
  mutate(Stream_Name = case_when(
    Stream_Name=="Koskenkyl<e4>njoki 6030" ~ "Koskenkylanjoki 6030",
    Stream_Name=="N<e4>rpi<f6>njoki mts 6761" ~ "Narpionjoki mts 6761",
    Stream_Name=="Pyh<e4>joki Hourunk 11400" ~ "Pyhajoki Hourunk 11400",
    .default = Stream_Name
  ))

master_chem_cropped <- master_chem %>%
  filter(variable %in% c("DSi", "NOx", "NO3", "PO4", "SRP")) %>%
  filter(Stream_Name %in% published_sites$Stream_Name)

master_chem_cropped$year<-year(as.Date(master_chem_cropped$date))


master_chem_summary<-master_chem_cropped %>%
  dplyr::group_by(year, variable) %>%
  dplyr::summarise(num_streams=n_distinct(Stream_Name))

master_chem_summary <- master_chem_summary %>%
  mutate(clean_solute = case_when(
    variable=="SRP"~"P",
    variable=="PO4"~"P",
    variable=="NOx"~"N",
    variable=="NO3"~"N",
    variable=="DSi"~"DSi",
    .default = variable
  ))

master_chem_summary$variable<-factor(master_chem_summary$variable, levels = c("DSi", "NOx", "NO3", "SRP", "PO4"))
master_chem_summary$variable<-factor(master_chem_summary$variable, levels = c("PO4", "SRP", "NO3", "NOx", "DSi"))

png("PeriodOfRecord_SiNP.png", width=8, height = 4, res = 300, units = "in")

ggplot(master_chem_summary, aes(x=year, y=variable, col=num_streams))+geom_line(lwd=15)+
  theme_classic()+labs(y="", x="Year", col="Number of Sites")+
  theme(text = element_text(size = 20, family = "Times"))+
  scale_color_gradient(low = "grey76", high = "deepskyblue3", limits=c(0,200))+
  scale_y_discrete(labels=c(bquote(PO[4]), "SRP", bquote(NO[3]), bquote(NO[x]), "DSi"))

dev.off()
