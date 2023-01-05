## Evaluate monthly results

library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(data.table)
library(trend)
library(cowplot)
library(viridis)

##################################################
#### Code for monthly plots ##
## Read in monthly data
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS")
months = read.csv("WRTDS_GFN_MonthlyResults_AllSites_092822.csv")

###################
## Pull in site data for biome
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Data/BasinInfo")
site.info=read.csv("SiteCharacteristics_PCA_WRTDS_102322.csv")
colnames(site.info)=c("LTER","site",colnames(site.info[,3:ncol(site.info)]))
months=left_join(months,site.info,by=c("LTER","site"))


######################
### Color scheme for plots

## color scheme to match other figures - Keira
f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
cols <- f("Set2")

cols_final<-c(cols, "#A46B5C")
#show_col(cols_final)

#assign colors
col_values<-c("Boreal forest" = cols_final[1],
              "Temperate deciduous forest" = cols_final[2],
              "Alpine tundra" = cols_final[3],
              "Arctic tundra" = cols_final[4],
              "Tropical rainforest" = cols_final[5],
              "Tropical savanna" = cols_final[6],
              "Temperate grassland" = cols_final[7],
              "Polar desert" = cols_final[8],
              "Temperate coniferous forest" = cols_final[9])

#This can then be used in scale_color_manual or scale_fill_manual 

##########################
### Mann Kendall tests
modified_sens.slope <- function(x, ...) {
  result <- sens.slope(x, ...)
  tibble(
    p.value = result$p.value,
    statistic = result$statistic,
    estimates = result$estimates[1],
    low.conf = result$conf.int[1],
    high.conf = result$conf.int[2])
}

##############################################
## calculate monthly trends
FNconc=months%>%
  filter(!is.na(FNConc)) %>% 
  group_by(site, Month) %>%
  group_modify(~ modified_sens.slope(.x$FNConc)) %>% 
  mutate(variable = rep("FNConc"))

# remove ws1 from HBR
FNconc = filter(FNconc, site != "ws1")

FNconc$LTER = ifelse(FNconc$site %in% c("ALBION","MARTINELLI","SADDLE STREAM 007"),"NWT",
                     ifelse(FNconc$site %in% c("QS","Q1","Q2","Q3","RI","MPR"), "LUQ", 
                            ifelse(FNconc$site %in% c("ws2","ws3","ws4","ws5","ws6","ws7","ws8","ws9"), "HBR",
                                   ifelse(FNconc$site %in% c("S65","S65A","S65D","S65E"),"KRR",
                                          ifelse(FNconc$site %in% c("GSMACK","GSWS02","GSWS06","GSWS07","GSWS08","GSWS09","GSWS10"),"AND",
                                                 ifelse(FNconc$site %in% c("LMP73"),"LMP",
                                                        ifelse(FNconc$site %in% c("Kolyma","Lena","Mackenzie","Ob","Yenisey","Yukon"),"GRO",
                                                               ifelse(FNconc$site %in% c("Andersen Creek at H1","Canada Stream at F1",
                                                                                         "Green Creek at F9", "Lawson Creek at B3",
                                                                                         "Onyx River at Lake Vanda Weir", "Onyx River at Lower Wright Weir",
                                                                                         "Priscu Stream at B1","Von Guerard Stream at F6"), "MCM",
                                                                      ifelse(FNconc$site %in% c("Sagehen"),"Sagehen",
                                                                             ifelse(FNconc$site %in% c("BK01.0M","CH00.1M","CN00.1M","CU11.6M",
                                                                                                       "I080.2M","LM00.5M","M078.0B","M241.4K",
                                                                                                       "M556.4A","M701.1B","M764.3A","M786.2C",
                                                                                                       "MQ02.1M", "SG16.2C","WP02.6M"),"UMR",
                                                                                    ifelse(FNconc$site %in% c("Imnavait Weir"),"ARC","")))))))))))






FNconc=left_join(FNconc,site.info,by=c("LTER","site"))

## calculate flux trend
FNflux=months%>%
  filter(!is.na(FNFlux)) %>% 
  group_by(site, Month) %>%
  group_modify(~ modified_sens.slope(.x$FNFlux)) %>% 
  mutate(variable = rep("FNFlux"))

FNflux = filter(FNflux, site != "ws1")

FNflux$LTER = ifelse(FNflux$site %in% c("ALBION","MARTINELLI","SADDLE STREAM 007"),"NWT",
                   ifelse(FNflux$site %in% c("QS","Q1","Q2","Q3","RI","MPR"), "LUQ", 
                          ifelse(FNflux$site %in% c("ws2","ws3","ws4","ws5","ws6","ws7","ws8","ws9"), "HBR",
                                 ifelse(FNflux$site %in% c("S65","S65A","S65D","S65E"),"KRR",
                                        ifelse(FNflux$site %in% c("GSMACK","GSWS02","GSWS06","GSWS07","GSWS08","GSWS09","GSWS10"),"AND",
                                               ifelse(FNflux$site %in% c("LMP73"),"LMP",
                                                      ifelse(FNflux$site %in% c("Kolyma","Lena","Mackenzie","Ob","Yenisey","Yukon"),"GRO",
                                                             ifelse(FNflux$site %in% c("Andersen Creek at H1","Canada Stream at F1",
                                                                                       "Green Creek at F9", "Lawson Creek at B3",
                                                                                       "Onyx River at Lake Vanda Weir", "Onyx River at Lower Wright Weir",
                                                                                       "Priscu Stream at B1","Von Guerard Stream at F6"), "MCM",
                                                                    ifelse(FNflux$site %in% c("Sagehen"),"Sagehen",
                                                                           ifelse(FNflux$site %in% c("BK01.0M","CH00.1M","CN00.1M","CU11.6M",
                                                                                                     "I080.2M","LM00.5M","M078.0B","M241.4K",
                                                                                                     "M556.4A","M701.1B","M764.3A","M786.2C",
                                                                                                     "MQ02.1M", "SG16.2C","WP02.6M"),"UMR",
                                                                                  ifelse(FNflux$site %in% c("Imnavait Weir"),"ARC","")))))))))))




FNflux=left_join(FNflux,site.info,by=c("LTER","site"))

### Calculate Yield trend
FNyield=months%>%
  filter(!is.na(FNYield)) %>% 
  group_by(site, Month) %>%
  group_modify(~ modified_sens.slope(.x$FNYield)) %>% 
  mutate(variable = rep("FNYield"))

FNyield = filter(FNyield, site != "ws1")

FNyield$LTER = ifelse(FNyield$site %in% c("ALBION","MARTINELLI","SADDLE STREAM 007"),"NWT",
                     ifelse(FNyield$site %in% c("Q1","Q2","Q3","RI","MPR"), "LUQ", 
                            ifelse(FNyield$site %in% c("ws2","ws3","ws4","ws5","ws6","ws7","ws8","ws9"), "HBR",
                                   ifelse(FNyield$site %in% c("S65","S65A","S65D","S65E"),"KRR",
                                          ifelse(FNyield$site %in% c("GSMACK","GSWS02","GSWS06","GSWS07","GSWS08","GSWS09","GSWS10"),"AND",
                                                 ifelse(FNyield$site %in% c("LMP73"),"LMP",
                                                        ifelse(FNyield$site %in% c("Kolyma","Lena","Mackenzie","Ob","Yenisey","Yukon"),"GRO",
                                                               ifelse(FNyield$site %in% c("Andersen Creek at H1","Canada Stream at F1",
                                                                                         "Green Creek at F9", "Lawson Creek at B3",
                                                                                         "Onyx River at Lake Vanda Weir", "Onyx River at Lower Wright Weir",
                                                                                         "Priscu Stream at B1","Von Guerard Stream at F6"), "MCM",
                                                                      ifelse(FNyield$site %in% c("Sagehen"),"Sagehen",
                                                                             ifelse(FNyield$site %in% c("BK01.0M","CH00.1M","CN00.1M","CU11.6M",
                                                                                                       "I080.2M","LM00.5M","M078.0B","M241.4K",
                                                                                                       "M556.4A","M701.1B","M764.3A","M786.2C",
                                                                                                       "MQ02.1M", "SG16.2C","WP02.6M"),"UMR",
                                                                                    ifelse(FNyield$site %in% c("Imnavait Weir"),"ARC","")))))))))))




FNyield=left_join(FNyield,site.info,by=c("LTER","site"))


##################################################################################

months %>% 
  filter(LTER=="NWT") %>% 
  #filter(Month == 1 | Month == 12) %>% 
  ggplot(aes(Month,FNConc,col=as.factor(Year)))+
  geom_line()+
  #geom_line(aes(Year,FNFlux_106_kg_y, col=site))+
  facet_wrap(~site,scales="free")+
  theme(legend.position="right")

####################################################################################
#### Boxplots by Biome
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Manuscripts/SiSynthesis/Figures")

# remove months not modeled for Concentration
martinelli = FNconc %>% 
  filter(site == "MARTINELLI") %>% 
  filter(Month <4 | Month >10)

saddle = FNconc %>% 
  filter(site == "SADDLE STREAM 007") %>% 
  filter(Month <4 | Month >10)

mcm = FNconc %>% 
  filter(LTER == "MCM") %>% 
  filter(Month >1 & Month < 12)

conc.plot = anti_join(FNconc, martinelli)
conc.plot1 = anti_join(conc.plot, saddle)
conc.plot2 = anti_join(conc.plot1, mcm)

# reorder biomes for plotting
conc.plot2$Biome=as.factor(conc.plot2$Biome)
conc.plot3 = conc.plot2 %>% mutate(Biome = fct_relevel(Biome, "Tropical rainforest",
                                                       "Tropical savanna",
                                                       "Temperate coniferous forest",
                                                       "Temperate deciduous forest",
                                                       "Boreal forest",
                                                       "Temperate grassland",
                                                       "Alpine tundra",
                                                       "Arctic tundra", 
                                                       "Polar desert"))



conc.plot3$Month_name = as.factor(conc.plot3$Month)
levels(conc.plot3$Month_name)= c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# plot NWT

conc.season=
  conc.plot3 %>% 
  filter(Biome == "Alpine tundra") %>% 
  filter(p.value<=0.5) %>% 
  ggplot(aes(Month_name,estimates))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_bw()+
  facet_wrap(~site, scales="free_y")+
  theme_classic()+
  #theme(strip.text.y = element_blank())+
  ylab("Sen slope")+xlab("")


#### For paper
### Concentration
conc.season=
  conc.plot3 %>% 
  filter(Biome != "Polar desert") %>% 
  filter(p.value<=0.5) %>% 
  ggplot(aes(Month_name,estimates, fill=Biome))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_bw()+
  facet_wrap(~Biome, scales="free_y", nrow=12, strip.position = "right",
             labeller = labeller(Biome = label_wrap_gen(width = 16)))+
  theme_classic()+
  theme(strip.background = element_blank())+
  #theme(strip.text.y = element_blank())+
  ylab("Sen slope")+xlab("")+
  theme(legend.position="none")

c = conc.season + scale_fill_manual(values=col_values)
c

### Yield
# remove months not modeled for Yield
martinelli = FNyield %>% 
  filter(site == "MARTINELLI") %>% 
  filter(Month <4 | Month >10)

saddle = FNyield %>% 
  filter(site == "SADDLE STREAM 007") %>% 
  filter(Month <4 | Month >10)

mcm = FNyield %>% 
  filter(LTER == "MCM") %>% 
  filter(Month >1 & Month < 12)

yield.plot = anti_join(FNyield, martinelli)
yield.plot1 = anti_join(yield.plot, saddle)
yield.plot2 = anti_join(yield.plot1, mcm)

# reorder Biome for plotting
yield.plot2$Biome=as.factor(yield.plot2$Biome)
yield.plot3 = yield.plot2 %>% mutate(Biome = fct_relevel(Biome, "Tropical rainforest",
                                                         "Tropical savanna",
                                                         "Temperate coniferous forest",
                                                         "Temperate deciduous forest",
                                                         "Boreal forest",
                                                         "Temperate grassland",
                                                         "Alpine tundra",
                                                         "Arctic tundra", 
                                                         "Polar desert"))




yield.plot3$Month_name = as.factor(yield.plot3$Month)
levels(yield.plot3$Month_name)= c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

yield.season = 
  yield.plot3 %>% 
  filter(Biome != "Polar desert") %>% 
  filter(p.value <=0.05) %>% 
  ggplot(aes(Month_name,estimates, fill=Biome))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_bw()+
  facet_wrap(~Biome, scales="free_y", nrow=9, strip.position="right",
             labeller = labeller(Biome = label_wrap_gen(width = 16)))+
  theme_classic()+
  #theme(axis.text.x=element_blank())+
  theme(strip.background = element_blank())+
  ylab("")+xlab("")+
  theme(legend.position="none")

y = yield.season+scale_fill_manual(values=col_values)
y
#ggsave("Yield_season.png", height=14, width=8)

plot_grid(c, y,
          labels=c("a","b"),
          ncol = 2, nrow = 1)

ggsave("Figure7_Season.png", height=12, width=9, units="in")

#####################################################################################3
##### Plot by month for certain sites #### 

nwt.c=
  conc.plot3 %>% 
  filter(LTER == "NWT") %>% 
  ggplot(aes(Month,estimates))+
  geom_line()+
  geom_hline(yintercept=0)+
  theme_bw()+
  facet_wrap(~site, scales="free_y", ncol=1)+
  theme(axis.text.x=element_text(angle=30))+
  ggtitle("Monthly flow-normalized concentration trends")+
  ylab("Sen slope")+xlab("Month")+
  theme(legend.position="none")

nwt.y=
  yield.plot3 %>% 
  filter(LTER == "NWT") %>% 
  ggplot(aes(Month,estimates))+
  geom_line()+
  geom_hline(yintercept=0)+
  theme_bw()+
  facet_wrap(~site, scales="free_y", ncol=1)+
  theme(axis.text.x=element_text(angle=30))+
  ggtitle("Monthly flow-normalized flux trends")+
  ylab("Sen slope")+xlab("Month")+
  theme(legend.position="none")


plot_grid(nwt.c,nwt.y,ncol=2,nrow=1)

colourCount = length(unique(months$Month))
getPalette = colorRampPalette(brewer.pal(11, "Spectral"))

# trend by month across years
months %>% 
  #filter(Month > 5 & Month < 11) %>% 
  filter(LTER == "UMR") %>% 
  ggplot(aes(Year,FNConc,col=as.factor(Month)))+
  geom_line()+
  #geom_point(aes(Year,Conc, col=as.factor(Month)))+
  facet_wrap(~site,scales="free")+
  theme_classic()+
  theme(legend.position="right")+
  #scale_fill_manual(values = getPalette(colourCount))+
  ggtitle("Monthly trends")+
  labs(colour = "Month")+
  ylab("Flow-normalized Flux (kg/yr)")+xlab("")
  #ylim

setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/Figures")
ggsave("UMR_MonthChange.png", height=6, width=8, units="in")

# long-term trend by months
months %>% 
  filter(LTER == "UMR") %>% 
  ggplot(aes(Month,FNConc,col=as.factor(Year)))+
  geom_line()+
  #geom_line(aes(Year,FNConc, col=as.factor(Month)))+
  facet_wrap(~site,scales="free")+
  theme_classic()+
  theme(legend.position="right")+
  scale_fill_manual(values = getPalette(colourCount))+
  labs(colour = "Year")+
  ylab("Flow-normalized Concentration (mg/L)")+
  ggtitle("Upper Miss: Seasonal Trends")

setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/Figures")
ggsave("UpperMiss_SeasonalChange.png", height=6, width=8, units="in")

## Another site for comparison
months %>% 
  filter(site == "ws3") %>% 
  ggplot(aes(Year,FNConc,col=as.factor(Month)))+
  geom_line()+
  #geom_line(aes(Year,FNConc, col=as.factor(Month)))+
  #facet_wrap(~site,scales="free")+
  theme_classic()+
  theme(legend.position="right")+
  scale_fill_manual(values = getPalette(colourCount))+
  ggtitle("Albion, Colorado, USA")

months %>% 
  filter(site == "ws3") %>% 
  filter(Year >2009 | Year <1980) %>% 
  ggplot(aes(Month,FNConc,col=as.factor(Year)))+
  geom_line()+
  #geom_line(aes(Year,FNConc, col=as.factor(Month)))+
  #facet_wrap(~site,scales="free")+
  theme_classic()+
  theme(legend.position="right")+
  scale_fill_manual(values = getPalette(colourCount))+
  labs(colour = "Year")+
  ylab("Flow-normalized Concentration (mg/L)")+
  ggtitle("Albion, Colorado, USA")

### don't need for seasonal plots ### 
## read out combined results
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS")
annual = read.csv("WRTDS_AnnualResults_AllSites_011622.csv")

annual %>% 
  filter(site == "BK01.0M") %>% 
  ggplot(aes(Year,Conc_mgL))+
  geom_point()+
  geom_line(aes(Year,FNConc_mgL))+
  #facet_wrap(~site,scales="free")+
  theme_classic()+
  ylim(0,6)+
  #theme(legend.position="right")+
  #scale_fill_manual(values = getPalette(colourCount))+
  ggtitle("Black River, Wisconsin, USA")+
  ylab("DSi Concentration (mg/L)")+xlab("")

setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/Figures")
ggsave("BlackRiver_AnnualChange.png", height=6, width=8, units="in")

annual %>% 
  filter(LTER == "HBR") %>%
  filter(site != "ws1") %>% 
  ggplot(aes(Year,Conc_mgL, col=site))+
  geom_point()+
  geom_line(aes(Year,FNConc_mgL, col=site))+
  #facet_wrap(~site,scales="free")+
  theme_classic()+
  #ylim(0,6)+
  scale_color_grey()+
  theme(legend.position="right")+
  #scale_fill_manual(values = getPalette(colourCount))+
  ggtitle("Hubbard Brook")+
  ylab("DSi Concentration (mg/L)")+xlab("")

setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/Figures")
ggsave("WS1_AnnualChange.png", height=6, width=8, units="in")
