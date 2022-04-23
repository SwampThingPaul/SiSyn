library(readr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(plyr)
library(RColorBrewer)
library(reshape2)
library(PupillometryR)
#install.packages("PupillometryR")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#read in files
results<-read.csv("WRTDS_GFN_AnnualResults_AllSites_030922.csv")
names(results)[2]<-"Site"
biome<-read.csv("Biome.csv")

#merge into one data frame
results<-left_join(results, biome, by=c("LTER", "Site"))

#set theme
raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, hjust = 1),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

#reorder the names of Biome so that they plot in this order on axis and legens
results$Biome2<-factor(results$Biome_new, levels = c("Tropical rainforest","Tropical savanna",
                                                   "Temperate coniferous forest","Temperate deciduous forest", 
                                                   "Temperate grassland", "Alpine tundra",
                                                   "Boreal forest","Arctic tundra", "Polar desert"))

#set color palette
color_pal<-c("lawngreen", "goldenrod1", "darkgreen", "darkorange2", "firebrick1", "mediumpurple3", 
             "royalblue3", "lightsteelblue3", "ivory3")

#assign color and fill (need to be different for points and boxplots) to one variable
col_pal<-list(scale_color_manual(values = color_pal),scale_fill_manual(values = color_pal))

#plot
pdf("Avg_Biome_Fig.pdf", height = 5, width = 12)
  
#this one was for plotting discharge as well - decided not to do that
  # p1<-ggplot(data = results, aes(y = log(Discharge_cms), x = Biome2, fill = Biome2)) +
  #   geom_point(aes(y = log(Discharge_cms), color = Biome2), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  #   geom_boxplot(width = .5, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  #   guides(fill = FALSE, color=guide_legend(override.aes = list(size=5))) +
  #   guides(color = FALSE) +
  #   theme_bw() +
  #   labs(y="Log Discharge (cms)", x="Biome", color="Biome")+
  #   raincloud_theme+
  #   theme(axis.title.x=element_blank(),
  #         axis.text.x=element_blank())+col_pal_list[[i]]
  
  
  p2<-ggplot(data = results, aes(y = Conc_mgL, x = Biome2, fill = Biome2)) +
    geom_point(aes(y = Conc_mgL, color = Biome2), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
    geom_boxplot(width = .5, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
    guides(fill = FALSE, color=guide_legend(override.aes = list(size=5))) +
    guides(color = FALSE) +
    theme_bw() +
    labs(y="Concentration (mg/L)", x="", color="Biome")+
    raincloud_theme+
    coord_flip()+
    scale_x_discrete(limits = rev)+
    col_pal
    #theme(axis.title.x=element_blank(),
          #axis.text.x=element_blank())
  
  p3<-ggplot(data = results, aes(y = Yield, x = Biome2, fill = Biome2)) +
    geom_point(aes(y = Yield, color = Biome2), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
    geom_boxplot(width = .5, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
    guides(fill = FALSE, color=guide_legend(override.aes = list(size=5))) +
    guides(color = FALSE) +
    theme_bw() +
    labs(y="Yield (10^6kg/km2/yr)", x="", color="Biome")+
    raincloud_theme+
    coord_flip()+
    theme(axis.text.y = element_blank())+
    scale_x_discrete(limits = rev)+
    col_pal
  
  #combine plots
  gg_print<-ggarrange(p2, p3, ncol = 2, widths = c(1.4, 1))
  #print plots
  print(gg_print)

dev.off()