#install.packages("wesanderson")
require(wesanderson)
#remotes::install_github("coolbutuseless/ggpattern", force = TRUE)
require(ggpattern)
require(reshape)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

##### FOR YIELD PLOTS #########

#read in data files and merge
GFN<-read.csv("WRTDS_GFN_Percent_All_030922.csv")
Biome<-read.csv("Biome.csv")
GFN<-merge(GFN, Biome, by=c("LTER","Site"))

#pull out rows pertaining to yield
yield<-grep("Yield Percent|Yield:Percent", GFN$Metric)

#subset dataframe to contain yield rows
GFN<-GFN[c(yield),]

#cast data - currently "Trend" is a column, now make it so that each trend type is a new column with
#the trend value associated in the column
GFN_cast<-cast(GFN, LTER+Site+Biome_new+DrainageArea_km2~Metric, value = 'Trend')

#rename cast dataframe
names(GFN_cast)<-c("LTER", "Site", "Biome", "DA", "Yield_Tot", "Yield_CQ", "Yield_Q")

#add column defining if trend is positive or negative and refactor so
#positive comes before negative in the legend of plot
GFN_cast$neg<-ifelse(GFN_cast$Yield_Tot < 0, "Negative percent change in yield", "Positive percent change in yield")
GFN_cast$neg<-factor(GFN_cast$neg, levels = c("Positive percent change in yield", "Negative percent change in yield"))


#factor dataframe so that biomes plot in this order
GFN_cast$Biome2<-factor(GFN_cast$Biome, levels = c("Tropical rainforest","Tropical savanna",
                                                         "Temperate coniferous forest","Temperate deciduous forest", 
                                                         "Temperate grassland", "Alpine tundra", "Arctic tundra",
                                                         "Boreal forest", "Polar desert"))

#quatify quadrant based on Q and CQ positive vs negative plot
GFN_cast$quadrant<-ifelse(GFN_cast$Yield_Q < 0 & GFN_cast$Yield_CQ > 0, "1",
                          ifelse(GFN_cast$Yield_Q > 0 & GFN_cast$Yield_CQ > 0, "2",
                                 ifelse(GFN_cast$Yield_Q < 0 & GFN_cast$Yield_CQ < 0, "4",
                                        ifelse(GFN_cast$Yield_Q > 0 &GFN_cast$Yield_CQ < 0, "3", NA))))

#reassign to quadrant plot variable
quantrant_plot<-GFN_cast

#define color palette
color_pal<-c("lawngreen", "goldenrod1", "darkgreen", "darkorange2", "firebrick1", "mediumpurple3", 
             "royalblue3", "lightsteelblue3", "ivory3")

#plot
pdf("QuadrantChart_Yield.pdf", width = 14, height = 9)

#quadrant chart - brewer Set 1
ggplot(GFN_cast)+geom_hline(yintercept=0)+geom_vline(xintercept=0)+theme_classic()+
  geom_point(aes(Yield_Q, Yield_CQ, fill=Biome2, size=abs(Yield_Tot), shape=neg))+
  labs(x="Percent change in yield attributed to discharge", 
       y="Percent change in yield attributed to biogeochemical changes",
       size="Overall Yield Percent Change",shape="Slope Direction", fill="Biome")+
  scale_fill_brewer(palette = "Set1")+
  scale_shape_manual(values = c(24,25))+
  guides(fill=guide_legend(override.aes=list(shape=24, size=5)), 
         size=guide_legend(override.aes = list(shape=24)),
         shape=guide_legend(override.aes = list(size=5)))+
  theme(text=element_text(size = 20))+
  scale_size_continuous(range=c(3,8), limits=c(0,150), breaks = c(0,50,100,150))

dev.off()

pdf("BarChart_Yield.pdf", width = 8.5, height = 7)

#bar chart - brewer Set 1
#weird space after quadrant becuase there is a fifth column that I am manually cropping
#out because Imnaviat has no change in Q - all CQ change
#since it is the only Arctic Tundra site, if Imnaviat is removed it messes up the legend
ggplot(quantrant_plot, mapping=aes(x=quadrant, fill=Biome2, na.rm=TRUE), 
       position="stack")+geom_bar(col="black", na.rm = TRUE)+
  scale_color_brewer(palette = "Set1")+theme_classic()+scale_fill_brewer(palette = "Set1")+
  labs(x="Quadrant             ", y="Count", fill="Biome")+
  theme(text=element_text(size = 20), legend.position = "none")

dev.off()  


###### SAME CODE BUT FOR CONCENTRATION PLOTS #######

GFN<-read.csv("WRTDS_GFN_Percent_All_030922.csv")
Biome<-read.csv("Biome.csv")

GFN<-merge(GFN, Biome, by=c("LTER","Site"))

conc<-grep("Conc Percent|Conc:Percent", GFN$Metric)

GFN<-GFN[c(conc),]

GFN_cast<-cast(GFN, LTER+Site+Biome_new+DrainageArea_km2~Metric, value = 'Trend')

names(GFN_cast)<-c("LTER", "Site", "Biome", "DA", "Conc_Tot", "Conc_CQ", "Conc_Q")

GFN_cast$neg<-ifelse(GFN_cast$Conc_Tot < 0, "Negative percent change in yield", "Positive percent change in yield")

GFN_cast$neg<-factor(GFN_cast$neg, levels = c("Positive percent change in yield", "Negative percent change in yield"))

GFN_cast$quadrant<-ifelse(GFN_cast$Conc_Q < 0 & GFN_cast$Conc_CQ > 0, "1",
                          ifelse(GFN_cast$Conc_Q > 0 & GFN_cast$Conc_CQ > 0, "2",
                                 ifelse(GFN_cast$Conc_Q < 0 & GFN_cast$Conc_CQ < 0, "4",
                                        ifelse(GFN_cast$Conc_Q > 0 &GFN_cast$Conc_CQ < 0, "3", NA))))

GFN_cast$Biome2<-factor(GFN_cast$Biome, levels = c("Tropical rainforest","Tropical savanna",
                                                   "Temperate coniferous forest","Temperate deciduous forest", 
                                                   "Temperate grassland", "Alpine tundra", "Arctic tundra",
                                                   "Boreal forest", "Polar desert"))

#reassign variable
quantrant_plot<-GFN_cast

pdf("QuadrantChart_Concentration.pdf", width = 14, height = 9)

ggplot(GFN_cast, aes(Conc_Q, Conc_CQ, fill=Biome2, shape=neg, size=abs(Conc_Tot)))+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+geom_point()+theme_classic()+
  labs(x="Percent change in concentration attributed to discharge", 
      y="Percent change in concentration attributed to biogeochemical changes",
      size="Overall Concetration Percent Change",shape="Slope Direction", fill="Biome")+
  scale_fill_brewer(palette = "Set1")+
  scale_shape_manual(values = c(24,25))+
  guides(fill=guide_legend(override.aes=list(shape=24, size=5)), 
         size=guide_legend(override.aes = list(shape=24)),
         shape=guide_legend(override.aes = list(size=5)))+
  theme(text=element_text(size = 20))+ylim(-50, 250)+
  scale_size_continuous(range=c(3,8), limits=c(0,250), breaks = c(0,50,100,150,200,250))

dev.off()

pdf("BarChart_Concentration.pdf", width = 7, height = 9)

#bar chart
ggplot(quantrant_plot, mapping=aes(x=quadrant, fill=Biome2), 
       position="stack")+geom_bar(col="black")+
  scale_color_manual(palette = "Set1")+theme_classic()+scale_fill_brewer(palette = "Set1")+
  labs(x="Quadrant             ", y="Count", fill="Biome")+
  theme(text=element_text(size = 20), legend.position = "none")

dev.off()

