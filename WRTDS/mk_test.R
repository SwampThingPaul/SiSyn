## Running trends on annual modeled concentration and flux (not flow-normalized)

library(tidyverse)
library(trend)
library(lubridate)
library(broom)
library(mblm)
library(Kendall)

# all sites - annual
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS")
dat=read.csv("WRTDS_GFN_AnnualResults_AllSites_030922.csv")
colnames(dat)=c("LTER","Site","Year","Discharge","Conc","FNConc","Flux","FNFlux","DrainageArea_km2",
                "Yield","FNYield")

dat %>% 
  filter(site == "MO78.0B" | site == "M241.4K" | site == "M556.4A"|site=="M701.1B"|site=="M764.3A") %>% 
  ggplot(aes(Year, Discharge))+
  facet_wrap(~site, scales="free")+
  geom_point()+
  geom_smooth(method="lm")

dat %>% filter(site=="GSWS10") %>% 
  ggplot(aes(Year,Conc))+geom_point()+
  geom_line(aes(Year,FNConc))+
  ylim(8,10)

dat_long=dat %>% 
  select(LTER,Site,Year,Discharge,Conc,FNConc,Flux,FNFlux,Yield,FNYield) %>% 
  pivot_longer(cols=Discharge:FNYield, names_to="Metric",values_to="Value")

# run Mann Kendall test
#fitted_models <-  dat_long %>%
  #filter(!is.na(Value)) %>%
  #filter(Metric == "Discharge") %>% 
  #group_by(site) %>%
  #do(model = tidy(mk.test(Value, data=., continuity = TRUE))) %>%
  #unnest(model)

## USE THIS
# ideas from Stack Overflow for using mk.test
# https://stackoverflow.com/questions/64845777/compute-statistical-tests-groupwise-in-r-using-dplyr
# this works but stores each site as an invididual value in a list

# "wrapper" functions to be able to store all outputs in a single df rather than a list
# for each type of test - sen slope, mk.test and mblm
modified_sens.slope <- function(x, ...) {
  result <- sens.slope(x, ...)
  tibble(
    p.value = result$p.value,
    statistic = result$statistic,
    estimates = result$estimates[1],
    low.conf = result$conf.int[1],
    high.conf = result$conf.int[2])
}

modified_mk_test <- function(x, ...) {
  result <- mk.test(x, ...)
  tibble(
    p.value = result$p.value,
    statistic = result$statistic,
    estimates = result$estimates[1])
}

modified_mk_test <- function(x, ...) {
  result <- mk.test(x, ...)
  tibble(
    p.value = result$tau,
    statistic = result$s1,
    estimates = result$estimates[1])
}

test=filter(dat, Site=="M078.0B")
x=mk.test(test$Discharge, alternative="two.sided")

modified_mblm <- function(x, ...) {
  result <- mblm(x, ...)
  tibble(
    p.value = result$p.value,
    statistic = result$statistic,
    estimates = result$estimates[1])
}

#slope_ks_coef = coef(mblm(Value ~ Year, data = discharge))[2],
#intercept_ks_coef = coef(mblm(Value ~ Year, data = discharge))[1],
#ks_pval = as.numeric(summary(mblm(Value ~ Year, data=discharge))$coefficients[2,4]))


####  Run this first 
# slopes
# stores in dataframe
Q=dat_long %>%
  filter(!is.na(Value)) %>% 
  filter(Metric == "Discharge") %>% 
  group_by(Site) %>%
  group_modify(~ modified_sens.slope(.x$Value)) %>% 
  select(Site,estimates,p.value)

colnames(Q) = c("Site","Q_slope","Q_pvalue")
  
conc=dat_long %>%
  filter(!is.na(Value)) %>% 
  filter(Metric == "Conc") %>% 
  group_by(Site) %>%
  group_modify(~ modified_sens.slope(.x$Value)) %>% 
  mutate(variable = rep("conc")) %>% 
  select(Site,estimates,p.value)

colnames(conc) = c("Site","conc_slope","conc_pvalue")


flux=dat_long %>%
  filter(!is.na(Value)) %>% 
  filter(Metric == "Flux") %>% 
  group_by(Site) %>%
  group_modify(~ modified_sens.slope(.x$Value)) %>% 
  mutate(variable = rep("flux")) %>% 
  select(Site,estimates,p.value)

colnames(flux) = c("Site","flux_slope","flux_pvalue")

FNconc=dat_long %>%
  filter(!is.na(Value)) %>% 
  filter(Metric == "FNConc") %>% 
  group_by(Site) %>%
  group_modify(~ modified_sens.slope(.x$Value)) %>% 
  mutate(variable = rep("flux")) %>% 
  select(Site,estimates,p.value)

colnames(FNconc) = c("Site","FNConc_slope","FNConc_pvalue")

FNflux=dat_long %>%
  filter(!is.na(Value)) %>% 
  filter(Metric == "FNFlux") %>% 
  group_by(Site) %>%
  group_modify(~ modified_sens.slope(.x$Value)) %>% 
  mutate(variable = rep("flux")) %>% 
  select(Site,estimates,p.value)

colnames(FNflux) = c("Site","FNFlux_slope","FNFlux_pvalue")

yield=dat_long %>%
  filter(!is.na(Value)) %>% 
  filter(Metric == "Yield") %>% 
  group_by(Site) %>%
  group_modify(~ modified_sens.slope(.x$Value)) %>% 
  mutate(variable = rep("flux")) %>% 
  select(Site,estimates,p.value)

colnames(yield) = c("Site","Yield_slope","Yield_pvalue")

FNyield=dat_long %>%
  filter(!is.na(Value)) %>% 
  filter(Metric == "FNYield") %>% 
  group_by(Site) %>%
  group_modify(~ modified_sens.slope(.x$Value)) %>% 
  mutate(variable = rep("flux")) %>% 
  select(Site,estimates,p.value)

colnames(FNyield) = c("Site","FNYield_slope","FNYield_pvalue")


# combine data
dat=bind_cols(Q,conc,flux, FNconc, FNflux,yield,FNyield)
dat1=dat %>% select(Site...1,Q_slope,Q_pvalue,conc_slope,conc_pvalue,flux_slope,flux_pvalue,
                   FNConc_slope,FNConc_pvalue,FNFlux_slope,FNFlux_pvalue,
                   Yield_slope,Yield_pvalue,FNYield_slope,FNYield_pvalue)


colnames(dat1)=c("site",colnames(dat1[,2:ncol(dat1)]))

dat1$LTER = ifelse(dat1$site %in% c("ALBION","MARTINELLI","SADDLE STREAM 007"),"NWT",
                 ifelse(dat1$site %in% c("QS","Q1","Q2","Q3","RI","MPR"), "LUQ", 
                        ifelse(dat1$site %in% c("ws1","ws2","ws3","ws4","ws5","ws6","ws7","ws8","ws9"), "HBR",
                               ifelse(dat1$site %in% c("S65","S65A","S65D","S65E"),"KRR",
                                      ifelse(dat1$site %in% c("GSMACK","GSWS02","GSWS06","GSWS07","GSWS08","GSWS09","GSWS10"),"AND",
                                             ifelse(dat1$site %in% c("LMP73"),"LMP",
                                                    ifelse(dat1$site %in% c("Kolyma","Lena","Mackenzie","Ob","Yenisey","Yukon"),"GRO",
                                                           ifelse(dat1$site %in% c("Andersen Creek at H1","Canada Stream at F1",
                                                                                 "Green Creek at F9", "Lawson Creek at B3",
                                                                                 "Onyx River at Lake Vanda Weir", "Onyx River at Lower Wright Weir",
                                                                                 "Priscu Stream at B1","Von Guerard Stream at F6"), "MCM",
                                                                  ifelse(dat1$site %in% c("Sagehen"),"Sagehen",
                                                                         ifelse(dat1$site %in% c("BK01.0M","CH00.1M","CN00.1M","CU11.6M",
                                                                                               "I080.2M","LM00.5M","M078.0B","M241.4K",
                                                                                               "M556.4A","M701.1B","M764.3A","M786.2C",
                                                                                               "MQ02.1M", "SG16.2C","WP02.6M"),"UMR",
                                                                                ifelse(dat1$site %in% c("Imnavait Weir"),"ARC","")))))))))))




setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS")
write.csv(dat1, "All_MK_Trends_GFN_030922.csv", row.names=FALSE)

dat1 %>% 
  ggplot(aes(x=LTER, FNFlux_slope, fill=site))+
  geom_bar(stat="identity", position="dodge")+
  #geom_errorbar(aes(ymin=percent-(SD/sqrt(N)), ymax=percent+(SD/sqrt(N))), width=.1)+
  theme_classic()+
  theme(legend.position = "none", axis.text.x = element_text(angle=0, size=10))+
  xlab("")+ylab("")+
  scale_fill_grey()+
  scale_x_discrete(labels = scales::wrap_format(10))


################################################
# all sites - monthly
setwd("C:/Users/kjankowski/OneDrive - DOI/Documents/Projects/Silica Synthesis/Results/WRTDS")
month=read.csv("WRTDS_MonthlyResults_AllSites_011622.csv")


FNconc=month %>%
  select(LTER,site,Month,Year,Conc,FNConc)
  #filter(!is.na(Value)) %>% 
  #filter(Metric == "FNConc") %>% 
  group_by(Site,Month) %>%
  group_modify(~ modified_sens.slope(.x$Value)) %>% 
  mutate(variable = rep("flux")) %>% 
  select(Site,estimates,p.value)

###########################################
FNconc=dat_long %>%
  filter(!is.na(Value)) %>% 
  filter(Metric == "FNConc") %>% 
  group_by(site) %>%
  group_modify(~ modified_sens.slope(.x$Value))

FNflux=dat_long %>%
  filter(!is.na(Value)) %>% 
  filter(Metric == "FNFlux") %>% 
  group_by(site) %>%
  group_modify(~ modified_sens.slope(.x$Value))




# similar to above but less convenient - it stores output in a list
dat_long %>% 
  filter(!is.na(Value)) %>% 
  filter(Metric == "Discharge") %>% 
  group_by(site) %>%
  group_map(~ trend::sens.slope(.x$Value))


#####################################################
# plotting Mann-Kendall results
# Discharge
fitted_models %>% 
  filter(Metric == "Discharge") %>% 
  filter(p.value<=0.05) %>% 
  ggplot(aes(x=site,y=estimate))+
  geom_bar(stat="identity")+
  theme_classic()

# Conc
conc %>% 
  filter(p.value<=0.05) %>% 
  ggplot(aes(x=site,y=estimates))+
  geom_bar(stat="identity")+
  theme_classic()

# Flux
flux %>% 
  filter(p.value<=0.05) %>% 
  ggplot(aes(x=site,y=estimates))+
  geom_bar(stat="identity")+
  theme_classic()

# FNConc
FNconc %>% 
  filter(p.value<=0.05) %>% 
  ggplot(aes(x=site,y=estimates))+
  geom_bar(stat="identity")+
  theme_classic()

# Flux
FNflux %>% 
  filter(p.value<=0.05) %>% 
  ggplot(aes(x=site,y=estimates))+
  geom_bar(stat="identity")+
  theme_classic()

## From Patrick

# For correlations of DSi ~ year 
# (here ri is the correlation coefficient and ni is the number of years observed):

fitted_models <-  dat_long %>%
  filter(!is.na(Value)) %>%
  group_by(site, Metric) %>%
  do(model = tidy(cor.test(~Year + Value, data = ., meth = "kendall"))) %>%
  unnest(model)

# from Molly
# From VanAppledorn large wood code
# takes a long time to run!  
stats <- discharge %>%
  filter(!is.na(Value)) %>%
  group_by(site) %>%
  summarize(mk_z = round(mk.test(Value)$statistic, 3),
            mk_pval = round(mk.test(Value)$p.value, 3),
            mn_overall = round(mean(Value), 3),
            slope_ks_coef = coef(mblm(Value ~ Year, data = discharge))[2],
            intercept_ks_coef = coef(mblm(Value ~ Year, data = discharge))[1],
            ks_pval = as.numeric(summary(mblm(Value ~ Year, data=discharge))$coefficients[2,4]))



# from Patrick - I got it to work this way too, but this only gives the correlation coefficient and not the p values and stuff

mktests <- dat_long %>% group_by(site, Metric) %>% 
  summarize(ri=cor(Year, Value, method = "kendall"),
            ni=n())

# other attempts
mk <- dat_long %>% 
  as.data.frame() %>% 
  group_by(site, Metric) %>% 
  summarise(out=mk.test(Value))

fitted_models <-  dat_long %>%
  filter(!is.na(Value)) %>% 
  group_by(site, Metric) %>%
  do(model = tidy(mk.test(Value))) %>% # only works if you include the broom::tidy in here
  unnest(model)
