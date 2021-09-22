#get number of Si-Q measurements per site and year
library(lubridate)
Si_Q_WRTDS_sites$year = year(Si_Q_WRTDS_sites$Date)

library(dplyr)
Si_Q_WRTDS_sites_stats = 
  Si_Q_WRTDS_sites %>%
  group_by(LTER, site.name, year) %>%
  summarize(
    n=n()
  )

#how many sites are in Si_Q_WRTDS_sites?
length(unique(Si_Q_WRTDS_sites$site.name))

#run lm model for one site in one year
AndersenCreekatH1_1995 = subset(Si_Q_WRTDS_sites, Si_Q_WRTDS_sites$site.name=="Andersen Creek at H1" & Si_Q_WRTDS_sites$year==1995)
plot(log(AndersenCreekatH1_1995$Q)~log(AndersenCreekatH1_1995$Si))
fit = lm(log(AndersenCreekatH1_1995$Q)~log(AndersenCreekatH1_1995$Si))
summary(fit)
slope = as.numeric(fit$coefficients[2])
pvalue = summary(fit)$coefficients[2,4]

#create loop to run through all sites for each year
site_list = unique(Si_Q_WRTDS_sites$site.name)

#create empty list to store output from loop
site_slope = list()

for (i in 1:length(site_list)){
  year_slope = list() #clear year_slope list between every site
  
  site_dat = subset(Si_Q_WRTDS_sites, Si_Q_WRTDS_sites$site.name==site_list[i]) #subset all site data
  
  site_years = unique(site_dat$year) #get list of years in dataset
  
  for (j in 1:length(site_years)) { #loop through all years in site_dat; unique(site_years)[1]:unique(site_years)[length(site_years)]
    tryCatch({
      annual_site_dat = subset(site_dat, site_dat$year==site_years[j]) #subset each year
      
      fit = lm(log(annual_site_dat$Q)~log(annual_site_dat$Si)) #linear regression between Si-Q
      
      slope = as.numeric(fit$coefficients[2]) #get slope from linear model
      
      pvalue = summary(fit)$coefficients[2,4]
      
      year_slope_dat = data.frame(site.name=annual_site_dat$site.name[1],
                                  year=annual_site_dat$year[1],
                                  slope=slope,
                                  pvalue=pvalue)
      
      year_slope[[j]] = year_slope_dat
    }, error=function(e){cat("ERROR: ",conditionMessage(e),"\n")}) #if there is an error with the lm model, print error message and skip
  }
  
  site_slope[[i]] = year_slope
}

annual_site_slopes = unlist(site_slope, recursive=F) #expand list
library(plyr)
annual_site_slopes = ldply(annual_site_slopes, data.frame) #move all site slopes to one data frame
annual_site_slopes$sig = ifelse(annual_site_slopes$pvalue<=0.05, "sig","not sig") #are slopes significant?

#plot slopes by year, facet by site
library(ggplot2)
ggplot(annual_site_slopes, aes(x=year, y=slope, color=sig))+
  geom_abline(intercept=0,slope=0, lty="dashed", color="black")+
  geom_point()+
  facet_wrap(~site.name, scales="free")
