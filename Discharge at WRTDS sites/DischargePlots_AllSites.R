library(ggplot2)

#plot all sites
ggplot(WRTDS_discharge_allsites, aes(x=Date, y=Q)) +
 geom_point() +
 facet_wrap(~site.name)

#plot all sites with discharge data
discharge = na.omit(WRTDS_discharge_allsites)

ggplot(discharge, aes(x=Date,y=Q)) +
  geom_point() +
  facet_wrap(~site.name)
