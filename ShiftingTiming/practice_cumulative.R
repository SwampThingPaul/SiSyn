head(daily_kalman_v1)

site <-  daily_kalman_v1 %>% 
  filter(Stream_Name == "GSMACK") %>% 
  filter(chemical == "DSi") %>% 
  mutate(year = year(as.Date(Date))) %>% 
  group_by(year) %>% 
  mutate(cumsum_yield = cumsum(Yield), 
         max_ann_yield = max(cumsum_yield),
         half_ann_yield = max_ann_yield/2)

site %>% 
  ggplot(aes(x=year, y=half_ann_yield))+
  geom_point() +
  geom_smooth(method="loess")

midpoint <- site %>% 
  group_by(year) %>% 
  filter(cumsum_yield > half_ann_yield -0.01*half_ann_yield & 
           cumsum_yield < half_ann_yield + half_ann_yield*0.01)


midpoint %>% 
  ggplot(aes(year,Day))+
  geom_point()