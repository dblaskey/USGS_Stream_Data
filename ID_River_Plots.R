rm(list = ls())

library(pacman)
pacman::p_load(dataRetrieval, tidyverse, rnaturalearthdata, mapdata, mblm, 
               Kendall, data.table, colorspace, zoo, lubridate, ggsci, RColorBrewer)

#Chena river in Central Alaska
chena <- filter(sites, site_no == 15511000) %>%
  group_by(Year, Month) %>%
  summarise(month_dis = sum(X_00060_00003)) %>%
  filter(Year>=1970 & Year<2020) %>%
  mutate(decade = floor(Year/10)*10) %>% 
  group_by(decade, Month) %>% 
  summarise(ave_month_dis = mean(month_dis))
           
ggplot() + 
  geom_line(data = chena, aes(x = as.numeric(Month), y = ave_month_dis, color = factor(decade))) + 
  scale_x_continuous(breaks = 1:12) +
  labs(title="Chena River in Central Alaska", color="Decade", x = "Month", y = "Average Monthly Discharge (cfs)") +
  theme(plot.title = element_text(hjust = 0.5))

# KUPARUK R NR DEADHORSE AK
Kuparuk <- filter(sites, site_no == 15896000) %>%
  group_by(Year, Month) %>%
  summarise(month_dis = sum(X_00060_00003)) %>%
  filter(Year>=1980 & Year<2020) %>%
  mutate(decade = floor(Year/10)*10) %>% 
  group_by(decade, Month) %>% 
  summarise(ave_month_dis = mean(month_dis))

ggplot() + 
  geom_line(data = Kuparuk, aes(x = as.numeric(Month), y = ave_month_dis, color = factor(decade))) + 
  scale_x_continuous(breaks = 1:12) +
  labs(title="Kuparuk River in Northern Alaska", color="Decade", x = "Month", y = "Average Monthly Discharge (cfs)") +
  theme(plot.title = element_text(hjust = 0.5))

# YUKON R AT EAGLE AK
Yukon <- filter(sites, site_no == 15356000) %>%
  group_by(Year, Month) %>%
  summarise(month_dis = sum(X_00060_00003)) %>%
  filter(Year>=1960 & Year<2020) %>%
  mutate(decade = floor(Year/10)*10) %>% 
  group_by(decade, Month) %>% 
  summarise(ave_month_dis = mean(month_dis))

ggplot() + 
  geom_line(data = Yukon, aes(x = as.numeric(Month), y = ave_month_dis, color = factor(decade))) + 
  scale_x_continuous(breaks = 1:12) +
  labs(title="Yukon River in Eastern Alaska", color="Decade", x = "Month", y = "Average Monthly Discharge (cfs)") +
  theme(plot.title = element_text(hjust = 0.5))

# KUSKOKWIM R AT CROOKED CREEK AK
Kuskokwim <- filter(sites, site_no == 15304000) %>%
  group_by(Year, Month) %>%
  summarise(month_dis = sum(X_00060_00003)) %>%
  filter(Year>=1960 & Year<2020) %>%
  mutate(decade = floor(Year/10)*10) %>% 
  group_by(decade, Month) %>% 
  summarise(ave_month_dis = mean(month_dis))

ggplot() + 
  geom_line(data = Kuskokwim, aes(x = as.numeric(Month), y = ave_month_dis, color = factor(decade))) + 
  scale_x_continuous(breaks = 1:12)+
  labs(title="Kuskokwim River in Southwest Alaska", color="Decade", x = "Month", y = "Average Monthly Discharge (cfs)") +
  theme(plot.title = element_text(hjust = 0.5))

#FISH C NR KETCHIKAN AK
Fish <- filter(sites, site_no == 15072000) %>%
  group_by(Year, Month) %>%
  summarise(month_dis = sum(X_00060_00003)) %>%
  filter(Year>=1920 & Year<2020) %>%
  mutate(decade = floor(Year/10)*10) %>% 
  group_by(decade, Month) %>% 
  summarise(ave_month_dis = mean(month_dis))

ggplot() + 
  geom_line(data = Fish, aes(x = as.numeric(Month), y = ave_month_dis, color = factor(decade))) + 
  scale_x_continuous(breaks = 1:12)
