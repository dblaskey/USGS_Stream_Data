library(pacman)
pacman::p_load(tidyverse)

climate <- read.csv("climate.csv", header = FALSE)
names <- c("site_no", "wt_year", "t_mean", "pcp")
colnames(climate) <- names

climate <- climate %>%
  mutate(site_no = as.character(site_no))

qy_percentile <- qy %>% 
  group_by(site_no) %>%
  arrange(Annual_Discharge)

qy_percentile <- qy_percentile %>%
  mutate(PCT = ntile(site_no, 10)) %>%
  mutate(PCT_bin = ifelse(PCT==1,1,ifelse(PCT<=3,2,ifelse(PCT<=5,3,ifelse(PCT<=7,4,ifelse(PCT<=9,5,6)))))) %>%
  arrange(site_no, wt_year)

Analysis <- inner_join(qy_percentile, climate)

Analysis <- Analysis %>% 
  mutate(PCT_bin = as.numeric(PCT_bin), pcp = as.numeric(pcp), t_mean = as.numeric(t_mean))

Centroid <- Analysis %>%
  group_by(PCT_bin) %>%
  mutate(ave_t = mean(t_mean), ave_pcp = mean(pcp)) %>%
  distinct(PCT_bin, .keep_all = TRUE)

ggplot() +
  geom_point(data = Analysis, aes(x=t_mean, y=pcp, color=factor(PCT_bin))) +
  ggtitle("Discharge Percentile basee on Temperature and Precipitation Anomaly") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab(expression('Temperature Anomaly ('*degree*C*')')) +
  ylab ("Precipitation Anomaly (mm)") + 
  scale_color_brewer(name="Discharge\nPercentile", palette = "RdBu", direction = -1, breaks = c("6", "5", "4", "3", "2", "1"), labels = c(">90", "70-90", "50-70", "30-50", "10-30","<10"))

ggplot() +
  geom_point(data= Centroid, aes(x=ave_t, y=ave_pcp, color = factor(PCT_bin))) +
  ggtitle("Centroid of Discharge Percentile basee on Anomaly") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab(expression('Temperature Anomaly ('*degree*C*')')) +
  ylab ("Precipitation Anomaly (mm)") + 
  theme_dark() +
  scale_color_brewer(name="Discharge\nPercentile", palette = "RdBu", direction = -1, breaks = c("6", "5", "4", "3", "2", "1"), labels = c(">90", "70-90", "50-70", "30-50", "10-30","<10"))
