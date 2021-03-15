library(pacman)
pacman::p_load(tidyverse)

# Subset stations
Coastal <- 61.5
North <- 67.5

data_AK_subset<- data_AK_final %>%
  filter(dec_lat_va>North)

sites_subset <- semi_join(sites2, data_AK_subset, by = "site_no")
  
climate <- read.csv("climateyear.csv", header = FALSE)
names <- c("site_no", "wt_year", "t_mean", "pcp")
colnames(climate) <- names

climate <- climate %>%
  mutate(site_no = as.character(site_no))

qy_final <- semi_join(qy, sites_subset, by = "site_no")

qy_percentile <- qy_final %>% 
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
  mutate(ave_t = mean(t_mean, na.rm=TRUE), ave_pcp = mean(pcp, na.rm=TRUE)) %>%
  distinct(PCT_bin, .keep_all = TRUE)

colorpal <- c("dodgerblue4", "dodgerblue", "aliceblue", "salmon", "firebrick1", "red3")

ggplot() +
  geom_hline(yintercept=0) + 
  geom_vline(xintercept = 0)+
  geom_point(data = Analysis, aes(x=t_mean, y=pcp, color=factor(PCT_bin)), alpha=0.65) +
  ggtitle("Annual Discharge and Centroids for Northern Alaska") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill=NA, size=1), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey")) +
  xlab(expression('Temperature Anomaly ('*degree*C*')')) +
  ylab ("Precipitation Anomaly (mm)") + 
  scale_color_manual(values = colorpal, breaks = c("6", "5", "4", "3", "2", "1"),  guide = "none") +
  geom_point(data= Centroid, shape = 21, aes(x=ave_t, y=ave_pcp, fill=factor(PCT_bin)), color="black", size = 4, stroke=1) + 
  scale_fill_manual(name="Percentile\nCentroid", values = colorpal, breaks = c("6", "5", "4", "3", "2", "1"), labels = c(">90", "70-90", "50-70", "30-50", "10-30","<10"))

#Cold Analysis
climatecold <- read.csv("climatecold.csv", header = FALSE)
names <- c("site_no", "wt_year", "t_mean", "pcp")
colnames(climatecold) <- names

climatecold <- climatecold %>%
  mutate(site_no = as.character(site_no))

#Calculate Cold Weather Discharge
cold <- sites_subset %>%
  group_by(site_no) %>%
  filter(4>=as.numeric(Month) | 11>=as.numeric(Month))

cold_criteria <- 160

cd <- cold %>%
  count(site_no, wt_year) %>%
  filter(n<cold_criteria)

cold_discharge <- anti_join(cold, cd) %>%
  group_by(site_no, wt_year) %>%
  summarise(Cold_Discharge = mean(X_00060_00003))

cd_percentile <- cold_discharge %>% 
  group_by(site_no) %>%
  arrange(Cold_Discharge)

cd_percentile <- cd_percentile %>%
  mutate(PCT = ntile(site_no, 10)) %>%
  mutate(PCT_bin = ifelse(PCT==1,1,ifelse(PCT<=3,2,ifelse(PCT<=5,3,ifelse(PCT<=7,4,ifelse(PCT<=9,5,6)))))) %>%
  arrange(site_no, wt_year)

Analysis <- inner_join(cd_percentile, climatecold)

Analysis <- Analysis %>% 
  mutate(PCT_bin = as.numeric(PCT_bin), pcp = as.numeric(pcp), t_mean = as.numeric(t_mean))

Centroid <- Analysis %>%
  group_by(PCT_bin) %>%
  mutate(ave_t = mean(t_mean, na.rm=TRUE), ave_pcp = mean(pcp, na.rm=TRUE)) %>%
  distinct(PCT_bin, .keep_all = TRUE) %>%
  mutate(PCT_cent = PCT_bin + 5)

ggplot() +
  geom_hline(yintercept=0) + 
  geom_vline(xintercept = 0)+
  geom_point(data = Analysis, aes(x=t_mean, y=pcp, color=factor(PCT_bin)), alpha=0.65) +
  ggtitle("Cold Months (NDJFMA) Discharge and Centroids for Northern Alaska") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill=NA, size=1), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey")) +
  xlab(expression('Temperature Anomaly ('*degree*C*')')) +
  ylab ("Precipitation Anomaly (mm)") + 
  scale_color_manual(name="Discharge\nPercentile", values = colorpal, breaks = c("6", "5", "4", "3", "2", "1"), labels = c(">90", "70-90", "50-70", "30-50", "10-30","<10"), guide = "none") +
  geom_point(data= Centroid, shape = 21, aes(x=ave_t, y=ave_pcp, fill=factor(PCT_bin)), color="black", size = 4, stroke=1) + 
  scale_fill_manual(name="Percentile\nCentroid", values = colorpal, breaks = c("6", "5", "4", "3", "2", "1"), labels = c(">90", "70-90", "50-70", "30-50", "10-30","<10"))
  
#Winter Analysis
climate <- read.csv("climatewinter.csv", header = FALSE)
names <- c("site_no", "wt_year", "t_mean", "pcp")
colnames(climate) <- names

climate <- climate %>%
  mutate(site_no = as.character(site_no))

#Calculate Cold Weather Discharge
winter <- sites_subset %>%
  group_by(site_no) %>%
  filter(2>=as.numeric(Month) | 12 == as.numeric(Month))

winter_criteria <- 80

wd <- winter %>%
  count(site_no, wt_year) %>%
  filter(n<winter_criteria)

winter_discharge <- anti_join(winter, wd) %>%
  group_by(site_no, wt_year) %>%
  summarise(Winter_Discharge = mean(X_00060_00003))

wd_percentile <- winter_discharge %>% 
  group_by(site_no) %>%
  arrange(Winter_Discharge)

wd_percentile <- wd_percentile %>%
  mutate(PCT = ntile(site_no, 10)) %>%
  mutate(PCT_bin = ifelse(PCT==1,1,ifelse(PCT<=3,2,ifelse(PCT<=5,3,ifelse(PCT<=7,4,ifelse(PCT<=9,5,6)))))) %>%
  arrange(site_no, wt_year)

Analysis <- inner_join(wd_percentile, climate)

Analysis <- Analysis %>% 
  mutate(PCT_bin = as.numeric(PCT_bin), pcp = as.numeric(pcp), t_mean = as.numeric(t_mean))

Centroid <- Analysis %>%
  group_by(PCT_bin) %>%
  mutate(ave_t = mean(t_mean, na.rm=TRUE), ave_pcp = mean(pcp, na.rm=TRUE)) %>%
  distinct(PCT_bin, .keep_all = TRUE)

ggplot() +
  geom_hline(yintercept=0) + 
  geom_vline(xintercept = 0)+
  geom_point(data = Analysis, aes(x=t_mean, y=pcp, color=factor(PCT_bin)), alpha=0.65) +
  ggtitle("Winter Months (DJF) Discharge and Centroids for Northern Alaska") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill=NA, size=1), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey")) +
  xlab(expression('Temperature Anomaly ('*degree*C*')')) +
  ylab ("Precipitation Anomaly (mm)") + 
  scale_color_manual(name="Discharge\nPercentile", values = colorpal, breaks = c("6", "5", "4", "3", "2", "1"), labels = c(">90", "70-90", "50-70", "30-50", "10-30","<10"), guide = "none") +
  geom_point(data= Centroid, shape = 21, aes(x=ave_t, y=ave_pcp, fill=factor(PCT_bin)), color="black", size = 4, stroke=1) + 
  scale_fill_manual(name="Percentile\nCentroid", values = colorpal, breaks = c("6", "5", "4", "3", "2", "1"), labels = c(">90", "70-90", "50-70", "30-50", "10-30","<10"))

#Spring Analysis
climate <- read.csv("climatespring.csv", header = FALSE)
names <- c("site_no", "wt_year", "t_mean", "pcp")
colnames(climate) <- names

climate <- climate %>%
  mutate(site_no = as.character(site_no))

#Calculate Spring Weather Discharge
spring <- sites_subset %>%
  group_by(site_no) %>%
  filter(3 <= as.numeric(Month) & 5 >= as.numeric(Month))

spring_criteria <- 75

sd <- spring %>%
  count(site_no, wt_year) %>%
  filter(n<spring_criteria)

spring_discharge <- anti_join(spring, sd) %>%
  group_by(site_no, wt_year) %>%
  summarise(Spring_Discharge = mean(X_00060_00003))

sd_percentile <- spring_discharge %>% 
  group_by(site_no) %>%
  arrange(Spring_Discharge)

sd_percentile <- sd_percentile %>%
  mutate(PCT = ntile(site_no, 10)) %>%
  mutate(PCT_bin = ifelse(PCT==1,1,ifelse(PCT<=3,2,ifelse(PCT<=5,3,ifelse(PCT<=7,4,ifelse(PCT<=9,5,6)))))) %>%
  arrange(site_no, wt_year)

Analysis <- inner_join(sd_percentile, climate)

Analysis <- Analysis %>% 
  mutate(PCT_bin = as.numeric(PCT_bin), pcp = as.numeric(pcp), t_mean = as.numeric(t_mean))

Centroid <- Analysis %>%
  group_by(PCT_bin) %>%
  mutate(ave_t = mean(t_mean, na.rm=TRUE), ave_pcp = mean(pcp, na.rm=TRUE)) %>%
  distinct(PCT_bin, .keep_all = TRUE)

ggplot() +
  geom_hline(yintercept=0) + 
  geom_vline(xintercept = 0)+
  geom_point(data = Analysis, aes(x=t_mean, y=pcp, color=factor(PCT_bin)), alpha=0.65) +
  ggtitle("Spring Months (MAM) Discharge and Centroids for Northern Alaska") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill=NA, size=1), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey")) +
  xlab(expression('Temperature Anomaly ('*degree*C*')')) +
  ylab ("Precipitation Anomaly (mm)") + 
  scale_color_manual(name="Discharge\nPercentile", values = colorpal, breaks = c("6", "5", "4", "3", "2", "1"), labels = c(">90", "70-90", "50-70", "30-50", "10-30","<10"), guide = "none") +
  geom_point(data= Centroid, shape = 21, aes(x=ave_t, y=ave_pcp, fill=factor(PCT_bin)), color="black", size = 4, stroke=1) + 
  scale_fill_manual(name="Percentile\nCentroid", values = colorpal, breaks = c("6", "5", "4", "3", "2", "1"), labels = c(">90", "70-90", "50-70", "30-50", "10-30","<10"))

#Summer Analysis
climate <- read.csv("climatesummer.csv", header = FALSE)
names <- c("site_no", "wt_year", "t_mean", "pcp")
colnames(climate) <- names

climate <- climate %>%
  mutate(site_no = as.character(site_no))

#Calculate Summer Weather Discharge
summer <- sites_subset %>%
  group_by(site_no) %>%
  filter(6 <= as.numeric(Month) & 8 >= as.numeric(Month))

summer_criteria <- 80

sd <- summer %>%
  count(site_no, wt_year) %>%
  filter(n<summer_criteria)

summer_discharge <- anti_join(summer, sd) %>%
  group_by(site_no, wt_year) %>%
  summarise(Summer_Discharge = mean(X_00060_00003))

sd_percentile <- summer_discharge %>% 
  group_by(site_no) %>%
  arrange(Summer_Discharge)

sd_percentile <- sd_percentile %>%
  mutate(PCT = ntile(site_no, 10)) %>%
  mutate(PCT_bin = ifelse(PCT==1,1,ifelse(PCT<=3,2,ifelse(PCT<=5,3,ifelse(PCT<=7,4,ifelse(PCT<=9,5,6)))))) %>%
  arrange(site_no, wt_year)

Analysis <- inner_join(sd_percentile, climate)

Analysis <- Analysis %>% 
  mutate(PCT_bin = as.numeric(PCT_bin), pcp = as.numeric(pcp), t_mean = as.numeric(t_mean))

Centroid <- Analysis %>%
  group_by(PCT_bin) %>%
  mutate(ave_t = mean(t_mean, na.rm=TRUE), ave_pcp = mean(pcp, na.rm=TRUE)) %>%
  distinct(PCT_bin, .keep_all = TRUE)

ggplot() +
  geom_hline(yintercept=0) + 
  geom_vline(xintercept = 0)+
  geom_point(data = Analysis, aes(x=t_mean, y=pcp, color=factor(PCT_bin)), alpha=0.65) +
  ggtitle("Summer Months (JJA) Discharge and Centroids for Alaska") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill=NA, size=1), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey")) +
  xlab(expression('Temperature Anomaly ('*degree*C*')')) +
  ylab ("Precipitation Anomaly (mm)") + 
  scale_color_manual(name="Discharge\nPercentile", values = colorpal, breaks = c("6", "5", "4", "3", "2", "1"), labels = c(">90", "70-90", "50-70", "30-50", "10-30","<10"), guide = "none") +
  geom_point(data= Centroid, shape = 21, aes(x=ave_t, y=ave_pcp, fill=factor(PCT_bin)), color="black", size = 4, stroke=1) + 
  scale_fill_manual(name="Percentile\nCentroid", values = colorpal, breaks = c("6", "5", "4", "3", "2", "1"), labels = c(">90", "70-90", "50-70", "30-50", "10-30","<10"))

#Warm Analysis
climate <- read.csv("climatewarm.csv", header = FALSE)
names <- c("site_no", "wt_year", "t_mean", "pcp")
colnames(climate) <- names

climate <- climate %>%
  mutate(site_no = as.character(site_no))

#Calculate Warm Weather Discharge
warm <- sites_subset %>%
  group_by(site_no) %>%
  filter(4<as.numeric(Month) & 11>as.numeric(Month))

warm_criteria <- 160

warmd <- warm %>%
  count(site_no, wt_year) %>%
  filter(n<warm_criteria)

warm_discharge <- anti_join(warm, warmd) %>%
  group_by(site_no, wt_year) %>%
  summarise(Warm_Discharge = mean(X_00060_00003))

warmd_percentile <- warm_discharge %>% 
  group_by(site_no) %>%
  arrange(Warm_Discharge)

warmd_percentile <- warmd_percentile %>%
  mutate(PCT = ntile(site_no, 10)) %>%
  mutate(PCT_bin = ifelse(PCT==1,1,ifelse(PCT<=3,2,ifelse(PCT<=5,3,ifelse(PCT<=7,4,ifelse(PCT<=9,5,6)))))) %>%
  arrange(site_no, wt_year)

Analysis <- inner_join(warmd_percentile, climate)

Analysis <- Analysis %>% 
  mutate(PCT_bin = as.numeric(PCT_bin), pcp = as.numeric(pcp), t_mean = as.numeric(t_mean))

Centroid <- Analysis %>%
  group_by(PCT_bin) %>%
  mutate(ave_t = mean(t_mean, na.rm=TRUE), ave_pcp = mean(pcp, na.rm=TRUE)) %>%
  distinct(PCT_bin, .keep_all = TRUE)

ggplot() +
  geom_hline(yintercept=0) + 
  geom_vline(xintercept = 0)+
  geom_point(data = Analysis, aes(x=t_mean, y=pcp, color=factor(PCT_bin)), alpha=0.65) +
  ggtitle("Warm Months (MJJASO) Discharge and Centroids for Northern Alaska") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill=NA, size=1), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey")) +
  xlab(expression('Temperature Anomaly ('*degree*C*')')) +
  ylab ("Precipitation Anomaly (mm)") + 
  scale_color_manual(name="Discharge\nPercentile", values = colorpal, breaks = c("6", "5", "4", "3", "2", "1"), labels = c(">90", "70-90", "50-70", "30-50", "10-30","<10"), guide = "none") +
  geom_point(data= Centroid, shape = 21, aes(x=ave_t, y=ave_pcp, fill=factor(PCT_bin)), color="black", size = 4, stroke=1) + 
  scale_fill_manual(name="Percentile\nCentroid", values = colorpal, breaks = c("6", "5", "4", "3", "2", "1"), labels = c(">90", "70-90", "50-70", "30-50", "10-30","<10"))

#Fall Analysis
climate <- read.csv("climatefall.csv", header = FALSE)
names <- c("site_no", "wt_year", "t_mean", "pcp")
colnames(climate) <- names

climate <- climate %>%
  mutate(site_no = as.character(site_no))

#Calculate Fall Weather Discharge
fall <- sites_subset %>%
  group_by(site_no) %>%
  filter(9<=as.numeric(Month) & 11>=as.numeric(Month))

fall_criteria <- 80

fd <- fall %>%
  count(site_no, wt_year) %>%
  filter(n<fall_criteria)

fall_discharge <- anti_join(fall, fd) %>%
  group_by(site_no, wt_year) %>%
  summarise(Fall_Discharge = mean(X_00060_00003))

fd_percentile <- fall_discharge %>% 
  group_by(site_no) %>%
  arrange(Fall_Discharge)

fd_percentile <- fd_percentile %>%
  mutate(PCT = ntile(site_no, 10)) %>%
  mutate(PCT_bin = ifelse(PCT==1,1,ifelse(PCT<=3,2,ifelse(PCT<=5,3,ifelse(PCT<=7,4,ifelse(PCT<=9,5,6)))))) %>%
  arrange(site_no, wt_year)

Analysis <- inner_join(fd_percentile, climate)

Analysis <- Analysis %>% 
  mutate(PCT_bin = as.numeric(PCT_bin), pcp = as.numeric(pcp), t_mean = as.numeric(t_mean))

Centroid <- Analysis %>%
  group_by(PCT_bin) %>%
  mutate(ave_t = mean(t_mean, na.rm=TRUE), ave_pcp = mean(pcp, na.rm=TRUE)) %>%
  distinct(PCT_bin, .keep_all = TRUE)

ggplot() +
  geom_hline(yintercept=0) + 
  geom_vline(xintercept = 0)+
  geom_point(data = Analysis, aes(x=t_mean, y=pcp, color=factor(PCT_bin)), alpha=0.65) +
  ggtitle("Fall Months (SON) Discharge and Centroids for Northern Alaska") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill=NA, size=1), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey")) +
  xlab(expression('Temperature Anomaly ('*degree*C*')')) +
  ylab ("Precipitation Anomaly (mm)") + 
  scale_color_manual(name="Discharge\nPercentile", values = colorpal, breaks = c("6", "5", "4", "3", "2", "1"), labels = c(">90", "70-90", "50-70", "30-50", "10-30","<10"), guide = "none") +
  geom_point(data= Centroid, shape = 21, aes(x=ave_t, y=ave_pcp, fill=factor(PCT_bin)), color="black", size = 4, stroke=1) + 
  scale_fill_manual(name="Percentile\nCentroid", values = colorpal, breaks = c("6", "5", "4", "3", "2", "1"), labels = c(">90", "70-90", "50-70", "30-50", "10-30","<10"))
