rm(list = ls())

library(pacman)
pacman::p_load(tidyverse)

load("/Users/dybl4375/USGS_Stream_Data_Pull/40yearsfinal.Rdata")
load("/Users/dybl4375/USGS_Stream_Data_Pull/40yearsites.Rdata")

sites_subset <- full_join(final, data_AK_final, by = "site_no")
  
climate_temp <- read.csv("/Users/dybl4375/USGS_Stream_Data_Pull/climate_data", header = FALSE)
names <- c("t_tot", "pcp_tot", "Date", "COMID")
colnames(climate_temp) <- names

COMIDS <- unique(climate_temp$COMID)
site_nos <- c("1548400", "15276000", "15515500", "15290000", "15302000", "15304000",
              "15514000", "15356000", "1525800", "15292000")

df <- data.frame(COMIDS, site_nos)
colnames(df) <- c("COMID", "site_no")

climate_temp <- full_join(df,climate_temp) %>%
  mutate(Date=lubridate::ymd_hms(Date)) %>%
  mutate(Year = lubridate::year(Date), Month = lubridate::month(Date), Day = lubridate::day(Date)) %>%
  mutate(wt_year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year)))

annomoly_graph <- function(clim_first_month, clim_last_month, 
                           dis_first_month, dis_last_month, season, dis, place){

if (clim_first_month == dis_first_month & clim_first_month<clim_last_month){
climate <- climate_temp %>%
  group_by(site_no, wt_year) %>%
  filter(as.numeric(Month) >= clim_first_month & as.numeric(Month) <= clim_last_month) %>%
  summarise(t_annual=mean(t_tot), pcp_annual = sum(pcp_tot)) %>%
  ungroup() %>%
  group_by(site_no) %>%
  mutate(t_mean = t_annual-mean(t_annual), pcp = (pcp_annual-mean(pcp_annual)))

qy <- sites_subset %>%
  filter(as.numeric(Month) >= dis_first_month & as.numeric(Month) <= dis_last_month) %>%
  group_by(site_no, wt_year) %>%
  summarise(Annual_Discharge = sum(X_00060_00003))
}
else if (clim_first_month == dis_first_month & clim_first_month>clim_last_month){
    climate <- climate_temp %>%
      group_by(site_no, wt_year) %>%
      filter(as.numeric(Month) >= clim_first_month | as.numeric(Month) <= clim_last_month) %>%
      summarise(t_annual=mean(t_tot), pcp_annual = sum(pcp_tot)) %>%
      ungroup() %>%
      group_by(site_no) %>%
      mutate(t_mean = t_annual-mean(t_annual), pcp = (pcp_annual-mean(pcp_annual)))
    
    qy <- sites_subset %>%
      filter(as.numeric(Month) >= dis_first_month | as.numeric(Month) <= dis_last_month) %>%
      group_by(site_no, wt_year) %>%
      summarise(Annual_Discharge = sum(X_00060_00003))
}
else if (dis_first_month < dis_last_month & clim_first_month > clim_last_month){
    climate <- climate_temp %>%
      group_by(site_no, wt_year) %>%
      filter(as.numeric(Month) >= clim_first_month | as.numeric(Month) <= clim_last_month) %>%
      summarise(t_annual=mean(t_tot), pcp_annual = sum(pcp_tot)) %>%
      ungroup() %>%
      group_by(site_no) %>%
      mutate(t_mean = t_annual-mean(t_annual), pcp = (pcp_annual-mean(pcp_annual)))
    
    qy <- sites_subset %>%
      filter(as.numeric(Month) >= dis_first_month & as.numeric(Month) <= dis_last_month) %>%
      group_by(site_no, wt_year) %>%
      summarise(Annual_Discharge = sum(X_00060_00003))
}
else {
  print("error")
}  

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
  mutate(ave_t = mean(t_mean, na.rm=TRUE), ave_pcp = mean(pcp, na.rm=TRUE)) %>%
  distinct(PCT_bin, .keep_all = TRUE)

colorpal <- c("dodgerblue4", "dodgerblue", "aliceblue", "salmon", "firebrick1", "red3")

ggplot() +
  geom_hline(yintercept=0) + 
  geom_vline(xintercept = 0)+
  geom_point(data = Analysis, aes(x=t_mean, y=pcp, color=factor(PCT_bin)), alpha=0.65) +
  ggtitle(paste(season,"Discharge vs", dis, "and Centroids for", place)) +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill=NA, size=1), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey")) +
  xlab(expression('Temperature Anomaly ('*degree*C*')')) +
  ylab ("Precipitation Anomaly (mm)") + 
  scale_color_manual(values = colorpal, breaks = c("6", "5", "4", "3", "2", "1"),  guide = "none") +
  geom_point(data= Centroid, shape = 21, aes(x=ave_t, y=ave_pcp, fill=factor(PCT_bin)), color="black", size = 4, stroke=1) + 
  scale_fill_manual(name="Percentile\nCentroid", values = colorpal, breaks = c("6", "5", "4", "3", "2", "1"), labels = c(">90", "70-90", "50-70", "30-50", "10-30","<10"))
}

annomoly_graph(12,2,12,2,"Winter", "Winter Climate", "Alaska")
annomoly_graph(3,5,3,5,"Spring", "Spring Climate", "Alaska")
annomoly_graph(6,8,6,8,"Summer", "Summer Climate","Alaska")
annomoly_graph(9,11,9,11,"Fall", "Fall Climate","Alaska")
annomoly_graph(11,4,11,4,"Cold Season", "Cold Climate","Alaska")
annomoly_graph(5,10,5,10,"Warm Season", "Warm Climate","Alaska")
annomoly_graph(1,12,1,12,"Annual", "Annual Climate","Alaska")
annomoly_graph(11,6,4,6,"AMJ", "NDJFMAMJ Climate","Alaska")
annomoly_graph(11,3,4,6,"AMJ", "NDJFM Climate","Alaska")

# Analysis for Specific Station
sites_subset <- filter(sites_subset, as.numeric(site_no)==15515500)

annomoly_graph(12,2,12,2,"Winter", "Winter Climate", "The Tanana River")
annomoly_graph(3,5,3,5,"Spring", "Spring Climate", "The Tanana River")
annomoly_graph(6,8,6,8,"Summer", "Summer Climate","The Tanana River")
annomoly_graph(9,11,9,11,"Fall", "Fall Climate","The Tanana River")
annomoly_graph(11,4,11,4,"Cold Season", "Cold Climate","The Tanana River")
annomoly_graph(5,10,5,10,"Warm Season", "Warm Climate","The Tanana River")
annomoly_graph(1,12,1,12,"Annual", "Annual Climate","The Tanana River")

# Analysis by location
Coastal <- 61.5
North <- 67.5

sites_subset <- sites_subset %>%
filter(dec_lat_va>North)

# Testing
climate <- climate_temp %>%
  group_by(site_no, wt_year) %>%
  filter(as.numeric(Month) >= 11 | as.numeric(Month) <= 6) %>%
  summarise(t_annual=mean(t_tot), pcp_annual = sum(pcp_tot)) %>%
  ungroup() %>%
  group_by(site_no) %>%
  mutate(t_mean = t_annual-mean(t_annual), pcp = (pcp_annual-mean(pcp_annual)))

qy <- sites_subset %>%
  filter(as.numeric(Month) >= 4 & as.numeric(Month) <= 6) %>%
  group_by(site_no, wt_year) %>%
  summarise(Annual_Discharge = sum(X_00060_00003))

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
  mutate(ave_t = mean(t_mean, na.rm=TRUE), ave_pcp = mean(pcp, na.rm=TRUE)) %>%
  distinct(PCT_bin, .keep_all = TRUE)

colorpal <- c("dodgerblue4", "dodgerblue", "aliceblue", "salmon", "firebrick1", "red3")

ggplot() +
  geom_hline(yintercept=0) + 
  geom_vline(xintercept = 0)+
  geom_point(data = Analysis, aes(x=t_mean, y=pcp, color=factor(PCT_bin)), alpha=0.65) +
  ggtitle("AMJ Discharge vs NDJFMAMJ Climateand Centroids for") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill=NA, size=1), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey")) +
  xlab(expression('Temperature Anomaly ('*degree*C*')')) +
  ylab ("Precipitation Anomaly (mm)") + 
  scale_color_manual(values = colorpal, breaks = c("6", "5", "4", "3", "2", "1"),  guide = "none") +
  geom_point(data= Centroid, shape = 21, aes(x=ave_t, y=ave_pcp, fill=factor(PCT_bin)), color="black", size = 4, stroke=1) + 
  scale_fill_manual(name="Percentile\nCentroid", values = colorpal, breaks = c("6", "5", "4", "3", "2", "1"), labels = c(">90", "70-90", "50-70", "30-50", "10-30","<10"))


