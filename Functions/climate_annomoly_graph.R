climate_annomoly_graph <- function(df_path, clim_first_month, clim_last_month, 
                           dis_first_month, dis_last_month, season, dis, place){
  df=loadRData(df_path) %>%
    mutate(site_no = as.numeric(site_no))
  
  climate_temp <- read.csv("/Users/dybl4375/USGS_Stream_Data_Pull/Data/raw/climate_data_final", header = TRUE)
  
  climate_temp <- left_join(site_list, climate_temp) %>%
    mutate(Date=lubridate::ymd_hms(Date)) %>%
    mutate(Year = lubridate::year(Date), Month = lubridate::month(Date), Day = lubridate::day(Date)) %>%
    mutate(wt_year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year))) %>%
    mutate(Precip_tot = Precip*3600*24) %>% #convert from avearge precipitation rate to total precip
    group_by(site_no) %>% 
    mutate(Snow_acc = Snow_Depth - lag(Snow_Depth)) %>% #Accumulated snow depth
    filter(wt_year>=1960)

  sites_subset <- left_join(df, site_list) %>%
    group_by(site_no) %>%
    mutate(ave_Q = mean(X_00060_00003, na.rm = TRUE))
  
if (clim_first_month == dis_first_month & clim_first_month<=clim_last_month){
  if ( 10 > clim_first_month & 10 <= clim_last_month){
    climate <- climate_temp %>%
      group_by(site_no, Year) %>%
      filter(as.numeric(Month) >= clim_first_month & as.numeric(Month) <= clim_last_month) %>%
      summarise(t_annual=mean(Temperature), pcp_annual = sum(Precip_tot)) %>%
      ungroup() %>%
      group_by(site_no) %>%
      mutate(t_mean = t_annual-mean(t_annual), pcp = (pcp_annual-mean(pcp_annual))/mean(pcp_annual)*100) %>%
      rename(wt_year = Year)

  qy <- sites_subset %>%
    filter(as.numeric(Month) >= dis_first_month & as.numeric(Month) <= dis_last_month) %>%
    group_by(site_no, Year) %>%
    summarise(Annual_Discharge = sum(X_00060_00003)) %>%
    rename(wt_year = Year) 
  } else {
    climate <- climate_temp %>%
      group_by(site_no, wt_year) %>%
      filter(as.numeric(Month) >= clim_first_month & as.numeric(Month) <= clim_last_month) %>%
      summarise(t_annual=mean(Temperature), pcp_annual = sum(Precip_tot)) %>%
      ungroup() %>%
      group_by(site_no) %>%
      mutate(t_mean = t_annual-mean(t_annual), pcp = (pcp_annual-mean(pcp_annual))/mean(pcp_annual)*100)
      
    qy <- sites_subset %>%
      filter(as.numeric(Month) >= dis_first_month & as.numeric(Month) <= dis_last_month) %>%
      group_by(site_no, wt_year) %>%
      summarise(Annual_Discharge = sum(X_00060_00003)) 
  }
} else if (clim_first_month == dis_first_month & clim_first_month>clim_last_month){
    climate <- climate_temp %>%
      group_by(site_no, wt_year) %>%
      filter(as.numeric(Month) >= clim_first_month | as.numeric(Month) <= clim_last_month) %>%
      summarise(t_annual=mean(Temperature), pcp_annual = sum(Precip_tot)) %>%
      ungroup() %>%
      group_by(site_no) %>%
      mutate(t_mean = t_annual-mean(t_annual), pcp = (pcp_annual-mean(pcp_annual))/mean(pcp_annual)*100)
      
    qy <- sites_subset %>%
      filter(as.numeric(Month) >= dis_first_month | as.numeric(Month) <= dis_last_month) %>%
      group_by(site_no, wt_year) %>%
      summarise(Annual_Discharge = sum(X_00060_00003))
}
else if (dis_first_month < dis_last_month & clim_first_month > clim_last_month){
    climate <- climate_temp %>%
      group_by(site_no, wt_year) %>%
      filter(as.numeric(Month) >= clim_first_month | as.numeric(Month) <= clim_last_month) %>%
      summarise(t_annual=mean(Temperature), pcp_annual = sum(Precip_tot)) %>%
      ungroup() %>%
      group_by(site_no) %>%
      mutate(t_mean = t_annual-mean(t_annual), pcp = (pcp_annual-mean(pcp_annual))/mean(pcp_annual)*100)
      
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

Analysis <- inner_join(qy_percentile, climate) %>% 
  mutate(PCT_bin = as.numeric(PCT_bin), pcp = as.numeric(pcp), t_mean = as.numeric(t_mean))

Centroid <- Analysis %>%
  group_by(PCT_bin) %>%
  mutate(ave_t = mean(t_mean, na.rm=TRUE), ave_pcp = mean(pcp, na.rm=TRUE)) %>%
  distinct(PCT_bin, .keep_all = TRUE)

#colorpal <- c("#b2182b", "#ef8a62", "#fddbc7",  "#d1e5f0", "#67a9cf", "#2166ac")
colorpal <- c("#762a83", "#af8dc3", "#e7d4e8", "#d9f0d3", "#7fbf7b", "#1b7837")

ggplot() +
  geom_hline(yintercept=0) + 
  geom_vline(xintercept = 0)+
  geom_point(data = Analysis, aes(x=t_mean, y=pcp, color=factor(PCT_bin)), alpha=0.75) +
  ggtitle(paste(season,"Discharge vs", dis, "and Centroids for", place)) +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill=NA, size=1), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey")) +
  xlab(expression('Temperature Anomaly ('*degree*C*')')) +
  ylab ("Precipitation Anomaly (% Change)") + 
  scale_color_manual(values = rev(colorpal), breaks = c("6", "5", "4", "3", "2", "1"),  guide = "none") +
  geom_point(data= Centroid, shape = 21, aes(x=ave_t, y=ave_pcp, fill=factor(PCT_bin)), color="black", size = 4, stroke=1) + 
  scale_fill_manual(name="Discharge\nPercentile", values = rev(colorpal), breaks = c("6", "5", "4", "3", "2", "1"), labels = c(">90", "70-90", "50-70", "30-50", "10-30","<10"))

  ggsave(path = "./documents/figures/", filename=paste(season,"Discharge_vs", dis, "and_Centroids_for", place, sep="_"), width = 7, height = 5, device="jpeg", dpi=700)
}
