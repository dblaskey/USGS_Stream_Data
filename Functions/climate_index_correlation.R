climate_index_correlation = function (path_df, year_length){
  
  # Read site data
  load(path_df)
  # Read Climate data obtained from https://psl.noaa.gov/data/climateindices/
  filenames <- list.files('/Users/dybl4375/USGS_Stream_Data_Pull/Data/raw/', 
                          pattern = '.data$', full.names = TRUE) 
  
  # Creates an average index over the time period specified above for each water year
  climate_data=lapply(filenames, function(x) {
    data <- read.table(x,
                       header = FALSE)
    names(data)=c("Year", 1:12)
    data = data %>%
      gather(Month, index, -Year) %>%
      mutate(wt_year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year))) %>%
      filter(wt_year>=(2020-year_length) & wt_year<2020) %>%
      group_by(wt_year, Month) %>%
      summarise(subset_index = mean(index))
    return(data)
  })
  
  # Combine climate data files
  climate_df=as.data.frame(cbind(climate_data[[1]][[1]], climate_data[[1]][[2]], climate_data[[1]][[3]],
                                 climate_data[[2]][[3]], climate_data[[3]][[3]], climate_data[[4]][[3]]))
  names(climate_df)=c("wt_year", "Month", "AO", "ENSO", "PDO", "PNA")
  
  flow = final_sites %>%
    mutate(wt_year = as.numeric(wt_year), Month = as.numeric(Month)) %>%
    group_by(site_name, wt_year, Month) %>%
    summarise(Q = sum(X_00060_00003)) 
        
  climate_df_temp = climate_df %>%
    mutate(wt_year = as.numeric(wt_year), Month = as.numeric(Month)) %>%
    group_by(wt_year, Month) %>%
    summarise(AO = mean(as.numeric(AO)), PDO = mean(as.numeric(PDO)), PNA = mean(as.numeric(PNA)), ENSO = mean(as.numeric(ENSO)))
        
  # Create long format
  corr_df_long = left_join(climate_df_temp, flow) %>%
    gather(clim_var, index, -c(Q,site_name,wt_year,Month)) %>%
    drop_na()
  
  # Month and corr
  anom_plot = corr_df_long %>%
    group_by(site_name, Month) %>%
    mutate(ave_month = mean(Q)) %>%
    ungroup() %>%
    mutate(percent_anom = (Q-ave_month)/ave_month) %>%
    group_by(wt_year, Month, clim_var, index) %>%
    summarise(mean_anom = mean(percent_anom)) 
  
  anom_plot_reduced = anom_plot %>%
    filter(clim_var == "PDO")
  
  # Test correlation using spearman
  corr_results = corr_df_long %>%
    filter(wt_year >= 1990) %>%
    group_by(site_name, clim_var, Month) %>%
    do(corr=as.numeric(cor(.$index, .$Q, method = "spearman"))) %>%
    mutate(corr = as.numeric(corr), site_name = as.character(site_name)) %>%
    replace(is.na(.), 0)
  
  corr_average = corr_results %>%
    drop_na() %>%
    group_by(Month, clim_var) %>%
    summarise(Ave_cor = mean(corr))
  
  #aa_cor = corr_average %>%
   # pivot_wider(names_from = clim_var, values_from = Ave_cor)
  
  # Historic period
  corr_results = corr_df_long %>%
    filter(wt_year < 1990) %>%
    group_by(site_name, clim_var, Month) %>%
    do(corr=as.numeric(cor(.$index, .$Q, method = "spearman"))) %>%
    mutate(corr = as.numeric(corr), site_name = as.character(site_name)) %>%
    replace(is.na(.), 0)
  
  corr_averageh = corr_results %>%
    drop_na() %>%
    group_by(Month, clim_var) %>%
    summarise(Ave_corh = mean(corr))
  
 
  corr_averagecb = left_join(corr_averageh, corr_average) %>%
    pivot_longer(c(Ave_cor, Ave_corh), names_to = "type", values_to = "corr")
  
  corr_averagecb = corr_averagecb %>% 
    mutate(Month = month.abb[Month]) %>%
    mutate(Month = factor(Month, levels = month.abb))
  
 # corr_averagecb$clim_var <- factor(corr_averagecb$clim_var, levels = c("t_ave", "pcp_ave", "soil_temp", "soil_moist"),
  #                                  labels = c("Air Temperature", "Precipitation", "Soil Temperature", "Soil Moisture"))
  
  
  ggplot(corr_averagecb, aes(x = Month, y = corr, fill = type)) +
    geom_col(position = "dodge") +
    scale_x_discrete(name = "Month", breaks = month.abb, labels = substr(month.abb, 1,1)) +
    scale_y_continuous(name = "Mean Spearman Rank Correlation", limits=c(-1, 1)) +
    facet_wrap(~ clim_var) +
    scale_fill_manual(name = "", values = c("#E1BE6A", "#40B0A6"), labels = c("Recent (1990-2019)", "Historic (1960-1989)")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          strip.text = element_text(face="bold", size=9),
          strip.background = element_rect(fill="white", colour="black"),
          strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")),
          legend.position ="bottom")
  
  ggsave(path = "./documents/figures/", filename='climatic_index_correlation_panel', width = 6.5, height = 6, device="jpeg", dpi=700)
  
  
  ggplot(data=corr_average, aes(x=Month, y=Ave_cor)) +
    geom_line(aes(color=clim_var)) +
    scale_x_continuous(name = "Month", breaks = 1:12, labels = substr(month.abb, 1,1)) +
    scale_y_continuous(name = "Average Correlation", limits=c(-1, 1)) +
    ggtitle("Correlation of Hydroclimatic Conditions to Discharge") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          strip.text = element_text(face="bold", size=9),
          strip.background = element_rect(fill="white", colour="black"),
          strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"))) +
    scale_color_manual(name = "Correlation", values = c("blue", "red", "purple", "green"))
  
  
  ggplot(data=corr_results, aes(x=site_name, y=clim_var)) +
    facet_wrap(vars(Month)) +
    geom_tile(aes(fill = corr)) +
    xlab("USGS Sites") +
    ylab("Climate Index") +
    ggtitle("Correlation of Climate Indexes to Discharge") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          strip.text = element_text(face="bold", size=9),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          strip.background = element_rect(fill="white", colour="black"),
          strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"))) +
    scale_fill_continuous_divergingx(name = "Correlation", palette = 'RdBu', mid = 0, limits = c(-0.8,0.8))
  
  ggsave(path = "./documents/figures/", filename='climate_index_correlation', width = 6.5, height = 6, device="jpeg", dpi=700)
  
  # Long term trend of climate record
  
  # Month
  mon = seq(1,12)
  
  climate_df_long =  climate_df_temp %>%
    pivot_longer(c(AO, PDO, PNA, ENSO), names_to = "type", values_to = "value")
  
  
  index_month_trend = lapply(mon, function(mon){
    climate_df_long %>%
      filter(Month == mon) %>%
      group_by(type) %>%
      #summarise(Month_mean_Discharge = mean(X_00060_00003)) %>%
      #ungroup(wt_year) %>%
      do(test_mk = mkttest(.$value)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[5],
             test = paste0(month.abb[mon]))%>%
      select(-test_mk)
  }) %>%
    bind_rows()
  
  
  # season
  flow = final_sites %>%
    mutate(wt_year = as.numeric(wt_year), Month = as.numeric(Month)) %>%
    mutate(season = ifelse(Month>2 & Month<6, "Spring", ifelse(Month>5 & Month <9, "Summer", ifelse(Month>8 & Month<12, "Autum", "Winter")))) %>%
    group_by(site_name, wt_year, season) %>%
    summarise(Q = sum(X_00060_00003)) 
  
  climate_df_temp = climate_df %>%
    mutate(wt_year = as.numeric(wt_year), Month = as.numeric(Month)) %>%
    mutate(season = ifelse(Month>2 & Month<6, "Spring", ifelse(Month>5 & Month <9, "Summer", ifelse(Month>8 & Month<12, "Autum", "Winter")))) %>%
    group_by(wt_year, season) %>%
    summarise(AO = mean(as.numeric(AO)), PDO = mean(as.numeric(PDO)), PNA = mean(as.numeric(PNA)), ENSO = mean(as.numeric(ENSO)))
  
  # Create long format
  corr_df_long = left_join(climate_df_temp, flow) %>%
    gather(clim_var, index, -c(Q,site_name,wt_year,season)) %>%
    drop_na()
  
  # Test correlation using spearman
  corr_results = corr_df_long %>%
    group_by(site_name, clim_var, season) %>%
    do(corr=as.numeric(cor(.$index, .$Q, method = "spearman"))) %>%
    mutate(corr = as.numeric(corr), site_name = as.character(site_name)) %>%
    replace(is.na(.), 0)
  
  ggplot(data=corr_results, aes(x=site_name, y=clim_var)) +
    facet_wrap(vars(season)) +
    geom_tile(aes(fill = corr)) +
    xlab("USGS Sites") +
    ylab("Climate Index") +
    ggtitle("Correlation of Climate Indexes to Discharge") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          strip.text = element_text(face="bold", size=9),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          strip.background = element_rect(fill="white", colour="black"),
          strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"))) +
    scale_fill_continuous_divergingx(name = "Correlation", palette = 'RdBu', mid = 0, limits = c(-0.8,0.8))
  
  ggsave(path = "./documents/figures/", filename='climate_index_correlation', width = 6.5, height = 6, device="jpeg", dpi=700)
  
  # Test multiple regression
  

  
  # Create long format
#  corr_df_long = correlation_matrix %>%
 #   select(-start_month_index) %>%
  #  gather(clim_var, index, -c(Q,site_name,wt_year,Timespan)) %>%
   # drop_na()
  
#  linear_model = correlation_matrix %>%
 #   select(-start_month_index) %>%
  #  group_by(site_name, Timespan) %>%
   # do(linearfit=lm(Q ~ AO + PNA + PDO + ENSO, data=.)) %>%
    #mutate(r_square = summary(linearfit)$r.squared)
}