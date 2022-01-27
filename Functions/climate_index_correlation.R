climate_index_correlation = function (path_df, year_length){
  
  # Read site data
  load(path_df)
  # Read Climate data obtained from https://psl.noaa.gov/data/climateindices/
  filenames <- list.files('/Users/dybl4375/USGS_Stream_Data_Pull/Data/raw/', 
                          pattern = '.long.data$', full.names = TRUE) 
  
  # Creates an average index over the time period specified above for each water year
  climate_data=lapply(filenames, function(x) {
    data <- read.table(x,
                       header = FALSE)
    names(data)=c("Year", 1:12)
    data = data %>%
      gather(Month, index, -Year) %>%
      mutate(wt_year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year))) %>%
      filter(wt_year>=(2019-year_length) & wt_year<2020) %>%
      group_by(wt_year, Month) %>%
      summarise(subset_index = mean(index))
    return(data)
  })
  
  # Combine climate data files
  climate_df=as.data.frame(cbind(climate_data[[1]][[1]], climate_data[[1]][[2]], climate_data[[1]][[3]],
                                 climate_data[[2]][[3]], climate_data[[3]][[3]], climate_data[[4]][[3]]))
  names(climate_df)=c("wt_year", "Month", "AO", "NP", "PDO", "SOI")
  
        flow = final_sites %>%
          mutate(wt_year = as.numeric(wt_year), Month = as.numeric(Month)) %>%
          group_by(site_name, wt_year, Month) %>%
          summarise(Q = sum(X_00060_00003)) 
        
        climate_df_temp = climate_df %>%
          mutate(wt_year = as.numeric(wt_year), Month = as.numeric(Month)) %>%
          group_by(wt_year, Month) %>%
          summarise(AO = mean(as.numeric(AO)), PDO = mean(as.numeric(PDO)), NP = mean(as.numeric(NP)), SOI = mean(as.numeric(SOI)))
        
  # Create long format
  corr_df_long = left_join(climate_df_temp, flow) %>%
    gather(clim_var, index, -c(Q,site_name,wt_year,Month)) %>%
    drop_na()
  
  # Test correlation using kendall
  corr_results = corr_df_long %>%
    group_by(site_name, clim_var, Month) %>%
    do(corr=as.numeric(cor(.$index, .$Q, method = "kendall"))) %>%
    mutate(corr = as.numeric(corr), site_name = as.character(site_name)) 
  
  corr_average = corr_results %>%
    group_by(Month, clim_var) %>%
    summarise(Ave_cor = mean(corr))
  
  ggplot(data=corr_average, aes(x=Month, y=Ave_cor)) +
    geom_line(aes(color=clim_var)) +
    scale_x_continuous(name = "Month", breaks = 1:12, labels = substr(month.abb, 1,1)) +
    scale_y_continuous(name = "Average Kendall Tau Correlation", limits=c(-1, 1)) +
    ggtitle("Correlation of Hydroclimatic Conditions to Discharge") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          strip.text = element_text(face="bold", size=9),
          strip.background = element_rect(fill="white", colour="black"),
          strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"))) +
    scale_color_manual(name = "Correlation", values = c("blue", "red", "purple", "green"))
  
  
  ggplot(data=corr_results, aes(x=site_name, y=clim_var)) +
    facet_wrap(vars(Month), scales = "free") +
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
    scale_fill_continuous_divergingx(name = "Correlation", palette = 'RdBu', mid = 0)
  
  ggsave(path = "./documents/figures/", filename='climate_index_correlation', width = 7, height = 5, device="jpeg", dpi=700)
  
  # Test multiple regression
  

  
  # Create long format
  corr_df_long = correlation_matrix %>%
    select(-start_month_index) %>%
    gather(clim_var, index, -c(Q,site_name,wt_year,Timespan)) %>%
    drop_na()
  
  linear_model = correlation_matrix %>%
    select(-start_month_index) %>%
    group_by(site_name, Timespan) %>%
    do(linearfit=lm(Q ~ AO + NP + PDO + SOI, data=.)) %>%
    mutate(r_square = summary(linearfit)$r.squared)
}