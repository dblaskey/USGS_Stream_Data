MKtrend_Climate = function(path_df){
  
  climate_temp <- read.csv(path_df, header = TRUE)
  
  climate_temp <- left_join(site_list, climate_temp) %>%
    mutate(Date=lubridate::ymd_hms(Date)) %>%
    mutate(Year = lubridate::year(Date), Month = lubridate::month(Date), Day = lubridate::day(Date)) %>%
    mutate(wt_year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year))) %>%
    mutate(Precip_tot = Precip*3600*24) %>%#convert from avearge precipitation rate to total precip
    group_by(site_no) %>% 
    mutate(Snow_acc = Snow_Depth - lag(Snow_Depth)) %>% #Accumulated snow depth
    filter(wt_year>=1960) %>%
    mutate(Rain = ifelse(Precip_tot - Snow_acc >= 0, Precip_tot - Snow_acc, 0))
  
  #Yearly mean discharge
  Mean_temp = climate_temp %>%
      group_by(site_no, wt_year) %>%
      summarise(Annual_Mean = mean(Temperature)) %>%
      drop_na() %>%
      ungroup(wt_year) %>%
      do(test_mk = pwmk(.$Annual_Mean)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = "Mean Temperature")%>%
      select(-test_mk)
  
  # Total Precipitation
  total_precip = climate_temp %>%
    group_by(site_no, wt_year) %>%
    summarise(Annual_sum = sum(Precip_tot)) %>%
    drop_na() %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$Annual_sum)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Precipitation")%>%
    select(-test_mk)
  
  # Total Rain
  total_rain = climate_temp %>%
    group_by(site_no, wt_year) %>%
    summarise(Annual_sum = sum(Rain)) %>%
    drop_na() %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$Annual_sum)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Rain")%>%
    select(-test_mk)
  
  # Total Days of Precipitation
  total_days_precip = climate_temp %>%
    group_by(site_no, wt_year) %>%
    summarise(Annual_days = sum(Precip_tot>1)) %>%
    drop_na() %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$Annual_days)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Days of Precipitation")%>%
    select(-test_mk)
  
  #Snow Days
  Snow_days = climate_temp %>%
    group_by(site_no, wt_year) %>%
    summarise(Annual_days = sum(Snow_acc>0.01)) %>%
    drop_na() %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$Annual_days)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Days of Snow")%>%
    select(-test_mk)
  
  #Snow Precip
  Snow_amount = climate_temp %>%
    mutate(Snow=ifelse(Snow_acc>0, Snow_acc, 0)) %>%
    group_by(site_no, wt_year) %>%
    summarise(Annual_days = sum(Snow)) %>%
    drop_na() %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$Annual_days)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Total Snowfall (SWE)")%>%
    select(-test_mk)
  
  #Snow Days
  Snow_depth = climate_temp %>%
    group_by(site_no, wt_year) %>%
    summarise(max_snowpack = max(Snow_Depth)) %>%
    drop_na() %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$max_snowpack)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Max Snow Depth")%>%
    select(-test_mk)
  
  # Rain Precip
  total_days_warm_precip = climate_temp %>%
    group_by(site_no, wt_year) %>%
    summarise(Annual_days = sum(Precip_tot>10 & Temperature > 273)) %>%
    drop_na() %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$Annual_days)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Days of Rain")%>%
    select(-test_mk)
  
  # Annual above freezing
  annual_warm_days = climate_temp %>%
    group_by(site_no, wt_year) %>%
    summarise(Days_above_freezing = sum(as.numeric(Temperature)>273)) %>%
    drop_na() %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$Days_above_freezing)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Days above Freezing")%>%
    select(-test_mk)
  
  # Spring Melt
  thawing_days = climate_temp %>%
    filter(as.numeric(Month) >= 1 & as.numeric(Month) <= 7) %>%
    mutate(above_freezing = ifelse(Temperature>273,1,0)) %>%
    group_by(site_no, wt_year, ID = data.table::rleid(above_freezing == 1)) %>%
    mutate(Consec_Days = if_else(above_freezing == 1, row_number(), 0L)) %>%
    filter(Consec_Days == 5) %>%
    group_by(site_no, wt_year) %>%
    slice(1) %>%
    mutate(jday = yday(make_date(Year, Month, Day))) %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$jday)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Date of Spring Melt")%>%
    select(-test_mk)
  
  # Winter Freeze
  freeze_days = climate_temp %>%
    filter(as.numeric(Month) >= 8 | as.numeric(Month) <= 1) %>%
    mutate(below_freezing = ifelse(Temperature<273.15,1,0)) %>%
    group_by(site_no, wt_year, ID = data.table::rleid(below_freezing == 1)) %>%
    mutate(Consec_Days = if_else(below_freezing == 1, row_number(), 0L)) %>%
    filter(Consec_Days == 5) %>%
    group_by(site_no, wt_year) %>%
    slice(1) %>%
    mutate(jday = yday(make_date(Year, Month, Day))) %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$jday)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Date of Winter Freeze")%>%
    select(-test_mk)
  
  climate_trends = rbind(Mean_temp,
                         total_precip, total_days_precip, total_days_warm_precip,
                         annual_warm_days, Snow_amount, 
                         Snow_days, Snow_depth, thawing_days, freeze_days)
  
  climate_trends = left_join(climate_trends, site_list)
  
  save(climate_trends, file="./Data/Analyzed_ClimateA_Data.RData")
  
  ### Monthly climate change ###
  
  climate_temp <- read.csv(path_df, header = TRUE)
  
    climate_temp <- left_join(site_list, climate_temp) %>%
      mutate(Date=lubridate::ymd_hms(Date)) %>%
      mutate(Year = lubridate::year(Date), Month = lubridate::month(Date), Day = lubridate::day(Date)) %>%
      mutate(wt_year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year))) %>%
      mutate(Precip_tot = Precip*3600*24) %>% #convert from avearge precipitation rate to total precip
      group_by(site_no) %>% 
      mutate(Snow_acc = Snow_Depth - lag(Snow_Depth)) %>% #Accumulated snow depth
      filter(wt_year>=1960) %>%
      mutate(Rain = ifelse(Precip_tot - Snow_acc >= 0, Precip_tot - Snow_acc, 0))
    
    
    # Soil Moisture on April 1st
    Jan1_moisture = climate_temp %>%
      group_by(site_no, wt_year, Month) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(ave_soil_moisture = rowMeans(.[,8:11])) %>%
      group_by(site_no, Month) %>%
      do(test_mk = pwmk(.$ave_soil_moisture)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = "Soil Moisture on 1st of Month")%>%
      select(-test_mk)
    
    # mean temp
    Mean_temp = climate_temp %>%
      group_by(site_no, wt_year, Month) %>%
      summarise(Annual_Mean = mean(Temperature)) %>%
      group_by(site_no, Month) %>%
      do(test_mk = pwmk(.$Annual_Mean)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = "Mean Temperature")%>%
      select(-test_mk)
    
    #Melt Sum
    Melt_sum = climate_temp %>%
      group_by(site_no, wt_year, Month) %>%
      mutate(Melt = ifelse(Snow_acc <= 0, abs(Snow_acc), 0)) %>%
      summarise(sum_melt = sum(Melt)) %>%
      group_by(site_no, Month) %>%
      mutate(ave = mean(sum_melt)) %>%
      filter(ave>0) %>%
      do(test_mk = pwmk(.$sum_melt)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = "Ablation (SWE)")%>%
      select(-test_mk)
    
    # Winter Soil Temperature
    winter_soil_temp = climate_temp %>%
      ungroup() %>%
      mutate(day_soil_temp = rowMeans(.[,12:15])) %>%
      group_by(site_no, wt_year, Month) %>%
      summarise(ave_soil_temp = mean(day_soil_temp)) %>%
      group_by(site_no, Month) %>%
      do(test_mk = pwmk(.$ave_soil_temp)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = "Mean Soil Temperature")%>%
      select(-test_mk)
    
    # Winter Soil Moisture
    winter_soil_moisture = climate_temp %>%
      ungroup() %>%
      mutate(day_soil_water = rowMeans(.[,8:11])) %>%
      group_by(site_no, wt_year, Month) %>%
      summarise(ave_soil_water = mean(day_soil_water)) %>%
      group_by(site_no, Month) %>%
      do(test_mk = pwmk(.$ave_soil_water)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = "Mean Soil Moisture")%>%
      select(-test_mk)
    
    # Winter Top Soil Temperature
    winter_topsoil_temp = climate_temp %>%
      ungroup() %>%
      group_by(site_no, wt_year, Month) %>%
      summarise(ave_soil_temp = mean(Soil_temp_1)) %>%
      group_by(site_no, Month) %>%
      do(test_mk = pwmk(.$ave_soil_temp)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = "Mean Top Layer Soil Temperature")%>%
      select(-test_mk)
    
    # Winter Soil Moisture
    winter_topsoil_moisture = climate_temp %>%
      ungroup() %>%
      #mutate(day_soil_water = rowMeans(.[,8:9])) %>%
      group_by(site_no, wt_year, Month) %>%
      summarise(ave_soil_water = mean(Soil_moisture_1)) %>%
      group_by(site_no, Month) %>%
      do(test_mk = pwmk(.$ave_soil_water)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = "Mean Top Layer Soil Moisture")%>%
      select(-test_mk)
    
    # Total Precipitation
    total_precip = climate_temp %>%
      group_by(site_no, wt_year, Month) %>%
      summarise(Annual_sum = sum(Precip_tot)) %>%
      drop_na() %>%
      group_by(site_no, Month) %>%
      do(test_mk = pwmk(.$Annual_sum)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = "Precipitation")%>%
      select(-test_mk)
    
    # Total Days of Precipitation
    total_days_precip = climate_temp %>%
      group_by(site_no, wt_year, Month) %>%
      summarise(Annual_days = sum(Precip_tot>1)) %>%
      drop_na() %>%
      group_by(site_no, Month) %>%
      do(test_mk = pwmk(.$Annual_days)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = "Days of Precipitation")%>%
      select(-test_mk)
    
    #Snow Days
    Snow_days = climate_temp %>%
      group_by(site_no, wt_year, Month) %>%
      summarise(Annual_days = sum(Snow_acc>0.01)) %>%
      drop_na() %>%
      group_by(site_no, Month) %>%
      mutate(ave = mean(Annual_days)) %>%
      filter(ave>0) %>%
      do(test_mk = pwmk(.$Annual_days)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = "Days of Snow")%>%
      select(-test_mk)
    
    #Snow Precip
    Snow_amount = climate_temp %>%
      mutate(Snow=ifelse(Snow_acc>0, Snow_acc, 0)) %>%
      group_by(site_no, wt_year, Month) %>%
      summarise(Annual_days = sum(Snow)) %>%
      drop_na() %>%
      group_by(site_no, Month) %>%
      mutate(ave = mean(Annual_days)) %>%
      filter(ave>0) %>%
      do(test_mk = pwmk(.$Annual_days)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = "Snow")%>%
      select(-test_mk)
    
    #Snow depth
    Snow_depth = climate_temp %>%
      group_by(site_no, wt_year, Month) %>%
      summarise(max_snowpack = mean(Snow_Depth)) %>%
      drop_na() %>%
      group_by(site_no, Month) %>%
      mutate(ave = mean(max_snowpack)) %>%
      filter(ave>0) %>%
      do(test_mk = pwmk(.$max_snowpack)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = "Mean SWE in Snowpack")%>%
      select(-test_mk)
    
    # Rain Precip
    total_days_warm_precip = climate_temp %>%
      group_by(site_no, wt_year, Month) %>%
      summarise(Annual_days = sum(Precip_tot>10 & Temperature > 273)) %>%
      group_by(site_no, Month) %>%
      mutate(ave = mean(Annual_days)) %>%
      filter(ave>0) %>%
      do(test_mk = pwmk(.$Annual_days)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = "Days of Rain")%>%
      select(-test_mk)
    
    # Rain total
    total_winter_rain = climate_temp %>%
      group_by(site_no, wt_year, Month) %>%
      mutate(rain = ifelse(Temperature > 273.15, Precip_tot, 0)) %>%
      summarise(rain_tot = sum(rain)) %>%
      group_by(site_no, Month) %>%
      mutate(ave = mean(rain_tot)) %>%
      filter(ave>0) %>%
      do(test_mk = pwmk(.$rain_tot)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = "Rain")%>%
      select(-test_mk)
    
    # Days above freezing
    warm_days = climate_temp %>%
      group_by(site_no, wt_year, Month) %>%
      summarise(Days_above_freezing = sum(as.numeric(Temperature)>273)) %>%
      group_by(site_no, Month) %>%
      mutate(ave = mean(Days_above_freezing), diff = Days_above_freezing - mean(Days_above_freezing)) %>%
      filter(ave>0 & diff!=0) %>%
      do(test_mk = pwmk(.$Days_above_freezing)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = "Days above Freezing")%>%
      select(-test_mk)
    
    #Melt Days
    Melt_days = climate_temp %>%
      group_by(site_no, wt_year, Month) %>%
      summarise(Annual_days = sum(Snow_acc < -0.001)) %>%
      group_by(site_no, Month) %>%
      mutate(ave = mean(Annual_days)) %>%
      filter(ave>0) %>%
      do(test_mk = pwmk(.$Annual_days)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = "Days of Melt")%>%
      select(-test_mk)
    
    climate_trends3 = rbind(Mean_temp, winter_soil_temp,
                            Melt_sum, Snow_depth, total_winter_rain,
                            warm_days, Snow_amount, winter_soil_moisture) 
    
    # Can add total_days_precip, Snow_days, total_days_warm_precip, 
    # total_precip, freeze_moisture
    climate_trends3 = left_join(climate_trends3, site_list)
    
    climate_trends3$test = factor(climate_trends3$test, levels = 
                                    c("Ablation (SWE)", "Mean Soil Moisture",
                                      "Mean Soil Temperature", "Mean SWE in Snowpack", "Snow",
                                      "Rain", "Days above Freezing", "Mean Temperature"))
  
    temp_df <- climate_trends3 %>%
      ungroup() %>%
      mutate(sig = ifelse(0.01 >= as.numeric(P_Value) & as.numeric(Sens_Slope)>0, 
                          "Increasing at 0.01 Significance", 
                          ifelse(0.05 >= as.numeric(P_Value) & as.numeric(Sens_Slope)>0, 
                                 "Increasing at 0.05 Significance", 
                                        ifelse(0.01 >= as.numeric(P_Value) & as.numeric(Sens_Slope)<0, 
                                               "Decreasing at 0.01 Significance", 
                                               ifelse(0.05 >= as.numeric(P_Value) & as.numeric(Sens_Slope)<0, 
                                                      "Decreasing at 0.05 Significance", "Not Significant")))))
    
    temp_df$Month = month.abb[temp_df$Month]
    temp_df$Month = factor(temp_df$Month, levels = month.abb[1:12])
    temp_df$site_name = factor(temp_df$site_name, levels = 
                               c("Nuyakuk River", "Kenai River",
                                 "Ship Creek", "Little Susitna River", "Kuskokwim River",
                                 "Susitna River", "Salcha River", "Chena River", "Kuparuk River"))
    
    
    colorpal <- c("#762a83", "#af8dc3", "#1b7837", "#7fbf7b", "white")
    ggplot(data=temp_df, aes(x=site_name, y=test)) + 
      facet_wrap(vars(Month), nrow = 4) +
      geom_tile(aes(fill = sig), color = "black") +
      ggtitle("Monthly Climate Trends By Basin") +
      theme_bw() +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_discrete(expand = c(0,0)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5),
            strip.text = element_text(face="bold", size=9),
            axis.text.x = element_text(angle = 45, size = 10, vjust = 1, hjust=1),
            strip.background = element_rect(fill="white", colour="black"),
            strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"))) +
      scale_fill_manual(name="", values = colorpal)
    
  ggsave(path = "./documents/figures/", filename="Monthly_climate", width = 12, height = 12, device="jpeg", dpi=700)
  
  save(climate_trends3, file="./Data/Analyzed_ClimateApril_Data.RData")
  
}
  