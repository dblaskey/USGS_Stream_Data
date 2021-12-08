MKtrend_Climate = function(path_df){
  
  climate_temp <- read.csv(path_df, header = TRUE)
  
  climate_temp <- left_join(site_list, climate_temp) %>%
    mutate(Date=lubridate::ymd_hms(Date)) %>%
    mutate(Year = lubridate::year(Date), Month = lubridate::month(Date), Day = lubridate::day(Date)) %>%
    mutate(wt_year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year))) %>%
    mutate(Precip_tot = Precip*3600*24) %>% #convert from avearge precipitation rate to total precip
    group_by(site_no) %>% 
    mutate(Snow_acc = Snow_Depth - lag(Snow_Depth)) %>% #Accumulated snow depth
    filter(wt_year>=1960)
  
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
  
  ### Winter climate change ###
  
  # Soil Moisture at Freezeup
  freeze_moisture = climate_temp %>%
    filter(as.numeric(Month) >= 8 | as.numeric(Month) <= 1) %>%
    mutate(below_freezing = ifelse(Soil_temp_1<273.15,1,0)) %>%
    group_by(site_no, wt_year, ID = data.table::rleid(below_freezing == 1)) %>%
    mutate(Consec_Days = if_else(below_freezing == 1, row_number(), 0L)) %>%
    filter(Consec_Days == 5) %>%
    group_by(site_no, wt_year) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(ave_soil_moisture = rowMeans(.[,8:11])) %>%
    group_by(site_no) %>%
    do(test_mk = pwmk(.$ave_soil_moisture)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Soil Moisture at Freeze Up")%>%
    select(-test_mk)
  
  #Melt frac
  Melt_frac = climate_temp %>%
    group_by(site_no, wt_year) %>%
    mutate(Melt = ifelse(Snow_acc <= 0, Snow_acc, 0)) %>%
    mutate(tot_melt = sum(Melt)) %>%
    filter(Month>=12 | Month<=2) %>%
    mutate(DJF_melt = sum(Melt)) %>%
    mutate(melt_frac = DJF_melt/tot_melt) %>%
    slice(1) %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$melt_frac)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Winter Melt Fraction")%>%
    select(-test_mk)
  
  climate_temp = climate_temp %>%
    filter(Month<=5 | Month<=10)
  
  # Soil Moisture on Jan 1st
  Jan1_moisture = climate_temp %>%
    group_by(site_no, wt_year) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(ave_soil_moisture = rowMeans(.[,8:11])) %>%
    group_by(site_no) %>%
    do(test_mk = pwmk(.$ave_soil_moisture)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Soil Moisture on Jan 1st")%>%
    select(-test_mk)
  
  # mean temp
  Mean_temp = climate_temp %>%
    group_by(site_no, wt_year) %>%
    summarise(Annual_Mean = mean(Temperature)) %>%
    drop_na() %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$Annual_Mean)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Mean Temperature")%>%
    select(-test_mk)
  
  #Melt Sum
  Melt_sum = climate_temp %>%
    group_by(site_no, wt_year) %>%
    mutate(Melt = ifelse(Snow_acc <= 0, abs(Snow_acc), 0)) %>%
    summarise(DJF_melt = sum(Melt)) %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$DJF_melt)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Winter Melt (Total SWE)")%>%
    select(-test_mk)
  
  # Winter Soil Temperature
  winter_soil_temp = climate_temp %>%
    ungroup() %>%
    mutate(day_soil_temp = rowMeans(.[,12:15])) %>%
    group_by(site_no, wt_year) %>%
    summarise(ave_soil_temp = mean(day_soil_temp)) %>%
    do(test_mk = pwmk(.$ave_soil_temp)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Mean Soil Temperature")%>%
    select(-test_mk)
  
  # Winter Soil Moisture
  winter_soil_moisture = climate_temp %>%
    ungroup() %>%
    mutate(day_soil_water = rowMeans(.[,8:11])) %>%
    group_by(site_no, wt_year) %>%
    summarise(ave_soil_water = mean(day_soil_water)) %>%
    do(test_mk = pwmk(.$ave_soil_water)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Mean Soil Moisture")%>%
    select(-test_mk)
  
  # Winter Top Soil Temperature
  winter_topsoil_temp = climate_temp %>%
    ungroup() %>%
    #mutate(day_soil_temp = rowMeans(.[,12:13])) %>%
    group_by(site_no, wt_year) %>%
    summarise(ave_soil_temp = mean(Soil_temp_1)) %>%
    do(test_mk = pwmk(.$ave_soil_temp)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Mean Top Layer Soil Temperature")%>%
    select(-test_mk)
  
  # Winter Soil Moisture
  winter_topsoil_moisture = climate_temp %>%
    ungroup() %>%
    #mutate(day_soil_water = rowMeans(.[,8:9])) %>%
    group_by(site_no, wt_year) %>%
    summarise(ave_soil_water = mean(Soil_moisture_1)) %>%
    do(test_mk = pwmk(.$ave_soil_water)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Mean Top Layer Soil Moisture")%>%
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
           test = "Total Snow (SWE)")%>%
    select(-test_mk)
  
  #Snow depth
  Snow_depth = climate_temp %>%
    group_by(site_no, wt_year) %>%
    summarise(max_snowpack = mean(Snow_Depth)) %>%
    drop_na() %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$max_snowpack)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Mean Snow Depth")%>%
    select(-test_mk)
  
  # Rain Precip
  total_days_warm_precip = climate_temp %>%
    group_by(site_no, wt_year) %>%
    summarise(Annual_days = sum(Precip_tot>10 & Temperature > 273)) %>%
    ungroup(wt_year) %>%
    mutate(ave = mean(Annual_days)) %>%
    filter(ave>0) %>%
    do(test_mk = pwmk(.$Annual_days)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Days of Rain")%>%
    select(-test_mk)
  
  # Rain total
  total_winter_rain = climate_temp %>%
    group_by(site_no, wt_year) %>%
    mutate(rain = ifelse(Temperature > 273.15, Precip_tot, 0)) %>%
    summarise(rain_tot = sum(rain)) %>%
    ungroup(wt_year) %>%
    mutate(ave = mean(rain_tot)) %>%
    filter(ave>0) %>%
    do(test_mk = pwmk(.$rain_tot)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Total Rain")%>%
    select(-test_mk)
  
  # Days above freezing
  warm_days = climate_temp %>%
    group_by(site_no, wt_year) %>%
    summarise(Days_above_freezing = sum(as.numeric(Temperature)>273)) %>%
    ungroup(wt_year) %>%
    mutate(ave = mean(Days_above_freezing)) %>%
    filter(ave>0) %>%
    do(test_mk = pwmk(.$Days_above_freezing)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Days above Freezing")%>%
    select(-test_mk)
  
  #Melt Days
  Melt_days = climate_temp %>%
    group_by(site_no, wt_year) %>%
    summarise(Annual_days = sum(Snow_acc < -0.001)) %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$Annual_days)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Days of Melt")%>%
    select(-test_mk)
  
  climate_trends2 = rbind(Mean_temp, winter_soil_temp,
                         Melt_sum, Snow_depth,
                         warm_days, Snow_amount, winter_soil_moisture, total_winter_rain,
                         winter_topsoil_moisture, 
                         winter_topsoil_temp) 
  
  # Can add total_days_precip, Melt_frac, Melt_days, Snow_days, total_days_warm_precip, 
  # total_precip, freeze_moisture
  
  climate_trends2 = left_join(climate_trends2, site_list)
  
  save(climate_trends2, file="./Data/Analyzed_ClimateW_Data.RData")
  
  ### April climate change ###
  
  climate_temp <- read.csv(path_df, header = TRUE)
  
  climate_temp <- left_join(site_list, climate_temp) %>%
    mutate(Date=lubridate::ymd_hms(Date)) %>%
    mutate(Year = lubridate::year(Date), Month = lubridate::month(Date), Day = lubridate::day(Date)) %>%
    mutate(wt_year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year))) %>%
    mutate(Precip_tot = Precip*3600*24) %>% #convert from avearge precipitation rate to total precip
    group_by(site_no) %>% 
    mutate(Snow_acc = Snow_Depth - lag(Snow_Depth)) %>% #Accumulated snow depth
    filter(wt_year>=1960 & Month == 4)
  
  # Soil Moisture on April 1st
  Jan1_moisture = climate_temp %>%
    group_by(site_no, wt_year) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(ave_soil_moisture = rowMeans(.[,8:11])) %>%
    group_by(site_no) %>%
    do(test_mk = pwmk(.$ave_soil_moisture)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Soil Moisture on April 1st")%>%
    select(-test_mk)
  
  # mean temp
  Mean_temp = climate_temp %>%
    group_by(site_no, wt_year) %>%
    summarise(Annual_Mean = mean(Temperature)) %>%
    drop_na() %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$Annual_Mean)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Mean Temperature")%>%
    select(-test_mk)
  
  #Melt Sum
  Melt_sum = climate_temp %>%
    group_by(site_no, wt_year) %>%
    mutate(Melt = ifelse(Snow_acc <= 0, abs(Snow_acc), 0)) %>%
    summarise(DJF_melt = sum(Melt)) %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$DJF_melt)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Melt (Total SWE)")%>%
    select(-test_mk)
  
  # Winter Soil Temperature
  winter_soil_temp = climate_temp %>%
    ungroup() %>%
    mutate(day_soil_temp = rowMeans(.[,12:15])) %>%
    group_by(site_no, wt_year) %>%
    summarise(ave_soil_temp = mean(day_soil_temp)) %>%
    do(test_mk = pwmk(.$ave_soil_temp)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Mean Soil Temperature")%>%
    select(-test_mk)
  
  # Winter Soil Moisture
  winter_soil_moisture = climate_temp %>%
    ungroup() %>%
    mutate(day_soil_water = rowMeans(.[,8:11])) %>%
    group_by(site_no, wt_year) %>%
    summarise(ave_soil_water = mean(day_soil_water)) %>%
    do(test_mk = pwmk(.$ave_soil_water)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Mean Soil Moisture")%>%
    select(-test_mk)
  
  # Winter Top Soil Temperature
  winter_topsoil_temp = climate_temp %>%
    ungroup() %>%
    #mutate(day_soil_temp = rowMeans(.[,12:13])) %>%
    group_by(site_no, wt_year) %>%
    summarise(ave_soil_temp = mean(Soil_temp_1)) %>%
    do(test_mk = pwmk(.$ave_soil_temp)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Mean Top Layer Soil Temperature")%>%
    select(-test_mk)
  
  # Winter Soil Moisture
  winter_topsoil_moisture = climate_temp %>%
    ungroup() %>%
    #mutate(day_soil_water = rowMeans(.[,8:9])) %>%
    group_by(site_no, wt_year) %>%
    summarise(ave_soil_water = mean(Soil_moisture_1)) %>%
    do(test_mk = pwmk(.$ave_soil_water)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Mean Top Layer Soil Moisture")%>%
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
    mutate(ave = mean(Annual_days)) %>%
    filter(ave>0) %>%
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
    mutate(ave = mean(Annual_days)) %>%
    filter(ave>0) %>%
    do(test_mk = pwmk(.$Annual_days)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Total Snowfall (SWE)")%>%
    select(-test_mk)
  
  #Snow depth
  Snow_depth = climate_temp %>%
    group_by(site_no, wt_year) %>%
    summarise(max_snowpack = mean(Snow_Depth)) %>%
    drop_na() %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$max_snowpack)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Mean Snow Depth")%>%
    select(-test_mk)
  
  # Rain Precip
  total_days_warm_precip = climate_temp %>%
    group_by(site_no, wt_year) %>%
    summarise(Annual_days = sum(Precip_tot>10 & Temperature > 273)) %>%
    ungroup(wt_year) %>%
    mutate(ave = mean(Annual_days)) %>%
    filter(ave>0) %>%
    do(test_mk = pwmk(.$Annual_days)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Days of Rain")%>%
    select(-test_mk)
  
  # Rain total
  total_winter_rain = climate_temp %>%
    group_by(site_no, wt_year) %>%
    mutate(rain = ifelse(Temperature > 273.15, Precip_tot, 0)) %>%
    summarise(rain_tot = sum(rain)) %>%
    ungroup(wt_year) %>%
    mutate(ave = mean(rain_tot)) %>%
    filter(ave>0) %>%
    do(test_mk = pwmk(.$rain_tot)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Total Rain")%>%
    select(-test_mk)
  
  # Days above freezing
  warm_days = climate_temp %>%
    group_by(site_no, wt_year) %>%
    summarise(Days_above_freezing = sum(as.numeric(Temperature)>273)) %>%
    ungroup(wt_year) %>%
    mutate(ave = mean(Days_above_freezing)) %>%
    filter(ave>0) %>%
    do(test_mk = pwmk(.$Days_above_freezing)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Days above Freezing")%>%
    select(-test_mk)
  
  #Melt Days
  Melt_days = climate_temp %>%
    group_by(site_no, wt_year) %>%
    summarise(Annual_days = sum(Snow_acc < -0.001)) %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$Annual_days)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Days of Melt")%>%
    select(-test_mk)
  
  climate_trends3 = rbind(Mean_temp, winter_soil_temp, Melt_days,
                          Melt_sum, Snow_depth, Jan1_moisture,
                          warm_days, Snow_amount, winter_soil_moisture, total_winter_rain,
                          winter_topsoil_moisture, 
                          winter_topsoil_temp) 
  
  # Can add total_days_precip, Snow_days, total_days_warm_precip, 
  # total_precip, freeze_moisture
  
  climate_trends3 = left_join(climate_trends3, site_list)
  
  save(climate_trends3, file="./Data/Analyzed_ClimateApril_Data.RData")
  
}
  