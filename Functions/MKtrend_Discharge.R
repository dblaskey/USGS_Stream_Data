MKtrend_Discharge = function(path_2_data){
  
  load(path_2_data)
  
  final_sites = filter(final_sites, wt_year>=1960)
  
  #Yearly mean discharge
  Mean_trend <- final_sites %>%
    group_by(site_name, wt_year) %>%
    summarise(Annual_Mean_Discharge = mean(X_00060_00003)) %>%
    drop_na() %>%
    ungroup(wt_year) %>%
    do(test_mk = pwmk(.$Annual_Mean_Discharge)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Annual Mean Discharge")%>%
    select(-test_mk)
  
  percentiles = c(0.1, 0.25, 0.5, 0.75, 0.9)
  
  percentile_trend = lapply(percentiles, function(tile){
    final_sites %>%
      group_by(site_name, wt_year) %>%
      summarise(Annual_tile_Discharge = quantile(X_00060_00003, tile)) %>%
      drop_na() %>%
      ungroup(wt_year) %>%
      do(test_mk = pwmk(.$Annual_tile_Discharge)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = paste0(tile*100, "th Percentile Discharge"))%>%
      select(-test_mk)
  }) %>%
    bind_rows()
  
  #Peak Flow
  ave_criteria <- 5
  
  peak_flow <- final_sites %>%
    group_by(site_name, wt_year) %>%
    mutate(ave_q = rollapply(X_00060_00003,  FUN = mean, width = ave_criteria, fill = NA, align = "center")) %>%
    filter(ave_q>0 & ave_q == max(ave_q, na.rm = TRUE)) %>%
    slice(1) %>%
    mutate(jday = yday(make_date(Year, Month, Day)))
  
  # Discharge
  PF_Q_trend <- peak_flow %>%
    group_by(site_name) %>%
    do(test_mk = pwmk(.$ave_q)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Peak Discharge")%>%
    select(-test_mk)

  # Date
  PF_Date_trend <- peak_flow %>%
    group_by(site_name) %>%
    do(test_mk = pwmk(.$jday)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Day of Peak Discharge")%>%
    select(-test_mk)
  
  # Min Flow
  min_flow <- final_sites %>%
    filter(Month <= 8) %>%
    group_by(site_name, wt_year) %>%
    mutate(ave_q = rollapply(X_00060_00003,  FUN = mean, width = ave_criteria, fill = NA, align = "center")) %>%
    filter(ave_q == min(ave_q, na.rm = TRUE)) %>%
    slice(n()) %>%
    mutate(jday = yday(make_date(Year, Month, Day)))
  
  # Discharge
  min_Q_trend <- min_flow %>%
    ungroup() %>%
    group_by(site_name) %>%
    mutate(ave = mean(ave_q)) %>%
    filter(ave>0) %>%
    do(test_mk = pwmk(.$ave_q)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Minimum Discharge")%>%
    select(-test_mk)
  
  # Date
  min_Date_trend <- min_flow %>%
    ungroup() %>%
    group_by(site_name) %>%
    do(test_mk = pwmk(.$jday)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Day of Minimum Discharge")%>%
    select(-test_mk)

  #Flashiness
  flashiness_trend <- final_sites %>%
    filter(Month == 8) %>%
    group_by(site_name) %>%
    mutate(date = make_date(Year, Month, Day)) %>%
    mutate(dq = ifelse(date-lag(date,default = date[1])>0 ,abs(X_00060_00003-lag(X_00060_00003)),NA)) %>%
    group_by(site_name, wt_year, Month) %>%
    summarise(sum_dq = sum(dq, na.rm=TRUE), sum_q = sum(X_00060_00003)) %>%
    mutate(dq_q = ifelse(sum_q>0, sum_dq/sum_q, NA)) %>%
    drop_na() %>%
    ungroup(wt_year, Month) %>%
    do(test_mk = pwmk(.$dq_q)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Flashiness")%>%
    select(-test_mk)
  
  #Centroid of Discharge
  Cumulative_Q_locations = c(0.25, 0.5, 0.75)

  Centroid_trend = lapply(Cumulative_Q_locations, function(locs){
    final_sites %>%
      group_by(site_name, wt_year) %>%
      mutate(Yearly_flow = locs*sum(X_00060_00003), cum_sum = cumsum(X_00060_00003)) %>%
      mutate(diff = Yearly_flow - cum_sum) %>%
      slice_min(order_by = abs(diff)) %>%
      mutate(jday = (as.integer(difftime(make_date(Year, Month, Day), make_date(Year, 01, 01), units = "days")))) %>%
      ungroup(wt_year) %>%
      do(test_mk = pwmk(.$jday)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = paste0("Day of ", locs*100, "% Discharge"))%>%
      select(-test_mk)
  }) %>%
    bind_rows()
  
  # Recession
  Fast_recess = 3
  
  Recession_trend <- final_sites %>%
    filter(as.numeric(Month)>6 & as.numeric(Month)<10) %>%
    group_by(site_name) %>%
    mutate(date = make_date(Year, Month, Day)) %>%
    mutate(dq = X_00060_00003-lag(X_00060_00003)) %>%
    mutate(Year=as.numeric(Year)) %>%
    ungroup() %>%
    group_by(ID = data.table::rleid(dq < 0)) %>%
    mutate(Consec_Days = if_else(dq < 0, row_number(), 0L)) %>%
    filter(Consec_Days>Fast_recess) %>%
    mutate(Rslope = -dq/X_00060_00003) %>%
    ungroup() %>%
    group_by(site_name, Year) %>%
    summarise(Annual_Rslope=median(Rslope)) %>%
    ungroup(Year) %>%
    do(test_mk = pwmk(.$Annual_Rslope)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
           test = "Recession")%>%
    select(-test_mk)
  
  data_AK_final = rbind(Mean_trend, percentile_trend, PF_Q_trend,
                        PF_Date_trend, flashiness_trend, Centroid_trend, Recession_trend,
                        min_Q_trend, min_Date_trend)
  
  save(data_AK_final, file=paste0("./Data/FinalDischarge_Analyzed_Data.RData"))
  
  ### By Month/Season
  
  # Month
  mon = seq(1,12)

  month_trend = lapply(mon, function(mon){
    final_sites %>%
      filter(Month == mon) %>%
      group_by(site_name, wt_year) %>%
      summarise(Month_mean_Discharge = mean(X_00060_00003)) %>%
      ungroup(wt_year) %>%
      do(test_mk = pwmk(.$Month_mean_Discharge)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             test = paste0(month.abb[mon]))%>%
      select(-test_mk)
  }) %>%
    bind_rows()
  
  month_trend$test = factor(month_trend$test, levels = c(month.abb[10:12], month.abb[1:9]))
  
  save(month_trend, file=paste0("./Data/FinalDischargeTemporal_Analyzed_Data.RData"))
  
}
