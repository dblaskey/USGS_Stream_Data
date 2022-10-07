MKtrend_Discharge = function(path_2_data){
  
  load(path_2_data)
  
  final_sites = filter(final_sites, wt_year>=1960)
  
  # Monthly mean
  month_stats <- final_sites %>%
    group_by(site_name, wt_year, Month) %>%
    summarise(sum_month = sum(X_00060_00003*3600*24)) %>%
    ungroup() %>%
    group_by(site_name, Month) %>% 
    mutate(percentile_bin = ifelse(
      sum_month > quantile(sum_month, 0.9), ">90th Percentile", 
      ifelse(sum_month > quantile(sum_month, 0.70), "70th - 90th Percentile", ifelse(
        sum_month > quantile(sum_month, 0.5), "50th - 75th Percentile", ifelse(
          sum_month > quantile(sum_month, 0.3), "30th - 50th Percentile", ifelse(
            sum_month > quantile(sum_month, 0.1), "10th - 30th Percentile", "<10th Percentile"
          )
        )
      ))))
  
  month_stats$percentile_bin <- factor(month_stats$percentile_bin, levels = 
                                  c(">90th Percentile", "70th - 90th Percentile", 
                                    "50th - 75th Percentile", "30th - 50th Percentile", 
                                    "10th - 30th Percentile", "<10th Percentile"))
  
  colorpal <- c("#762a83", "#af8dc3", "#e7d4e8", "#d9f0d3", "#7fbf7b", "#1b7837")
  
  ggplot(month_stats, aes(Month, wt_year, fill=percentile_bin)) + 
    facet_wrap(~site_name) +
    geom_tile() +
    xlab("Month") +
    ylab ("Year") + 
    scale_fill_manual(name="Discharge\nPercentile", values = rev(colorpal)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), 
          strip.text = element_text(face="bold", size=9),
          legend.position ="bottom",
          strip.background = element_rect(fill="white", colour="black"),
          strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"))) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE))
  
  ggsave(path = "./documents/figures/", filename='monthly_discharge_sum', width = 7.5, height = 7.5, device="jpeg", dpi=700)
  
  #Yearly mean discharge
  Mean_trend <- final_sites %>%
    group_by(site_name, wt_year) %>%
    summarise(Annual_Mean_Discharge = mean(X_00060_00003)) %>%
    drop_na() %>%
    ungroup(wt_year) %>%
    do(test_mk = mkttest(.$Annual_Mean_Discharge)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[5],
           test = "Annual Mean Discharge")%>%
    select(-test_mk)
  
  percentiles = c(0.1, 0.25, 0.5, 0.75, 0.9)
  
  percentile_trend = lapply(percentiles, function(tile){
    final_sites %>%
      group_by(site_name, wt_year) %>%
      summarise(Annual_tile_Discharge = quantile(X_00060_00003, tile)) %>%
      drop_na() %>%
      ungroup(wt_year) %>%
      do(test_mk = mkttest(.$Annual_tile_Discharge)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[5],
             test = paste0(tile*100, "th Percentile Discharge"))%>%
      select(-test_mk)
  }) %>%
    bind_rows()
  
  
  #Winter Base Flow
  baseflow_trend <- final_sites %>%
    group_by(site_name, wt_year) %>%
    filter(Month < 5) %>%
    summarise(Annual_Mean_Discharge = sum(X_00060_00003)) %>%
    drop_na() %>%
    ungroup(wt_year) %>%
    do(test_mk = mkttest(.$Annual_Mean_Discharge)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[5],
           test = "Winter Baseflow")%>%
    select(-test_mk)
  
  #Peak Flow
  ave_criteria = 5
  
  peak_flow <- final_sites %>%
    group_by(site_name, wt_year) %>%
    mutate(ave_q = rollapply(X_00060_00003,  FUN = mean, width = ave_criteria, fill = NA, align = "center")) %>%
    filter(ave_q>0 & ave_q == max(ave_q, na.rm = TRUE)) %>%
    slice(1) %>%
    mutate(jday = yday(make_date(Year, Month, Day)))
  
  peak_flow_1d <- final_sites %>%
    group_by(site_name, wt_year) %>%
    filter(X_00060_00003 == max(X_00060_00003, na.rm = TRUE)) %>%
    slice(1) %>%
    mutate(jday = yday(make_date(Year, Month, Day)))%>%
    ungroup(wt_year) %>%
    do(test_mk = mkttest(.$jday)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[5],
           test = "Peak Discharge")%>%
    select(-test_mk)
    
  
  # Discharge
  PF_Q_trend <- peak_flow %>%
    group_by(site_name) %>%
    do(test_mk = mkttest(.$ave_q)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[5],
           test = "Peak Discharge")%>%
    select(-test_mk)

  # Date
  PF_Date_trend <- peak_flow %>%
    group_by(site_name) %>%
    do(test_mk = mkttest(.$jday)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[5],
           test = "Day of Peak Discharge")%>%
    select(-test_mk)
  
  # Min Flow
  ave_criteria = 10
  
  min_flow <- final_sites %>%
    filter(Month <= 8) %>%
    group_by(site_name, wt_year) %>%
    mutate(ave_q = rollapply(X_00060_00003,  FUN = mean, width = ave_criteria, fill = NA, align = "center")) %>%
    filter(X_00060_00003 > 5*(min(X_00060_00003, na.rm = TRUE)+1)) %>%
    slice(1) %>%
    mutate(jday = yday(make_date(Year, Month, Day)))
  
  # Discharge
  min_Q_trend <- min_flow %>%
    ungroup() %>%
    group_by(site_name) %>%
    mutate(ave = mean(ave_q)) %>%
    filter(ave>0) %>%
    do(test_mk = mkttest(.$ave_q)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[5],
           test = "Minimum Discharge")%>%
    select(-test_mk)
  
  # Date
  min_Date_trend <- min_flow %>%
    ungroup() %>%
    group_by(site_name) %>%
    do(test_mk = mkttest(.$jday)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[5],
           test = "Day of Minimum Discharge")%>%
    select(-test_mk)

  # Ice affected discharge last day
  ice_flow <- final_sites %>%
    filter(X_00060_00003_cd == "A e") %>%
    mutate(jday = yday(make_date(Year, Month, Day))) %>%
    group_by(site_name, wt_year) %>%
    slice_max(order_by = Date, n = 1) %>%
    ungroup(wt_year) %>%
    do(test_mk = mkttest(.$jday)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[5],
           test = "Last Day of Ice")%>%
    select(-test_mk)

  # Summer Flow
  summer_flow_start <- final_sites %>%
    group_by(site_name, wt_year) %>%
    mutate(p50 = quantile(X_00060_00003, 0.5)) %>%
    mutate(summer_flows = ifelse(X_00060_00003<p50, 1, 0))%>%
    filter(summer_flows == 1) %>%
    slice(1) %>%
    mutate(jday_start = yday(make_date(Year, Month, Day))) %>%
    select(site_name, wt_year, jday_start)
  
  summer_flow_end <- final_sites %>%
    group_by(site_name, wt_year) %>%
    mutate(p50 = quantile(X_00060_00003, 0.5)) %>%
    mutate(summer_flows_end = ifelse(X_00060_00003>p50, 1, 0))%>%
    filter(summer_flows_end == 1) %>%
    filter(Month> 9) %>%
    slice(n()) %>%
    mutate(jday_end = yday(make_date(Year, Month, Day))) %>%
    select(site_name, wt_year, jday_end)
    
  summer_flow = left_join(summer_flow_start, summer_flow_end)%>%
    ungroup() %>%
    group_by(site_name) %>%
    mutate(diff = jday_end - jday_start)%>%
    do(test_mk = mkttest(.$diff)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[5],
           test = "Length of Summer")%>%
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
    do(test_mk = mkttest(.$dq_q)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[5],
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
      do(test_mk = mkttest(.$jday)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[5],
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
    do(test_mk = mkttest(.$Annual_Rslope)) %>%
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[5],
           test = "Recession")%>%
    select(-test_mk)
  
  data_AK_final = rbind(Mean_trend, percentile_trend, PF_Q_trend,
                        PF_Date_trend, flashiness_trend, Centroid_trend, Recession_trend,
                        min_Q_trend, min_Date_trend, summer_flow, baseflow_trend)
  
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
      do(test_mk = mkttest(.$Month_mean_Discharge)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[5],
             test = paste0(month.abb[mon]))%>%
      select(-test_mk)
  }) %>%
    bind_rows()
  
  hist_month = final_sites %>%
      filter(wt_year <= 1989) %>%
      group_by(site_name, Month) %>%
      summarise(Hist_mean_Discharge = mean(X_00060_00003)) %>% 
    mutate(test = month.abb[Month]) %>%
    select(-Month)
  
  month_trend = left_join(month_trend, hist_month) %>%
    group_by(site_name) %>%
    #mutate(P_Value_ave = rollapply(P_Value,  FUN = mean, width = 5, fill = P_Value , align = "center")) %>%
    mutate(percent_change = ifelse(as.numeric(P_Value <= 0.05), paste0(round(10*Sens_Slope/Hist_mean_Discharge*100),"%"), NA))
  
  month_trend$test = factor(month_trend$test, levels = c(month.abb[10:12], month.abb[1:9]))
  
  save(month_trend, file=paste0("./Data/FinalDischargeTemporal_Analyzed_Data.RData"))
  
}






