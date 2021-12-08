# Create function for percentile analysis
TrendSlope_Discharge=function(start_year, end_year, tile){
  if (end_year - start_year >= 29){
    final_sites %>%
      filter(wt_year>=start_year & wt_year<= end_year)%>%
      group_by(site_name, wt_year) %>%
      summarise(Annual_Q_Discharge = quantile(X_00060_00003, tile)) %>%
      ungroup(wt_year) %>%
      do(test_mk = pwmk(.$Annual_Q_Discharge)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             Start_Year = start_year, End_Year = end_year, test = paste0(tile*100,"th Percentile"))%>%
      select(-test_mk)
  }
}

