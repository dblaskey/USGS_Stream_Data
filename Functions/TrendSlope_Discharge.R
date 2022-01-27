# Create function for percentile analysis
TrendSlope_Discharge=function(start_year, end_year, mon){
  if (end_year - start_year >= 29){
    final_sites %>%
      filter(wt_year>=start_year & wt_year<= end_year & Month == mon)%>%
      group_by(site_name, wt_year) %>%
      summarise(Discharge = mean(X_00060_00003)) %>%
      ungroup(wt_year) %>%
      do(test_mk = pwmk(.$Discharge)) %>%
      mutate(sig = ifelse(0.05 >= as.numeric(test_mk[4]) & as.numeric(test_mk[2])>0, 
                                   "Significant Increase", 
                                          ifelse(0.05 >= as.numeric(test_mk[4]) & as.numeric(test_mk[2])<0, 
                                                 "Significant Decrease", "Not Significant"))) %>%
      mutate(Start_Year = start_year, End_Year = end_year, test = month.abb[mon])%>%
      select(-test_mk)
  }
}

