ID_River_Plots = function(path_df, start_year, end_year, plot_title){

  load(path_df)
  
  river <- final_sites %>%
    mutate(Year=lubridate::year(Date), Month=lubridate::month(Date), Day=lubridate::day(Date)) %>%
    group_by(site_name, Year, Month) %>%
    summarise(month_dis = sum(X_00060_00003)*3600*0.0283168) %>%
    filter(Year>=start_year & Year<end_year) %>%
    mutate(decade = floor(Year/10)*10) %>% 
    group_by(site_name, decade, Month) %>% 
    summarise(ave_month_dis = mean(month_dis))
  
  ggplot() + 
    facet_wrap(vars(site_name), scales = "free", nrow = 5) +
    geom_line(data = river, aes(x = as.numeric(Month), y = ave_month_dis, color = factor(decade))) + 
    scale_color_npg(name = "Decade") +
    scale_x_continuous(name = "Month", breaks = 1:12, labels = substr(month.abb, 1,1)) +
    scale_y_continuous(name = "Average Monthly Volumetric Discharge (cubic meters)", labels = scales::scientific) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          strip.text = element_text(face="bold", size=9),
          strip.background = element_rect(fill="white", colour="black"),
          strip.text.x = element_text(colour = "black", margin = margin(.1, 0, .1, 0, "cm"))) +
    ggtitle(plot_title) 
  
  ggsave(path = "./documents/figures/", filename="decadal_discharge", width = 7, height = 8, device="jpeg", dpi=700)
}
