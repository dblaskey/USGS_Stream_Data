p10_final <- data_AK_final %>%
  select(site_no, MK_Q10yearsig, qyQ10_Slope) %>%
  mutate(percent_station_change = ifelse(MK_Q10yearsig=="Significant" & qyQ10_Slope>0, "Increasing", ifelse(MK_Q10yearsig=="Significant", "Decreasing", "Not Significant"))) %>%
  mutate(type = "10th Percentile")

p50_final <- data_AK_final %>%
  select(site_no, MK_Q50yearsig, qyQ50_Slope) %>%
  mutate(percent_station_change = ifelse(MK_Q50yearsig=="Significant" & qyQ50_Slope>0, "Increasing", ifelse(MK_Q50yearsig=="Significant", "Decreasing", "Not Significant"))) %>%
  mutate(type = "50th Percentile")

p90_final <- data_AK_final %>%
  select(site_no, MK_Q90yearsig, qyQ90_Slope) %>%
  mutate(percent_station_change = ifelse(MK_Q90yearsig=="Significant" & qyQ90_Slope>0, "Increasing", ifelse(MK_Q90yearsig=="Significant", "Decreasing", "Not Significant"))) %>%
  mutate(type = "90th Percentile")

skew_final <- data_AK_final %>%
  select(site_no, MK_Skewyearsig, Skew_Slope) %>%
  mutate(percent_station_change = ifelse(MK_Skewyearsig=="Significant" & Skew_Slope>0, "Increasing", ifelse(MK_Skewyearsig=="Significant", "Decreasing", "Not Significant"))) %>%
  mutate(type = "Skew")

barplot_final_30 <- bind_rows(p10_final, p50_final, p90_final, skew_final) %>%
  drop_na(percent_station_change) %>%
  group_by(type) %>%
  count(percent_station_change) %>%
  mutate(Data_years = "30 Year Record")

barplot_final <- rbind(barplot_final_30, barplot_final_40, barplot_final_50, barplot_final_60) 

ggplot(barplot_final, aes(x=type, y=n, fill = fct_rev(percent_station_change))) + 
  geom_bar(stat = 'identity', position = 'fill') +
  facet_grid(.~Data_years) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = "white")) +
  ylab("Fraction of Stations") + 
  scale_fill_manual(name = "", values = c( "gainsboro",  "green", "red"))

