z_score_average_plot = function (df, test){
  
  df = df %>%
    group_by(Start_Year, End_Year) %>%
    summarise(Ave_Z = ave(Z_Score)) %>%
    unique()
  
  ggplot(df, aes(Start_Year, End_Year)) +
    geom_raster(aes(fill = Ave_Z)) +
    xlab("Start Year") +
    ylab("End Year") +
    ggtitle(paste0("Average Trend Variability of ", test, " Percentile Discharge")) +
    coord_cartesian(xlim = c(1955, 1990), ylim = c(1984, 2020), expand = FALSE) +
    theme_dark() +
    theme(plot.title = element_text(hjust = 0.5),
          strip.text = element_text(face="bold", size=9),
          strip.background = element_rect(fill="white", colour="black"),
          strip.text.x = element_text(colour = "black", margin = margin(.1, 0, .1, 0, "cm"))) +
    scale_fill_continuous_divergingx(name = "Average\nZ Score", palette = 'RdBu', mid = 0)
  
    ggsave(path = "./documents/figures/", filename=paste("Average_Trend", test, "Precentile", sep="_"), width = 5, height = 5, device="jpeg", dpi=700)
}
