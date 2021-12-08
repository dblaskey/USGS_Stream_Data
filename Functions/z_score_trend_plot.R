z_score_trend_plot = function (df, test){
  
  ggplot(df, aes(Start_Year, End_Year)) +
    facet_wrap(vars(site_name), scales = "free", nrow = 3) +
    geom_raster(aes(fill = Z_Score)) +
    xlab("Start Year") +
    ylab("End Year") +
    ggtitle(paste0("Trend Variability of ", test, " Percentile Discharge")) +
    coord_cartesian(xlim = c(1955, 1990), ylim = c(1984, 2020), expand = FALSE) +
    theme_dark() +
    theme(plot.title = element_text(hjust = 0.5),
          strip.text = element_text(face="bold", size=9),
          strip.background = element_rect(fill="white", colour="black"),
          strip.text.x = element_text(colour = "black", margin = margin(.1, 0, .1, 0, "cm"))) +
    scale_fill_continuous_divergingx(name = "Z Score", palette = 'RdBu', mid = 0)
  
  ggsave(path = "./documents/figures/", filename=paste("z_score_trend", test, "Percentile", sep="_"), width = 6, height = 6, device="jpeg", dpi=700)
}

