z_score_trend_plot = function (df, mon){
  
  colorpal <- c("#1b7837", "#762a83", "black")
  
  trend_levels <- c("Significant Increase", "Significant Decrease", "Not Significant")
  
  df$site_name = factor(df$site_name, levels = 
                             c("Nuyakuk River", "Kenai River",
                               "Ship Creek", "Little Susitna River", "Kuskokwim River",
                               "Susitna River", "Salcha River", "Chena River", "Kuparuk River"))
  
  ggplot(df, aes(Start_Year, End_Year)) +
    facet_wrap(vars(site_name), scales = "free", nrow = 3) +
    geom_raster(aes(fill = sig)) +
    xlab("Start Year") +
    ylab("End Year") +
    ggtitle(paste0("Trend Variability of ", month.name[mon], " Mean Discharge")) +
    coord_cartesian(xlim = c(1955, 1990), ylim = c(1984, 2020), expand = FALSE) +
    theme_dark() +
    theme(plot.title = element_text(hjust = 0.5),
          strip.text = element_text(face="bold", size=9),
          strip.background = element_rect(fill="white", colour="black"),
          strip.text.x = element_text(colour = "black", margin = margin(.1, 0, .1, 0, "cm"))) +
    scale_fill_manual(name="", values = colorpal, breaks = trend_levels)
  
  ggsave(path = "./documents/figures/", filename=paste("z_score_trend", mon, sep="_"), width = 9, height = 6, device="jpeg", dpi=700)
}

