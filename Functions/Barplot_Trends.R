Barplot_Trends = function (df_path, type){
  
  df <- loadRData(df_path)

  temp_df <- df %>%
    ungroup() %>%
    mutate(sig = ifelse(0.01 >= as.numeric(P_Value) & as.numeric(Sens_Slope)>0, 
                        "Increasing at 0.01 Significance", 
                        ifelse(0.05 >= as.numeric(P_Value) & as.numeric(Sens_Slope)>0, 
                               "Increasing at 0.05 Significance", 
                               ifelse(0.01 >= as.numeric(P_Value) & as.numeric(Sens_Slope)<0, 
                                      "Decreasing at 0.01 Significance", 
                                      ifelse(0.05 >= as.numeric(P_Value) & as.numeric(Sens_Slope)<0, 
                                             "Decreasing at 0.05 Significance", "Not Significant")))))

  #colorpal <- c("#b2182b", "#ef8a62", "#fddbc7", "#2166ac", "#67a9cf", "#d1e5f0", "white")
  colorpal <- c("#762a83", "#af8dc3", "#1b7837", "#7fbf7b", "white")
  ggplot(data=temp_df, aes(x=site_name, y=test)) +
    geom_tile(aes(fill = sig), color = "black") +
    ggtitle(paste0("Long-term ", type ," Trends By Basin")) +
    theme_bw() +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          strip.text = element_text(face="bold", size=9),
          axis.text.x = element_text(angle = 45, size = 10, vjust = 1, hjust=1),
          strip.background = element_rect(fill="white", colour="black"),
          strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"))) +
    scale_fill_manual(name="", values = colorpal) +
    geom_text(aes(label=percent_change))
  
#  temp_df <- df %>%
#    ungroup() %>%
#    mutate(sig = ifelse(Sens_Slope == 0 | P_Value > 0.1, "Not Significant", ifelse(0.01 >= as.numeric(P_Value), 
#                        "Significance at 0.01", 
#                        ifelse(0.05 >= as.numeric(P_Value), 
#                               "Significance at 0.05",
#                                      "Significance at 0.1"))))
#  
#  colorpal = c("white", "grey40", "grey60", "grey80")
  
#  ggplot(data=temp_df, aes(x=site_name, y=test)) +
#    geom_tile(aes(fill = sig), color = "black") +
#    ggtitle(paste0("Long-term ", type ," Trends By Basin")) +
#    theme_bw() +
#    scale_x_discrete(expand = c(0,0)) +
#    scale_y_discrete(expand = c(0,0)) +
#    theme(axis.title.x = element_blank(),
#          axis.title.y = element_blank(),
#          plot.title = element_text(hjust = 0.5),
 #         strip.text = element_text(face="bold", size=9),
#          axis.text.x = element_text(angle = 45, size = 10, vjust = 1, hjust=1),
#          strip.background = element_rect(fill="white", colour="black"),
#          strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"))) +
#    scale_fill_manual(name="", values = colorpal) +
#    geom_point(aes(shape=ifelse(Sens_Slope>0 & P_Value<0.1, "up_dot", ifelse(Sens_Slope<0 & P_Value<0.1, "down_dot", "no_dot"))), col="white", fill="black", size = 5) +
#    scale_shape_manual(values=c(up_dot = 24, down_dot=25, no_dot=NA), guide="none")
  
  
  ggsave(path = "./documents/figures/", filename=paste0("Long-term_Trends_", type), width = 7, height = 5, device="jpeg", dpi=700)
}
