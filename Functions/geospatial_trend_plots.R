# Geospatial Trend Plots

geospatial_trend_plots = function (df_path, test_name, pval, graph_title, legend_title){

  load(df_path) 
  
  df = data_AK_final%>%
    ungroup %>%
    filter(test == test_name) %>%
    mutate(sig = ifelse(pval >= as.numeric(P_Value), "Significant", "Not Significant"))
  
  # Pull site locations
  data_AK <- whatNWISdata(stateCd="AK", parameterCd="00060") %>%
    distinct(site_no, .keep_all = T)
  
  df_final = left_join(df, data_AK)
  
  #Plot locations
  ak <- map_data('worldHires','USA:Alaska')
  ak <- subset(ak, long<0) #drop the end of the Aleutian Islands 

  ggplot() + 
    geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
    geom_point(data=df_final, aes(x=dec_long_va, y=dec_lat_va, color = Z_Score, shape=sig, size=sig)) +
    labs(title=graph_title, color=" ") +
    theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_rect(fill = "white")) +
    scale_colour_gradient2(name = legend_title) + 
    scale_shape_manual(name ="", values=c(15, 16)) +
    scale_size_manual(guide=FALSE, values=c(2,4)) +
    guides(colour = guide_colorbar(order=1), shape = guide_legend(order=2), size = "none")

  ggsave(path = "./documents/figures/", filename=paste0("Geospatial_Trend_in_", gsub(" ", "_", test_name, fixed = TRUE)), width = 7, height = 5, device="jpeg", dpi=700)
  
}
