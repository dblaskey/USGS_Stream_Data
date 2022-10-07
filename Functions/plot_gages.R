plot_gages = function(df_path) {
  load(df_path)

basins = NULL
for (i in site_list$COMID) {
   sf_obj = st_read(paste0("./Data/shapefile/subbasin_", i, ".gpkg"))%>%
     mutate(COMID = i)
   basins = rbind(basins, sf_obj)
  }

basins = left_join(basins, site_list)

  #Plot locations
  ak <- map_data('worldHires','USA:Alaska')
  ak <- subset(ak, long<0) #drop the end of the Aleutian Islands 
  
  main=ggplot() + 
    geom_polygon(data=ak, aes(long, lat, group=group), fill="white", col = "black") +
    geom_sf(data = basins, aes(fill=site_name), lwd = 0) +
    geom_point(data = data_AK_final, aes(x=dec_long_va, y=dec_lat_va), size=1) +
    ggtitle("Study Basins") +
    theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_rect(fill = "white")) +
    scale_fill_brewer(name = "", palette = "Set1")
    #geom_label_repel(data= data_AK_final, aes(label=site_name, x=dec_long_va, y=dec_lat_va), min.segment.length = unit(0, 'lines'),
                     #nudge_y = 3)

  inset=ggplot() + 
    geom_polygon(data=ak, aes(long, lat, group=group), fill="white", col = "black") +
    geom_sf(data = basins, aes(fill=site_name), lwd = 0) +
    coord_sf(xlim = c(-151, -148), ylim = c(59.6, 62), expand = FALSE) +
    geom_point(data = data_AK_final, aes(x=dec_long_va, y=dec_lat_va), size=1) +
    theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_rect(fill = "white")) +
    scale_fill_brewer(palette = "Set1", guide = "none")
  
  #geom_label_repel(data= data_AK_final, aes(label=site_name, x=dec_long_va, y=dec_lat_va), min.segment.length = unit(0, 'lines'),
  #nudge_y = 3)
  
  ggsave(path = "./documents/figures/", filename="USGS_Station_Data_Final", width = 6, height = 5, device="jpeg", dpi=700)
}


