load("./Data/60yearssiteslocation.RData")
#load("./Data/60yearsfinalsites.RData")

#final_sites = final_sites %>%
 # group_by(site_no) %>%
  #summarise(ave_Q = mean(X_00060_00003))

#data_AK_final = left_join(data_AK_final, final_sites)

#Plot locations
ak <- map_data('worldHires','USA:Alaska')
ak <- subset(ak, long<0) #drop the end of the Aleutian Islands 

ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", col = "black") +
  geom_point(data = data_AK_final, aes(x=dec_long_va, y=dec_lat_va), size=3) +
  ggtitle("USGS Gages with 60 Years of Continuous Data") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  geom_label_repel(data= data_AK_final, aes(label=site_no, x=dec_long_va, y=dec_lat_va), min.segment.length = unit(0, 'lines'),
                   nudge_y = 3)
  #scale_color_viridis(name="Average\nDischarge (cfs)")

ggsave(path = "./documents/figures/", filename="USGS_Station_Data_60_Years", width = 6, height = 5, device="jpeg", dpi=700)
