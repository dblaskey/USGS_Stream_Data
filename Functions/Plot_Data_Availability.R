Plot_Data_Availability = function (df, end_year, percent_complete){
  
  #Histogram of Data Availability
  Year_data_available <- df %>%
    group_by(site_no) %>%
    count(wt_year) 
  
  Years_per_site <- Year_data_available %>%
    select(-n) %>%
    count(site_no)
  
  jpeg("./documents/figures/hist_of_UGSG_Sites", units="in", width=7, height=4, res=700)
  
  hist(Year_data_available$wt_year, breaks = 109, main="Available Stream Gauge Data per Year",
       xlab = "Year", xlim=c(1900,2020), ylab = "Number of Gauges")
  
  dev.off()
  
  #Pull Geo Locations
  data_AK <- whatNWISdata(stateCd="AK", parameterCd="00060") %>%
    distinct(site_no, .keep_all = T) %>%
    mutate(Record_length = as.numeric((end_date - begin_date)/365.25)) %>%
    filter(year(end_date)>=end_year) %>%
    mutate(bin = cut(Record_length, breaks = c(-Inf, 30, 40, 50, 60, Inf), labels = c("<30", "30-40", "40-50", "50-60", ">60"))) %>%
    mutate(pass = ifelse(bin == "<30" | Record_length*percent_complete*365 > count_nu,"no","yes"))
  
  #Plot locations
  ak <- map_data('worldHires','USA:Alaska')
  ak <- subset(ak, long<0) #drop the end of the Aleutian Islands 
  data_AK <<- subset(data_AK, dec_long_va<0) 
  
  ggplot() + 
    geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
    geom_point(data = data_AK, aes(x=dec_long_va, y=dec_lat_va, color = bin, size = pass)) +
    ggtitle("Record Length of Stations") +
    theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_rect(fill = "white")) +
    scale_color_npg(name = "Years") +
    scale_size_manual(name = "Pass\nCriteria?", values=c(0.5, 3))
  
  ggsave(path = "./documents/figures/", filename="USGS Station Data", width = 6, height = 5, device="jpeg", dpi=700)
}