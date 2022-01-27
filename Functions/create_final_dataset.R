create_final_dataset = function(df, day_crit, end_year) {
  
  end_period <- as.Date(paste(end_year,"10-01", sep="-"))
  
  #Constrain to the desired range of years
  temp <- df %>%
    filter(Date<end_period)%>%
    group_by(site_name) %>%
    mutate(start_date = min(Date), end_date=max(Date)) %>%
    filter(lubridate::year(end_date)==end_year)
  
  #Create table of observations per year
  obsd <- temp %>%
    group_by(site_name) %>%
    count(wt_year, name = "Yearly_obs") %>%
    mutate(Ypass = ifelse(Yearly_obs>day_crit,1,0)) %>%
    filter(Ypass==0)
  
  #Remove lines that fail day criteria
  final_sites <- anti_join(temp,obsd) 
  
  data_AK <- whatNWISdata(stateCd="AK", parameterCd="00060") %>%
    distinct(site_no, .keep_all = T) %>%
    mutate(site_no = as.numeric(site_no))
  
  # Reduce to needed sites
  data_AK_final <- semi_join (data_AK, final_sites, by="site_no") %>%
    select(site_no, station_nm, dec_lat_va, dec_long_va)
  
  data_AK_final <- left_join(data_AK_final, site_list)
  
  save(final_sites, file = paste0("./Data/finalsites.RData"))
  save(data_AK_final, file=paste0("./Data/finalsiteslocation.RData"))
}