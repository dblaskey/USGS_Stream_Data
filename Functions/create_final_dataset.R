create_final_dataset = function(df, day_crit, end_year) {
  
  #Constrain to the desired range of years
  temp <- df %>%
    filter(wt_year<=end_year & wt_year > end_year - 60) 
  
  #Create table of observations per year
  obsd <- temp %>%
    group_by(site_no) %>%
    count(wt_year, name = "Yearly_obs") %>%
    mutate(Ypass = ifelse(Yearly_obs<day_crit,0,1)) %>%
    filter(Ypass==1)
  
  #Remove lines that fail day criteria
  final_sites <- left_join(temp,obsd) %>%
    group_by(site_no) %>%
    drop_na() %>%
    mutate(start_date = min(Date), end_date=max(Date))
  
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