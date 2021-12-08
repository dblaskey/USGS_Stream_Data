#Function to remove incomplete USGS Stream Gage data
#df is the dataframe containing the record
#day_crit is the number of days in a year needed for a complete record year
#precent_complete is the percent of the total record needed to be considered complete
#year_length is the length of the record needed
#end_year is when all the records should end

Remove_Gages_Insufficient_Data <- function(df, day_crit, percent_complete, 
                                           year_length, end_year) {
  
  end_period <- as.Date(paste(end_year,"10-01", sep="-"))
  start_period<- as.Date(paste(end_year - year_length, "10-01", sep="-"))

  #Constrain to the desired range of years
  temp <- df %>%
    filter(Date>=start_period & Date<end_period)%>%
    group_by(site_no) %>%
    mutate(start_date = min(Date), end_date=max(Date)) %>%
    filter(lubridate::year(end_date)==end_year)
  
  #Create table of observations per year
  obsd <- temp %>%
    group_by(site_no) %>%
    count(wt_year, name = "Yearly_obs") %>%
    mutate(Ypass = ifelse(Yearly_obs>day_crit,1,0))
  
  fail <- obsd %>%
    filter(Ypass==0)
  
  #Remove lines that fail day criteria
  temp <- anti_join(temp,fail) 
  
  #Test for yearly criteria
  obsy <- temp %>%
    group_by(site_no) %>%
    summarise(Obs = n(), Record_length = ceiling((end_date-start_date)/365.25)) %>%
    unique()
    
  fail <- obsy %>%
    filter(Record_length<year_length | Obs<(365.25*year_length*percent_complete))
  
  #Remove years that don't pass
  final_sites <- anti_join(temp,fail)
  
  data_AK <- whatNWISdata(stateCd="AK", parameterCd="00060") %>%
      distinct(site_no, .keep_all = T)

  # Reduce to needed sites
  data_AK_final <- semi_join (data_AK, final_sites, by="site_no") %>%
    select(site_no, station_nm, dec_lat_va, dec_long_va)
  
  save(final_sites, file = paste0("./Data/",year_length,"yearsfinalsites.RData"))
  save(data_AK_final, file=paste0("./Data/",year_length,"yearssiteslocation.RData"))
}

