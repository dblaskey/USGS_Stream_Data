ice_bbplot = function () {

  # Load Data
  BreakUp=read.csv('./Data/raw/aprfc_breakupDB_dump.csv') %>%
    select(siteID, year, breakup) %>%
    mutate(wt_year=as.numeric(year), siteID=as.numeric(siteID), 
           breakup=yday(as.Date(breakup, "%m/%d/%Y"))) %>%
    drop_na() %>%
    select(-year)
  
  FreezeUp=read.csv('./Data/raw/aprfc_freezupDB_dump.csv') %>%
    select(siteID, year, first_ice) %>%
    mutate(wt_year=as.numeric(year+1), siteID=as.numeric(siteID), 
           first_ice=yday(as.Date(first_ice, "%m/%d/%Y"))) %>%
    drop_na() %>%
    select(-year)
  
  siteid=read.csv('./Data/raw/breakupSites.csv') %>%
    mutate(siteID=as.numeric(id)) %>%
    select(-id, -lid)
  
  # Create unified data frame
  df = full_join(BreakUp, FreezeUp)
  
  ids = c(43, 149, 150, 151,152, 153, 162, 163, 185, 186, 187, 188, 189, 190, 191, 192, 193,
          268, 269, 270, 271,272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 284,
          285, 286, 287, 288, 289, 370, 371, 372, 343, 344, 345, 346, 347, 348)
  
  df = left_join(df, siteid) %>%
    filter(siteID %in% ids)
  
  sum_dates = df %>%
    group_by(river) %>%
    summarize(mean_break = mean(breakup, na.rm = TRUE), mean_freeze= mean(first_ice, na.rm = TRUE))
  
  sum_dates$mean_break <- as.Date(sum_dates$mean_break,    # Convert Julian day to date
                     origin = as.Date("2021-01-01"))
  
  sum_dates$mean_freeze <- as.Date(sum_dates$mean_freeze,    # Convert Julian day to date
                                  origin = as.Date("2021-01-01"))
  
  # Create unified data frame
  df = full_join(BreakUp, FreezeUp)
  
  siteid = mutate(siteid, station_nm = toupper(paste(river, atnr, location, "AK", sep = " ")))
  
  siteid$station_nm = gsub("RIVER", "R", siteid$station_nm)
  siteid$station_nm = gsub("CREEK", "C", siteid$station_nm)
  
  df_final = stringdist_join(data_AK, siteid, 
                  by = "station_nm",
                  mode = "left",
                  ignore_case = FALSE, 
                  method = "jw", 
                  max_dist = 99, 
                  distance_col = "dist") %>%
    group_by(station_nm.x) %>%
    slice_min(order_by = dist, n = 1)
  
  df_final = siteid %>% fuzzy_inner_join(data_AK, by = c("station_nm" = "station_nm"), match_fun = str_detect)
  
  ice_sites = read.csv("./ice_sites.csv", header = TRUE)

  site_map = ice_sites %>% select(site_no, siteID)
  
  load("./Data/raw_sites.Rdata")
  
  #Create Year, wt_year, Month, and Day columns and  
  Date2wt_year(sites, "Date")
  
  # Ice affected discharge first day
  ice_flow_first <- sites %>%
    filter(Month > 8, X_00060_00003_cd == "A e") %>%
    mutate(jday_fu = yday(make_date(Year, Month, Day)), site_no = as.numeric(site_no)) %>%
    group_by(site_no, wt_year) %>%
    slice_min(order_by = Date, n = 1) %>%
    select(site_no, wt_year, jday_fu)
  
  # Ice affected discharge last day
  ice_flow_last <- sites %>%
    filter(Month <= 8, X_00060_00003_cd == "A e") %>%
    mutate(jday_bu = yday(make_date(Year, Month, Day)), site_no = as.numeric(site_no)) %>%
    group_by(site_no, wt_year) %>%
    slice_max(order_by = Date, n = 1) %>%
    select(site_no, wt_year, jday_bu)
  
  df = left_join(site_map, df)
  
  df_final = left_join(df, ice_flow_last)
  df_final = left_join(df_final, ice_flow_first)
  
  df_QC = df_final %>% 
    filter(breakup < 180, jday_bu < 180)
  
  test = df_QC %>%
    mutate(diff_ice_in = first_ice - jday_fu) %>%
    drop_na(diff_ice_in) %>%
    summarise(mean_diff_fu = mean(diff_ice_in))
  
  test = df_QC %>%
    mutate(diff_ice_out = breakup - jday_bu) %>%
    drop_na(diff_ice_out) %>%
    summarise(mean_diff_bu = mean(diff_ice_out))
  
  # Reduced to desired date range
  df = df %>%
    filter(year>1970)
  
  # Visualize data
  df_breakup = left_join(df, siteid) %>%
    drop_na(breakup) %>%
    group_by(siteID) %>%
    #filter(n() >= 10) %>%
    ungroup()
  
  df_freeze = left_join(df, siteid) %>%
    drop_na(first_ice) %>%
    group_by(siteID) %>%
    #filter(n() >= 10) %>%
    ungroup()
  
  # Plot Regional Dates
  boxplot(df_breakup$breakup ~ df_breakup$river, outline=FALSE,
          main="Date of River Breakup", ylab="Breakup (Julian Day)", xlab="River")
  boxplot(df_freeze$first_ice ~ df_freeze$river, outline=FALSE,
          main="Date of First Ice", ylab="First Ice (Julian Day)", xlab="River")
  
  # Calculate Trends
  
  # Breakup
  temp1 <- df %>%
    drop_na(breakup) %>%
    group_by(siteID) %>%
    filter(n() >= 3) %>%
    do(test_mk=pwmk(.$breakup)) %>% 
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4], test = "breakup")%>%
    select(-test_mk)
  
  # First Ice
  temp2 <- df %>%
    drop_na(first_ice) %>%
    group_by(siteID) %>%
    filter(n() >= 3) %>%
    do(test_mk=pwmk(.$first_ice)) %>% 
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4], test = "first_ice")%>%
    select(-test_mk)
  
  # Combine to final data frame
  temp = rbind(temp1, temp2)
  Final_Trends = left_join(temp, siteid) %>%
    mutate(trend_sites = ifelse(P_Value < 0.05, Sens_Slope, 0)) %>%
    group_by(river, test) %>%
    summarise(trend = ave(trend_sites))%>%
    unique()
}




