ice_bbplot = function () {

  # Load Data
  BreakUp=read.csv('./Data/raw/aprfc_breakupDB_dump.csv') %>%
    select(siteID, year, breakup) %>%
    mutate(year=as.numeric(year), siteID=as.numeric(siteID), 
           breakup=yday(as.Date(breakup, "%m/%d/%Y"))) %>%
    drop_na()
  
  FreezeUp=read.csv('./Data/raw/aprfc_freezupDB_dump.csv') %>%
    select(siteID, year, first_ice) %>%
    mutate(year=as.numeric(year), siteID=as.numeric(siteID), 
           first_ice=yday(as.Date(first_ice, "%m/%d/%Y"))) %>%
    drop_na()
  
  siteid=read.csv('./Data/raw/breakupSites.csv') %>%
    mutate(siteID=as.numeric(id)) %>%
    select(-id, -lid)
  
  # Create unified data frame
  df = full_join(BreakUp, FreezeUp)
  
  # Reduced to desired date range
  df = df %>%
    filter(year>1970)
  
  # Visualize data
  df_breakup = left_join(df, siteid) %>%
    drop_na(breakup) %>%
    group_by(siteID) %>%
    filter(n() >= 10) %>%
    ungroup()
  
  df_freeze = left_join(df, siteid) %>%
    drop_na(first_ice) %>%
    group_by(siteID) %>%
    filter(n() >= 10) %>%
    ungroup()
  
  # Plot Regional Dates
  boxplot(df_breakup$breakup ~ df_breakup$region, outline=FALSE,
          main="Date of River Breakup", ylab="Breakup (Julian Day)", xlab="Region")
  boxplot(df_freeze$first_ice ~ df_freeze$region, outline=FALSE,
          main="Date of First Ice", ylab="First Ice (Julian Day)", xlab="Region")
  
  # Calculate Trends
  
  # Breakup
  temp1 <- df %>%
    drop_na(breakup) %>%
    group_by(siteID) %>%
    filter(n() >= 10) %>%
    do(test_mk=pwmk(.$breakup)) %>% 
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4], test = "breakup")%>%
    select(-test_mk)
  
  # First Ice
  temp2 <- df %>%
    drop_na(first_ice) %>%
    group_by(siteID) %>%
    filter(n() >= 10) %>%
    do(test_mk=pwmk(.$first_ice)) %>% 
    mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4], test = "first_ice")%>%
    select(-test_mk)
  
  # Combine to final data frame
  temp = rbind(temp1, temp2)
  Final_Trends = left_join(temp, siteid) %>%
    group_by(river, test) %>%
    summarise(trend = ave(Sens_Slope), sig=ave(P_Value))%>%
    unique()
}




