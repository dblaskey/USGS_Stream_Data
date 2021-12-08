climate_change_graphs <- function(clim_first_month, clim_last_month, 
                                  temp_graph_title, day_graph_title){
 
  climate_temp2 <- read.csv("/Users/dybl4375/USGS_Stream_Data_Pull/climate_data", header = FALSE)
  names <- c("t_tot", "pcp_tot", "Date", "COMID")
  colnames(climate_temp2) <- names
  
  climate_temp1 <- read.csv("/Users/dybl4375/USGS_Stream_Data_Pull/hist_climate_data", header = TRUE)
  colnames(climate_temp1) <- names
  
  climate_temp <- rbind(climate_temp1, climate_temp2) 
  
  COMIDS <- unique(climate_temp$COMID)
  site_nos <- c("15484000", "15276000", "15515500", "15290000", "15302000", "15304000",
                "15514000", "15356000", "15258000", "15292000")
  
  df <- data.frame(COMIDS, site_nos)
  colnames(df) <- c("COMID", "site_no")
  
  climate_temp <- full_join(df, climate_temp) %>%
    mutate(Date=lubridate::ymd_hms(Date)) %>%
    mutate(Year = lubridate::year(Date), Month = lubridate::month(Date), Day = lubridate::day(Date)) %>%
    mutate(wt_year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year)))
  
  if (clim_first_month<clim_last_month){
    if ( 10 > clim_first_month & 10 <= clim_last_month){
      climate <- climate_temp %>%
        group_by(site_no, Year) %>%
        filter(as.numeric(Month) >= clim_first_month & as.numeric(Month) <= clim_last_month) %>%
        summarise(t_annual=mean(t_tot)-273.15, pcp_annual = sum(pcp_tot)) %>%
        ungroup() %>%
        group_by(site_no) %>%
        mutate(t_mean = t_annual-mean(t_annual), pcp = (pcp_annual-mean(pcp_annual)))%>%
        rename(wt_year = Year)
 
    } else {
      daily_climate <- climate_temp %>%
        group_by(site_no, wt_year) %>%
        filter(as.numeric(Month) >= clim_first_month & as.numeric(Month) <= clim_last_month)
      
        climate <- daily_climate %>% 
          group_by(site_no, wt_year) %>%
          summarise(t_annual=mean(t_tot)-273.15, pcp_annual = sum(pcp_tot)) %>%
          ungroup(wt_year) %>%
          mutate(t_mean = t_annual-mean(t_annual), pcp = (pcp_annual-mean(pcp_annual)))
    }
  } else {
    daily_climate <- climate_temp %>%
      group_by(site_no, wt_year) %>%
      filter(as.numeric(Month) >= clim_first_month | as.numeric(Month) <= clim_last_month)
      
    climate <- daily_climate %>% 
      group_by(site_no, wt_year) %>%
      summarise(t_annual=mean(t_tot)-273.15, pcp_annual = sum(pcp_tot)) %>%
      ungroup(wt_year) %>%
      mutate(t_mean = t_annual-mean(t_annual), pcp = (pcp_annual-mean(pcp_annual)))
  }
  
  total_temp_trend = climate %>%
    group_by(wt_year) %>%
    summarise(ave_temp = mean(t_annual)) %>%
    ungroup()
  
  total_temp_trend <- lm(ave_temp ~ wt_year, total_temp_trend)
  x = seq(1955,2016)
  y = coef(total_temp_trend)[[2]]*x+coef(total_temp_trend)[[1]]
  df = as.data.frame(cbind(x,y))
  
  total_thawing_days_trend = daily_climate %>%
    group_by(site_no, wt_year) %>%
    summarise(Days_above_freezing = sum(as.numeric(t_tot)>273.15)) %>%
    ungroup(site_no) %>%
    mutate(Ave_Days_above_freezing=mean(Days_above_freezing))
  
  total_thaw_trend <- lm(Ave_Days_above_freezing ~ wt_year, total_thawing_days_trend)
  y2 = coef(total_thaw_trend)[[2]]*x+coef(total_thaw_trend)[[1]]
  df2 = as.data.frame(cbind(x,y))

    ggplot() +
      geom_line(data = total_thawing_days_trend, aes(x=wt_year, y=Days_above_freezing, group=site_no), alpha=0.35) +
      ggtitle(day_graph_title) +
      theme(plot.title = element_text(hjust = 0.5), 
            panel.border = element_rect(color = "black", fill=NA, size=1), 
            panel.background = element_rect(fill = "white"), 
            panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey"), 
            legend.key=element_blank(),
            legend.position="bottom") +
      xlab('Water Year') +
      scale_x_continuous(limits=c(1955, 2015), breaks=seq(1955,2015,5), labels = seq(1955,2015,5), expand = c(0, 1)) +
      ylab ('Days Above Freezing') +
      geom_line(data = df2, aes(x=x, y=y2, col="red")) +
      scale_colour_manual(name='',
                          labels = "Days Above Freezing Trend", 
                          values="red")
    
    ggsave(path = "./documents/figures/", filename=day_graph_title, width = 7, height = 4, device="jpeg", dpi=700)
    
    ggplot() +
      geom_line(data = climate, aes(x=wt_year, y=t_annual, group=site_no), alpha=0.35) +
      ggtitle(temp_graph_title) +
      theme(plot.title = element_text(hjust = 0.5), 
            panel.border = element_rect(color = "black", fill=NA, size=1), 
            panel.background = element_rect(fill = "white"), 
            panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey"), 
            legend.key=element_blank(),
            legend.position="bottom") +
      xlab('Water Year') +
      scale_x_continuous(limits=c(1955, 2015), breaks=seq(1955,2015,5), labels = seq(1955,2015,5), expand = c(0, 1)) +
      ylab (expression('Average Temperature ('*degree*C*')')) +
      geom_line(data = df, aes(x=x, y=y, col="red")) +
      scale_colour_manual(name='',
                          labels = "Average Air Temperature Trend", 
                          values="red")
    ggsave(path = "./documents/figures/", filename=temp_graph_title, width = 7, height = 4, device="jpeg", dpi=700)
    
}

