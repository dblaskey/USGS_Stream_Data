library(pacman)
pacman::p_load(dataRetrieval, dplyr, tidyverse, leaflet)

q_downloader <- function(site_no = sites){
  df <- readNWISdv(site_no,
                   parameterCd = '00060') %>%
    rename(q_cfs = X_00060_00003,
           q_cd = X_00060_00003_cd) %>%
  return(df)
}

Qakf <- q_downloader(site_no = 'sites')