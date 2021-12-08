#### Final Ice Data

rm(list = ls())

library("tidyverse", "lubridate")

# Ice data
ice=read.csv("/Users/dybl4375/Desktop/GEOG 5100 Machine Learning/Final Project/NenanaIceClassic_1917-2021.csv", header = TRUE) %>%
  mutate(Leap_Year=ifelse(lubridate::leap_year(as.numeric(Year))==TRUE,1,0))


# Climate Index Data
filenames <- list.files('/Users/dybl4375/USGS_Stream_Data_Pull/Data/raw/', pattern = '.long.data$', full.names = TRUE)

# Creates an average index over the time period specified above for each water year
climate_data=lapply(filenames, function(x) {
  data <- read.table(x,
                     header = FALSE)
  names(data)=c("Year", 1:12)
  data = data %>%
    gather(Month, index, -Year) %>%
    mutate(wt_year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year))) %>%
    group_by(wt_year, Month) %>%
    summarise(total_index = sum(index))
  return(data)
}) %>% reduce(full_join, by = c("wt_year", "Month"))

names(climate_data)=c("Year", "Month", "AO", "NP", "PDO", "SOI")

temp = climate_data %>%
  filter(Month>=11 | Month <=2) %>%
  group_by(Year) %>%
  summarise(AO_Winter = sum(AO), NP_Winter = sum(NP), PDO_Winter = sum(PDO), SOI_Winter = sum(SOI))

df = left_join(ice, temp)

temp = climate_data %>%
  filter(Month==3) %>%
  select(-Month) %>%
  rename(AO_March = AO, PDO_March = PDO, NP_March = NP, SOI_March = SOI)

df = left_join(df, temp)

# Streamflow Data
load("/Users/dybl4375/USGS_Stream_Data_Pull/sites.Rdata")

temp=sites %>%
  filter(site_no == 15514000 | site_no == 15484000 | site_no == 15515500) %>%
  mutate(Date = as.POSIXct(Date)) %>%
  mutate(Year = lubridate::year(Date), Month = lubridate::month(Date), Day = lubridate::day(Date)) %>%
  mutate(Year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year))) %>%
  filter(Month == 1 | Month == 3 | Month == 11) %>%
  group_by(site_no, Year, Month) %>%
  summarise(Ave_Month_Q = mean(X_00060_00003), Max_Month_Q = max(X_00060_00003), 
            Min_Month_Q = min(X_00060_00003))%>%
  pivot_wider(names_from = c(site_no, Month), values_from = c(Ave_Month_Q, Max_Month_Q, Min_Month_Q), names_sep="") 

df = full_join(df, temp)

# ERA5 Data
climate_temp <- read.csv("/Users/dybl4375/USGS_Stream_Data_Pull/Data/raw/climate_data_final", header = TRUE)
site_list <- read.csv("/Users/dybl4375/USGS_Stream_Data_Pull/Data/site_list.csv", header = TRUE)

climate_temp2 <- left_join(site_list, climate_temp) %>%
  mutate(Date=lubridate::ymd_hms(Date)) %>%
  mutate(Year = lubridate::year(Date), Month = lubridate::month(Date), Day = lubridate::day(Date)) %>%
  mutate(Year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year))) %>%
  mutate(Precip_tot = Precip*3600*24) %>% #convert from avearge precipitation rate to total precip
  group_by(site_no) %>% 
  mutate(Snow_acc = Snow_Depth - lag(Snow_Depth)) %>%
  mutate(Melt = ifelse(Snow_acc <= 0, Snow_acc, 0)) %>%
  mutate(Precip = Precip*3600*24) %>%
  filter(site_no == 15514000 | site_no == 15484000 | site_no == 15515500) %>%
  filter(Month == 1 | Month == 3 | Month == 11) %>%
  group_by(Year, Month) %>%
  summarise(Ave_Month_temp = mean(Temperature), Max_Month_temp = max(Temperature), 
            Min_Month_temp = min(Temperature), Days_above_freezing = sum(as.numeric(Temperature)>273.15), sum_daily_temp = sum(Temperature),
            precip = sum(Precip), Days_of_precip = sum(Precip>1), Max_snow_depth = max(Snow_Depth), Snow_acc = sum(Snow_acc), soil_temp_1 = mean(Soil_temp_1),
            soil_temp_2 = mean(Soil_temp_2), soil_moisture_1 = mean(Soil_moisture_1), soil_moisture_2 = mean(Soil_moisture_2),
            melt = sum(Melt)) %>%
  pivot_wider(names_from = Month, 
              values_from = c(Ave_Month_temp, Max_Month_temp, Min_Month_temp, 
                              Days_above_freezing, sum_daily_temp, precip, Days_of_precip, 
                              soil_moisture_1, soil_moisture_2, soil_temp_1, soil_temp_2,
                              Snow_acc, Max_snow_depth, melt), names_sep="") 

df = left_join(df, climate_temp2)

# Winter Freeze Date
freeze_days = left_join(site_list, climate_temp) %>%
  filter(site_no == 15515500) %>%
  mutate(Date=lubridate::ymd_hms(Date)) %>%
  mutate(Year = lubridate::year(Date), Month = lubridate::month(Date), Day = lubridate::day(Date)) %>%
  mutate(Year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year))) %>%
  ungroup() %>%
  filter(as.numeric(Month) >= 7 & as.numeric(Month) <= 12) %>%
  mutate(below_freezing = ifelse(Temperature<273.15,1,0)) %>%
  group_by(ID = data.table::rleid(below_freezing == 1)) %>%
  mutate(Consec_Days = if_else(below_freezing == 1, row_number(), 0L)) %>%
  filter(Consec_Days == 7) %>%
  mutate(jday = lubridate::yday(lubridate::make_date(Year, Month, Day))) %>%
  group_by(Year) %>%
  summarise(jday_freezeup = ave(jday)) %>%
  unique()

df = left_join(df, freeze_days)

# Wind Data 
# Downloaded from https://mesonet.agron.iastate.edu/request/download.phtml?network=AK_ASOS
airport = read.csv("/Users/dybl4375/Desktop/GEOG 5100 Machine Learning/Final Project/PANN.csv", header = TRUE)[,2:3]
names(airport) = c("Date", "Wind_speed")

airport_df = airport %>%
  drop_na() %>%
  mutate(Date=lubridate::ymd_hm(Date)) %>%
  mutate(Year = lubridate::year(Date), Month = lubridate::month(Date), Day = lubridate::day(Date)) %>%
  mutate(wt_year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year))) %>%
  filter(Month == 4 & Day<=15) %>%
  group_by(Year) %>%
  summarise(sum_wind_speed = sum(Wind_speed))

df = full_join(df, airport_df) 

write.csv(df,"/Users/dybl4375/Desktop/GEOG 5100 Machine Learning/Final Project/processed_data", row.names = FALSE)

# Ice Data for LSTM
# Streamflow Data
load("/Users/dybl4375/USGS_Stream_Data_Pull/sites.Rdata")

temp=sites %>%
  filter(site_no == 15514000 | site_no == 15484000 | site_no == 15515500) %>%
  mutate(Date = floor_date(Date, "day")) %>%
  select(Date, X_00060_00003, site_no) %>%
  pivot_wider(names_from = site_no, values_from = X_00060_00003) 

ice = mutate(ice, Month = match(Month, month.name))
ice$Date <- paste(ice$Year, ice$Month, ice$Day, sep="-") %>% ymd() %>% as.Date()
days = data.frame(seq(as.Date("1917-01-01"), as.Date("2020-12-31"), by="days"))
names(days) = "Date"

ice_bu = left_join(days, ice) %>%
  mutate(Break_up = ifelse(is.na(Day), 0, 1)) %>%
  select(Date, Break_up)

df = left_join(ice_bu, temp)

# ERA5 Data
climate_temp2 <- left_join(site_list, climate_temp) %>%
  mutate(Date=lubridate::ymd_hms(Date), Precip_tot = Precip*3600*24) %>%
  group_by(site_no) %>% 
  mutate(Snow_acc = Snow_Depth - lag(Snow_Depth)) %>%
  mutate(Melt = ifelse(Snow_acc <= 0, Snow_acc, 0)) %>%
  filter(site_no == 15514000 | site_no == 15484000) %>%
  select(-c(COMID, site_name, Precip, Precip_Frac)) %>%
  pivot_wider(names_from = site_no, 
              values_from = c(Temperature, Precip_tot, Snow_Depth, Snow_acc, 
                              Soil_moisture_1, Soil_moisture_2, Soil_moisture_3, Soil_moisture_4,
                              Soil_temp_1, Soil_temp_2, Soil_temp_3, Soil_temp_4,
                              Melt), names_sep="") 

df = left_join(df, climate_temp2)

write.csv(df,"/Users/dybl4375/Desktop/GEOG 5100 Machine Learning/Final Project/processed_data_LSTM", row.names = FALSE)

