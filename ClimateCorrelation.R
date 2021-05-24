rm(list = ls())

library(pacman)
p_load("tidyverse", "mapdata")

# Load Data
year_length <-30 # Set length of record

load("/Users/dybl4375/USGS_Stream_Data_Pull/30yearsfinal.Rdata") # Read site data
load("/Users/dybl4375/USGS_Stream_Data_Pull/30yearsites.Rdata") # Read location data

Spring_flow = final %>%
  filter(as.numeric(Month) >=4 & as.numeric(Month) <= 6 & wt_year>2019-year_length) %>%
  group_by(site_no, wt_year) %>%
  summarise(springQ = sum(X_00060_00003))

filenames <- list.files('/Users/dybl4375/USGS_Stream_Data_Pull/', pattern = '.long.data$', full.names = TRUE)

# Obtain winter numbers
climate_data=lapply(filenames, function(x) {
  data <- read.table(x,
                     header = FALSE)
  names(data)=c("Year", 1:12)
  data = data %>%
    gather(Month, index, -Year) %>%
    mutate(wt_year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year))) %>%
    filter(wt_year>=(2019-year_length) & wt_year<2020) %>%
    filter(Month <= 3 & Month >= 11) %>%
    group_by(wt_year) %>%
    summarise(winter_index = mean(index))
  return(data)
})

climate_df=as.data.frame(cbind(climate_data[[1]][[1]], climate_data[[1]][[2]], 
                               climate_data[[2]][[2]], climate_data[[3]][[2]], climate_data[[4]][[2]]))
names(climate_df)=c("wt_year", "AO", "NP", "PDO", "SOI")

Previous_spring = final %>%
  filter(as.numeric(Month) >=4 & as.numeric(Month) <= 6) %>%
  group_by(site_no, wt_year) %>%
  summarise(prespringQ = sum(X_00060_00003)) %>%
  mutate(wt_year=wt_year+1)

# Create data frame
corr_df=left_join(Spring_flow,climate_df)
corr_df=left_join(corr_df, Previous_spring)

# Create long format
corr_df_long = corr_df %>%
  gather(clim_var, index, -c(springQ,site_no,wt_year)) %>%
  drop_na()

# Test correlation
corr_results = corr_df_long %>%
  group_by(site_no, clim_var) %>%
  do(corr=cor(.$index, .$springQ, method = "spearman"))

# Winter Streamflow
Winter_flow = final %>%
  filter(as.numeric(Month) <= 3 | as.numeric(Month) >= 11 & wt_year>2019-year_length) %>%
  group_by(site_no, wt_year) %>%
  summarise(winterQ = sum(X_00060_00003))

# Create data frame
corr_winter_df=left_join(Winter_flow,climate_df)

# Create long format
corr_winter_df_long = corr_winter_df %>%
  gather(clim_var, index, -c(winterQ,site_no,wt_year)) 

# Test correlation
corr_winter_results = corr_winter_df_long %>%
  group_by(site_no, clim_var) %>%
  do(corr=cor(.$index, .$winterQ, method = "spearman"))

# PDO Plot
df = filter(corr_winter_results, clim_var=="PDO")
df = left_join(df, data_AK_final)
ak <- map_data('worldHires','USA:Alaska')
ak <- subset(ak, long<0) #drop the end of the Aleutian Islands 

ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=df, aes(x=dec_long_va, y=dec_lat_va, color = as.numeric(corr)), size=2) +
  labs(title="Correlation of Winter Discharge and PDO", color=" ") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_colour_gradient2(name = "Correlation") 

# SOI Plot
df = filter(corr_winter_results, clim_var=="SOI")
df = left_join(df, data_AK_final)

ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=df, aes(x=dec_long_va, y=dec_lat_va, color = as.numeric(corr)), size=2) +
  labs(title="Correlation of Winter Discharge and SOI", color=" ") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_colour_gradient2(name = "Correlation") 

# AO Plot
df = filter(corr_winter_results, clim_var=="AO")
df = left_join(df, data_AK_final)

ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=df, aes(x=dec_long_va, y=dec_lat_va, color = as.numeric(corr)), size=2) +
  labs(title="Correlation of Winter Discharge and AO", color=" ") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_colour_gradient2(name = "Correlation") 

# NP Plot
df = filter(corr_winter_results, clim_var=="NP")
df = left_join(df, data_AK_final)

ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=df, aes(x=dec_long_va, y=dec_lat_va, color = as.numeric(corr)), size=2) +
  labs(title="Correlation of Winter Discharge and NP", color=" ") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_colour_gradient2(name = "Correlation") 

