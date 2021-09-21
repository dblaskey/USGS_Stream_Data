# This code uses stream flow data from USGS gauges in Alaska and correlates it
# with climate indicies obtained from https://psl.noaa.gov/data/climateindices/
# Written by Dylan Blaskey, May 8, 2021

# Load libraries ----
library(pacman)
p_load("tidyverse", "mapdata")

# Load Data ----
load("/Users/dybl4375/USGS_Stream_Data_Pull/30yearsfinal.Rdata") # Read site data
load("/Users/dybl4375/USGS_Stream_Data_Pull/30yearsites.Rdata") # Read location data
filenames <- list.files('/Users/dybl4375/USGS_Stream_Data_Pull/', pattern = '.long.data$', full.names = TRUE)

# Set variables ----
year_length <-30 # Length of stream flow record
min_month <- 4 # Start annual of subset (1 if for the full year)
max_month <- 6 # End of annual subset (12 if for the full year)
end_year <- 2019 # Last year of complete data

# Subset stream flow file for your selected time frame 
flow = final %>%
  filter(as.numeric(Month) >=min_month & as.numeric(Month) <= max_month & wt_year>end_year-year_length) %>%
  group_by(site_no, wt_year) %>%
  summarise(Q = sum(X_00060_00003)) # This creates total flow for set time frame

# Set time frame of climate data.
clim_min_month <- 4 # Start annual of subset (1 if for the full year)
clim_max_month <- 6 # End of annual subset (12 if for the full year)

# Creates an average index over the time period specified above for each water year
climate_data=lapply(filenames, function(x) {
  data <- read.table(x,
                     header = FALSE)
  names(data)=c("Year", 1:12)
  data = data %>%
    gather(Month, index, -Year) %>%
    mutate(wt_year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year))) %>%
    filter(wt_year>=(2019-year_length) & wt_year<2020) %>%
    filter(Month <= clim_max_month & Month >= clim_max_month) %>%
    group_by(wt_year) %>%
    summarise(subset_index = mean(index))
  return(data)
})

# Combine climate data files
climate_df=as.data.frame(cbind(climate_data[[1]][[1]], climate_data[[1]][[2]], 
                               climate_data[[2]][[2]], climate_data[[3]][[2]], climate_data[[4]][[2]]))
names(climate_df)=c("wt_year", "AO", "NP", "PDO", "SOI")

# Create a variable called the previous spring
Previous_spring = final %>%
  filter(as.numeric(Month) >=4 & as.numeric(Month) <= 6) %>%
  group_by(site_no, wt_year) %>%
  summarise(prespringQ = sum(X_00060_00003)) %>%
  mutate(wt_year=wt_year+1)

# Create data frame with all cimate variables and stream flow
corr_df=left_join(flow,climate_df)
corr_df=left_join(corr_df, Previous_spring)

# Create long format
corr_df_long = corr_df %>%
  gather(clim_var, index, -c(Q,site_no,wt_year)) %>%
  drop_na()

# Test correlation using spearman
corr_results = corr_df_long %>%
  group_by(site_no, clim_var) %>%
  do(corr=cor(.$index, .$Q, method = "spearman"))

# Plot Correlation with climate indices at each station in Alaska
corr_plot <- function(climate_indices, annual_subset){
  df = filter(corr_results, clim_var==climate_indices)
  df = left_join(df, data_AK_final)
  ak <- map_data('worldHires','USA:Alaska')
  ak <- subset(ak, long<0) #drop the end of the Aleutian Islands 
  
  ggplot() + 
    geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
    geom_point(data=df, aes(x=dec_long_va, y=dec_lat_va, color = as.numeric(corr)), size=2) +
    labs(title=paste("Correlation of ", annual_subset, "Discharge and ", climate_indices), color=" ") +
    theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_rect(fill = "white")) +
    scale_colour_gradient2(name = "Correlation") 
}
corr_plot("PDO", "Spring")
corr_plot("SOI", "Spring")
corr_plot("NP", "Spring")
corr_plot("AO", "Spring")
