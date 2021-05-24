# Freezeup and Breakup
rm(list = ls())

# Load Libraries
library(pacman)
p_load("tidyverse", "lubridate", "mblm")

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
  
# Calculate Trends

# Breakup
TS_df <- df %>%
  drop_na(breakup) %>%
  group_by(siteID) %>%
  filter(n() >= 10) %>%
  do(TS=mblm(breakup ~ year, .)) %>% 
  mutate(BU_Slope=TS$coefficients[2]) %>%
  select(-TS)
  
# Combine to final data frame
Final_Trends = full_join(siteid, TS_df)

# First Ice
TS_df <- df %>%
  drop_na(first_ice) %>%
  group_by(siteID) %>%
  filter(n() >= 10) %>%
  do(TS=mblm(first_ice ~ year, .)) %>% 
  mutate(FI_Slope=TS$coefficients[2]) %>%
  select(-TS)

# Combine to final data frame
Final_Trends = full_join(Final_Trends, TS_df)

# Final Analysis
Regional_Trends = Final_Trends %>%
  group_by(region) %>%
  summarise(breakupave=mean(BU_Slope, na.rm=TRUE), firsticeave=mean(FI_Slope, na.rm=TRUE))

River_Trends = Final_Trends %>%
  group_by(river) %>%
  summarise(breakupave=mean(BU_Slope, na.rm=TRUE), firsticeave=mean(FI_Slope, na.rm=TRUE)) %>%
  drop_na()

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



