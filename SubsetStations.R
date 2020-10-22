library(pacman)
pacman::p_load(dataRetrieval, dplyr, tidyverse, leaflet)

sites <- dataset

#Summarize data
Summarized_data <- sites %>%
  group_by(site_no) %>%
  summarise(start_date = min(Date), end_date=max(Date), observations=max(X))

Summarized_data <- Summarized_data %>%
  mutate(end_date=as.POSIXct(end_date,tz="AST")) %>%
  mutate(start_date=as.POSIXct(start_date,tz="AST")) %>%
  mutate(Record_length = (end_date - start_date)/365.25) %>%
  mutate(Completeness = (observations/365.25)/as.double(Record_length)) %>%
  mutate(Effective_obs = Completeness*Record_length)

#Arrange for monthly and annual analysis
sites2 <- separate(sites, "Date", c("Year", "Month", "Day"), sep = "-")

#Create table of observations per month
obsy <- sites2 %>%
  group_by(site_no) %>%
  count(Year, name = "Yearly_obs")

obsm <- sites2 %>%
  group_by(site_no, Year) %>%
  count(Month, name = "Monthly_obs")

#Set criteria for passing
Ycriteria <- 0.9*365
Mcriteria <- 0.8*30

#Analyize what passes this criteria
obsy %>%
  mutate(Ypass = ifelse(Yearly_obs>Ycriteria,1,0))
obsm %>%
  mutate(Mpass = ifelse(Monthly_obs>Mcriteria,1,0))


