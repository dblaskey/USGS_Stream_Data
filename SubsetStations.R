library(pacman)
pacman::p_load(dataRetrieval, tidyverse, zoo)

#Arrange for monthly and annual analysis
sites2 <- separate(final_sites, "Date", c("Year", "Month", "Day"), sep = "-")

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
MinYcriteria <- 11

#Analyze what passes this criteria
obsy <- obsy %>%
  mutate(Ypass = ifelse(Yearly_obs>Ycriteria,1,0))
obsm <- obsm %>%
  mutate(Mpass = ifelse(Monthly_obs>Mcriteria,1,0))

pass_month <- obsm %>%
  group_by(site_no, Year) %>%
  summarise(Months = sum(Mpass))

pass_year <- left_join(obsy, pass_month)

fail <- pass_year %>%
  filter(Months<MinYcriteria | Ypass==0)

#Remove lines that fail criteria
final <- anti_join(sites2,fail) %>%
  mutate(date=as.POSIXct(paste(Year, Month, Day, sep="-")))

#Monthly and yearly discharge
qy <- final %>%
  group_by(site_no, Year) %>%
  summarise(Annual_Discharge = mean(X_00060_00003))

qm <- final %>%
  group_by(site_no, Year, Month) %>%
  summarise(Monthly_Discharge = mean(X_00060_00003))

#Flashiness
flash <- final %>%
  group_by(site_no) %>%
  mutate(dq = ifelse(date-lag(date,default = date[1])>0 ,abs(X_00060_00003-lag(X_00060_00003)),NA))

flashiness <- flash %>%
  group_by(site_no, Year, Month) %>%
  summarise(sum_dq = sum(dq, na.rm=TRUE), sum_q = sum(X_00060_00003)) %>%
  mutate(dq_q = ifelse(sum_q>0, sum_dq/sum_q, NA))

#Winter Discharge
winter <- sites2 %>%
  group_by(site_no) %>%
  filter(3>=as.numeric(Month))

wd_criteria <- 75

wd <- winter %>%
  count(site_no, Year) %>%
  filter(n<wd_criteria)

winter_discharge <- anti_join(winter, wd) %>%
  group_by(site_no, Year) %>%
  summarise(Winter_Discharge = mean(X_00060_00003))

#Peak Flow
ave_criteria <- 10

peak_flow <- final %>%
  group_by(site_no, Year) %>%
  mutate(ave_q = rollapply(X_00060_00003,  FUN = mean, width = ave_criteria, fill = NA, align = "center")) %>%
  filter(ave_q>0 & ave_q == max(ave_q, na.rm = TRUE)) 
  
#Freeze Up


#Ressesions 

