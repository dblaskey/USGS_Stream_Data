library(pacman)
pacman::p_load(dataRetrieval, tidyverse)

#Pull discharge Data
Qak <- readNWISdata(stateCd="AK", parameterCd="00060", service="dv")
data_file <- select(Qak, site_no)

nsites <- nrow(data_file)-1

for(i in 1:nsites){
  if(i==1)
    sites <- readNWISdv(data_file[i,],
                       parameterCd = '00060')
  else
    temp <- readNWISdv(data_file[i,],
                   parameterCd = '00060')
    sites <- rbind(sites,temp)
}

#Set criteria for passing
year_length <- 20
complete_record <- 0.9

#Summarize data
Summarized_data <- sites %>%
  count(site_no)
  
Summarized_data2 <- sites %>%
  group_by(site_no) %>%
  summarise(start_date = min(Date), end_date=max(Date))

Summarized_data <- merge(Summarized_data2, Summarized_data)
  
Summarized_data <- Summarized_data %>%
  mutate(end_date=as.POSIXct(end_date)) %>%
  mutate(start_date=as.POSIXct(start_date)) %>%
  mutate(Record_length = (end_date - start_date)/365.25) %>%
  mutate(Completeness = (n/365.25)/as.double(Record_length)) %>%
  mutate(Effective_obs = Completeness*Record_length) 

fail <- Summarized_data %>%
  filter(Effective_obs<year_length)

#Remove years that don't pass
final_sites <- anti_join(sites,fail)
