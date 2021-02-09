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

#For local use only: DO NOT PUBLISH
load(file="sites.Rdata")

#Set criteria for passing
year_length <-28
complete_record <- 0.9
end_year <- as.POSIXct("2013-10-01 00:00:00")
start_year <- as.POSIXct("1983-10-01 00:00:00")

#Constrain to the last 30 years
sites <- sites %>%
  mutate(Date=as.POSIXct(Date)) %>%
  filter(Date>start_year & Date<end_year)

#Remove Provisional Data
sites <- sites[!grepl("P", sites$X_00060_00003_cd),]

#Summarize data
Summarized_data <- sites %>%
  count(site_no)
  
Summarized_data2 <- sites %>%
  group_by(site_no) %>%
  summarise(start_date = min(Date), end_date=max(Date))

Summarized_data <- merge(Summarized_data2, Summarized_data)
  
Summarized_data <- Summarized_data %>%
  mutate(Record_length = (end_date - start_date)/365.25)

fail <- Summarized_data %>%
  filter(Record_length<(year_length) | n<(365.25*(year_length)*complete_record))

#Remove years that don't pass
final_sites <- anti_join(sites,fail)
