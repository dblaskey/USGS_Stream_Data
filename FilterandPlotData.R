rm(list = ls())

library(pacman)
pacman::p_load(dataRetrieval, tidyverse, rnaturalearthdata, mapdata, mblm, 
               Kendall, data.table, colorspace, zoo, lubridate, ggsci, RColorBrewer)

#Pull discharge Data
Qak <- readNWISdata(stateCd="AK", parameterCd="00060", service="dv") %>%
  distinct(site_no, .keep_all = T)
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
load("/Users/dybl4375/USGS_Stream_Data_Pull/sites.Rdata")

#Remove Provisional Data
sites <- sites[!grepl("P", sites$X_00060_00003_cd),]

#Turn year into water year
sites <- sites %>%
  mutate(Date = as.POSIXct(Date)) %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date)) %>%
  mutate(wt_year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year)))

#Set criteria for passing
Ycriteria <- 350
complete_record <- 0.7

#Histogram of Data Availability
Year_data_available <- sites %>%
  group_by(site_no) %>%
  count(wt_year) 

Years_per_site <- Year_data_available %>%
  select(-n) %>%
  count(site_no)

barplot(Years_per_site$n, main="Years of Data Distribution", horiz=TRUE, xlab="Years")
abline(v=30, col="Red")

hist(Year_data_available$wt_year, breaks = 109, main="Available Stream Gauge Data per Year",
     xlab = "Year", xlim=c(1900,2020), ylab = "Number of Gauges")

#Set criteria for passing
year_length <-60
end_year <- as.POSIXct("2019-10-01")
start_year <- as.POSIXct("1958-09-30")

#Pull Geo Locations
data_AK <- whatNWISdata(stateCd="AK", parameterCd="00060") %>%
  distinct(site_no, .keep_all = T) %>%
  mutate(Record_length = as.numeric((end_date - begin_date)/365.25)) %>%
  filter(year(end_date)>year(end_year)-1) %>%
  mutate(bin = cut(Record_length, breaks = c(-Inf, 30, 40, 50, 60, Inf), labels = c("<30", "30-40", "40-50", "50-60", ">60"))) %>%
  mutate(pass = ifelse(bin == "<30" | Record_length*complete_record*365 > count_nu,"no","yes"))

#Plot locations
ak <- map_data('worldHires','USA:Alaska')
ak <- subset(ak, long<0) #drop the end of the Aleutian Islands 
data_AK <- subset(data_AK, dec_long_va<0) 

ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data = data_AK, aes(x=dec_long_va, y=dec_lat_va, color = bin, size = pass)) +
  ggtitle("Record Length of Stations") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_color_npg(name = "Years") +
  scale_size_manual(name = "Pass\nCriteria?", values=c(0.5, 3))

#Constrain to the desired range of years
sites <- sites %>%
  mutate(Date=as.POSIXct(Date)) %>%
  mutate(Date = date(Date)) %>%
  filter(Date>start_year & Date<end_year)

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

#Arrange for monthly and annual analysis
sites2 <- separate(final_sites, "Date", c("Year", "Month", "Day"), sep = "-")

#Turn year into water year
sites2 <- sites2 %>%
  mutate(wt_year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year)))

#Create table of observations per year
obsy <- sites2 %>%
  group_by(site_no) %>%
  count(wt_year, name = "Yearly_obs")

#Analyze what passes yearly criteria
pass_year <- obsy %>%
  mutate(Ypass = ifelse(Yearly_obs>Ycriteria,1,0))

fail <- pass_year %>%
  filter(Ypass==0)

#Remove lines that fail criteria
final <- anti_join(sites2,fail) 

# Reduce to needed sites
data_AK_final <- semi_join (data_AK, final, by="site_no") %>%
  select(site_no, station_nm, dec_lat_va, dec_long_va)

# Plot removed data
pass_year <- obsy %>%
  mutate(Ypass = ifelse(Yearly_obs>Ycriteria,1,NA)) %>%
  drop_na()

ggplot(pass_year, aes(wt_year, site_no, fill = is.na(Ypass))) +
  geom_tile() +
  ggtitle("Alaskan Stream Gauge Data Availability") +
  xlab("Year") +
  ylab("Sites") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill=NA, size=1), 
        panel.background = element_rect(fill = "white"), 
        axis.text.y=element_blank(), legend.position = "none") +
  scale_fill_manual(name="Data Available?", values= c("Black", "White"),) 

save(final, file = "60yearsfinal.RData")
save(data_AK_final, file="60yearsites.RData")
