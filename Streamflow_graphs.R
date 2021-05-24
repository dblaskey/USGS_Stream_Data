rm(list = ls())

library(pacman)
pacman::p_load(dataRetrieval, tidyverse, rnaturalearthdata, mapdata, mblm, 
               Kendall, data.table, colorspace, zoo, lubridate, ggsci, RColorBrewer)

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

#Set criteria for passing
year_length <- 50
end_year <- as.POSIXct("2019-10-01")
start_year <- as.POSIXct("1968-09-30")

#Pull Geo Locations
data_AK <- whatNWISdata(stateCd="AK", parameterCd="00060") %>%
  distinct(site_no, .keep_all = T) %>%
  mutate(Record_length = as.numeric((end_date - begin_date)/365.25)) %>%
  filter(year(end_date)>year(end_year)-1) %>%
  mutate(bin = cut(Record_length, breaks = c(-Inf, 30, 40, 50, 60, Inf), labels = c("<30", "30-40", "40-50", "50-60", ">60"))) %>%
  mutate(pass = ifelse(bin == "<30" | Record_length*complete_record*365 > count_nu,"no","yes"))
           
ak <- map_data('worldHires','USA:Alaska')
ak <- subset(ak, long<0) #drop the end of the Aleutian Islands 
data_AK <- subset(data_AK, dec_long_va<0) 

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

pass_year <- obsy %>%
  mutate(Ypass = ifelse(Yearly_obs>Ycriteria,1,NA)) %>%
  drop_na()

#Yearly mean discharge
qy_mean <- final %>%
  group_by(site_no, wt_year) %>%
  summarise(Annual_Mean_Discharge = mean(X_00060_00003)) %>%
  drop_na()

#Yearly discharge statistical analysis
p_val <- 0.1

qy_stat <- qy_mean %>%
  group_by(site_no) %>% 
  do(MKtest_meanyear=MannKendall(.$Annual_Mean_Discharge)) %>%
  mutate(MK_meanyearp=MKtest_meanyear$sl, MK_meanyearsig=ifelse(MK_meanyearp<=p_val,"Significant","Not Significant"))

data_AK_final <- inner_join(data_AK_final, qy_stat) %>%
  select(-MKtest_meanyear)

qy_stat2 <- qy_mean %>%
  group_by(site_no) %>%
  drop_na() %>%
  mutate(Year=as.numeric(wt_year)) %>%
  do(TSqymean=mblm(Annual_Mean_Discharge ~ Year, .)) %>%
  mutate(qymean_Slope=TSqymean$coefficients[2])

data_AK_final <- inner_join(data_AK_final, qy_stat2) %>%
  select(-TSqymean)

qy_stat3 <- qy_mean %>%
  group_by(site_no) %>%
  drop_na() %>%
  summarise(QAve=mean(Annual_Mean_Discharge))

data_AK_final <- inner_join(data_AK_final, qy_stat3) %>%
  mutate(percent_change_mean = qymean_Slope/QAve) %>%
  select(-QAve)

#Yearly 10 percentile discharge
qy_Q10 <- final %>%
  group_by(site_no, wt_year) %>%
  summarise(Annual_Q10_Discharge = quantile(X_00060_00003, 0.1)) %>%
  drop_na()

#Yearly 10 percentile discharge statistical analysis
qy_stat <- qy_Q10 %>%
  group_by(site_no) %>% 
  do(MKtest_Q10year=MannKendall(.$Annual_Q10_Discharge)) %>%
  mutate(MK_Q10yearp=MKtest_Q10year$sl, MK_Q10yearsig=ifelse(MK_Q10yearp<=p_val,"Significant","Not Significant"))

data_AK_final <- inner_join(data_AK_final, qy_stat) %>%
  select(-MKtest_Q10year)

qy_stat2 <- qy_Q10 %>%
  group_by(site_no) %>%
  drop_na() %>%
  mutate(Year=as.numeric(wt_year)) %>%
  do(TSqyQ10=mblm(Annual_Q10_Discharge ~ Year, .)) %>%
  mutate(qyQ10_Slope=TSqyQ10$coefficients[2])

data_AK_final <- inner_join(data_AK_final, qy_stat2) %>%
  select(-TSqyQ10)

qy_stat3 <- qy_Q10 %>%
  group_by(site_no) %>%
  drop_na() %>%
  summarise(QAve=mean(Annual_Q10_Discharge))

data_AK_final <- inner_join(data_AK_final, qy_stat3) %>%
  mutate(percent_change_Q10 = qyQ10_Slope/QAve) %>%
  select(-QAve)

#Yearly 25 percentile discharge
qy_Q25 <- final %>%
  group_by(site_no, wt_year) %>%
  summarise(Annual_Q25_Discharge = quantile(X_00060_00003, 0.25)) %>%
  drop_na()

#Yearly 25 percentile discharge statistical analysis
qy_stat <- qy_Q25 %>%
  group_by(site_no) %>% 
  do(MKtest_Q25year=MannKendall(.$Annual_Q25_Discharge)) %>%
  mutate(MK_Q25yearp=MKtest_Q25year$sl, MK_Q25yearsig=ifelse(MK_Q25yearp<=p_val,"Significant","Not Significant"))

data_AK_final <- inner_join(data_AK_final, qy_stat) %>%
  select(-MKtest_Q25year)

qy_stat2 <- qy_Q25 %>%
  group_by(site_no) %>%
  drop_na() %>%
  mutate(Year=as.numeric(wt_year)) %>%
  do(TSqyQ25=mblm(Annual_Q25_Discharge ~ Year, .)) %>%
  mutate(qyQ25_Slope=TSqyQ25$coefficients[2])

data_AK_final <- inner_join(data_AK_final, qy_stat2) %>%
  select(-TSqyQ25)

qy_stat3 <- qy_Q25 %>%
  group_by(site_no) %>%
  drop_na() %>%
  summarise(QAve=mean(Annual_Q25_Discharge))

data_AK_final <- inner_join(data_AK_final, qy_stat3) %>%
  mutate(percent_change_Q25 = (qyQ25_Slope*year_length/2)/QAve) %>%
  select(-QAve)

#Yearly 50 percentile discharge
qy_Q50 <- final %>%
  group_by(site_no, wt_year) %>%
  summarise(Annual_Q50_Discharge = quantile(X_00060_00003, 0.5)) %>%
  drop_na()

#Yearly 50 percentile discharge statistical analysis
qy_stat <- qy_Q50 %>%
  group_by(site_no) %>% 
  do(MKtest_Q50year=MannKendall(.$Annual_Q50_Discharge)) %>%
  mutate(MK_Q50yearp=MKtest_Q50year$sl, MK_Q50yearsig=ifelse(MK_Q50yearp<=p_val,"Significant","Not Significant"))

data_AK_final <- inner_join(data_AK_final, qy_stat) %>%
  select(-MKtest_Q50year)

qy_stat2 <- qy_Q50 %>%
  group_by(site_no) %>%
  drop_na() %>%
  mutate(Year=as.numeric(wt_year)) %>%
  do(TSqyQ50=mblm(Annual_Q50_Discharge ~ Year, .)) %>%
  mutate(qyQ50_Slope=TSqyQ50$coefficients[2])

data_AK_final <- inner_join(data_AK_final, qy_stat2) %>%
  select(-TSqyQ50)

qy_stat3 <- qy_Q50 %>%
  group_by(site_no) %>%
  drop_na() %>%
  summarise(QAve=mean(Annual_Q50_Discharge))

data_AK_final <- inner_join(data_AK_final, qy_stat3) %>%
  mutate(percent_change_Q50 = qyQ50_Slope/QAve) %>%
  select(-QAve)

#Yearly 75 percentile discharge
qy_Q75 <- final %>%
  group_by(site_no, wt_year) %>%
  summarise(Annual_Q75_Discharge = quantile(X_00060_00003, 0.75)) %>%
  drop_na()

#Yearly 75 percentile discharge statistical analysis
qy_stat <- qy_Q75 %>%
  group_by(site_no) %>% 
  do(MKtest_Q75year=MannKendall(.$Annual_Q75_Discharge)) %>%
  mutate(MK_Q75yearp=MKtest_Q75year$sl, MK_Q75yearsig=ifelse(MK_Q75yearp<=p_val,"Significant","Not Significant"))

data_AK_final <- inner_join(data_AK_final, qy_stat) %>%
  select(-MKtest_Q75year)

qy_stat2 <- qy_Q75 %>%
  group_by(site_no) %>%
  drop_na() %>%
  mutate(Year=as.numeric(wt_year)) %>%
  do(TSqyQ75=mblm(Annual_Q75_Discharge ~ Year, .)) %>%
  mutate(qyQ75_Slope=TSqyQ75$coefficients[2])

data_AK_final <- inner_join(data_AK_final, qy_stat2) %>%
  select(-TSqyQ75)

qy_stat3 <- qy_Q75 %>%
  group_by(site_no) %>%
  drop_na() %>%
  summarise(QAve=mean(Annual_Q75_Discharge))

data_AK_final <- inner_join(data_AK_final, qy_stat3) %>%
  mutate(percent_change_Q75 = qyQ75_Slope/QAve) %>%
  select(-QAve)

#IQR
IQR <- full_join(qy_Q75, qy_Q25) %>%
  mutate(Annual_IQR_Discharge = Annual_Q75_Discharge - Annual_Q25_Discharge)

#IQR discharge statistical analysis
IQR_stat <- IQR %>%
  group_by(site_no) %>% 
  do(MKtest_IQRyear=MannKendall(.$Annual_IQR_Discharge)) %>%
  mutate(MK_IQRyearp=MKtest_IQRyear$sl, MK_IQRyearsig=ifelse(MK_IQRyearp<=p_val,"Significant","Not Significant"))

data_AK_final <- inner_join(data_AK_final, IQR_stat) %>%
  select(-MKtest_IQRyear)

IQR_stat2 <- IQR %>%
  group_by(site_no) %>%
  drop_na() %>%
  mutate(Year=as.numeric(wt_year)) %>%
  do(TSIQR=mblm(Annual_IQR_Discharge ~ Year, .)) %>%
  mutate(IQR_Slope=TSIQR$coefficients[2])

data_AK_final <- inner_join(data_AK_final, IQR_stat2) %>%
  select(-TSIQR)

IQR_stat3 <- IQR %>%
  group_by(site_no) %>%
  drop_na() %>%
  summarise(QAve=mean(Annual_IQR_Discharge))

data_AK_final <- inner_join(data_AK_final, IQR_stat3) %>%
  mutate(percent_change_IQR = IQR_Slope/QAve) %>%
  select(-QAve)

#Quartile Skew
Skew <- full_join(IQR, qy_Q50) %>%
  mutate(Annual_Skew_Discharge=((Annual_Q75_Discharge-Annual_Q50_Discharge)-(Annual_Q50_Discharge-Annual_Q25_Discharge))/Annual_IQR_Discharge)

#Skew discharge statistical analysis
Skew_stat <- Skew %>%
  group_by(site_no) %>% 
  do(MKtest_Skewyear=MannKendall(.$Annual_Skew_Discharge)) %>%
  mutate(MK_Skewyearp=MKtest_Skewyear$sl, MK_Skewyearsig=ifelse(MK_Skewyearp<=p_val,"Significant","Not Significant"))

data_AK_final <- inner_join(data_AK_final, Skew_stat) %>%
  select(-MKtest_Skewyear)

Skew_stat2 <- Skew %>%
  group_by(site_no) %>%
  drop_na() %>%
  mutate(Year=as.numeric(wt_year)) %>%
  do(TSSkew=mblm(Annual_Skew_Discharge ~ Year, .)) %>%
  mutate(Skew_Slope=TSSkew$coefficients[2])

data_AK_final <- inner_join(data_AK_final, Skew_stat2) %>%
  select(-TSSkew)

Skew_stat3 <- Skew %>%
  group_by(site_no) %>%
  drop_na() %>%
  summarise(QAve=mean(Annual_Skew_Discharge))

data_AK_final <- inner_join(data_AK_final, Skew_stat3) %>%
  mutate(percent_change_Skew = Skew_Slope/QAve) %>%
  select(-QAve)

#Yearly 90 percentile discharge
qy_Q90 <- final %>%
  group_by(site_no, wt_year) %>%
  summarise(Annual_Q90_Discharge = quantile(X_00060_00003, 0.9)) %>%
  drop_na()

#Yearly 90 percentile discharge statistical analysis
qy_stat <- qy_Q90 %>%
  group_by(site_no) %>% 
  do(MKtest_Q90year=MannKendall(.$Annual_Q90_Discharge)) %>%
  mutate(MK_Q90yearp=MKtest_Q90year$sl, MK_Q90yearsig=ifelse(MK_Q90yearp<=p_val,"Significant","Not Significant"))

data_AK_final <- inner_join(data_AK_final, qy_stat) %>%
  select(-MKtest_Q90year)

qy_stat2 <- qy_Q90 %>%
  group_by(site_no) %>%
  drop_na() %>%
  mutate(Year=as.numeric(wt_year)) %>%
  do(TSqyQ90=mblm(Annual_Q90_Discharge ~ Year, .)) %>%
  mutate(qyQ90_Slope=TSqyQ90$coefficients[2])

data_AK_final <- inner_join(data_AK_final, qy_stat2) %>%
  select(-TSqyQ90)

qy_stat3 <- qy_Q90 %>%
  group_by(site_no) %>%
  drop_na() %>%
  summarise(QAve=mean(Annual_Q90_Discharge))

data_AK_final <- inner_join(data_AK_final, qy_stat3) %>%
  mutate(percent_change_Q90 = qyQ90_Slope/QAve) %>%
  select(-QAve)

#Outlier Resistant Mean
Temp <- full_join(qy_Q90, qy_Q50) 
Trim_mean <- full_join(Temp, qy_Q10) %>%
  mutate(Annual_Trim_mean_Discharge=((Annual_Q90_Discharge-Annual_Q50_Discharge)-(Annual_Q50_Discharge-Annual_Q10_Discharge))/(Annual_Q90_Discharge - Annual_Q10_Discharge))

#Outlier Resistant Mean discharge statistical analysis
Trim_mean_stat <- Trim_mean %>%
  group_by(site_no) %>% 
  do(MKtest_Trim_meanyear=MannKendall(.$Annual_Trim_mean_Discharge)) %>%
  mutate(MK_Trim_meanyearp=MKtest_Trim_meanyear$sl, MK_Trim_meanyearsig=ifelse(MK_Trim_meanyearp<=p_val,"Significant","Not Significant"))

# Plots
png("Mean_Annual_60.png")
ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va, color = percent_change_mean, shape=MK_meanyearsig, size=MK_meanyearsig)) +
  labs(title="Relative Change in Annual Mean Discharge", color=" ") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_colour_gradient2(name = "Relative\nChange") +
  scale_shape_manual(name = "", values=c(15, 16)) +
  scale_size_manual(guide=FALSE, values=c(2,4)) +
  guides(colour = guide_colorbar(order=1), shape = guide_legend(order=2), size = "none")
dev.off()
png("Med_Annual_60.png")
ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va, color = percent_change_Q50, shape=MK_Q50yearsig, size=MK_Q50yearsig)) +
  labs(title="Relative Change in Annual Median Discharge", color=" ") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_colour_gradient2(name = "Relative\nChange") + 
  scale_shape_manual(name ="", values=c(15, 16)) +
  scale_size_manual(guide=FALSE, values=c(2,4)) +
  guides(colour = guide_colorbar(order=1), shape = guide_legend(order=2), size = "none")
dev.off()
png("P10_Annual_60.png")
ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va, color = percent_change_Q10, shape=MK_Q10yearsig, size=MK_Q10yearsig)) +
  labs(title="Relative Change in Annual P10 Discharge", color=" ") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_colour_gradient2(name = "Relative\nChange") + 
  scale_shape_manual(name ="", values=c(15, 16)) +
  scale_size_manual(guide=FALSE, values=c(2,4)) +
  guides(colour = guide_colorbar(order=1), shape = guide_legend(order=2), size = "none")
dev.off()
png("P90_Annual_60.png")
ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va, color = percent_change_Q90, shape=MK_Q90yearsig, size=MK_Q90yearsig)) +
  labs(title="Relative Change in Annual P90 Discharge", color=" ") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_colour_gradient2(name = "Relative\nChange") + 
  scale_shape_manual(name ="", values=c(15, 16)) +
  scale_size_manual(guide=FALSE, values=c(2,4)) +
  guides(colour = guide_colorbar(order=1), shape = guide_legend(order=2), size = "none")
dev.off()
png("IQR_Annual_60.png")
ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va, color = percent_change_IQR, shape=MK_IQRyearsig, size=MK_IQRyearsig)) +
  labs(title="Relative Change in Annual IQR of Discharge", color=" ") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_colour_gradient2(name = "Relative\nChange") + 
  scale_shape_manual(name ="", values=c(15, 16)) +
  scale_size_manual(guide=FALSE, values=c(2,4)) +
  guides(colour = guide_colorbar(order=1), shape = guide_legend(order=2), size = "none")
dev.off()
png("Skew_Annual_60.png")
ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va, color = percent_change_Skew, shape=MK_Skewyearsig, size=MK_Skewyearsig)) +
  labs(title="Relative Change in Annual Skew of Discharge", color=" ") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_colour_gradient2(name = "Relative\nChange") + 
  scale_shape_manual(name ="", values=c(15, 16)) +
  scale_size_manual(guide=FALSE, values=c(2,4)) +
  guides(colour = guide_colorbar(order=1), shape = guide_legend(order=2), size = "none")
dev.off()

#Peak Flow
ave_criteria <- 10

peak_flow <- final %>%
  group_by(site_no, wt_year) %>%
  mutate(ave_q = rollapply(X_00060_00003,  FUN = mean, width = ave_criteria, fill = NA, align = "center")) %>%
  filter(ave_q>0 & ave_q == max(ave_q, na.rm = TRUE)) %>%
  slice(1) %>%
  mutate(jday = (as.integer(difftime(make_date(Year, Month, Day), make_date(Year, 01, 01), units = "days"))))

#Peak Flow discharge statistical analysis
PF_stat <- peak_flow %>%
  group_by(site_no) %>% 
  do(MKPFtest=MannKendall(.$ave_q)) %>%
  mutate(MK_PF_p=MKPFtest$sl, MK_PF_sig=ifelse(MK_PF_p<=p_val,"Significant","Not Significant"))

PF_stat2 <- peak_flow %>%
  group_by(site_no) %>%
  drop_na() %>%
  do(PF_slope=mblm(ave_q ~ wt_year, .)) %>%
  mutate(PF_Slope=PF_slope$coefficients[2])

data_AK_final <- full_join(data_AK_final, PF_stat) %>%
  select(-MKPFtest)

data_AK_final <- full_join(data_AK_final, PF_stat2) %>%
  select(-PF_slope)

PF_stat3 <- peak_flow %>%
  group_by(site_no) %>%
  drop_na() %>%
  summarise(PFAve=mean(ave_q))

data_AK_final <- inner_join(data_AK_final, PF_stat3) %>%
  mutate(percent_change_PF = PF_Slope/PFAve) %>%
  select(-PFAve)

#Peak Flow date statistical analysis
PF_date_stat <- peak_flow %>%
  group_by(site_no) %>% 
  do(MKPFdatetest=MannKendall(.$jday)) %>%
  mutate(MK_PF_date_p=MKPFdatetest$sl, MK_PF_date_sig=ifelse(MK_PF_date_p<=p_val,"Significant","Not Significant"))

PF_date_stat2 <- peak_flow %>%
  group_by(site_no) %>%
  drop_na() %>%
  do(PF_date_slope=mblm(jday ~ wt_year, .)) %>%
  mutate(PF_date_Slope=PF_date_slope$coefficients[2])

data_AK_final <- full_join(data_AK_final, PF_date_stat) %>%
  select(-MKPFdatetest)

data_AK_final <- full_join(data_AK_final, PF_date_stat2) %>%
  select(-PF_date_slope)

PF_date_stat3 <- peak_flow %>%
  group_by(site_no) %>%
  drop_na() %>%
  summarise(PF_date_Ave=mean(jday))

data_AK_final <- inner_join(data_AK_final, PF_date_stat3) %>%
  mutate(day_change_PFdate = PF_date_Slope) %>%
  select(-PF_date_Ave)

png("Max_Annual_60.png")
ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va, color = percent_change_PF, shape=MK_PF_sig, size=MK_PF_sig)) +
  labs(title="Relative Change in Annual Maximum Discharge", color=" ") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_colour_gradient2(name = "Relative\nChange") + 
  scale_shape_manual(name ="", values=c(15, 16)) +
  scale_size_manual(guide=FALSE, values=c(2,4)) +
  guides(colour = guide_colorbar(order=1), shape = guide_legend(order=2), size = "none")
dev.off()
png("MaxDay_Annual_60.png")
ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va, color = day_change_PFdate, shape=MK_PF_date_sig, size=MK_PF_date_sig)) +
  labs(title="Change in Date of Annual Maximum Discharge", color=" ") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_colour_gradient2(name = "Day\nChange") + 
  scale_shape_manual(name ="", values=c(15, 16)) +
  scale_size_manual(guide=FALSE, values=c(2,4)) +
  guides(colour = guide_colorbar(order=1), shape = guide_legend(order=2), size = "none")
dev.off()

#Flashiness
flash <- final %>%
  group_by(site_no) %>%
  mutate(date = make_date(Year, Month, Day)) %>%
  mutate(dq = ifelse(date-lag(date,default = date[1])>0 ,abs(X_00060_00003-lag(X_00060_00003)),NA))

flashiness <- flash %>%
  group_by(site_no, wt_year, Month) %>%
  summarise(sum_dq = sum(dq, na.rm=TRUE), sum_q = sum(X_00060_00003)) %>%
  mutate(dq_q = ifelse(sum_q>0, sum_dq/sum_q, NA))

#Flashiness statistical analysis
flashiness_stat <- flashiness %>%
  group_by(site_no) %>% 
  do(MKflashtest=MannKendall(.$dq_q)) %>%
  mutate(MK_flash_p=MKflashtest$sl, Mk_flash_sig=ifelse(MK_flash_p<=p_val,"Significant","Not Significant"))

flashiness_stat2 <- flashiness %>%
  group_by(site_no) %>%
  drop_na() %>%
  mutate(Year=as.numeric(wt_year)) %>%
  do(TSflash_year=mblm(dq_q ~ Year, .)) %>%
  mutate(Flash_Slope=TSflash_year$coefficients[2])

data_AK_final <- full_join(data_AK_final, flashiness_stat) %>%
  select(-MKflashtest)

data_AK_final <- full_join(data_AK_final, flashiness_stat2) %>%
  select(-TSflash_year)

flashiness_stat3 <- flashiness %>%
  group_by(site_no) %>%
  drop_na() %>%
  summarise(flash_Ave=mean(dq_q))

data_AK_final <- inner_join(data_AK_final, flashiness_stat3) %>%
  mutate(change_flash = Flash_Slope/flash_Ave) %>%
  select(-flash_Ave)

png("Flash_Annual_60.png")
ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va, color = change_flash, shape=Mk_flash_sig, size=Mk_flash_sig)) +
  labs(title="Change in Flashiness of Streamflow", color=" ") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_colour_gradient2(name = "Relative\nChange") + 
  scale_shape_manual(name ="", values=c(15, 16)) +
  scale_size_manual(guide=FALSE, values=c(2,4)) +
  guides(colour = guide_colorbar(order=1), shape = guide_legend(order=2), size = "none")
dev.off()

