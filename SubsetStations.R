library(pacman)
pacman::p_load(dataRetrieval, sf, tidyverse, zoo, lfstat, Kendall, lubridate, rnaturalearthdata, mapdata, trend, forecast, tstools, mblm)

#Arrange for monthly and annual analysis
sites2 <- separate(final_sites, "Date", c("Year", "Month", "Day"), sep = "-")

#Turn year into water year
sites2 <- sites2 %>%
  mutate(wt_year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year)))

#Remove Provisional Data
sites2 <- sites2[!grepl("P", sites2$X_00060_00003_cd),]

#Create table of observations per month
obsy <- sites2 %>%
  group_by(site_no) %>%
  count(wt_year, name = "Yearly_obs")

obsm <- sites2 %>%
  group_by(site_no, wt_year) %>%
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
  group_by(site_no, wt_year) %>%
  summarise(Months = sum(Mpass))

pass_year <- left_join(obsy, pass_month)

fail <- pass_year %>%
  filter(Months<MinYcriteria | Ypass==0)

#Remove lines that fail criteria
final <- anti_join(sites2,fail) %>%
  mutate(date=as.POSIXct(paste(Year, Month, Day, sep="-")), 
         wt_year=ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year))) 

#Pull Geo Locations
data_AK <- whatNWISdata(stateCd="AK", parameterCd="00060") %>%
  distinct(site_no, .keep_all = T)

data_AK_final <- semi_join(data_AK, final, by="site_no")%>%
  select(site_no, station_nm, dec_lat_va, dec_long_va)

#Plot locations
ak <- map_data('worldHires','USA:Alaska')
ak <- subset(ak, long<0) #drop the end of the Aleutian Islands 
data_AK <- subset(data_AK, dec_long_va<0)
data_AK_final <- subset(data_AK_final, dec_long_va<0) 

ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=data_AK, aes(x=dec_long_va, y=dec_lat_va), color="darkred") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va), color="darkgreen") +
  ggtitle("Passing Stations")
  

#Monthly and yearly discharge
qy <- final %>%
  group_by(site_no, wt_year) %>%
  summarise(Annual_Discharge = mean(X_00060_00003)) %>%
  drop_na()

qm <- final %>%
  group_by(site_no, wt_year, Month) %>%
  summarise(Monthly_Discharge = mean(X_00060_00003)) %>%
  drop_na()

#Monthly and yearly discharge statistical analysis
p_val <- 0.05

qy_stat <- qy %>%
  group_by(site_no) %>% 
  do(MKtest_year=MannKendall(.$Annual_Discharge)) %>%
  mutate(MK_yearp=MKtest_year$sl, MK_yearsig=ifelse(MK_yearp<=p_val,"Significant","Not Significant"))

qm_stat <- qm %>%
  group_by(site_no, Month) %>% 
  do(MKtest_month=MannKendall(.$Monthly_Discharge)) %>%
  mutate(MK_monthp=MKtest_month$sl, MK_monthsig=ifelse(MK_monthp<=p_val,"Significant","Not Significant")) 

qy_stat2 <- qy %>%
  group_by(site_no) %>%
  drop_na() %>%
  mutate(Year=as.numeric(Year)) %>%
  do(TSqy=mblm(Annual_Discharge ~ Year, .)) %>%
  mutate(qy_Slope=TSqy$coefficients[2])

qy_stat3 <- qy %>%
  group_by(site_no) %>%
  drop_na() %>%
  summarise(QAve=mean(Annual_Discharge))

data_AK_final_monthly <- full_join(data_AK_final, qm_stat) %>%
  select(-MKtest_month)

data_AK_final <- full_join(data_AK_final, qy_stat) %>%
  select(-MKtest_year)

data_AK_final <- full_join(data_AK_final, qy_stat2) %>%
  select(-TSqy)

data_AK_final <- full_join(data_AK_final, qy_stat3) %>%
  mutate(Percent_change=SigQ/QAve)

#Flashiness
flash <- final %>%
  group_by(site_no) %>%
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
  mutate(Year=as.numeric(Year)) %>%
  do(TSflash_year=mblm(dq_q ~ Year, .)) %>%
  mutate(Flash_Slope=TSflash_year$coefficients[2])

data_AK_final <- full_join(data_AK_final, flashiness_stat) %>%
  select(-MKflashtest)

data_AK_final <- full_join(data_AK_final, flashiness_stat2) %>%
  select(-TSflash_year)

#Winter Discharge
winter <- sites2 %>%
  group_by(site_no) %>%
  filter(3>=as.numeric(Month))

wd_criteria <- 75

wd <- winter %>%
  count(site_no, wt_year) %>%
  filter(n<wd_criteria)

winter_discharge <- anti_join(winter, wd) %>%
  group_by(site_no, wt_year) %>%
  summarise(Winter_Discharge = mean(X_00060_00003))

#Winter Discharge statistical analysis
WD_stat <- winter_discharge %>%
  group_by(site_no) %>% 
  do(MK_WD_test=MannKendall(.$Winter_Discharge)) %>%
  mutate(MK_WD_p=MK_WD_test$sl, MK_WD_sig=ifelse(MK_WD_p<=p_val,"Significant","Not Significant"))

WD_stat2 <- winter_discharge %>%
  group_by(site_no) %>%
  drop_na() %>%
  mutate(Year=as.numeric(Year)) %>%
  do(WD_year=mblm(Winter_Discharge ~ Year, .)) %>%
  mutate(WD_Slope=WD_year$coefficients[2])

data_AK_final <- full_join(data_AK_final, WD_stat) %>%
  select(-MK_WD_test)

data_AK_final <- full_join(data_AK_final, WD_stat2) %>%
  select(-WD_year)

#Peak Flow
ave_criteria <- 10

peak_flow <- final %>%
  group_by(site_no, wt_year) %>%
  mutate(ave_q = rollapply(X_00060_00003,  FUN = mean, width = ave_criteria, fill = NA, align = "center")) %>%
  filter(ave_q>0 & ave_q == max(ave_q, na.rm = TRUE)) %>%
  slice(1) %>%
  mutate(jday = (as.integer(difftime(date,ymd(paste0(as.numeric(Year),'-01-01')), units = "days"))))

#Peak Flow statistical analysis
PF_stat <- peak_flow %>%
  group_by(site_no) %>% 
  do(MKPFtest=MannKendall(.$ave_q)) %>%
  mutate(MK_PF_p=MKPFtest$sl, MK_PF_sig=ifelse(MK_PF_p<=p_val,"Significant","Not Significant"))

PF_stat2 <- peak_flow %>%
  group_by(site_no) %>%
  drop_na() %>%
  do(PF_slope=mblm(ave_q ~ jday, .)) %>%
  mutate(PF_Slope=PF_slope$coefficients[2])

data_AK_final <- full_join(data_AK_final, PF_stat) %>%
  select(-MKPFtest)

data_AK_final <- full_join(data_AK_final, PF_stat2) %>%
  select(-PF_slope)

#Freeze Up
freeze_up <- final %>%
  mutate(date = as.Date(date)) %>%
  mutate(wt_year = ifelse(as.numeric(Month)>=8, as.numeric(Year) + 1, as.numeric(Year)))%>%
  filter(as.numeric(Month)>=8) %>%
  group_by(site_no, Year, X_00060_00003_cd) %>%
  filter(X_00060_00003_cd == "A e") %>%
  slice(1) %>%
  mutate(wtr_day = (as.integer(difftime(date,ymd(paste0(wt_year - 1 ,'-09-30')), units = "days"))))

#Freeze Up statistical analysis
FU_stat <- freeze_up %>%
  group_by(site_no) %>% 
  filter(n()>10) %>%
  do(MKFUtest=MannKendall(.$wtr_day)) %>%
  mutate(MK_FU_p=MKFUtest$sl, MK_FU_sig=ifelse(MK_FU_p<=p_val,"Significant","Not Significant"))

FU_stat2 <- freeze_up %>%
  group_by(site_no) %>%
  drop_na() %>%
  filter(n()>10) %>%
  mutate(wt_year=as.numeric(wt_year), wtr_day=as.numeric(wtr_day)) %>%
  do(FU=mblm(wtr_day~wt_year, .)) %>%
  mutate(FU_Slope=FU$coefficients[2])

data_AK_final <- full_join(data_AK_final, FU_stat) %>%
  select(-MKFUtest)

data_AK_final <- full_join(data_AK_final, FU_stat2) %>%
  select(-FU)

#Ressesions
#ReFinal <- final %>%
  #group_by(site_no, wt_year) %>%
  #drop_na() %>%
    #mutate(dq = ifelse(date-lag(date,default = date[1])>0 ,X_00060_00003-lag(X_00060_00003),NA)) %>%
    #filter(dq<0, as.numeric(Month)==8) %>%
    #mutate(logq = log10(X_00060_00003), logdq = log10(abs(dq)))
  #rename(day = Day, month = Month, year = Year, flow = X_00060_00003) %>%
  #group_by(site_no, wt_year) %>%
  #nest()

#RecessionsAnalysis <- final %>%
  #group_by(site_no, wt_year) %>%
  #drop_na() %>%
  #summarise(annual_q = sum(X_00060_00003)) %>%
  #add_column(recess = NA)

#nobj <- nrow(ReFinal)

#for(i in 1:nobj){
#temp <- ReFinal[[3]][[2]] %>%
#createlfobj(temp, hyearstart = 7, baseflow = FALSE, meta = list())

#RecessionsAnalysis[[4]][[2]] <- recession(temp, method = "IRS", seglen = 8, threshold = 70, thresbreaks = "fixed", na.rm = TRUE) 
#}

#For testing purposes only
data_AK_final <- data_AK_final %>%
  mutate(SigQ = ifelse(MK_yearsig=="Significant",Percent_change,0), ) #ADD MORE SIG CHANGES

ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="black", color="black") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va, color=Percent_change)) +
  scale_colour_gradient2() +
  labs(title="Change in Annual Discharge", color=" ")


#Plots

ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va, color=factor(MK_yearsig))) +
  labs(title="Significant Change in Yearly Discharge", color=" ")

ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va, color=factor(Mk_flash_sig))) +
  labs(title="Significant Change in Flashiness", color=" ")

ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va, color=factor(MK_WD_sig))) +
  labs(title="Significant Change in Winter Discharge", color=" ")

ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va, color=factor(MK_PF_sig))) +
  labs(title="Significant Change in Peak Flow", color=" ")


for (i in 1:12){
  temp <- data_AK_final_monthly %>% 
    filter(as.numeric(Month)==i)
  
  ggplot() + 
         geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
         geom_point(data=temp, aes(x=dec_long_va, y=dec_lat_va, color=factor(MK_monthsig))) +
         labs(title=paste0("Significant Monthly Discharge Change for Month Number ", i), color=" ")
  }