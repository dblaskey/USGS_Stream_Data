library(pacman)
pacman::p_load(dataRetrieval, sf, tidyverse, zoo, lfstat, Kendall, lubridate, rnaturalearthdata, mapdata, trend, forecast, tstools)

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
  mutate(date=as.POSIXct(paste(Year, Month, Day, sep="-")))

#Pull Geo Locations
data_AK <- whatNWISdata(stateCd="AK", parameterCd="00060") %>%
  distinct(site_no, .keep_all = T)

data_AK_final <- semi_join(data_AK, final, by="site_no")%>%
  select(site_no, station_nm, dec_lat_va, dec_long_va)

#Not needed right now
#final <- full_join(data_AK,final, by="site_no") %>%
  #select(site_no, station_nm, dec_lat_va, dec_long_va, Year, Month, Day, X_00060_00003, wt_year, date) %>%
  #drop_na()

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

#Spearman Test
#qm_stat2 <- qm %>%
 # group_by(site_no) %>% 
 # do(Sptest=partial.cor.trend.test(qm$Monthly_Discharge,1:length(qm$Monthly_Discharge), "spearman")) %>%
  #mutate(Sp_p=Sptest$p.value, Sp_sig=ifelse(Sp_p<=p_val,1,0))

data_AK_final_monthly <- full_join(data_AK_final, qm_stat) %>%
  select(-MKtest_month)

data_AK_final <- full_join(data_AK_final, qy_stat) %>%
  select(-MKtest_year)

#qm_lm <- qm %>%
 # mutate(fulldate=as.yearmon(paste(wt_year, Month), "%Y %m")) %>%
  #cmav()

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

data_AK_final <- full_join(data_AK_final, flashiness_stat) %>%
  select(-MKflashtest)

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

data_AK_final <- full_join(data_AK_final, WD_stat) %>%
  select(-MK_WD_test)

#Peak Flow
ave_criteria <- 10

peak_flow <- final %>%
  group_by(site_no, wt_year) %>%
  mutate(ave_q = rollapply(X_00060_00003,  FUN = mean, width = ave_criteria, fill = NA, align = "center")) %>%
  filter(ave_q>0 & ave_q == max(ave_q, na.rm = TRUE)) %>%
  slice(1)

#Peak Flow statistical analysis
PF_stat <- peak_flow %>%
  group_by(site_no) %>% 
  do(MKPFtest=MannKendall(.$ave_q)) %>%
  mutate(MK_PF_p=MKPFtest$sl, MK_PF_sig=ifelse(MK_PF_p<=p_val,"Significant","Not Significant"))

data_AK_final <- full_join(data_AK_final, PF_stat) %>%
  select(-MKPFtest)

#Freeze Up (Need some work still)
freeze_up <- final %>%
  mutate(date = as.Date(date)) %>%
  mutate(wt_year = ifelse(as.numeric(Month)>=9, as.numeric(Year) + 1, as.numeric(Year)))%>%
  group_by(site_no, Year, X_00060_00003_cd) %>%
  filter(X_00060_00003_cd == "A e") %>%
  slice(1) %>%
  mutate(wtr_day = (as.integer(difftime(date,ymd(paste0(wt_year - 1 ,'-09-30')), units = "days"))))

#Freeze Up statistical analysis
FU_stat <- freeze_up %>%
  group_by(site_no) %>% 
  do(test=MannKendall(.$wtr_day)) %>%
  mutate(tau=test$tau,sl=test$sl,D=test$D, varS=test$varS, S=test$S, Sig=ifelse(sl<=p_val,1,0))

#Ressesions
ReFinal <- final %>%
  rename(day = Day, month = Month, year = Year, flow = X_00060_00003) %>%
  group_by(site_no, year) %>%
  nest()

RecessionsAnalysis <- wy_final %>%
  group_by(site_no, year) %>%
  summarise(annual_q = sum(X_00060_00003)) %>%
  add_column(recess = NA)

nobj <- nrow(ReFinal)

for(i in 1:nobj){
temp <- ReFinal[[3]][[i]] %>%
createlfobj(temp, hyearstart = 10, baseflow = FALSE, meta = list())

RecessionsAnalysis[[4]][[i]] <- recession(temp, method = "MRC", seglen = 7, threshold = 70, thresbreaks = "fixed", plotMRC = FALSE, na.rm = TRUE) 
}

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