


MKtrend_Discharge = function(path_2_data, path_2_loc, year_length, p_val){
  
  load(path_2_data)
  load(path_2_loc)
  
  #Yearly mean discharge
  qy_mean <- final %>%
    group_by(site_no, wt_year) %>%
    summarise(Annual_Mean_Discharge = mean(X_00060_00003)) %>%
    drop_na()
  
  #Yearly discharge statistical analysis
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
    mutate(percent_change_mean = (qymean_Slope*year_length/2)/QAve*100) %>%
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
    mutate(percent_change_Q10 = (qyQ10_Slope*year_length/2)/QAve*100) %>%
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
    mutate(percent_change_Q25 = (qyQ25_Slope*year_length/2)/QAve*100) %>%
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
    mutate(percent_change_Q75 = (qyQ75_Slope*year_length/2)/QAve*100) %>%
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
  
  data_AK_final <- inner_join(data_AK_final, qy_stat3) %>%
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
  
  #Skew discharge statistical analysis
  Trim_mean_stat <- Trim_mean %>%
    group_by(site_no) %>% 
    do(MKtest_Trim_meanyear=MannKendall(.$Annual_Trim_mean_Discharge)) %>%
    mutate(MK_Trim_meanyearp=MKtest_Trim_meanyear$sl, MK_Trim_meanyearsig=ifelse(MK_Trim_meanyearp<=p_val,"Significant","Not Significant"))
  
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
    mutate(percent_change_PF = (PF_Slope*year_length/2)/PFAve*100) %>%
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
    mutate(day_change_PFdate = (PF_date_Slope*year_length/2)) %>%
    select(-PF_date_Ave)
  
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
    mutate(change_flash = Flash_Slope/flash_Ave*100) %>%
    select(-flash_Ave)
  
  save(data_AK_final, file=paste0(year_length,"yearssiteslocation.RData"))
}



# Plots
ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va, color = percent_change_mean, shape=MK_meanyearsig, size=MK_meanyearsig)) +
  labs(title="Percent Change in Annual Mean Discharge", color=" ") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_colour_gradient2(name = "Percent\nChange") +
  scale_shape_manual(name = "", values=c(15, 16)) +
  scale_size_manual(guide=FALSE, values=c(2,4)) +
  guides(colour = guide_colorbar(order=1), shape = guide_legend(order=2), size = "none")

ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va, color = percent_change_Q50, shape=MK_Q50yearsig, size=MK_Q50yearsig)) +
  labs(title="Percent Change in Annual Median Discharge", color=" ") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_colour_gradient2(name = "Percent\nChange") + 
  scale_shape_manual(name ="", values=c(15, 16)) +
  scale_size_manual(guide=FALSE, values=c(2,4)) +
  guides(colour = guide_colorbar(order=1), shape = guide_legend(order=2), size = "none")


ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va, color = percent_change_PF, shape=MK_PF_sig, size=MK_PF_sig)) +
  labs(title="Percent Change in Annual Maximum Discharge", color=" ") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_colour_gradient2(name = "Percent\nChange") + 
  scale_shape_manual(name ="", values=c(15, 16)) +
  scale_size_manual(guide=FALSE, values=c(2,4)) +
  guides(colour = guide_colorbar(order=1), shape = guide_legend(order=2), size = "none")

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
  scale_colour_gradient2(name = "Relative\n% Change") + 
  scale_shape_manual(name ="", values=c(15, 16)) +
  scale_size_manual(guide=FALSE, values=c(2,4)) +
  guides(colour = guide_colorbar(order=1), shape = guide_legend(order=2), size = "none")

#Centroid of Discharge
per_flow = 0.5

discharge_percentile <- final %>%
  group_by(site_no, Year) %>%
  mutate(Yearly_flow = per_flow*sum(X_00060_00003), cum_sum = cumsum(X_00060_00003)) %>%
  mutate(diff = Yearly_flow - cum_sum) %>%
  slice_min(order_by = abs(diff)) %>%
  mutate(jday = (as.integer(difftime(make_date(Year, Month, Day), make_date(Year, 01, 01), units = "days")))) %>%
  mutate(Year=as.numeric(Year))

#Centroid statistical analysis date
cent_date_stat <- discharge_percentile %>%
  group_by(site_no) %>% 
  do(MKcentdatetest=MannKendall(.$jday)) %>%
  mutate(MK_cent_date_p=MKcentdatetest$sl, MK_cent_date_sig=ifelse(MK_cent_date_p<=p_val,"Significant","Not Significant"))

cent_date_stat2 <- discharge_percentile %>%
  group_by(site_no) %>%
  drop_na() %>%
  do(cent_date_slope=mblm(jday ~ Year, .)) %>%
  mutate(cent_date_Slope=cent_date_slope$coefficients[2])

data_AK_final <- full_join(data_AK_final, cent_date_stat) %>%
  select(-MKcentdatetest)

data_AK_final <- full_join(data_AK_final, cent_date_stat2) %>%
  select(-cent_date_slope)

# Recession
Fast_recess = 3

Recession <- final %>%
  filter(as.numeric(Month)==8) %>%
  group_by(site_no) %>%
  mutate(date = make_date(Year, Month, Day)) %>%
  mutate(dq = X_00060_00003-lag(X_00060_00003)) %>%
  mutate(Year=as.numeric(Year)) %>%
  ungroup() %>%
  group_by(ID = data.table::rleid(dq < 0)) %>%
  mutate(Consec_Days = if_else(dq < 0, row_number(), 0L)) %>%
  filter(Consec_Days>Fast_recess) %>%
  mutate(Rslope = -dq/X_00060_00003) %>%
  ungroup() %>%
  #mutate(bin=cut(Year, breaks=c(1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2020), 
                 #labels = c(1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2019), right=FALSE)) %>%
  group_by(site_no, Year) %>%
  summarise(Annual_Rslope=median(Rslope))
  
#Recession statistical analysis
recess_stat <- Recession %>%
  group_by(site_no) %>% 
  do(MKrecesstest=MannKendall(.$Annual_Rslope)) %>%
  mutate(MK_recess_p=MKrecesstest$sl, MK_recess_sig=ifelse(MK_recess_p<=p_val,"Significant","Not Significant"))

recess_stat2 <- Recession %>%
  group_by(site_no) %>%
  drop_na() %>%
  do(recess_slope=mblm(Annual_Rslope ~ Year, .)) %>%
  mutate(recess_Slope=recess_slope$coefficients[2])

data_AK_final <- full_join(data_AK_final, recess_stat) %>%
  select(-MKrecesstest)

data_AK_final <- full_join(data_AK_final, recess_stat2) %>%
  select(-recess_slope)

recess_stat3 <- Recession %>%
  group_by(site_no) %>%
  drop_na() %>%
  summarise(recess_Ave=mean(Annual_Rslope))

data_AK_final <- inner_join(data_AK_final, recess_stat3) %>%
  mutate(change_recess = recess_Slope/recess_Ave*100) %>%
  select(-recess_Ave)

#Plot locations
ak <- map_data('worldHires','USA:Alaska')
ak <- subset(ak, long<0) #drop the end of the Aleutian Islands 

ggplot() + 
  geom_polygon(data=ak, aes(long, lat, group=group), fill="white", color="black") +
  geom_point(data=data_AK_final, aes(x=dec_long_va, y=dec_lat_va, color = change_recess, shape=MK_recess_sig, size=MK_recess_sig)) +
  labs(title="Change in Recession Slope of August Streamflow", color=" ") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_colour_gradient2(name = "Change") + 
  scale_shape_manual(name ="", values=c(15, 16)) +
  scale_size_manual(guide=FALSE, values=c(2,4)) +
  guides(colour = guide_colorbar(order=1), shape = guide_legend(order=2), size = "none")


