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


