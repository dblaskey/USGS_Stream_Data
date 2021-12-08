#Plot Missing Data
end_date <- as.Date(paste(end_year,"09-30", sep="-"))
start_date <- as.Date(paste(end_year - year_length,"10-01", sep="-"))

#Constrain to the desired range of years
temp <- df %>%
  filter(Date>=start_date & Date<=end_date)

#Create table of observations per year
obsd <- temp %>%
  group_by(site_no) %>%
  count(wt_year, name = "Yearly_obs") %>%
  mutate(Ypass = ifelse(Yearly_obs>day_crit,1,NA))

ggplot(obsd, aes(wt_year, site_no, fill = is.na(Ypass))) +
  geom_tile() +
  ggtitle("Alaskan Stream Gauge Data Availability") +
  xlab("Year") +
  ylab("Sites") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill=NA, size=1), 
        panel.background = element_rect(fill = "white"), 
        axis.text.y=element_blank(), legend.position = "none") +
  scale_fill_manual(name="Data Available?", values= c("Black", "White"),) 