rm(list = ls())

library(pacman)
pacman::p_load("tidyverse", "lubridate")

climate <- read.csv("/Users/dybl4375/USGS_Stream_Data_Pull/wrf_climate_data_hist_total.csv", header = TRUE) 
discharge <- read.csv("/Users/dybl4375/USGS_Stream_Data_Pull/daily_obsnsim_historic.csv", header = TRUE) 

climate_df <- cbind(discharge, climate) %>%
  mutate(Year = lubridate::year(Date), Month = lubridate::month(Date), Day = lubridate::day(Date)) %>%
  mutate(wt_year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year))) %>%
  mutate(precip_tot = Precip - lag(Precip)) %>%
  na.omit() 

future_climate <- read.csv("/Users/dybl4375/USGS_Stream_Data_Pull/wrf_climate_data_future_total.csv", header = TRUE) 
future_discharge <- read.csv("/Users/dybl4375/USGS_Stream_Data_Pull/daily_obsnsim_future.csv", header = TRUE) 

future_climate_df <- cbind(future_discharge, future_climate) %>%
  mutate(f_precip_tot = Precip - lag(Precip)) %>%
  na.omit() 

names(future_climate_df) <- c("f_sim","V0","V1", "V2","f_Temp","f_Precip","f_Precip_frac","f_Sub_ROFF", "f_Sur_ROFF","V3","Date","f_precip_tot")

df <- inner_join(future_climate_df, climate_df) %>%
  mutate(f_run_frac = f_Sur_ROFF/f_Sub_ROFF, run_frac = SF_ROFF/Sub_ROFF)

df_monthly <- df %>%
  group_by(Month) %>%
  #summarise(f_run_frac_tot = mean(f_run_frac), run_frac_tot = mean(run_frac))
  summarise(frac_f = mean(f_Precip_frac), frac=mean(Precip_frac))

plot(df_monthly$Month, df_monthly$frac_f, type="l", col="darkred", ylim=c(0,1), xlab="Month", ylab="Fraction of Frozen Precip")
lines(df_monthly$Month, df_monthly$frac, col="steelblue")
legend("topright", c("End of Century", "Current"), col=c("darkred","steelblue"), lty=1, cex=0.8) 

ROS <- df %>%
  mutate(f_ROS=ifelse(f_Precip_frac<0.5,1,0), ROS=ifelse(Precip_frac<0.5,1,0)) %>%
  #filter(Month>10 | Month < 5) %>%
  group_by(Month) %>%
  summarise(f_ROS = sum(f_ROS)/14, ROS = sum(ROS)/14)

plot(ROS$Month, ROS$f_ROS, type="l", col="darkred", xlab="Month", ylab="ROS Days", ylim = c(0,35))
lines(ROS$Month, ROS$ROS, col="steelblue")
legend("topright", c("End of Century", "Current"), col=c("darkred","steelblue"), lty=1, cex=0.8) 

Rain <- df %>%
  mutate(f_rain=ifelse(f_Precip_frac>1/3 & f_Precip_frac<0.5,1,0), rain=ifelse(Precip_frac>1/3 & f_Precip_frac<0.5,1,0)) %>%
  filter(Month>10 | Month < 5) %>%
  filter(wt_year>2002 & wt_year<2016) %>%
  group_by(wt_year) %>%
  summarise(f_rain = sum(f_rain), rain = sum(rain))

plot(Rain$wt_year, Rain$f_rain, type="l", col="darkred", xlab="Month", ylab="ROS Events")
lines(Rain$wt_year, Rain$rain, col="steelblue")
legend("topright", c("End of Century", "Current"), col=c("darkred","steelblue"), lty=1, cex=0.8) 

Precip <- df %>%
  filter(wt_year>2002 & wt_year<2016 & X>0) %>%
  group_by(Month) %>%
  summarise(f_precip = sum(f_precip_tot)/13, precip = sum(precip_tot)/13)

plot(Precip$Month, Precip$f_precip, type="l", col="darkred", xlab="Month", ylab="Average Monthly Precipitation (mm)", ylim = c(0,100))
lines(Precip$Month, Precip$precip, col="steelblue")
legend("topright", c("End of Century", "Current"), col=c("darkred","steelblue"), lty=1, cex=0.8) 

Temp <- df %>%
  filter(wt_year>2002 & wt_year<2016 & X>0) %>%
  group_by(Month) %>%
  summarise(f_temp = mean(f_Temp), temp = mean(Temperature))

plot(Temp$Month, Temp$f_temp, type="l", col="darkred", xlab="Month", ylab="Temperature (K)", ylim = c(250, 300))
lines(Temp$Month, Temp$temp, col="steelblue")
legend("topright", c("End of Century", "Current"), col=c("darkred","steelblue"), lty=1, cex=0.8) 

Temp <- df %>%
  filter(wt_year>2002 & wt_year<2016 & X>0) %>%
  mutate(warm_days = ifelse(Temperature>273, 1, 0), f_warm_days = ifelse(f_Temp>273, 1, 0)) %>%
  group_by(Month) %>%
  summarise(f_temp = sum(f_warm_days)/13, temp = sum(warm_days)/13)

plot(Temp$Month, Temp$f_temp, type="l", col="darkred", xlab="Month", ylab="Days Above Freezing")
lines(Temp$Month, Temp$temp, col="steelblue")
legend("topright", c("End of Century", "Current"), col=c("darkred","steelblue"), lty=1, cex=0.8) 

# Correlation Graphs
df_cor <- df %>%
  filter(wt_year>2002 & wt_year < 2016) %>%
  filter(Month > 11 | Month < 3) %>%
  group_by(wt_year) %>%
  summarise(precip = sum(precip_tot), temp = mean(Temperature), f_precip = sum(f_precip_tot), f_temp = mean(f_Temp), dis = mean(sim), f_dis = mean(future_sim) )

plot(df_cor$f_precip, df_cor$f_dis, main = "Winter Discharge vs Precipitation",
     ylim = c(0.99*min(df_cor$dis), 1.1*max(df_cor$f_dis)), xlim = c(0.99*min(df_cor$precip), 1.01*max(df_cor$f_precip)), 
     xlab="Precipitation (mm)", ylab="Average Discharge (cfs)", pch = 19, col = "darkred")
points(df_cor$precip, df_cor$dis, pch = 19, col = "steelblue")
legend("topleft", c("End of Century", "Current"), col=c("darkred","steelblue"), pch = 19) 

plot(df_cor$f_temp, df_cor$f_dis, main = "Winter Discharge vs Temperature",
     ylim = c(0.99*min(df_cor$dis), 1.1*max(df_cor$f_dis)), xlim = c(0.99*min(df_cor$temp), 1.01*max(df_cor$f_temp)), 
     xlab="Temperature (K)", ylab="Average Discharge (cfs)", pch = 19, col = "darkred")
points(df_cor$temp, df_cor$dis, pch = 19, col = "steelblue")
legend("topleft", c("End of Century", "Current"), col=c("darkred","steelblue"), pch = 19) 

# Future Climate
future_climate_df <- future_climate_df %>%
  filter(wt_year>2002 & wt_year<2016) %>%
  filter(Month>5 & Month<9) %>%  
  group_by(wt_year) %>%
  summarise(precip_tot = sum(precip_tot))
  
climate_df <- climate_df %>%
  filter(wt_year>2002 & wt_year<2016) %>%
  group_by(wt_year) %>%
  summarise(precip_tot = sum(precip_tot))

##BOOTSTRAPPING
#generate nsim bootstrap samples
x <- climate_df$precip_tot
y <- future_climate_df$precip_tot

N = length(x)

#generate nsim bootstrap samples
nsim = 10000

#statistics desired..
hmeansim = 1:nsim
hvarsim = 1:nsim
hmedsim = 1:nsim
hlowsim = 1:nsim
hhighsim = 1:nsim

for(i in 1:nsim){
  xsamp = sample(x, N, replace=TRUE)
  hmeansim[i]=mean(xsamp)
  hvarsim[i]=var(xsamp)
  hmedsim[i]=quantile(xsamp, 0.5)
  hlowsim[i]=quantile(xsamp, 0.1)
  hhighsim[i]=quantile(xsamp, 0.9)
}

# Confidence interval on the population mean

#statistics desired..
ecmeansim = 1:nsim
ecvarsim = 1:nsim
ecmedsim = 1:nsim
eclowsim = 1:nsim
echighsim = 1:nsim

for(i in 1:nsim){
  xsamp = sample(y, N, replace=TRUE)
  ecmeansim[i]=mean(xsamp)
  ecvarsim[i]=var(xsamp)
  ecmedsim[i]=quantile(xsamp, 0.5)
  eclowsim[i]=quantile(xsamp, 0.1)
  echighsim[i]=quantile(xsamp, 0.9)
}

echighsim_df <- as.data.frame(echighsim) %>%
  mutate(test = "90th Percentile", data="End of Century") %>%
  rename(Results = echighsim)
eclowsim_df <- as.data.frame(eclowsim) %>%
  mutate(test = "10th Percentile", data="End of Century") %>%
  rename(Results = eclowsim)
ecmedsim_df <- as.data.frame(ecmedsim) %>%
  mutate(test = "50th Percentile", data="End of Century") %>%
  rename(Results = ecmedsim)
ecmeansim_df <- as.data.frame(ecmeansim) %>%
  mutate(test = "Mean", data="End of Century") %>%
  rename(Results = ecmeansim)

hhighsim_df <- as.data.frame(hhighsim) %>%
  mutate(test = "90th Percentile", data="Current") %>%
  rename(Results = hhighsim)
hlowsim_df <- as.data.frame(hlowsim) %>%
  mutate(test = "10th Percentile", data="Current")%>%
  rename(Results = hlowsim)
hmedsim_df <- as.data.frame(hmedsim) %>%
  mutate(test = "50th Percentile", data="Current")%>%
  rename(Results = hmedsim)
hmeansim_df <- as.data.frame(hmeansim) %>%
  mutate(test = "Mean", data="Current") %>%
  rename(Results = hmeansim)

df <- rbind(hhighsim_df, hlowsim_df, hmedsim_df, hmeansim_df, echighsim_df,
            eclowsim_df, ecmedsim_df, ecmeansim_df)

# Box Plots
ggplot(df, aes(x=test, y=Results, fill=data)) + 
  geom_boxplot(outlier.color = "lightgrey") +
  facet_wrap(~test, scale="free") +
  labs(fill="", y = "Total Precip (mm)", x = "")





# Test correlation
climate_df_test <- climate_df %>%
  filter(Month < 5 | Month > 10) %>%
  group_by(wt_year) %>%
  summarise(Discharge = sum(sim), precip = sum(precip_tot), temp = mean(Temperature))

cold_p_cor <- cor(climate_df_test$precip, climate_df_test$Discharge, method = "kendall")
cold_temp_cor <- cor(climate_df_test$temp, climate_df_test$Discharge, method = "kendall")

climate_df_test <- climate_df %>%
  filter(Month > 4 & Month < 11) %>%
  group_by(wt_year) %>%
  summarise(Discharge = sum(sim), precip = sum(precip_tot), temp = mean(Temperature))

warm_p_cor <- cor(climate_df_test$precip, climate_df_test$Discharge, method = "kendall")
warm_temp_cor <- cor(climate_df_test$temp, climate_df_test$Discharge, method = "kendall")

climate_df_test <- climate_df %>%
  filter(Month > 2 & Month < 6) %>%
  group_by(wt_year) %>%
  summarise(Discharge = sum(sim), precip = sum(precip_tot), temp = mean(Temperature))

sp_p_cor <- cor(climate_df_test$precip, climate_df_test$Discharge, method = "kendall")
sp_temp_cor <- cor(climate_df_test$temp, climate_df_test$Discharge, method = "kendall")

climate_df_test <- climate_df %>%
  filter(Month > 5 & Month < 9) %>%
  group_by(wt_year) %>%
  summarise(Discharge = sum(sim), precip = sum(precip_tot), temp = mean(Temperature))

su_p_cor <- cor(climate_df_test$precip, climate_df_test$Discharge, method = "kendall")
su_temp_cor <- cor(climate_df_test$temp, climate_df_test$Discharge, method = "kendall")

climate_df_test <- climate_df %>%
  filter(Month > 8 & Month < 12) %>%
  group_by(wt_year) %>%
  summarise(Discharge = sum(sim), precip = sum(precip_tot), temp = mean(Temperature))

fa_p_cor <- cor(climate_df_test$precip, climate_df_test$Discharge, method = "kendall")
fa_temp_cor <- cor(climate_df_test$temp, climate_df_test$Discharge, method = "kendall")

climate_df_test <- climate_df %>%
  filter(Month > 11 | Month < 3) %>%
  group_by(wt_year) %>%
  summarise(Discharge = sum(sim), precip = sum(precip_tot), temp = mean(Temperature))

wi_p_cor <- cor(climate_df_test$precip, climate_df_test$Discharge, method = "kendall")
wi_temp_cor <- cor(climate_df_test$temp, climate_df_test$Discharge, method = "kendall")

climate_df_test <- climate_df %>%
  group_by(wt_year) %>%
  summarise(Discharge = sum(sim), precip = sum(precip_tot), temp = mean(Temperature))

an_p_cor <- cor(climate_df_test$precip, climate_df_test$Discharge, method = "kendall")
an_temp_cor <- cor(climate_df_test$temp, climate_df_test$Discharge, method = "kendall")

# Mismatched
# Cold Precip, spring runoff
climate_df_temp <- climate_df %>%
  filter(Month > 10 | Month < 5) %>%
  group_by(wt_year) %>%
  summarise(precip = sum(precip_tot), temp = mean(Temperature))

dis_df <- climate_df %>%
  filter(Month > 3 & Month < 7) %>%
  group_by(wt_year) %>%
  summarise(Discharge = sum(sim))

climate_df_temp <- inner_join(climate_df_temp, dis_df)
wcspd_p_cor <- cor(climate_df_test$precip, climate_df_test$Discharge, method = "kendall")
wcspd_temp_cor <- cor(climate_df_test$temp, climate_df_test$Discharge, method = "kendall")
