rm(list = ls())

library(tidyverse, lubridate)

modeled <- read.csv("/Users/dybl4375/USGS_Stream_Data_Pull/daily_obsnsim_future.csv", header = TRUE) 
historic <- read.csv("/Users/dybl4375/USGS_Stream_Data_Pull/daily_obsnsim_historic.csv", header = TRUE) 
dates <- as.data.frame(seq(as.Date("2002/09/01"), as.Date("2016/08/31"), "days"))

df <- as.data.frame(cbind(historic[,1], modeled[,1], dates))
names(df) <- c("historic", "modeled", "Date")
df_year <- mutate(df, year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date)) %>%
  filter(year>=2003)

##BOOTSTRAPPING
#generate nsim bootstrap samples
x <- df_year$historic
y <- df_year$modeled

N = length(x)

#generate nsim bootstrap samples
nsim = 1000

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
  labs(fill="", y = "Discharge (cfs)", x = "")


# Plot daily data
plot(df_year$Date, df_year$modeled, type="l", col="darkred", main="Climate Induced Changes", xlab="Year", ylab="Daily Discharge (cfs)")
lines(df_year$Date, df_year$historic, col="steelblue")
legend("topleft", legend = c("End of Century", "Current"), col=c("darkred", "steelblue"), lty = 1, cex=0.8)

# Monthly Data
Monthly <- df_year %>%
  group_by(month) %>%
  summarise(Hist_Month_Ave = ave(historic), Future_Month_Ave = ave(modeled)) %>%
  slice(1)

plot(Monthly$month, Monthly$Future_Month_Ave, type="l", col="darkred", 
     main="Climate Induced Changes in Monthly Hydrograph", xlab="Month", 
     ylab="Monthly Discharge (cfs)", ylim = c(0,60000))
lines(Monthly$month, Monthly$Hist_Month_Ave, col="steelblue")
legend("topleft", legend = c("End of Century", "Current"), col=c("darkred", "steelblue"), lty = 1, cex=0.8)

# Daily
daily <- df_year %>%
  mutate(jdate = lubridate::yday(Date)) %>%
  group_by(jdate) %>%
  summarise(Hist_Month_Ave = ave(historic), Future_Month_Ave = ave(modeled)) %>%
  slice(1)

plot(daily$jdate, daily$Future_Month_Ave, type="l", col="darkred", 
     main="Climate Induced Changes in Hydrograph", xlab="Julian Day", 
     ylab="Daily Discharge (cfs)", ylim = c(0,70000))
lines(daily$jdate, daily$Hist_Month_Ave, col="steelblue")
legend("topleft", legend = c("End of Century", "Current"), col=c("darkred", "steelblue"), lty = 1, cex=0.8)

