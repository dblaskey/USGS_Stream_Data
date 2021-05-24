# Trends through time
rm(list = ls())

library(pacman)
p_load("tidyverse", "mblm")

# Set up moving criteria
clim_record=30
N=60-clim_record

# Load Data
load("/Users/dybl4375/USGS_Stream_Data_Pull/60yearsfinal.Rdata") # Read site data
load("/Users/dybl4375/USGS_Stream_Data_Pull/60yearsites.Rdata") # Read location data

# Reduce to winter flows
#final = filter(final, as.numeric(Month)>=11 | as.numeric(Month)<=3)

# Historical data to normalize results
p10_hist = final %>%
  group_by(site_no) %>%
  summarise(Q10_Discharge = quantile(X_00060_00003, 0.1))

p50_hist = final %>%
  group_by(site_no) %>%
  summarise(Q50_Discharge = quantile(X_00060_00003, 0.5))

p90_hist = final %>%
  group_by(site_no) %>%
  summarise(Q90_Discharge = quantile(X_00060_00003, 0.9))


# Create function for precentile analysis
percentil_bin=function(x, tile){
  start_year=1958+x
  final=filter(final, wt_year>=start_year & wt_year<start_year+N)

final %>%
  group_by(site_no, wt_year) %>%
  summarise(Annual_Q_Discharge = quantile(X_00060_00003, tile)) %>%
  drop_na() %>%
  ungroup(wt_year) %>% 
  mutate(Year=as.numeric(wt_year)) %>%
  do(TS_Q=mblm(Annual_Q_Discharge ~ Year, .)) %>%
  mutate(tile_Slope=TS_Q$coefficients[2]) %>%
  select(-TS_Q) %>%
  mutate(Start_Year=start_year)
}

# Execute functions
p10results = lapply(1:N, percentil_bin, tile=0.01) %>% 
  bind_rows()

p10final = left_join(p10results, p10_hist) %>%
  mutate(normQ = tile_Slope/Q10_Discharge, test="10th Percentile")%>%
  select(-Q10_Discharge)

p50results = lapply(1:N, percentil_bin, tile=0.05) %>% 
  bind_rows()

p50final = left_join(p50results, p50_hist) %>%
  mutate(normQ = tile_Slope/Q50_Discharge, test="50th Percentile") %>%
  select(-Q50_Discharge)

p90results = lapply(1:N, percentil_bin, tile=0.09) %>% 
  bind_rows()

p90final = left_join(p90results, p90_hist) %>%
  mutate(normQ = tile_Slope/Q90_Discharge, test="90th Percentile") %>%
  select(-Q90_Discharge)

# Create Line Plots
ggplot(p10final, aes(Start_Year, normQ)) +
  geom_smooth(aes(colour=site_no), se=FALSE) +
  labs(title="Normalized Trend in 10th Percentile Annual Discharge", color="USGS Site") +
  xlab("Analysis Start Year") +
  ylab('Normalized Trend') +
  theme(plot.title = element_text(hjust = 0.5))
  
ggplot(p50final, aes(Start_Year, normQ)) +
  geom_smooth(aes(colour=site_no), se=FALSE)+
  labs(title="Normalized Trend in 50th Percentile Annual Discharge", color="USGS Site") +
  xlab("Analysis Start Year") +
  ylab('Normalized Trend') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(p90final, aes(Start_Year, normQ)) +
  geom_smooth(aes(colour=site_no), se=FALSE)+
  labs(title="Normalized Trend in 90th Percentile Annual Discharge", color="USGS Site") +
  xlab("Analysis Start Year") +
  ylab('Normalized Trend') +
  theme(plot.title = element_text(hjust = 0.5))

# Create Box Plots
df <- rbind(p90final, p50final, p10final)

ggplot(df, aes(x=test, y=normQ, fill=site_no)) + 
  geom_boxplot(outlier.color = "lightgrey") +
  facet_wrap(~test, scale="free") +
  labs(fill="USGS Site", y = "Normalized Change in Discharge", x = "")
