# Trends through time
rm(list = ls())

library(pacman)
p_load("tidyverse", "mblm")

# Load Data
load("/Users/dybl4375/USGS_Stream_Data_Pull/Data/60yearsfinalsites.RData") # Read site data
load("/Users/dybl4375/USGS_Stream_Data_Pull/60yearsites.Rdata") # Read location data

# Reduce to winter flows
#final = filter(final, as.numeric(Month)>=11 | as.numeric(Month)<=3)

# Create function for precentile analysis
TrendSlope_Discharge=function(x, tile){
  start_year=1959+x
  final=filter(final, wt_year>=start_year & wt_year<start_year+clim_record)
  
  final %>%
    group_by(site_no, wt_year) %>%
    summarise(Annual_Q_Discharge = quantile(X_00060_00003, tile)) %>%
    drop_na() %>%
    ungroup(wt_year) %>% 
    mutate(Year=as.numeric(wt_year)) %>%
    do(TS_Q=mblm(Annual_Q_Discharge ~ Year, .), MKtest=MannKendall(.$Annual_Q_Discharge)) %>%
    mutate(tile_Slope=TS_Q$coefficients[2], tile_p=MKtest$sl) %>%
    select(-TS_Q, -MKtest) %>%
    mutate(Start_Year=start_year, End_Year = start_year+clim_record )
}

# Set p_val for Mann Kendall test
p_val = 0.1

# Set up moving criteria
clim_record=50
N=60-clim_record

# Execute functions
ts1 = lapply(1:N, TrendSlope_Discharge, tile=0.1) %>% 
  bind_rows() %>%
  mutate(Results = ifelse(tile_p<=p_val & tile_Slope>=0, 4, ifelse(tile_p<=p_val & tile_Slope<0, 1, ifelse(tile_p>p_val & tile_Slope<0, 2, 3)))) %>%
  group_by(site_no) %>%
  count(Results) %>%
  mutate(Record_length = paste0(clim_record, " years of data"))

# Set up moving criteria
clim_record=40
N=60-clim_record

# Execute functions
ts2 = lapply(1:N, TrendSlope_Discharge, tile=0.1) %>% 
  bind_rows() %>%
  mutate(Results = ifelse(tile_p<=p_val & tile_Slope>=0, 4, ifelse(tile_p<=p_val & tile_Slope<0, 1, ifelse(tile_p>p_val & tile_Slope<0, 2, 3)))) %>%
  group_by(site_no) %>%
  count(Results) %>%
  mutate(Record_length = paste0(clim_record, " years of data"))

# Set up moving criteria
clim_record=30
N=60-clim_record

# Execute functions
ts3 = lapply(1:N, TrendSlope_Discharge, tile=0.1) %>% 
  bind_rows() %>%
  mutate(Results = ifelse(tile_p<=p_val & tile_Slope>=0, 4, ifelse(tile_p<=p_val & tile_Slope<0, 1, ifelse(tile_p>p_val & tile_Slope<0, 2, 3)))) %>%
  group_by(site_no) %>%
  count(Results) %>%
  mutate(Record_length = paste0(clim_record, " years of data"))

test = rbind(ts1,ts2,ts3)

# Create Line Plots
ggplot(ts1, aes(site_no, Results)) +
  geom_point(aes(colour=Start_Year), position = position_jitter(height = 0.2)) +
  scale_y_continuous(breaks = 1:4, labels = c("Significant Negative Trend", "Not Significant Negative Trend", "Not Significant Positive Trend", "Significant Positive Trend")) +
  labs(title="Trend in 10th Percentile Annual Discharge\nOver Various Timeframes", color="USGS Site") +
  xlab("USGS Stream Gage") +
  ylab('Trend') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(test, aes(x=site_no, fill=as.factor(Results), y=n)) +
  geom_bar(stat = 'identity', position = 'fill') +
  facet_grid(.~Record_length) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = "white")) +
  ylab("Fraction of Years with Trend") + 
  ggtitle("Variability in Median Discharge Trend by Data Amount") +
  scale_fill_brewer(name = "", palette = "RdBu", label = c("Significant Negative Trend", "Not Significant Negative Trend", "Not Significant Positive Trend", "Significant Positive Trend"))





# Create function for percentile analysis
TrendSlope_Discharge2=function(start_year, end_year, tile){
  
  if (end_year - start_year >= 29){
    final_sites %>%
      filter(wt_year>=start_year & wt_year<= end_year)%>%
      group_by(site_no, wt_year) %>%
      summarise(Annual_Q_Discharge = quantile(X_00060_00003, tile)) %>%
      ungroup(wt_year) %>%
      do(test_mk = pwmk(.$Annual_Q_Discharge)) %>%
      mutate(Z_Score = test_mk[1], Sens_Slope= test_mk[2], P_Value = test_mk[4],
             Start_Year = start_year, End_Year = end_year, test = paste0(tile*100,"th Percentile"))%>%
      select(-test_mk)
  }
}

start_years = seq(1960, 1990)
N = length(start_years)
end_years = rep(1989:2019, each = N)
start_years = rep(1960:1990, times = N)

df = map2_df(start_years, end_years, TrendSlope_Discharge2, tile=0.5)

testing = filter(df, site_no == 15276000)

ggplot(df, aes(Start_Year, End_Year)) +
  facet_wrap(vars(site_no), scales = "free", nrow = 5) +
  geom_raster(aes(fill = Z_Score)) +
  xlab("Start Year") +
  ylab("End Year") +
  coord_cartesian(xlim = c(1960, 1990), ylim = c(1989, 2020), expand = FALSE) +
  theme_bw() +
  theme(strip.text = element_text(face="bold", size=9),
        strip.background = element_rect(fill="white", colour="black"),
        strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"))) +
  scale_fill_continuous_divergingx(name = "Z Score", palette = 'RdBu', mid = 0)





# Set p_val for Mann Kendall test
p_val = 0.1
clim_record=30

# Execute functions
ts4 = outer(start_years, end_years, TrendSlope_Discharge2) %>% 
  bind_rows() %>%
  mutate(Results = ifelse(tile_p<=p_val & tile_Slope>=0, 4, ifelse(tile_p<=p_val & tile_Slope<0, 1, ifelse(tile_p>p_val & tile_Slope<0, 2, 3)))) %>%
  group_by(site_no) %>%
  count(Results) %>%
  mutate(Record_length = paste0(clim_record, " years of data"))

# Set up moving criteria
clim_record=40

# Execute functions
ts5 = lapply(1:10, TrendSlope_Discharge2, tile=0.5) %>% 
  bind_rows() %>%
  mutate(Results = ifelse(tile_p<=p_val & tile_Slope>=0, 4, ifelse(tile_p<=p_val & tile_Slope<0, 1, ifelse(tile_p>p_val & tile_Slope<0, 2, 3)))) %>%
  group_by(site_no) %>%
  count(Results) %>%
  mutate(Record_length = paste0(clim_record, " years of data"))

# Set up moving criteria
clim_record=50

# Execute functions
ts6 = lapply(1:10, TrendSlope_Discharge2, tile=0.5) %>% 
  bind_rows() %>%
  mutate(Results = ifelse(tile_p<=p_val & tile_Slope>=0, 4, ifelse(tile_p<=p_val & tile_Slope<0, 1, ifelse(tile_p>p_val & tile_Slope<0, 2, 3)))) %>%
  group_by(site_no) %>%
  count(Results) %>%
  mutate(Record_length = paste0(clim_record, " years of data"))

test = rbind(ts4,ts5,ts6)

# Create Line Plots
ggplot(ts1, aes(site_no, Results)) +
  geom_point(aes(colour=Start_Year), position = position_jitter(height = 0.2)) +
  scale_y_continuous(breaks = 1:4, labels = c("Significant Negative Trend", "Not Significant Negative Trend", "Not Significant Positive Trend", "Significant Positive Trend")) +
  labs(title="Trend in 10th Percentile Annual Discharge\nOver Various Timeframes", color="USGS Site") +
  xlab("USGS Stream Gage") +
  ylab('Trend') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(test, aes(x=site_no, fill=as.factor(Results), y=n)) +
  geom_bar(stat = 'identity', position = 'fill') +
  facet_grid(.~Record_length) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = "white")) +
  ylab("Fraction of Years with Trend") + 
  ggtitle("Variability in Median Discharge Trend by Data Amount") +
  scale_fill_brewer(name = "", palette = "RdBu", label = c("Not Significant Negative Trend", "Not Significant Positive Trend", "Significant Positive Trend"))
