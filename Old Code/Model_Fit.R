# Model Fit

# Load Libraries 
library(pacman)
p_load("tidyverse", "hydroGOF")

# Load Data
discharge <- read_csv('./USGS_Stream_Data_Pull/daily_obsnsim_historic.csv')
#discharge <- df[order(df$Year),]
sim = discharge$sim
obs = discharge$obs

# NSE
NSE(sim,obs)

# PBIAS
pbias(sim,obs)

# NRMSE
nrmse(sim,obs)

# Pearson
rPearson(sim, obs)
