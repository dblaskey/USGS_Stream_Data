#Plot available data
Plot_Data_Availability(sites, 2021, 0.7)

# Decadal plots
ID_River_Plots("./Data/finalsites.RData", 1960, 2020, "Decadal Hydrograph Change")

# Summarize data
Summerize_USGS_Data("./Data/finalsites.RData", "site_name", "Date", "X_00060_00003")

# Trend Stability Analysis
Trend_Stability_plot("./Data/finalsites.RData")

