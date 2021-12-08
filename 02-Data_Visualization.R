#Plot available data
Plot_Data_Availability(sites, 2019, 0.7)

# Decadal plots
ID_River_Plots("./Data/finalyearsfinalsites.RData", 1960, 2020, "Decadal Hydrograph Change")

# Determine trend length needed to average out climate variability
start_years = seq(1955, 1990)
N = length(start_years)
end_years = rep(1984:2019, each = N)
start_years = rep(1955:1990, times = N)

load("./Data/finalyearsfinalsites.RData") # Read site data

final_sites = filter(final_sites, wt_year>=1955 & site_no!=15896000)

df_percentile = map2_df(start_years, end_years, TrendSlope_Discharge, tile=0.1)

rm(final_sites)

# Plot results above
z_score_trend_plot(df_percentile, "10th")
z_score_average_plot(df_percentile, "10th")

# Summarize data
Summerize_USGS_Data("./Data/finalyearsfinalsites.RData", "site_name", "Date", "X_00060_00003")


