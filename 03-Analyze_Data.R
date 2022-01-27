# Site list
site_list = read.csv("./Data/site_list.csv", header = TRUE)

#Set path to data
df_path_raw = "./Data/finalsites.RData"

# Climate Index Trends
climate_index_correlation(df_path_raw, 60)

# Analyze Streamflow for final sites
MKtrend_Discharge(df_path_raw)

# Analyze Climate Record at gage location
MKtrend_Climate("/Users/dybl4375/USGS_Stream_Data_Pull/Data/raw/climate_data_final")

# Set path to analyzed data
df_discharge_month = "./Data/FinalDischargeTemporal_Analyzed_Data.RData"

# Bar plot trends
Barplot_Trends(df_discharge_month, "Mean Discharge")

# Plot Change in Hydrograph
HGChange(df_path_raw)
HC_Change_annual(df_path_raw)

# Plot Climate Annom
climate_annomoly_graph_total(df_path_raw)

# Plot Climate Change Graphs
#climate_change_graphs(12,2, "Average Winter Air Temperature in Alaska", "Average Winter Days Above Freezing")



