#Set path to data
df_path_raw = "./Data/finalyearsfinalsites.RData"

# Climate Index Trends
climate_index_correlation(df_path_raw, 60)

# Analyze Streamflow for final sites
MKtrend_Discharge(df_path_raw)

# Analyze Climate Record at gage location
MKtrend_Climate("/Users/dybl4375/USGS_Stream_Data_Pull/Data/raw/climate_data_final")

# Set path to analyzed data
df_path_analyzed = './Data/FinalDischarge_Analyzed_Data.RData'
df_climateA_path = './Data/Analyzed_ClimateA_Data.RData'
df_climateW_path = './Data/Analyzed_ClimateW_Data.RData'
df_climateApril_path = './Data/Analyzed_ClimateApril_Data.RData'
df_discharge_month = "./Data/FinalDischargeTemporal_Analyzed_Data.RData"

# Bar plot trends
Barplot_Trends(df_path_analyzed, "Discharge")
Barplot_Trends(df_discharge_month, "Mean Discharge")
Barplot_Trends(df_climateA_path, "Annual Climate")
Barplot_Trends(df_climateW_path, "Oct-May Climate")
Barplot_Trends(df_climateApril_path, "April Climate")

# Geospatial Plots
geospatial_trend_plots(df_path_analyzed, "10th Percentile Discharge", 0.05,
                       "Trends in the 10th Percentile of Annual Discharge", "Z Score")

# Plot Climate Annom
climate_annomoly_graph(df_path_raw, 1,3,1,3,"Winter", "Winter Climate", "Alaska")
climate_annomoly_graph(df_path_raw,3,5,3,5,"Spring", "Spring Climate", "Alaska")
climate_annomoly_graph(df_path_raw,6,8,6,8,"Summer", "Summer Climate","Alaska")
climate_annomoly_graph(df_path_raw,9,11,9,11,"Fall", "Fall Climate","Alaska")
climate_annomoly_graph(df_path_raw,11,4,11,4,"Cold Season", "Cold Climate","Alaska")
climate_annomoly_graph(df_path_raw,5,10,5,10,"Warm Season", "Warm Climate","Alaska")
climate_annomoly_graph(df_path_raw,10,9,1,12,"Annual", "Annual Climate","Alaska")
climate_annomoly_graph(df_path_raw,11,6,4,6,"AMJ", "NDJFMAMJ Climate","Alaska")
climate_annomoly_graph(df_path_raw,11,3,4,6,"AMJ", "NDJFM Climate","Alaska")
climate_annomoly_graph(df_path_raw,4,4,4,4,"April", "April Climate","Alaska")
climate_annomoly_graph(df_path_raw,10,4,10,4,"Oct-April", "Climate","Alaska")

# Plot Climate Change Graphs
climate_change_graphs(12,2, "Average Winter Air Temperature in Alaska", "Average Winter Days Above Freezing")



