#R Script to manipulate netCDF climate files for stream analysis
library(pacman)
pacman::p_load(ncdf4, tidyverse, tidync, RNetCDF, ncmeta, tidyr, Kendall, mblm, multiApply)

#Files to be selected
nc_data <- nc_open("alaska_anomaly_ens.nc")

pcp.array <- ncvar_get(nc_data, "pcp")
t_mean.array <- ncvar_get(nc_data, "t_mean")
#t_range.array <- ncvar_get(nc_data, "t_range")
t <- ncvar_get(nc_data, "time")

nc_data$dim$lon$vals -> lon
nc_data$dim$lat$vals -> lat

plot (pcp.array, t_mean.array)
nc_close(nc_data)

