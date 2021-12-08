rm(list = ls())

library(pacman)
p_load(tidyverse, sf)

permafrost <- st_read("/Users/dybl4375/USGS_Stream_Data_Pull/permafrost.kml")

