library(pacman)
pacman::p_load(dataRetrieval, dplyr, tidyverse, leaflet)

Qak <- readNWISdata(stateCd="AK", parameterCd="00060", service="dv")
sites <- select(Qak, site_no)
for(i in 1:507){
  Station <- readNWISdv(sites[i,],
                   parameterCd = '00060')
  write.csv(Station, paste0(i,".csv"))
}

data1 <- read.csv("./Data/90.csv", header=TRUE)%>%
  mutate(Date=as.POSIXct(Date,tz="Alaska time"))
first(data1$Date)
last(data1$Date)