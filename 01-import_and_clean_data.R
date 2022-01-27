#Determine which stations in Alaska have discharge data available
gages <- readNWISdata(stateCd="AK", parameterCd="00060", service="dv") %>%
  distinct(site_no)

#Using that created list pull all the discharge data from USGS server
sites = vector()
for(i in 1:nrow(gages)){
    temp <- readNWISdv(gages[i,], parameterCd = '00060')
    sites <- rbind(sites,temp)
}

#Remove provisional data and NaNs
sites <- sites[!grepl("P", sites$X_00060_00003_cd),]
sites <- na.omit(sites)

#Create Year, wt_year, Month, and Day columns and  
Date2wt_year(sites, "Date")

save(sites, file = "./Data/sites.Rdata")

###

load("./Data/Old Data/sites.Rdata")

# Create final dataset by removing Fish Creek, Yukon, and adding Kuparuk
site_list = read.csv("./Data/site_list.csv", header = TRUE)

sites = left_join(site_list, sites) 

create_final_dataset(sites, 350, 2019)

# Plot final sites
plot_gages("./Data/finalsiteslocation.RData")



