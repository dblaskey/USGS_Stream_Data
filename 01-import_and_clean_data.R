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

#Filter data to fit criteria specified in function
#Remove_Gages_Insufficient_Data(sites, 350, 0.7, 30, 2019) #30 years of data
#Remove_Gages_Insufficient_Data(sites, 350, 0.7, 40, 2019) #40 years of data
#Remove_Gages_Insufficient_Data(sites, 350, 0.7, 50, 2019) #50 years of data
#Remove_Gages_Insufficient_Data(sites, 350, 0.7, 60, 2019) #60 years of data

# Create final dataset by removing Fish Creek and adding Kuparuk
site_list = data.frame(c("15896000", "15258000", "15276000", "15290000", "15292000",
              "15302000", "15304000", "15356000", "15484000", "15514000"), 
              c("Kuparuk River", "Kenai River", "Ship Creek", "Little Susitna River", 
                "Susitna River", "Nuyakuk River", "Kuskokwim River", "Yukon River",
                "Salcha River", "Chena River"))

colnames(site_list) <- c("site_no", "site_name")

sites = left_join(site_list, sites) 

create_final_dataset(sites, 350, 2019)

# Plot final sites
plot_gages("./Data/finalyearssiteslocation.RData")



