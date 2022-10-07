#Determine which stations in Alaska have discharge data available
gages <- readNWISdata(stateCd="AK", parameterCd="00060", service="dv") %>%
  distinct(site_no)

#Using that created list pull all the discharge data from USGS server
sites = vector()
for(i in 1:nrow(gages)){
    temp <- readNWISdv(gages[i,], parameterCd = '00060')
    sites <- rbind(sites,temp)
}

#save raw sites
save(sites, file = "./Data/raw_sites.Rdata")

# load raw sites
load("./Data/raw_sites.Rdata")

#Remove provisional data and NaNs
sites <- sites[!grepl("P", sites$X_00060_00003_cd),]
sites <- sites %>%
  drop_na()

sites = sites %>%
  mutate(site_no = as.numeric(site_no))

df = data.frame(Date = seq(as.Date("1971/10/01"), as.Date("2019/9/30"), "days")) %>%
  mutate(site_no = 15896000, Q = 0, agency_cd = "USGS", X_00060_00003_cd = "A")

sites = full_join(df, sites) %>%
  mutate(X_00060_00003 = ifelse(is.na(X_00060_00003) == TRUE, Q, X_00060_00003)) %>%
  select(-Q) 

# Create final dataset by removing Fish Creek, Yukon, and adding Kuparuk
site_list = read.csv("./Data/site_list.csv", header = TRUE) 

sites = left_join(site_list, sites) 
  
#Create Year, wt_year, Month, and Day columns and  
Date2wt_year(sites, "Date")

create_final_dataset(sites, 350, 2019)

# Plot final sites
plot_gages("./Data/finalsiteslocation.RData")



