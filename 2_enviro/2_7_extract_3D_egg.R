# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 2.7. Extract 3D data from raster to points 
#-------------------------------------------------------------------------------
library(dplyr)

#Load data
data <- read.csv("temp/env_data2D.csv", sep = ";") #remember having date format in your .csv
summary(data)
head(data)



# 1. Make unique locations to extract data -------------------------------------
# Keep only the first row for each unique value of idn
data_tows <- data %>%
  distinct(code, .keep_all = TRUE)

# explore temporal and spatial range
# use same temporal resolution (day) and numeric for lon and lat
data_tows$date <- as.Date(data_tows$date) #if your time scale has not hours
data_tows$lon <- as.numeric(gsub(",", ".", data_tows$lon))
data_tows$lat <- as.numeric(gsub(",", ".", data_tows$lat))
data_tows$depth <- as.numeric(gsub(",", ".", data_tows$depth))
range(data_tows$date)
range(data_tows$lon)
range(data_tows$lat)
range(data_tows$depth)
range(data_tows$date_time)

# Add a new column with the year information
data_tows <- data_tows %>%
  mutate(Year = format(date, "%Y"),
         Month = format(date, "%m"),
         Day = format(date, "%d"))
head(data_tows)

#open catalog
catalog <- read.csv("input/Catalog_CMEMS.csv", sep=";")

cat <- catalog %>%
  filter(dimensions %in% c("3D")) #, variable %in% c("o2")
head(cat)







# 2. Extract your own data------------------------------------------------------

# Repository to folder where netCDFs are:
repo <- paste0(input_data, "/cmems") 

# Iterate over each productid in 'cat' dataframe
for (pid in unique(cat$id_product)) {
  # Filter data corresponding to current productid
  subset_data <- subset(cat, id_product == pid)
  
  # Apply cmems3d_bottom function to subset of data
  data_tows <- cmems3d_bottom(lon=data_tows$lon, lat=data_tows$lat, date=data_tows$date, productid=pid, repo=repo, data=data_tows)
  
  # Optionally, you can assign the modified 'data' back to your original dataframe or save it somewhere
  # For example, if you want to update 'data' in place, you can do:
  # data <- data
  
  # Print or save any necessary output or results
  print(paste("Processed productid:", pid))
}
head(data)

# Save dataframe
write.csv(data_tows, "temp/data_tows_2D_3D_dist.csv", row.names = FALSE)







# 3. Move data from unique locations to actual dataset -------------------------
head(data)
head(data_tows)
names(data_tows)

# Select the columns to transfer from data_tows
columns_to_transfer <- c("code", "seabottom_o2Reanalysis", "seabottom_nppvReanalysis", 
                         "seabottom_phReanalysis", "seabottom_nh4Reanalysis", 
                         "seabottom_no3Reanalysis", "seabottom_po4Reanalysis", 
                         "seabottom_soReanalysis", "seabottom_uoReanalysis", 
                         "seabottom_voReanalysis")

# Subset the required columns from data_tows (eliminate the rest to avoid repetition)
data_tows_subset <- data_tows[, columns_to_transfer]
head(data_tows_subset)

# Merge the data with data_tows_subset on the "code" column
# 'by.x' and 'by.y' ensure that merging is done based on the "code" column
data <- merge(data, data_tows_subset, by = "code", all.x = TRUE)
head(data)
write.csv(data, "temp/data_2D_3D_dist.csv", row.names = FALSE)








# 4. Calculate eddy kinetic energy (EKE)----------------------------------------

#Load data
data <- read.csv("temp/data_2D_3D_dist.csv", sep = ",") #remember having date format in your .csv
summary(data)
head(data)

# Calculate p for each row
data$eke <- (data$seabottom_uoReanalysis^2 + data$seabottom_voReanalysis^2) / 2

# Save dataframe
write.csv(data, "temp/data_2D_3D_dist_eke.csv", row.names = FALSE)





# 4. Load and extract data from SD static rasters -----------------------------
data <- read.csv("temp/data_2D_3D_dist_eke.csv", sep = ",") 

# use same temporal resolution (day) and numeric for lon and lat
data$date <- as.Date(data$date) #if your time scale has not hours
data$lon <- as.numeric(gsub(",", ".", data$lon))
data$lat <- as.numeric(gsub(",", ".", data$lat))

range(data$date)
range(data$lon)
range(data$lat)
range(data$date_time)


# 3.1. bottomT_SD 
SD_bottomT <- raster("input/SD_enviro/SD_bottomT.tif")
SD_bottomT

data$SD_bottomT <- raster::extract(SD_bottomT, cbind(data$lon, data$lat)) 
head(data)

# 3.2. o2_SD 
SD_o2 <- raster("input/SD_enviro/SD_o2.tif")
SD_o2

data$SD_o2 <- raster::extract(SD_o2, cbind(data$lon, data$lat)) 
head(data)

# Save dataframe
write.csv(data, "temp/data_2D_3D_dist_eke_SD.csv", row.names = FALSE)

# check:
file <- paste0(temp_data, "/data_2D_3D_dist_eke_SD.csv")
data <- read.csv(file, sep = ",")

