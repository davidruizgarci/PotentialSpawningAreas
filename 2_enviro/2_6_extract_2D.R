# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 2.6. Extract 2D data from raster to points 
#-------------------------------------------------------------------------------
library(raster)
library(ncdf4)
library(dplyr)
library(fasterize)

data <- read.csv2("temp/pres_absData.csv", sep = ";")
#data <- read.csv("temp/data_2D_3D_dist_eke_SD.csv", sep = ",") 

# add mins and secs:
# Note: 11:00:00 if you use 12:00:00 CMEMS use the next day
data$date_time <- paste0(data$date, " 11:00:00")


# use same temporal resolution (day) and numeric for lon and lat
data$date <- as.Date(data$date) #if your time scale has not hours
data$lon <- as.numeric(data$lon)
data$lat <- as.numeric(data$lat)

range(data$date)
range(data$lon)
range(data$lat)
range(data$date_time)

# 1) Load and extract data from all static rasters -----------------------------

# 1.1) Bathymetry (depth) 
bathy <- raster("input/gebco/Bathy.tif")
bathy

# Create a new raster with the desired resolution (0.042 degrees)
res <- 0.042
bathy_resampled <- raster(extent(bathy), resolution = res, crs = crs(bathy))

# Resample using bilinear interpolation
bathy <- resample(bathy, bathy_resampled, method = "bilinear")

data$bathy <- raster::extract(bathy, cbind(data$lon, data$lat)) 
data$bathy <- abs(data$bathy)
head(data)

# 1.2) Substrate 
# Run only once:
#library(sf)
#library(beepr)
#subs <- st_layers("input/emodnet/substrate_folk5/seabed_substrate_1m.shp")
#subs
#subs <- st_read("input/emodnet/substrate_folk5/seabed_substrate_1m.shp", layer = "seabed_substrate_1m")
#subs
#
## Create a raster with the specified resolution (0.0042 degrees per cell)
#r <- raster(extent(subs), res = 0.042)  # Change resolution as needed
#crs(r) <- st_crs(subs)$proj4string
#
## Convert to a factor
#subs$subs_factor <- as.factor(subs$folk_5cl)
#
## Convert the factor levels to numeric values
#subs$subs_numeric <- as.numeric(subs$subs_factor)
#
##Unique values:
#unique_values_df <- unique(data.frame(Type = subs$folk_5cl_t, NumericValue = subs$subs_numeric, original = subs$folk_5cl))
#
#transform_values <- c(
#  '1' = 1, '2' = 1, '3' = 2, '4' = 3,  '5' = 4, '6' = 5)
#
## Create the new column 'bioSubsFinal' by matching the values in 'SubAll'
#unique_values_df <- unique_values_df %>%
#  mutate(NumericValue = transform_values[as.character(NumericValue)])
#
##save
#write.csv2(unique_values_df, "input/emodnet/substrate_folk5/subTypes.csv", row.names = FALSE)
#
## Convert values in raster:
#subs$subs_numeric <- as.numeric(subs$subs_factor)
#
#subs <- subs %>%
#  mutate(subs_numeric = transform_values[as.character(subs_numeric)])
#
## Ensure your 'subs' object is an sf object
#subs_sf <- st_as_sf(subs)  # Convert if 'subs' is Spatial*
#
## Rasterize the vector data with the specified resolution
##rasterized <- rasterize(as(subs, "Spatial"), r, field = "subs_numeric")
#rasterized <- fasterize(subs_sf, r, field = "subs_numeric", fun = "max")

#plot(rasterized)
#writeRaster(rasterized, filename="input/emodnet/substrate_folk5/SubsFinal.tif", overwrite=TRUE)  # save binary file for slope

subs <- raster("input/emodnet/substrate_folk5/substrate_folk5.tif")
subs

data$subs <- raster::extract(rasterized, cbind(data$lon, data$lat)) 
head(data)

# 1.3) Slope 
# Calculate terrain characteristic from bathymetry

# Only run once:
#slope <- terrain(bathy, opt=c("slope"), unit='degrees')
#writeRaster(slope, filename="input/emodnet/slope/slope.tif", overwrite=TRUE)  # save binary file for slope

slope <- raster("input/emodnet/slope/slope.tif")
slope

data$slope <- raster::extract(slope, cbind(data$lon, data$lat)) 
head(data)

# 1.4) Roughness 
# Difference between the maximum and the minimum value of a cell and its 8 surrounding cells
#roughness <- terrain(bathy, opt=c("roughness"), unit='degrees')
roughness <- raster("input/gebco/roughness/roughness.tif")
roughness

data$roughness <- raster::extract(roughness, cbind(data$lon, data$lat)) 
head(data)

# 1.5) Fishing effort
fishingEffort <- raster("input/gfwr/summarydata/high_resolution/FishingEffort.tif")
fishingEffort

# Create a new raster with the desired resolution (0.042 degrees)
res <- 0.042
fishingEffort_resampled <- raster(extent(fishingEffort), resolution = res, crs = crs(fishingEffort))

# Resample using bilinear interpolation
fishingEffort <- resample(fishingEffort, fishingEffort_resampled, method = "bilinear")

data$fishingEffort <- raster::extract(fishingEffort, cbind(data$lon, data$lat)) 
data$fishingEffort <- abs(data$fishingEffort)
head(data)

# 1.6) Distance to canyons
distCanyons <- raster("input/distance_to/distance_canyons.tif")
distCanyons

data$distCanyons <- raster::extract(distCanyons, cbind(data$lon, data$lat)) 
data$distCanyons <- abs(data$distCanyons)
head(data)

# 1.7) Distance to mounts
distMounts <- raster("input/distance_to/distance_mounts.tif")
distMounts

data$distMounts <- raster::extract(distMounts, cbind(data$lon, data$lat)) 
data$distMounts <- abs(data$distMounts)
head(data)

# 1.8) Distance to fans
distFans <- raster("input/distance_to/distance_fans.tif")
distFans

data$distFans <- raster::extract(distFans, cbind(data$lon, data$lat)) 
data$distFans <- abs(data$distFans)
head(data)

# 1.9) Biogenic substrates 
 library(sf)
 library(beepr)
 BioSubs <- st_layers("input/emodnet/EUSeaMap/Cropped.shp")
 BioSubs
 BioSubs <- st_read("input/emodnet/EUSeaMap/Cropped.shp", layer = "Cropped")
 BioSubs
 ## Create a raster with the specified resolution (0.042 degrees per cell)
 r <- raster(extent(BioSubs), res = 0.042)  # Change resolution as needed
 crs(r) <- st_crs(BioSubs)$proj4string
 #
 ## Convert to a factor
 BioSubs$Allcomb_factor <- as.factor(BioSubs$AllcombD)
 BioSubs$Substrate_factor <- as.factor(BioSubs$Substrate)
 #
 ## Convert the factor levels to numeric values
 #BioSubs$Allcomb_numeric <- as.numeric(BioSubs$Allcomb_factor)
 BioSubs$Substrate_numeric <- as.numeric(BioSubs$Substrate_factor)
 #
 ##Unique values:
 #unique_values_df <- unique(data.frame(Type = BioSubs$Allcomb, NumericValue = BioSubs$Allcomb_numeric, FullName = BioSubs$AllcombD))
 unique_subs_df <- unique(data.frame(Type = BioSubs$Substrate, NumericValue = BioSubs$Substrate_numeric))
 #
 ##save
 #write.csv2(unique_values_df, "input/emodnet/EUSeaMap_2023/BioSubsType_ALL.csv", row.names = FALSE)
 write.csv2(unique_subs_df, "input/emodnet/EUSeaMap/BioSubsType.csv", row.names = FALSE)
 
 ## Rasterize the vector data with the specified resolution
 
 ## Ensure your 'subs' object is an sf object
 BioSubs_sf <- st_as_sf(BioSubs)  # Convert if 'BioSubs' is Spatial*
 
 ## Rasterize the vector data with the specified resolution
 #rasterized <- rasterize(as(BioSubs, "Spatial"), r, field = "Substrate_numeric")
 rasterized <- fasterize(BioSubs_sf, r, field = "Substrate_numeric", fun = "max")
 
 #writeRaster(rasterized, filename="input/emodnet/EUSeaMap/BioSubs_fineSubsFINAL.tif", overwrite=TRUE)  # save binary file for slope
 #plot(rasterized)
 #
 ## extract 
 #bioSubs <- raster("input/emodnet/EUSeaMap_2023/BioSubs_fineSubs.tif")
 #bioSubs
 #
 data$SubAll <- raster::extract(rasterized, cbind(data$lon, data$lat)) 
 head(data)
 hist(data$SubAll)
 ## Define the transformation rules as a named vector
 transform_values <- c(
   '1' = 7, '2' = 8, '3' = 3, '4' = 7,
   '5' = 8, '6' = 1, '7' = 1, '8' = 5,
   '9' = 2, '10' = 1, '11' = NA)
 #
 ## Create the new column 'bioSubsFinal' by matching the values in 'SubAll'
 data <- data %>%
   mutate(bioSubsFinal = transform_values[as.character(SubAll)])
 hist(data$bioSubsFinal)
 
 #write.csv2(data, "temp/data_2D_3D_dist_eke_SD_FINAL.csv", row.names = FALSE)

# Save dataframe
write.csv2(data, "temp/env_data.csv", row.names = FALSE)
data <- read.csv2("temp/env_data.csv", sep = ";")


# 2) Extract data from dynamic 2D variables-------------------------------------
catalog <- read.csv2("input/Catalog_CMEMS.csv", sep=";")
head(catalog)
cat <- catalog %>%
  filter(var_name %in% "SBT_Reanalysis")


# The only 2D variable is SBT (med-cmcc-tem-rean-d)
# make a loop to (1) open each file ".nc" (2) configure time format and (3) extract data

# Repository to folder where netCDFs are:
repo <- paste0(input_data, "/cmems") 
example <- brick("input/cmems/MEDSEA_MULTIYEAR_PHY_006_004/med-cmcc-tem-rean-d/SBT_Reanalysis/2020/06/18/SBT_Reanalysis_2020-06-18.nc")

# Iterate over each productid in 'cat' dataframe
for (pid in unique(cat$id_product)) {
  # Filter data corresponding to current productid
  # example for checking code:  id_product <- 1
  subset_data <- subset(cat, id_product == pid)

data <- cmems2d(lon=data$lon, lat=data$lat, date=data$date, productid=pid, repo=repo, data=data)
# Print or save any necessary output or results
print(paste("Processed productid:", pid))
}
head(data)

# Save dataframe
write.csv2(data, "temp/env_data2D.csv", row.names = FALSE)
