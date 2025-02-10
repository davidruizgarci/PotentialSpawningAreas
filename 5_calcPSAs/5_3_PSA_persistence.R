# ------------------------------------------------------------------------------

# Title:  Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 5.3. Analyse gap regions from seasonal maps
#------------------------------------------------------------------------------
# Description: This script calculates sampling gaps, considering both unsampled and undersampled cells

#1. Load libraries--------------------------------------------------------------
library(raster)
library(lubridate)
library(dplyr)
library(sf)

# 1. Import data----------------------------------------------------------------
# 1.1. Ocean mask---------------------------------------------------------------
# Import data
gebco<- raster("input/gebco/Bathy.tif")
print(gebco)
land <- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
print(land)
land <- st_transform(land, crs = 4326)

# Define the bounding box as an sf object
bbox_sf <- st_as_sfc(st_bbox(c(xmin = -3, ymin = 35, xmax = 7, ymax = 43), crs = st_crs(gebco)))

# Convert the sf object to an extent
bbox_extent <- as(extent(st_bbox(bbox_sf)), "Extent")

# Crop the raster using the extent
mask_b <- crop(gebco, bbox_extent)
print(mask_b)

# Assuming `mask` is already loaded as a RasterLayer
mask_f <- calc(mask_b, function(x) {
  x[x < -700 | x > 20] <- NA  # Set values outside the range to NA
  return(x)
})

# Load GSAs
GSA <- st_read("input/GSAs/GSAs_simplified.shp")
#print(GSA)

# Filter the sf object to keep only the features where SECT_COD is "GSA06"
GSA_filtered <- GSA %>%
  filter(SECT_COD == "GSA06")
print(GSA_filtered)

# set CRS
st_crs(GSA_filtered) <- st_crs(mask_f)

# Crop the raster to the extent of GSA_filtered
mask_cropped <- crop(mask_f, GSA_filtered)
mask_masked <- mask(mask_cropped, GSA_filtered)

# Assign a value of 1 to the remaining (non-NA) values
mask <- calc(mask_masked, function(x) {
  x[!is.na(x)] <- 1
  return(x)
})

# Save or visualize the filtered raster
plot(mask)

# 1.2. Stack seasonal maps------------------------------------------------------
#genus <- "Raja"
#family <- "LN_gaussian_Final2" 
#type <- "_NKm2" 

genus <- "Scyliorhinus"
family <- "bernuilli_Final2" 
type <- "_PA" 

seasons <- c("spring", "summer", "fall", "winter")

# Loop through each season
for (season in seasons) {
  # Construct the file path
  path <- paste0("output/shapefiles/", genus, "/", season, "_", genus, "_hurdle_habitat_raster.tif")
  
  # Read the CSV and dynamically assign it to a dataframe named <season>_df
  assign(paste0(season, "_df"), raster(path, sep = ";"))
}

# Now you can access them directly
print(spring_df)  # Dataframe for spring
plot(spring_df)
print(summer_df)  # Dataframe for summer
print(fall_df)  # Dataframe for autumn
print(winter_df)  # Dataframe for winter

# Stack the seasonal plots:
stack_scy <- stack(spring_df, summer_df, fall_df, winter_df) 
print(stack_scy)
plot(stack_scy)

# Ensure `mask` matches the resolution of `stack_scy`
mask <- resample(mask, stack_scy, method = "ngb")  # Use "ngb" for nearest neighbor interpolation


# 2. Calculate undersampled regions for seasons---------------------------------
# Differentiate between undersampled (1) and unsampled (2)

# 2.1. Build function-----------------------------------------------------------
# unsamp2 - Calculate unsampled cells, but differentiate between undersampled(1) and unsampled(2)
# Define the unsamp2 function
unsamp2 <- function(r, mask) {
  # Calculate the 90th percentile
  q90 <- quantile(r, probs = 0.90, type = 7, na.rm = TRUE, names = FALSE)
  
  # Get raster values and identify undersampled cells
  values <- getValues(r)
  values <- ifelse(values > q90 & !is.na(values), 1, NA)  # Set undersampled cells to 1
  
  # Create a new raster with modified values
  runder <- r
  values(runder) <- values
  
  # Apply the ocean mask
  runsamp <- runder * mask
  return(runsamp)
}

# 2.2. Calculate undersampled cells---------------------------------------------
# Initialize an empty stack
unders.season <- stack()

# Loop through the raster stack layers
for (i in 1:nlayers(stack_scy)) {
  r <- subset(stack_scy, i)
  iunsamp <- unsamp2(r, mask)
  unders.season <- stack(unders.season, iunsamp)
}

print(unders.season)
plot(unders.season)

## Save yearly undersampled maps in netcdf
path <- paste0("output/shapefiles/", genus, "/", "seasonal_persistence_", genus,  ".tif")
writeRaster(unders.season, filename=path, format="CDF", overwrite=TRUE)


# 3. Calculate Persistence of gaps areas---------------------------------------- 
# Calculate how many time one cell has been considered undersampled from yearly maps
persistence <- sum(unders.season, na.rm=TRUE) / nlayers(unders.season)
plot(persistence)

# Check values:
unique(persistence$layer)

# Save gap persistence map in netcdf
path <- paste0("output/shapefiles/", genus, "/", "persistence_", genus,  ".tif")
writeRaster(persistence, filename=path, format="GTIFF", overwrite=TRUE)
