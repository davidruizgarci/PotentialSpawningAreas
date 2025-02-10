# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 6.3. Prepare BPUE rasters (resample, crop, crs)
#-------------------------------------------------------------------------------
library(raster)
library(beepr)
library(lubridate)
library(dplyr)
library(sf)

# 1. Load BPUE rasters----------------------------------------------------------
# Raja
genus <- "Raja" 
family <- "LN_gaussian_Final2" 
type <- "_NKm2" 
mod_code <- "brt"
dataset <- "ALL" 

# Scyliorhinus
genus <- "Scyliorhinus" 
family <- "bernuilli_Final2" 
type <- "_PA" 
mod_code <- "brt"
dataset <- "ALL" 

# Create date sequences that you wish:
date_start <- as.Date("2021-01-01")
date_end <- as.Date("2021-12-31")
#date_end <- as.Date("2021-01-02")
dates <- seq.Date(date_start, date_end, by="day")  # define sequence

# Convert date sequences to dataframes
year_df <- data.frame(date = dates)
head(year_df)

# Prepare your date list and other necessary variables
dates <- year_df 
stack_list <- vector("list", nrow(dates))  # Pre-allocate list
season <- "2021" #2021, spring, winter, summer, autumn

# Loop through each date
for (i in 1:nrow(dates)) {
  
  # Extract and format the date information
  # i=1
  date <- dates$date[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # Construct the file pattern (chose for 95% CI or BPUE)
  pat <- paste0("hurdle_X", format(date, "%Y%m%d"), "_", genus, "_", mod_code, "_pred_cir.tif")
  #pat <- paste0("hurdle_X", format(date, "%Y%m%d"), "_", genus, "_", mod_code, "_pred.tif")
  
  # Construct the path to the directory containing TIFF files
  stack_repo <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", MM)
  
  # Debugging prints
  print(paste("Stack Repo:", stack_repo))
  print(paste("Pattern:", pat))
  
  # List all TIFF files that match the pattern
  tiffile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat, 
                        ignore.case = TRUE)
  print(tiffile)  # Check the output
  
  # Exclude files ending in '_cir' if using BPUE
  #tiffile <- tiffile[!grepl("_cir$", basename(tiffile))]
  print(tiffile)  # Check the output
  
  # Debugging print
  print(paste("Found TIFF files:", length(tiffile)))
  
  if (length(tiffile) > 0) {
    s <- tryCatch({
      raster::stack(tiffile)
    }, error = function(e) {
      cat("Error in stacking raster files:", e$message, "\n")
      NULL
    })
    
    if (!is.null(s)) {
      stack_list[[i]] <- s
    }
  }
}

# Print a message indicating completion
print("Processing completed.")
summary(stack_list)

#2. Load cropping rasters-------------------------------------------------------
# 2.1. Bathymetric mask---------------------------------------------------------
bathy<- raster("input/gebco/Bathy.tif")
print(bathy)
# plot(bathy)
# Filter the values between -20 and -700 and set values outside the range to NA
bathy_filtered <- calc(bathy, function(x) {
  x[x > -20 | x < -700] <- NA  # Set values outside the range to NA
  return(x)
})
# Assign a value of 1 to the remaining (non-NA) values
bathy_mask <- calc(bathy_filtered, function(x) {
  x[!is.na(x)] <- 1
  return(x)
})

# Load BPUE
path <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_hurdle_pred_median.tif")
yBPUE <- raster(path)
print(yBPUE)

# Resample bathy_mask to match the resolution of habitat
bathy_mask_resampled <- resample(bathy_mask, yBPUE, method = "bilinear")
print(bathy_mask_resampled)

# Apply the mask to the habitat raster
habitat_cropped <- raster::mask(yBPUE, bathy_mask_resampled)

# 2.2. GSA mask-----------------------------------------------------------------
GSA <- st_read("input/GSAs/GSAs_simplified.shp")
# Filter the sf object to keep only the features where SECT_COD is "GSA06"
GSA_filtered <- GSA %>%
  filter(SECT_COD == "GSA06")
print(GSA_filtered)

# 3. Crop-----------------------------------------------------------------------
# 3.1. Bathymetric crop---------------------------------------------------------
# Apply mask to each raster stack in the list
stack_list_masked <- lapply(stack_list, function(stack) {
  mask(stack, bathy_mask_resampled)
})
plot(stack_list_masked[[1]], main = "First Masked Raster")

# 3.2. GSA crop-----------------------------------------------------------------
# Keep only the first layer from each raster in the list
stack_list_single_layer <- lapply(stack_list_masked, function(raster_stack) {
  raster_stack[[1]]  # Extract the first layer
})

# Perform intersection for each raster
stack_list_intersection <- lapply(seq_along(stack_list_single_layer), function(i) {
  # Get the raster layer based on the index
  raster_layer <- stack_list_single_layer[[i]]
  
  # Convert raster to polygons (sf object)
  raster_sf <- st_as_sf(rasterToPolygons(raster_layer, dissolve = TRUE))
  
  # Perform intersection with GSA_filtered
  intersected <- st_intersection(raster_sf, GSA_filtered)
  
  # Print the index of the current object being processed
  print(i)
  
  return(intersected)
})
beep()


# Apply expm1 transformation to habitat values in each sf object
stack_list_transformed <- lapply(stack_list_intersection, function(sf_object) {
  # Extract the name of the first column, assuming it represents habitat values
  # sf_object <- stack_list_intersection[[1]]
  habitat_column <- names(sf_object)[1]  # Adjust if it's not always the first column
  # Check if the column exists
  if (habitat_column %in% names(sf_object)) {
    # Apply transformation to the habitat column
    sf_object[[habitat_column]] <- expm1(sf_object[[habitat_column]])
  } else {
    warning("The habitat column is missing in one of the sf objects.")
  }
  
  return(sf_object)
})

# 3.3. Plot the first sf object as example--------------------------------------
print(stack_list_transformed[[1]])
print(stack_list_intersection[[1]])
library(viridis)
library(ggplot2)

# get one name:
sf_object <- stack_list_transformed[[1]]
habitat_column <- names(sf_object)[1]
habitat_column

ggplot() +
  geom_sf(data = stack_list_transformed[[1]], aes(fill = hurdle_X20210101_Scyliorhinus_brt_pred_cir)) + #hurdle_X20210101_Scyliorhinus_brt_pred
  scale_fill_viridis() +  # Apply viridis color scale
  theme_minimal() +
  labs(title = "Spatial Plot of hurdle_X20210101_Raja_brt_pred_1") 

# 4. Save sf and raster objects-------------------------------------------------
# Directory to save the sf files
output_dir <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/hurdle_crop")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Reference raster for alignment (e.g., from raster_stack)
input_dir <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/")
path <- paste0(input_dir, "2021_hurdle_pred_median.tif")
reference_raster <- raster(path)  # Assuming all rasters share the same extent/resolution
plot(reference_raster)
print(reference_raster)

# Loop through the transformed sf objects and save them
for (i in seq_along(stack_list_transformed)) {
  # i=1
  sf_object <- stack_list_transformed[[i]]
  
  # Extract the date part from the column name (e.g., "20210101" from "hurdle_X20210101_Raja_brt_pred_1")
  date_string <- substr(names(sf_object)[1], 9, 16)
  
  # Create GeoJSON output filename
  geojson_filename <- file.path(output_dir, paste0("sf_cir_", date_string, ".geojson"))
  #geojson_filename <- file.path(output_dir, paste0("sf_", date_string, ".geojson"))
  
  # check:
  #plot(sf_object)
  #print(sf_object)
  
  # Save as GeoJSON (or replace with Shapefile format by changing ".geojson" to ".shp")
  if (file.exists(geojson_filename)) {
    file.remove(geojson_filename)  # Manually delete the file if it exists
  }
  st_write(sf_object, geojson_filename, append = FALSE)
  print(paste("Saved GeoJSON:", geojson_filename))
  
  # Convert sf to SpatialPolygonsDataFrame
  spatial_object <- as(sf_object, "Spatial")
  
  # Align extents and resolutions
  spatial_raster <- raster(spatial_object)
  spatial_raster <- crop(spatial_raster, extent(reference_raster))
  reference_raster <- extend(reference_raster, extent(spatial_raster))  # Optional
  
  # Resample to match reference raster
  aligned_raster <- resample(spatial_raster, reference_raster, method = "bilinear")
  field_name <- names(spatial_object)[1]
  
  # Ensure the column is numeric
  if (!is.numeric(spatial_object[[field_name]])) {
    spatial_object[[field_name]] <- as.numeric(spatial_object[[field_name]])
  }
  
  # Rasterize with the "total_bycatch" field
  rasterized <- rasterize(
    spatial_object,
    aligned_raster,
    field = field_name,
    fun = mean,
    background = NA
  )
 
  # check:
  #plot(rasterized)
  #print(rasterized)
  
  # Create a file name for the raster
  raster_filename <- file.path(output_dir, paste0("raster_cir_", date_string, ".tif"))
  #raster_filename <- file.path(output_dir, paste0("raster_", date_string, ".tif"))
  
  # Save as GeoTIFF
  writeRaster(rasterized, raster_filename, format = "GTiff", overwrite = TRUE)
  print(paste("Saved Raster:", raster_filename))
}
beep()

# Save stack on a whole:
summary(stack_list_transformed)
output_dir <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/hurdle_crop/stack/")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
#path <- paste0(output_dir, "stack_list_transformed.rds")
path <- paste0(output_dir, "stack_cir_list_transformed.rds")
saveRDS(stack_list_transformed, path)
