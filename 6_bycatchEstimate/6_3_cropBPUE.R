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

# Keep corrected hurdle predictions in capsules/km2; do not apply expm1 again.
if (!exists("code_dir")) stop("Run the local 0_setup.R to set code_dir first")
source(file.path(code_dir, "fun", "training_smearing.R"))

# 1. Select corrected central hurdle maps ---------------------------------------
# Choose "Raja" or "Scyliorhinus" and run this optional export once per genus.
# Both corrected joint-map series are saved in their occurrence model folders.
genus <- "Raja"
if (!genus %in% c("Raja", "Scyliorhinus")) stop("Choose Raja or Scyliorhinus")
family <- "bernuilli_Final2"
type <- "_PA"
mod_code <- "brt"
dataset <- "ALL"

# Use this script for cropped BPUE exports and inspection.
# The updated 6_4 reads the corrected daily maps directly and applies the 6_2 domain.

# Define the dates to export.
date_start <- as.Date("2021-01-01")
date_end <- as.Date("2021-12-31")
#date_end <- as.Date("2021-01-02")
dates <- seq.Date(date_start, date_end, by="day")  # define sequence

# Convert date sequences to dataframes
year_df <- data.frame(date = dates)
head(year_df)

# Keep dates beside the raster list so exported filenames use the correct calendar day.
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
  
  # Match the exact central-map name; exclude interval maps, metadata and backups.
  # Read central BPUE maps only; the daily interval maps are separate outputs of 4_6.
  pat <- paste0("^hurdle_X", format(date, "%Y%m%d"), "_", genus, "_", mod_code, "_pred\\.tif$")
  #pat <- paste0("hurdle_X", format(date, "%Y%m%d"), "_", genus, "_", mod_code, "_pred.tif")
  
  # Construct the path to the directory containing TIFF files
  stack_repo <- file.path(output_data, mod_code, paste0(genus, type, "_", family), "predict_boost", as.character(YYYY), MM)
  
  # Show the selected folder and exact filename pattern for this date.
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
  
  if (length(tiffile) != 1L) stop("Expected one corrected hurdle map for ", date)
  # Require the scale record written by 4_6; do not substitute an older unverified map.
  check_prediction_scale(tiffile, "capsules/km2")

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

# Confirm that all requested dates have been read.
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
# Use the first verified daily map for its grid, without requiring an annual summary.
if (!length(stack_list) || is.null(stack_list[[1]])) stop("Load the verified daily maps first")
yBPUE <- stack_list[[1]][[1]]
print(yBPUE)

# Resample bathy_mask to match the resolution of habitat
# Retain the existing crop below for this optional export.
# This older binary-mask interpolation is not the domain used by updated 6_4.
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
# Preserve the existing polygon intersection for these optional GeoJSON exports.
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


# Keep the abundance correction already applied before presence and abundance were multiplied.
# Keep the original object name for the downstream cropping/export code.
# Do not apply expm1 here: that would transform the corrected BPUE a second time.
stack_list_transformed <- stack_list_intersection

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
  geom_sf(data = stack_list_transformed[[1]], aes(fill = .data[[habitat_column]])) +
  scale_fill_viridis() +  # Apply viridis color scale
  theme_minimal() +
  labs(title = paste(genus, "central BPUE:", dates$date[1])) 

# 4. Save sf and raster objects-------------------------------------------------
# Directory to save the sf files
output_dir <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/hurdle_crop")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Reference raster for alignment (e.g., from raster_stack)
input_dir <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/")
# Reuse that same verified daily grid when exporting the cropped objects.
reference_raster <- yBPUE
plot(reference_raster)
print(reference_raster)

# Loop through the transformed sf objects and save them
for (i in seq_along(stack_list_transformed)) {
  # i=1
  sf_object <- stack_list_transformed[[i]]
  
  # Use the date stored with this list position instead of parsing an sf column name.
  date_string <- format(dates$date[i], "%Y%m%d")
  
  # Create GeoJSON output filename
  geojson_filename <- file.path(output_dir, paste0("sf_", date_string, ".geojson"))
  
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
  # Retain the existing export-grid handling; the bycatch integration does not read these exports.
  reference_raster <- extend(reference_raster, extent(spatial_raster))  # Optional
  
  # Resample to match reference raster
  aligned_raster <- resample(spatial_raster, reference_raster, method = "bilinear")
  field_name <- names(spatial_object)[1]
  
  # Keep the BPUE field numeric before converting the polygons back to raster cells.
  if (!is.numeric(spatial_object[[field_name]])) {
    spatial_object[[field_name]] <- as.numeric(spatial_object[[field_name]])
  }
  
  # Restore the central BPUE field on the reference grid; these values are capsules/km2.
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
  raster_filename <- file.path(output_dir, paste0("raster_", date_string, ".tif"))
  
  # Save as GeoTIFF
  writeRaster(rasterized, raster_filename, format = "GTiff", overwrite = TRUE)
  # Record the units of this central BPUE export for later checks.
  write_prediction_scale(raster_filename, "capsules/km2", "cropped_corrected_hurdle_point")
  print(paste("Saved Raster:", raster_filename))
}
beep()

# Save the cropped central BPUE objects together for later inspection.
summary(stack_list_transformed)
output_dir <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/hurdle_crop/stack/")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
path <- paste0(output_dir, "stack_list_transformed.rds")
saveRDS(stack_list_transformed, path)
