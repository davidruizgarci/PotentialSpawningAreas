# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 6.2. Prepare fishing effort rasters (resample, crop, crs)
#-------------------------------------------------------------------------------
library(raster)
library(beepr)
library(lubridate)
library(dplyr)
library(sf)

genus <- "Raja" 
family <- "LN_gaussian_Final2" 
type <- "_NKm2" 
mod_code <- "brt"
dataset <- "ALL"
season <- "2021"

# 1. Stack fishing effort-------------------------------------------------------
# 1.1. list and resample the files----------------------------------------------
input_dir <- "input/gfwr/rawdata/monthly/summarydata/daily/maps/"

# List all the .tif files in the directory (you can adjust the pattern if needed)
file_list <- list.files(input_dir, pattern = "*.tif", full.names = TRUE)
summary(file_list)

# Crop them all to BPUE resolution, extent and crs
# Load BPUE
path <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_hurdle_pred_median.tif")
yBPUE <- raster(path)
print(yBPUE)

# Crop:
# Assuming yBPUE is your reference raster for CRS, resampling, and cropping
# You already have the list of raster files
file_list <- list.files(input_dir, pattern = "*.tif", full.names = TRUE)

input_dir <- "input/gfwr/rawdata/monthly/summarydata/daily/maps/resampled"
if (!dir.exists(input_dir)) dir.create(input_dir, recursive = TRUE)

# Loop through each raster file in the list
for (raster_file in file_list) {
  # raster_file <- file_list[1]
  # Load the raster
  current_raster <- raster(raster_file)
  
  # Project the raster to match the CRS of yBPUE
  current_raster <- projectRaster(current_raster, yBPUE)
  
  # Resample the raster to match the resolution of yBPUE (bilinear method)
  current_raster <- resample(current_raster, yBPUE, method = "bilinear")
  
  # Crop the raster to match the extent of yBPUE
  current_raster_cropped <- crop(current_raster, yBPUE)
  
  # Optionally, you can save the output rasters if needed
  output_filename <- file.path(input_dir, paste0("cropped_resampled_", basename(raster_file)))
  writeRaster(current_raster_cropped, output_filename, format = "GTiff", overwrite = TRUE)
}
