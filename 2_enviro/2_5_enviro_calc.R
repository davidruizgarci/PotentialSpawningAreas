# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 2.5. Calculate variation in enviro files
#-------------------------------------------------------------------------------
library(raster)
library(sf)
library(dplyr)
library(lubridate)
library(beepr)
library(ncdf4)
library(viridis)

# It is interesting to evaluate the effect of variation in environmental conditions over time

# 1. Set data repository--------------------------------------------------------
# Import landmask
mask <- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
print(mask)
mask <- st_transform(mask, crs = 4326)

# crop it:
e <- c(-3, 5, 35, 43)
e <- extent(e)
bbox <- st_as_sfc(st_bbox(e))
# Set the CRS of bbox to match the mask
st_crs(bbox) <- st_crs(mask) 
#Crop the mask using the bounding box
mask <- st_intersection(mask, bbox)
print(mask)


# Create date sequences that you wish:
# Year:
date_start <- as.Date("2021-01-01")
date_end <- as.Date("2021-12-31")
dates <- seq.Date(date_start, date_end, by="day")  # define sequence

year_df <- data.frame(date = dates, season = "Autumn")


# 2. Merge bottomTemp to calculate standard deviation --------------------------

# Prepare your date list and other necessary variables
dates <- year_df #spring_df, winter_df, summer_df, autumn_df
stack_list <- vector("list", nrow(dates))  # Pre-allocate list
season <- "2021"

destination_folder <- paste0(input_data, "/cmems_predict_3d")


catalog <- read.csv2("input/Catalog_pred.csv", sep=";")
cat <- catalog %>%
  filter(variable %in% c("so"), product_type %in% c("Reanalysis")) 
var <- cat$variable[1]
var

# Loop through each date
for (i in 1:nrow(dates)) {
  
  # Extract and format the date information
  # i=1
  date <- dates$date[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # Construct the file pattern
  stack_repo <- file.path(destination_folder, YYYY, MM, DD)
  
  # Construct the path to the directory containing TIFF files
  pat <- paste0(YYYY,MM,DD,"_", var, "_3d.nc")
  
  # Debugging prints
  print(paste("Stack Repo:", stack_repo))
  print(paste("Pattern:", pat))
  
  # List all TIFF files that match the pattern
  tiffile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern =  pat)
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
beep()

# Identify which elements in the list are NULL
null_indices <- which(sapply(stack_list, is.null))
null_indices

# After parallel processing, create a stack from the list of raster stacks
pred_stack <- raster::stack(stack_list)

# Calculate the median of the raster stack
pred_med <- raster::calc(pred_stack, fun = mean) 
#pred_med <- raster::calc(pred_stack, fun = sd) #sd for O2
beep()

# Define output paths
path <- paste0("input/Mean_enviro")
if (!dir.exists(path)) dir.create(path, recursive = TRUE)
#path <- paste0("input/SD_enviro")
#if (!dir.exists(path)) dir.create(path, recursive = TRUE)

tifffile <- paste0(path, "/2021", var, "_mean.tif")
pngfile <- paste0(path, "/2021", var, "_mean.png")
#tifffile <- paste0(path, "/SD_", var, ".tif")
#pngfile <- paste0(path, "/SD_", var, ".png")

# Save the median raster as TIFF
writeRaster(pred_med, filename = tifffile, format = "GTiff", overwrite = TRUE)

# Save the median raster as PNG
png(pngfile, width = 560, height = 600, res = 100)
plot(pred_med, main = var, col = viridis(100))
plot(mask, col = "grey80", border = "grey60", add = TRUE)
text(x = -3.5, y = 44, labels = format(date, "%Y-%m-%d"))
box()
dev.off()

