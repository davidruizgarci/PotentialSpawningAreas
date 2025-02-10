# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 4.7. Predict BRT bootstrap maps
#-------------------------------------------------------------------------------
library(beepr)
library(lubridate)
library(sf)
library(raster)
library(viridis)
library(foreach)

bootstrap <- T

# Raja
genus <- "Raja" 
family <- "LN_gaussian_Final2" 
type <- "_NKm2" 
mod_code <- "brt"
dataset <- "ALL" 

## Scyliorhinus
#genus <- "Scyliorhinus" 
#family <- "bernuilli_Final2" 
#type <- "_PA" 
#mod_code <- "brt"
#dataset <- "ALL" 


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
#date_end <- as.Date("2021-01-02")
dates <- seq.Date(date_start, date_end, by="day")  # define sequence

# Convert date sequences to dataframes
year_df <- data.frame(date = dates)
head(year_df)

# Define a function to assign seasons based on the date
get_season <- function(date) {
  # Extract the month and day
  month <- as.numeric(format(date, "%m"))
  day <- as.numeric(format(date, "%d"))
  
  # Assign seasons based on month and day
  if ((month == 12 && day >= 21) || (month %in% c(1, 2)) || (month == 3 && day < 21)) {
    return("Winter")
  } else if ((month == 3 && day >= 21) || (month %in% c(4, 5)) || (month == 6 && day < 21)) {
    return("Spring")
  } else if ((month == 6 && day >= 21) || (month %in% c(7, 8)) || (month == 9 && day < 21)) {
    return("Summer")
  } else {
    return("Fall")
  }
}

# Apply the function to all dates
seasons <- sapply(dates, get_season)

# Combine dates and their corresponding seasons into a data frame
season_data <- data.frame(date = dates, season = seasons)

# Create separate data frames for each season
winter_df <- subset(season_data, season == "Winter")
spring_df <- subset(season_data, season == "Spring")
summer_df <- subset(season_data, season == "Summer")
fall_df <- subset(season_data, season == "Fall")

# View a sample from each season
head(winter_df)
head(spring_df)
head(summer_df)
head(fall_df)




# 2. Merge HABITAT maps to create seasonal means----------------------------------------

# Prepare your date list and other necessary variables
dates <- fall_df #spring_df, winter_df, summer_df, fall_df
stack_list <- vector("list", nrow(dates))  # Pre-allocate list
season <- "fall" #2021, spring, winter, summer, autumn


# Loop through each date
for (i in 1:nrow(dates)) {
  
  # Extract and format the date information
  # i=1
  date <- dates$date[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # Construct the file pattern
  pat <- paste0("hurdle_X", format(date, "%Y%m%d"), "_", genus, "_", mod_code, "_pred.tif")
  
  # Construct the path to the directory containing TIFF files
  stack_repo <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", MM)
  
  # Debugging prints
  print(paste("Stack Repo:", stack_repo))
  print(paste("Pattern:", pat))
  
  # List all TIFF files that match the pattern
  tiffile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat)
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

# Identify which elements in the list are NULL
null_indices <- which(sapply(stack_list, is.null))
null_indices
stack_list <- stack_list[!sapply(stack_list, is.null)]

# After parallel processing, create a stack from the list of raster stacks
pred_stack <- raster::stack(stack_list)

# Calculate the median of the raster stack
pred_med <- raster::calc(pred_stack, fun = median)
beep()

# Define output paths
tifffile <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_hurdle_pred_median.tif")
pngfile <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_hurdle_pred_median.png")

# Save the median raster as TIFF
writeRaster(pred_med, filename = tifffile, format = "GTiff", overwrite = TRUE)

# Save the median raster as PNG
png(pngfile, width = 560, height = 600, res = 100)
plot(pred_med, main = paste(genus, "   Model:", mod_code, "\n", season), col = viridis(100))
plot(mask, col = "grey80", border = "grey60", add = TRUE)
text(x = -3.5, y = 44, labels = format(date, "%Y-%m-%d"))
box()
dev.off()







# 3. Merge 95% CI maps to create seasonal means----------------------------------------
# Prepare your date list and other necessary variables
#dates <- year_df #spring_df, winter_df, summer_df, autumn_df
#stack_list <- vector("list", nrow(dates))  # Pre-allocate list
#season <- "2021"


# Loop through each date
for (i in 1:nrow(dates)) {
  
  # Extract and format the date information
  # i=1
  date <- dates$date[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # Construct the file pattern
  pat <- paste0("hurdle_X", format(date, "%Y%m%d"), "_", genus, "_", mod_code, "_pred_cir.tif")
  
  # Construct the path to the directory containing TIFF files
  stack_repo <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", MM)
  
  # Debugging prints
  print(paste("Stack Repo:", stack_repo))
  print(paste("Pattern:", pat))
  
  # List all TIFF files that match the pattern
  tiffile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat)
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

# Identify which elements in the list are NULL
null_indices <- which(sapply(stack_list, is.null))
null_indices
stack_list <- stack_list[!sapply(stack_list, is.null)]

# After parallel processing, create a stack from the list of raster stacks
pred_stack <- raster::stack(stack_list)

# Calculate the median of the raster stack
pred_med <- raster::calc(pred_stack, fun = median)
beep()

# Define output paths
tifffile <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_hurdle_pred_CIR_median.tif")
pngfile <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_hurdle_pred_CIR_median.png")

# Save the median raster as TIFF
writeRaster(pred_med, filename = tifffile, format = "GTiff", overwrite = TRUE)

# Save the median raster as PNG
png(pngfile, width = 560, height = 600, res = 100)
plot(pred_med, main = paste(genus, "   Model:", mod_code, "\n", season), col = viridis(100))
plot(mask, col = "grey80", border = "grey60", add = TRUE)
text(x = -3.5, y = 44, labels = format(date, "%Y-%m-%d"))
box()
dev.off()






## 4. Merge SD maps to create seasonal means----------------------------------------
## Prepare your date list and other necessary variables
#dates <- year_df #spring_df, winter_df, summer_df, autumn_df
#stack_list <- vector("list", nrow(dates))  # Pre-allocate list
#season <- "2021"
#
#
## Loop through each date
#for (i in 1:nrow(dates)) {
#  
#  # Extract and format the date information
#  # i=1
#  date <- dates$date[i]
#  YYYY <- year(date)
#  MM <- sprintf("%02d", month(date))
#  DD <- sprintf("%02d", day(date))
#  
#  # Construct the file pattern
#  pat <- paste0(format(date, "%Y%m%d"), "_", genus, "_", mod_code, "_pred_sd.tif")
#  
#  # Construct the path to the directory containing TIFF files
#  stack_repo <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", MM)
#  
#  # Debugging prints
#  print(paste("Stack Repo:", stack_repo))
#  print(paste("Pattern:", pat))
#  
#  # List all TIFF files that match the pattern
#  tiffile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat)
#  print(tiffile)  # Check the output
#  
#  # Debugging print
#  print(paste("Found TIFF files:", length(tiffile)))
#  
#  if (length(tiffile) > 0) {
#    s <- tryCatch({
#      raster::stack(tiffile)
#    }, error = function(e) {
#      cat("Error in stacking raster files:", e$message, "\n")
#      NULL
#    })
#    
#    if (!is.null(s)) {
#      stack_list[[i]] <- s
#    }
#  }
#}
#
## Print a message indicating completion
#print("Processing completed.")
#
## Identify which elements in the list are NULL
#null_indices <- which(sapply(stack_list, is.null))
#null_indices
#stack_list <- stack_list[!sapply(stack_list, is.null)]
#
## After parallel processing, create a stack from the list of raster stacks
#pred_stack <- raster::stack(stack_list)
#
## Calculate the median of the raster stack
#pred_med <- raster::calc(pred_stack, fun = median)
#beep()
#
## Define output paths
#tifffile <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_pred_SD_median.tif")
#pngfile <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_pred_SD_median.png")
#
## Save the median raster as TIFF
#writeRaster(pred_med, filename = tifffile, format = "GTiff", overwrite = TRUE)
#
## Save the median raster as PNG
#png(pngfile, width = 560, height = 600, res = 100)
#plot(pred_med, main = paste(genus, "   Model:", mod_code, "\n", season), col = viridis(100))
#plot(mask, col = "grey80", border = "grey60", add = TRUE)
#text(x = -3.5, y = 44, labels = format(date, "%Y-%m-%d"))
#box()
#dev.off()
#
#
#
#
#
#
#
#
#
## 5. Merge SE maps to create seasonal means----------------------------------------
## Prepare your date list and other necessary variables
#dates <- year_df #spring_df, winter_df, summer_df, autumn_df
#stack_list <- vector("list", nrow(dates))  # Pre-allocate list
#season <- "2021"
#
#
## Loop through each date
#for (i in 1:nrow(dates)) {
#  
#  # Extract and format the date information
#  # i=1
#  date <- dates$date[i]
#  YYYY <- year(date)
#  MM <- sprintf("%02d", month(date))
#  DD <- sprintf("%02d", day(date))
#  
#  # Construct the file pattern
#  pat <- paste0(format(date, "%Y%m%d"), "_", genus, "_", mod_code, "_pred_se.tif")
#  
#  # Construct the path to the directory containing TIFF files
#  stack_repo <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", MM)
#  
#  # Debugging prints
#  print(paste("Stack Repo:", stack_repo))
#  print(paste("Pattern:", pat))
#  
#  # List all TIFF files that match the pattern
#  tiffile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat)
#  print(tiffile)  # Check the output
#  
#  # Debugging print
#  print(paste("Found TIFF files:", length(tiffile)))
#  
#  if (length(tiffile) > 0) {
#    s <- tryCatch({
#      raster::stack(tiffile)
#    }, error = function(e) {
#      cat("Error in stacking raster files:", e$message, "\n")
#      NULL
#    })
#    
#    if (!is.null(s)) {
#      stack_list[[i]] <- s
#    }
#  }
#}
#
## Print a message indicating completion
#print("Processing completed.")
#
## Identify which elements in the list are NULL
#null_indices <- which(sapply(stack_list, is.null))
#null_indices
#stack_list <- stack_list[!sapply(stack_list, is.null)]
#
## After parallel processing, create a stack from the list of raster stacks
#pred_stack <- raster::stack(stack_list)
#
## Calculate the median of the raster stack
#pred_med <- raster::calc(pred_stack, fun = median)
#beep()
#
## Define output paths
#tifffile <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_pred_SE_median.tif")
#pngfile <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_pred_SE_median.png")
#
## Save the median raster as TIFF
#writeRaster(pred_med, filename = tifffile, format = "GTiff", overwrite = TRUE)
#
## Save the median raster as PNG
#png(pngfile, width = 560, height = 600, res = 100)
#plot(pred_med, main = paste(genus, "   Model:", mod_code, "\n", season), col = viridis(100))
#plot(mask, col = "grey80", border = "grey60", add = TRUE)
#text(x = -3.5, y = 44, labels = format(date, "%Y-%m-%d"))
#box()
#dev.off()
#