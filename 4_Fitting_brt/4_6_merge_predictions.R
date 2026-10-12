# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 4.6. merge hurdle prediction          Merge predictions
#-------------------------------------------------------------------------------
library(ggplot2)
library(sf)
library(pals)
library(beepr)
library(raster)
library(lubridate)

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


# 2. Merge habitat maps to create according to Hurdle ----------------------------------------
# Prepare your date list and other necessary variables
dates <- year_df #spring_df, winter_df, summer_df, autumn_df
stack_list <- vector("list", nrow(dates))  # Pre-allocate list
season <- "2021"

#2.1. Density-------------------------------------------------------------------
# Density Raja:
genus <- "Raja" 
family <- "LN_gaussian_Final2" 
type <- "_NKm2" 
mod_code <- "brt"
dataset <- "ALL" 

# Density Scyiorhinus:
#genus <- "Scyliorhinus" 
#family <- "gaussian_Final2" 
#type <- "_PA" 
#mod_code <- "brt"
#dataset <- "ALL" 

# Loop through each date
for (i in 1:nrow(dates)) {
  
  # Extract and format the date information
  # i=1
  date <- dates$date[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # Construct the file pattern
  pat <- paste0(format(date, "%Y%m%d"), "_", genus, "_", mod_code, "_pred.tif")
  
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
stack_list_Density <- stack_list[!sapply(stack_list, is.null)]




#2.2. PA------------------------------------------------------------------------
# Presence Raja:
genus <- "Raja" 
family <- "bernuilli_Final2"
type <- "_PA"
mod_code <- "brt"

# Presence Scyliorhinus
#genus <- "Scyliorhinus" 
#family <- "bernuilli_Final2" 
#type <- "_PA" 
#mod_code <- "brt"
#dataset <- "ALL" 

# Loop through each date
for (i in 1:nrow(dates)) {
  
  # Extract and format the date information
  # i=1
  date <- dates$date[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # Construct the file pattern
  pat <- paste0(format(date, "%Y%m%d"), "_", genus, "_", mod_code, "_pred.tif")
  
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
stack_list_PA <- stack_list[!sapply(stack_list, is.null)]



# 2.3. Multiply each level of the stack-----------------------------------------
head(stack_list_PA)
head(stack_list_Density)

indir <- paste(output_data, mod_code, paste0(genus, type, "_", family), sep="/")
outdir <- paste0(indir, "/predict_boost")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# Assuming stack_list_PA and stack_list_Density are your RasterStacks
# Initialize an empty list to store the results
# Loop through each layer to multiply corresponding layers and save them
for (i in 1:length(stack_list_PA)) {
  # i=1
  # Multiply corresponding layers from both stacks
  result_raster <- overlay(stack_list_PA[[i]], stack_list_Density[[i]], fun = `*`)
  
  # Extract the date from the 'dates' object (assuming it corresponds to the current layer index)
  date <- dates$date[i]
  print(date)
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  
  # set/create folder
  product_folder <- paste(outdir, YYYY, MM, sep="/")  # Set folder
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
  
  # Get the name of the current layer from stack_list_PA
  layer_name <- names(stack_list_PA[[i]])
  # Create a file name using the original layer name with "hurdle_" prefixed
  output_filename <- paste0(product_folder, "/hurdle_", layer_name, ".tif")
  # Save each raster with the constructed file name
  writeRaster(result_raster, filename = output_filename, format = "GTiff", overwrite = TRUE)
}






# 3. Merge ERROR (CI 95%) maps to create according to Hurdle ----------------------------------------
# Prepare your date list and other necessary variables
dates <- year_df #spring_df, winter_df, summer_df, autumn_df
stack_list <- vector("list", nrow(dates))  # Pre-allocate list
season <- "2021"

#2.1. Density-------------------------------------------------------------------
# Density Raja:
#genus <- "Raja" 
#family <- "LN_gaussian_Final2" 
#type <- "_NKm2" 
#mod_code <- "brt"
#dataset <- "ALL" 

# Density Scyiorhinus:
genus <- "Scyliorhinus" 
family <- "gaussian_Final2" 
type <- "_PA" 
mod_code <- "brt"
dataset <- "ALL" 

# Loop through each date
for (i in 1:nrow(dates)) {
  
  # Extract and format the date information
  # i=1
  date <- dates$date[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # Construct the file pattern
  pat <- paste0(format(date, "%Y%m%d"), "_", genus, "_", mod_code, "_pred_cir.tif")
  
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
stack_list_cir_Density <- stack_list[!sapply(stack_list, is.null)]




#3.2. PA------------------------------------------------------------------------
# Presence Raja:
#genus <- "Raja" 
#family <- "bernuilli_Final2"
#type <- "_PA"
#mod_code <- "brt"

# Presence Scyliorhinus
genus <- "Scyliorhinus" 
family <- "bernuilli_Final2" 
type <- "_PA" 
mod_code <- "brt"
dataset <- "ALL" 

# Loop through each date
for (i in 1:nrow(dates)) {
  
  # Extract and format the date information
  # i=1
  date <- dates$date[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # Construct the file pattern
  pat <- paste0(format(date, "%Y%m%d"), "_", genus, "_", mod_code, "_pred_cir.tif")
  
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
stack_list_cir_PA <- stack_list[!sapply(stack_list, is.null)]



# 2.3. Multiply each level of the stack-----------------------------------------
head(stack_list_cir_PA)
head(stack_list_cir_Density)

outdir <- paste0(indir, "/predict_boost")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# Assuming stack_list_PA and stack_list_Density are your RasterStacks
# Initialize an empty list to store the results
# Loop through each layer to multiply corresponding layers and save them
for (i in 1:length(stack_list_cir_PA)) {
  # i=1
  # Multiply corresponding layers from both stacks
  result_raster <- overlay(stack_list_cir_PA[[i]], stack_list_cir_Density[[i]], fun = `*`)
  
  # Extract the date from the 'dates' object (assuming it corresponds to the current layer index)
  date <- dates$date[i]
  print(date)
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  
  # set/create folder
  product_folder <- paste(outdir, YYYY, MM, sep="/")  # Set folder
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
  
  # Get the name of the current layer from stack_list_PA
  layer_name <- names(stack_list_cir_PA[[i]])
  # Create a file name using the original layer name with "hurdle_" prefixed
  output_filename <- paste0(product_folder, "/hurdle_", layer_name, ".tif")
  # Save each raster with the constructed file name
  writeRaster(result_raster, filename = output_filename, format = "GTiff", overwrite = TRUE)
}
