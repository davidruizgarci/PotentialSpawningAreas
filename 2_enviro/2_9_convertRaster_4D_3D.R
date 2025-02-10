# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

# This script transforms 4D rasters (lon, lat, depth, time) to 3D rasters (lon, lat, time)
# by keeping the deepest layer of each location (latest non-NA value)

#-------------------------------------------------------------------------------
# 2.9. Transform 4D to 3D
#-------------------------------------------------------------------------------
library(raster)
library(beepr)


# 1. Create function for conversion --------------------------------------------
# Convert 4D netCDF (lon,lat,depth,time) to 3D keeping only bottom (lon,lat,time)
convert_4d_to_3d_daily <- function(base_dir, output_dir) {
  # Create output directory if it does not exist
  # example to test code:
  #main_output_dir <- "input/cmems_predict_3d/2021"
  #folder_path <- "input/cmems_predict/2021"
  #base_dirs <- list.dirs(path = folder_path, full.names = TRUE, recursive = FALSE)
  
  
    if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  library(ncdf4)
  library(fs)
  library(purrr)
  
  # Helper function to process each netCDF file
  process_file <- function(input_file) {
    tryCatch({
     
      # Open en example to adapt your code to your particular data:
      # Specify the folder path to get a example and adapt the code to your needs:
      #folder_path <- "input/cmems_predict/2021/01/01"
      # List all .nc files in the specified folder
      #nc_files <- list.files(path = folder_path, pattern = "\\.nc$", full.names = TRUE)
      #input_file = nc_files[3]
      #output_dir <- folder_path
      
      # Open the netCDF file
      nc <- nc_open(input_file)
      # List available variables in the file
      var_names <- names(nc$var)
      # If no variables are found, skip the file
      if (length(var_names) == 0) {
        message(paste("No variables found in file:", input_file))
        return(NULL)
      }
      # Process the first variable (you can adjust this based on your needs)
      var_name <- var_names[1]
      
      # Extract dimension sizes
      lon_size <- length(nc$dim$longitude$vals)
      lat_size <- length(nc$dim$latitude$vals)
      depth_size <- length(nc$dim$depth$vals)
      time_size <- length(nc$dim$time$vals)
      
      # Process the 4D data
      # Extract dimension sizes
      lon <- nc$dim$longitude$vals
      lat <- nc$dim$latitude$vals
      depth <- nc$dim$depth$vals
      time <- nc$dim$time$vals
      
      # Initial conversion assuming the origin is "1970-01-01"
      ncday <- as.POSIXct(time, origin = "1970-01-01", tz = "UTC")
      date <- as.Date(ncday)
      day <- format(date, "%d")
      month <- format(date, "%m")
      
      # Check if the date does not start with "2021"
      if (!startsWith(format(date, "%Y"), "2021")) {
        # Apply the alternative conversion using "1900-01-01"
        ncday <- as.POSIXct(time*60, origin = "1900-01-01", tz = "UTC")
        date <- as.Date(ncday)
        day <- format(date, "%d")
        month <- format(date, "%m")
      }
      
      # JUST SAVE AS THEY ARE THOSE THAT ARE ALREADY 3D:
      # Check if the data is not 4D (i.e., if depth_size or time_size is 1)
      if (depth_size == 0) {
        # Simply copy the file to the output directory
        # Define output file path
        output_path <- file.path(output_dir, day)
        # Create output directory if it does not exist
        if (!dir.exists(output_path)) {
          dir.create(output_path, recursive = TRUE)
        }
        output_file <- file.path(output_path, basename(input_file))
        output_file <- sub("\\.nc$", "_3d.nc", output_file)
        file_copy(input_file, output_file, overwrite = TRUE)
        message(paste("Copied non-4D file:", input_file, "->", output_path))
      } else {
        
        
        # Identify attributes:
        # Extract the long_name attribute
        long_name <- ncatt_get(nc, varid = var_name, attname = "long_name")$value
        # Extract the units attribute directly as a character string
        units <- ncatt_get(nc, varid = var_name, attname = "units")$value
        # Extract the units of the time variable
        time_units <- ncatt_get(nc, varid = "time", attname = "units")$value
        # Extract the units of the latitude variable
        lat_units <- ncatt_get(nc, varid = "latitude", attname = "units")$value
        # Extract the units of the longitude variable
        lon_units <- ncatt_get(nc, varid = "longitude", attname = "units")$value
        
        
        # Read data
        data <- ncvar_get(nc, var_name)
        
        # Check dimensions of data
        if (length(dim(data)) == 3) {
          data_4d <- array(data, dim = c(dim(data), 1))
        } else {
          data_4d <- data
        }
        
        # Create a new array for 3D data
        data_3d <- array(NA_real_, dim = c(lon_size, lat_size, time_size))
        
        # Process each (lon, lat, time) point
        for (i in 1:lon_size) {
          for (j in 1:lat_size) {
            for (k in 1:time_size) {
              # Extract the slice for the current (lon, lat, time)
              slice <- data_4d[i, j, , k]
              last_valid_depth <- max(which(!is.na(slice)), na.rm = TRUE)
              if (!is.na(last_valid_depth)) {
                data_3d[i, j, k] <- slice[last_valid_depth]
              } else {
                data_3d[i, j, k] <- NA_real_
              }
            }
          }
        }
        
        # Define output file path
        output_path <- file.path(output_dir,  day)
        # Create output directory if it does not exist
        if (!dir.exists(output_path)) {
          dir.create(output_path, recursive = TRUE)
        }
        output_file <- paste(output_path, basename(input_file), sep = "/")
        output_file <- sub("\\.nc$", "_3d.nc", output_file)
        
        # Define dimensions
        dim_lon <- ncdim_def(name = "longitude", units = lon_units, vals = lon)
        dim_lat <- ncdim_def(name = "latitude", units = lat_units, vals = lat)
        dim_time <- ncdim_def(name = "time", units = time_units, vals = time)
        
        # Define variable
        var_out <- ncvar_def(var_name, "units", list(dim_lon, dim_lat, dim_time), missval = NA_real_)
        
        # Create a new netCDF file
        nc_out <- nc_create(output_file, list(var_out), force_v4 = TRUE)
        
        # Write data to the new netCDF file
        ncvar_put(nc_out, var_out, data_3d)
        
        # Add attributes (optional)
        ncatt_put(nc_out, var_out, long_name, var_name) #var should be changed by var_name
        
        # Close netCDF files
        nc_close(nc)
        nc_close(nc_out)
        
        message(paste("Processed file:", input_file, "->", output_file))
      }
    }, error = function(e) {
      message(paste("Error processing file:", input_file))
      message("Error message:", e$message)
    })
  }
  
  # Recursively list all netCDF files in the base directory
  nc_files <- dir_ls(base_dir, recurse = TRUE, regexp = "\\.nc$")
  
  # Process each file
  walk(nc_files, process_file)
}







# 2. Convert 4D in 3D ----------------------------------------------------------
# by selecting the deepest data

# Example usage
main_output_dir <- "input/cmems_predict_3d/2021"
folder_path <- "input/cmems_predict/2021"
base_dirs <- list.dirs(path = folder_path, full.names = TRUE, recursive = FALSE)

# Loop through each subfolder and apply the function
for (base_dir in base_dirs) {
  #base_dir <- base_dirs[1]
  
  # Create an output directory corresponding to the input subfolder
  output_dir <- file.path(main_output_dir, basename(base_dir))
  
  # Apply the conversion function
  convert_4d_to_3d_daily(base_dir, output_dir)
}
beep()


# 3. Check results:
library(ncdf4)
nppv <- nc_open("input/cmems_predict_3d/2021/01/01/20210101_nppv_3d.nc")
o2 <- nc_open("input/cmems_predict_3d/2021/01/01/20210101_o2_3d.nc")
so <- nc_open("input/cmems_predict_3d/2021/01/01/20210101_so_3d.nc")
po4 <- nc_open("input/cmems_predict_3d/2021/01/01/20210101_po4_3d.nc")
po4 <- brick("input/cmems_predict_3d/2021/01/01/20210101_po4_3d.nc")
plot(po4)


nh4 <- nc_open("input/cmems_predict_3d/2021/01/01/20210101_so_3d.nc")
no3 <- nc_open("input/cmems_predict_3d/2021/01/01/20210101_no3_3d.nc")
ph <- nc_open("input/cmems_predict_3d/2021/01/01/20210101_ph_3d.nc")
uo <- nc_open("input/cmems_predict_3d/2021/01/01/20210101_uo_3d.nc")
vo <- nc_open("input/cmems_predict_3d/2021/01/01/20210101_vo_3d.nc")







