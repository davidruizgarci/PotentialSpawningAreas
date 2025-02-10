# -----------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#--------------------------------------------------------------------------------------
# cmems3dmat       Extract vertical profiles into matrix from 3D ROMS numerical models netcdfs along the path
#--------------------------------------------------------------------------------------
#adapted from dmarch github: https://github.com/dmarch/ocean3d/blob/8b525bd1b13bea93f608e89f40ae9a561ca49e64/R/cmems2track_v2.R#L138

# Extract data from 2D:
cmems2d <- function(lon, lat, date, productid, repo, data) {
  # Description
  # Extracts oceanographic information from 3D numerical models downloaded from CMEMS
  
  # Arguments
  # data        Your database
  # lon         longitude
  # lat         latitude
  # date        POSIXct date time or Date
  # depth       depth value, in meters (positive)
  # productid   id of the product from catalog table. This is used to find the netcdf file from the repository (repo)
  # repo        repository path with netcdf files. it follows the same structure as the CMEMS FTP server
  
  # Description
  # Extraction of values is done using the nearest neighbor 2d point.
  
  # Load libraries
  library(lubridate)
  library(ncdf4)
  library(dplyr)
  
  # Get information and variable name for a given product
  # example for code testing: productid <- 1
  product_info <- filter(cat, id_product == productid)
  var <- as.character(product_info$variable)
  
  # Initialize a named vector to store the results
  results <- setNames(rep(NA, nrow(data)), data$code)
  
  # get data for each observation
  for (i in 1:length(date)) {
    #i=1
    print(i)
    
    # get day, lon, lat, depth
    iday <- as.POSIXct(data$date[i], format = "%Y-%m-%d", tz = "UTC")
    ilon <- data$lon[i]
    ilat <- data$lat[i]
    
    # create direction
    date <- as.character(iday)
    year = format(iday, "%Y")
    month = format(iday, "%m")
    day = format(iday, "%d")
    ncdir <- paste(repo, product_info$service, product_info$layer, product_info$var_name, year, month, day, sep="/")
    
    # open netcdf matching(d)
    product_files <- list.files(paste(ncdir), full.names=TRUE, recursive=TRUE)
    if (length(product_files) == 0) {
      print("No product files found in the specified repository path.")
      next  # <<<< Skips to the next iteration of the loop
    }
    ncfile <- product_files[1]
    nc <- nc_open(ncfile)
    
    # select first file and get dimensions
    nclon <- nc$dim$lon$vals # ncvar_get(nc, varid="lon")
    nclat <- nc$dim$lat$vals # ncvar_get(nc, varid="lat")
    nctime <- nc$dim$time$vals
    time_seconds <- nctime * 60  # Convert minutes to seconds
    ncday <- as.POSIXct(time_seconds, origin = "1900-01-01", tz = "UTC")
    
    # identify nearest neighbour locations
    minlon <- which.min(abs(nclon - ilon))
    minlat <- which.min(abs(nclat - ilat))
    mintime <- which.min(abs(ncday - iday))
    
    # get variable
    ncdata <- ncvar_get(nc, varid=var, start=c(minlon, minlat, mintime), count=c(1,1,1))
    
    # Store the result in the named vector
    if (!is.na(ncdata)) {
      results[data$code[i]] <- ncdata
    }
    
    # close nc
    nc_close(nc)
  }
  # Convert the named vector to a dataframe
  results_df <- data.frame(code = names(results), value = as.numeric(results), stringsAsFactors = FALSE)
  
  # Remove NA values from results_df
  results_df <- results_df[!is.na(results_df$value), ]
  
  # Rename the value column to the variable name
  colnames(results_df)[colnames(results_df) == "value"] <- paste0(product_info$variable,"_",product_info$product_type)
  data <- merge(data, results_df, by = "code", all.x = TRUE)
  
  return(data)
}



# Extract surface data from 3D:
cmems3d_surface <- function(lon, lat, date, productid, repo, data, maxZ = NULL) {
  # Description
  # Extracts oceanographic information from 3D numerical models downloaded from CMEMS
  
  # Arguments
  # data        Your database
  # lon         longitude
  # lat         latitude
  # date        POSIXct date time or Date
  # depth       depth value, in meters (positive)
  # productid   id of the product from catalog table. This is used to find the netcdf file from the repository (repo)
  # repo        repository path with netcdf files. it follows the same structure as the CMEMS FTP server
  # maxZ        optional. Maximum Z level to extract the data from. ROMS has 35 levels. (level 23=300m)
  
  # Value
  # A data frame with: varname, var0(optional), zmax(optional)
  
  # Description
  # Extraction of values is done using the nearest neighbor 3d point.
  
  # Load libraries
  library(lubridate)
  library(ncdf4)
  library(dplyr)
  
  # Get information and variable name for a given product
  # example for code checking: productid <- 2
  product_info <- filter(cat, id_product == productid)
  var <- as.character(product_info$variable)
  
  # Extract unique idns
  idns <- as.data.frame(unique(data$code))
  colnames(idns) <- "idns"
  
  # Initialize the results matrix
  max_depth_levels <- 79  # This should be the maximum depth levels based on your data
  results <- matrix(data = NA, nrow = max_depth_levels, ncol = nrow(idns), byrow = FALSE,  
                    dimnames = list(1:max_depth_levels, idns$idns))
  
  # get data for each observation
  for (i in 1:length(date)) {
    print(i)
    
    # get day, lon, lat, depth
    iday <- as.POSIXct(data$date[i], format = "%Y-%m-%d", tz = "UTC")
    ilon <- data$lon[i]
    ilat <- data$lat[i]
    idepth <- data$depth[i]
    
    # create direction
    date <- as.character(iday)
    year = format(iday, "%Y")
    month = format(iday, "%m")
    day = format(iday, "%d")
    ncdir <- paste(repo, product_info$service, product_info$layer, product_info$var_name, year, month, day, sep="/")

    # open netcdf matching(d)
    product_files <- list.files(paste(ncdir), full.names=TRUE, recursive=TRUE)
    if (length(product_files) == 0) {
      print("No product files found in the specified repository path.")
      next  # <<<< Skips to the next iteration of the loop
    }
    ncfile <- product_files[1]
    nc <- nc_open(ncfile)
    
    # select first file and get dimensions
    nclon <- nc$dim$lon$vals # ncvar_get(nc, varid="lon")
    nclat <- nc$dim$lat$vals # ncvar_get(nc, varid="lat")
    ncdepth <- nc$dim$depth$vals
    maxZ <- if (is.null(maxZ)) nc$dim$depth$len else maxZ
    nctime <- nc$dim$time$vals
    ncday <- as.POSIXct(nctime, origin = "1970-01-01", tz = "UTC")
    
    # identify nearest neighbour locations
    minlon <- which.min(abs(nclon - ilon))
    minlat <- which.min(abs(nclat - ilat))
    mintime <- which.min(abs(ncday - iday))
    mindepth <- which.min(abs(ncdepth - idepth))
    
    # get variable
    ncdata <- ncvar_get(nc, varid=var, start=c(minlon, minlat, 1, mintime), count=c(1,1,maxZ,1))
    
    # Find the column index in results that corresponds to the current idn
    col_idx <- which(idns$idns == code)
    results[1:maxZ, col_idx] <- ncdata
    
    # close nc
    nc_close(nc)
  }
  
  
  # (1) Make list of each of the columns you will use to create the dataframe
  depth_layer <- as.numeric(dimnames(results)[[1]])
  result_df <- data.frame(depth_layer)
  for (i in 1:ncol(results)) {
    result_df <- cbind(result_df, results[, i])
  }
  colnames(result_df)[-1] <- paste0("value_", colnames(results))
  
  # (2) Select the first, last, and nearest depth data
  unique_surface <- apply(results, 2, function(col) {
    first_non_na_row <- min(which(!is.na(col)))
    if (!is.na(first_non_na_row)) {
      return(col[first_non_na_row])
    } else {
      return(NA)
    }
  })
  
  # (5) Add the data to your dataframe
  data[[paste0("seasurface_", product_info$variable,"_", product_info$product_type)]] <- unique_surface
  
  
  # (2) Select the last depth data
  unique_deepest <- apply(results, 2, function(col) {
    last_non_na_row <- max(which(!is.na(col)))
    if (!is.na(last_non_na_row)) {
      return(col[last_non_na_row])
    } else {
      return(NA)
    }
  })
  data[[paste0("seabottom_", product_info$variable, product_info$product_type)]] <- unique_deepest
  
  # return final dataframe
  return(data)
}


# Extract bottom data from 3D:
cmems3d_bottom <- function(lon, lat, date, productid, repo, data, maxZ = NULL) {
  # Description
  # Extracts oceanographic information from 3D numerical models downloaded from CMEMS
  
  # Arguments
  # data        Your database
  # lon         longitude
  # lat         latitude
  # date        POSIXct date time or Date
  # idn         unique code for each tow
  # depth       depth value, in meters (positive)
  # productid   id of the product from catalog table. This is used to find the netcdf file from the repository (repo)
  # repo        repository path with netcdf files. it follows the same structure as the CMEMS FTP server
  # maxZ        optional. Maximum Z level to extract the data from. ROMS has 35 levels. (level 23=300m)
  
  # Value
  # A data frame with: varname, var0(optional), zmax(optional)
  
  # Description
  # Extraction of values is done using the nearest neighbor 3d point.
  
  # Load libraries
  library(lubridate)
  library(ncdf4)
  library(dplyr)
  
  # Get information and variable name for a given product
  # example for code checking: productid <- 2
  product_info <- filter(cat, id_product == productid)
  var <- as.character(product_info$variable)
  
  # Extract unique idns
  idns <- as.data.frame(unique(data$code))
  colnames(idns) <- "idns"
  
  # Initialize the results matrix
  max_depth_levels <- 79  # This should be the maximum depth levels based on your data
  results <- matrix(data = NA, nrow = max_depth_levels, ncol = nrow(idns), byrow = FALSE,  
                    dimnames = list(1:max_depth_levels, idns$idns))
  
  # get data for each observation
  for (i in 1:length(date)) {
    #i=5
    print(i)
    
    # get day, lon, lat, depth
    iday <- as.POSIXct(data$date[i], format="%Y-%m-%d", tz = "UTC")
    ilon <- data$lon[i]
    ilat <- data$lat[i]
    idepth <- data$depth[i]
    idn <- data$code[i]
    
    # create direction
    date <- as.character(iday)
    year = format(iday, "%Y")
    month = format(iday, "%m")
    day = format(iday, "%d")
    ncdir <- paste(repo, product_info$service, product_info$layer, product_info$var_name, year, month, day, sep="/")
    
    # open netcdf matching(d)
    product_files <- list.files(paste(ncdir), full.names=TRUE, recursive=TRUE)
    if (length(product_files) == 0) {
      print("No product files found in the specified repository path.")
      next  # <<<< Skips to the next iteration of the loop
    }
    ncfile <- product_files[1]
    nc <- nc_open(ncfile)
    
    # select first file and get dimensions
    nclon <- nc$dim$lon$vals # ncvar_get(nc, varid="lon")
    nclat <- nc$dim$lat$vals # ncvar_get(nc, varid="lat")
    ncdepth <- nc$dim$depth$vals
    #maxZ = NULL
    maxZ <- if (is.null(maxZ)) nc$dim$depth$len else maxZ
    nctime <- nc$dim$time$vals
    ncday <- as.POSIXct(nctime, origin = "1970-01-01", tz = "UTC")
    
    # identify nearest neighbour locations
    minlon <- which.min(abs(nclon - ilon))
    minlat <- which.min(abs(nclat - ilat))
    mintime <- which.min(abs(ncday - iday))
    mindepth <- which.min(abs(ncdepth - idepth))
    
    # get variable
    ncdata <- ncvar_get(nc, varid=var, start=c(minlon, minlat, 1, mintime), count=c(1,1,maxZ,1))
    
    # Find the column index in results that corresponds to the current idn
    col_idx <- which(idns$idns == idn)
    results[1:maxZ, col_idx] <- ncdata
    
    # close nc
    nc_close(nc)
  }
  
  # (1) Make list of each of the columns you will use to create the dataframe
  depth_layer <- as.numeric(dimnames(results)[[1]])
  result_df <- data.frame(depth_layer)
  for (i in 1:ncol(results)) {
    result_df <- cbind(result_df, results[, i])
  }
  colnames(result_df)[-1] <- paste0("value_", colnames(results))
  
  # (2) Select the last depth data
  unique_deepest <- apply(results, 2, function(col) {
    last_non_na_row <- max(which(!is.na(col)))
    if (!is.na(last_non_na_row)) {
      return(col[last_non_na_row])
    } else {
      return(NA)
    }
  })
  data[[paste0("seabottom_", product_info$variable, product_info$product_type)]] <- unique_deepest
  
  # return final dataframe
  return(data)
}


# Extract data from nearest point in bathymetry to your data from 3D:
cmems3d_nearest <- function(lon, lat, date, productid, repo, data, maxZ = NULL) {
  # Description
  # Extracts oceanographic information from 3D numerical models downloaded from CMEMS
  
  # Arguments
  # data        Your database
  # lon         longitude
  # lat         latitude
  # date        POSIXct date time or Date
  # depth       depth value, in meters (positive)
  # productid   id of the product from catalog table. This is used to find the netcdf file from the repository (repo)
  # repo        repository path with netcdf files. it follows the same structure as the CMEMS FTP server
  # maxZ        optional. Maximum Z level to extract the data from. ROMS has 35 levels. (level 23=300m)
  
  # Value
  # A data frame with: varname, var0(optional), zmax(optional)
  
  # Description
  # Extraction of values is done using the nearest neighbor 3d point.
  
  # Load libraries
  library(lubridate)
  library(ncdf4)
  library(dplyr)
  
  # Get information and variable name for a given product
  product_info <- filter(cat, id_product == productid)
  var <- as.character(product_info$variable)
  
  # Extract unique idns
  idns <- as.data.frame(unique(data$code))
  colnames(idns) <- "idns"
  
  # Initialize the results matrix
  max_depth_levels <- 79  # This should be the maximum depth levels based on your data
  results <- matrix(data = NA, nrow = max_depth_levels, ncol = nrow(idns), byrow = FALSE,  
                    dimnames = list(1:max_depth_levels, idns$idns))
  
  # get data for each observation
  for (i in 1:length(date)) {
    print(i)
    
    # get day, lon, lat, depth
    iday <- as.POSIXct(data$date[i], format = "%Y-%m-%d", tz = "UTC")
    ilon <- data$lon[i]
    ilat <- data$lat[i]
    idepth <- data$depth[i]
    idn <- data$code[i]
    
    # create direction
    date <- as.character(iday)
    year = format(iday, "%Y")
    month = format(iday, "%m")
    day = format(iday, "%d")
    ncdir <- paste(repo, product_info$service, product_info$layer, product_info$var_name, year, month, day, sep="/")
    
    # open netcdf matching(d)
    product_files <- list.files(paste(ncdir), full.names=TRUE, recursive=TRUE)
    if (length(product_files) == 0) {
      print("No product files found in the specified repository path.")
      next  # <<<< Skips to the next iteration of the loop
    }
    ncfile <- product_files[1]
    nc <- nc_open(ncfile)
    
    # select first file and get dimensions
    nclon <- nc$dim$lon$vals # ncvar_get(nc, varid="lon")
    nclat <- nc$dim$lat$vals # ncvar_get(nc, varid="lat")
    ncdepth <- nc$dim$depth$vals
    maxZ <- if (is.null(maxZ)) nc$dim$depth$len else maxZ
    nctime <- nc$dim$time$vals
    ncday <- as.POSIXct(nctime, origin = "1970-01-01", tz = "UTC")
    
    # identify nearest neighbour locations
    minlon <- which.min(abs(nclon - ilon))
    minlat <- which.min(abs(nclat - ilat))
    mintime <- which.min(abs(ncday - iday))
    mindepth <- which.min(abs(ncdepth - idepth))
    
    # get variable
    ncdata <- ncvar_get(nc, varid=var, start=c(minlon, minlat, 1, mintime), count=c(1,1,maxZ,1))
    
    # Find the column index in results that corresponds to the current idn
    col_idx <- which(idns$idns == idn)
    results[1:maxZ, col_idx] <- ncdata
    
    # close nc
    nc_close(nc)
  }
  
  # (1) Make list of each of the columns you will use to create the dataframe
  depth_layer <- as.numeric(dimnames(results)[[1]])
  result_df <- data.frame(depth_layer)
  for (i in 1:ncol(results)) {
    result_df <- cbind(result_df, results[, i])
  }
  colnames(result_df)[-1] <- paste0("value_", colnames(results))
  
  # (2) Select the nearest depth data
  unique_nearest <- vector("numeric", nrow(data))
  
  for (i in 1:nrow(data)) {
    # Retrieve the column corresponding to the current idn
    idn <- data$code[i]
    col_idx <- which(idns$idns == idn)
    col <- results[, col_idx]
    
    # Find the nearest depth in the results matrix
    # create direction
    iday <- as.POSIXct(data$date[i], format = "%Y-%m-%d", tz = "UTC")
    date <- as.character(iday)
    year = format(iday, "%Y")
    month = format(iday, "%m")
    day = format(iday, "%d")
    ncdir <- paste(repo, product_info$service, product_info$layer, product_info$var_name, year, month, day, sep="/")
    
    # open netcdf matching(d)
    product_files <- list.files(paste(ncdir), full.names=TRUE, recursive=TRUE)
    ncfile <- product_files[1]
    nc <- nc_open(ncfile)
    idepth <- data$depth[i]
    ncdepth <- nc$dim$depth$vals
    mindepth <- which.min(abs(ncdepth - idepth))
    unique_nearest[i] <- col[mindepth]
  }
  
  # (3) Add the data to your dataframe
  data[[paste0("nearest_", product_info$variable, product_info$product_type)]] <- unique_nearest
 

  # return final dataframe
  return(data)
  
  
}


# Extract the three 3D possibilities at once (surfece, bottom and nearest)
cmems3d_all <- function(lon, lat, date, productid, repo, data, maxZ = NULL) {
  # Description
  # Extracts oceanographic information from 3D numerical models downloaded from CMEMS
  
  # Arguments
  # data        Your database
  # lon         longitude
  # lat         latitude
  # date        POSIXct date time or Date
  # depth       depth value, in meters (positive)
  # productid   id of the product from catalog table. This is used to find the netcdf file from the repository (repo)
  # repo        repository path with netcdf files. it follows the same structure as the CMEMS FTP server
  # maxZ        optional. Maximum Z level to extract the data from. ROMS has 35 levels. (level 23=300m)
  
  # Value
  # A data frame with: varname, var0(optional), zmax(optional)
  
  # Description
  # Extraction of values is done using the nearest neighbor 3d point.
  
  # Load libraries
  library(lubridate)
  library(ncdf4)
  library(dplyr)
  
  # Get information and variable name for a given product
  product_info <- filter(cat, id_product == productid)
  var <- as.character(product_info$variable)
  
  # Extract unique idns
  idns <- as.data.frame(unique(data$code))
  colnames(idns) <- "idns"
  
  # Initialize the results matrix
  max_depth_levels <- 79  # This should be the maximum depth levels based on your data
  results <- matrix(data = NA, nrow = max_depth_levels, ncol = nrow(idns), byrow = FALSE,  
                    dimnames = list(1:max_depth_levels, idns$idns))
  
  # Initialize vectors to store surface, bottom, and nearest depth data
  unique_surface <- vector("numeric", length(data$date))
  unique_bottom <- vector("numeric", length(data$date))
  unique_nearest <- vector("numeric", length(data$date))
  
  # get data for each observation
  for (i in 1:length(date)) {
    print(i)
    
    # get day, lon, lat, depth
    iday <- as.POSIXct(data$date[i], format = "%Y-%m-%d", tz = "UTC")
    ilon <- data$lon[i]
    ilat <- data$lat[i]
    idepth <- data$depth[i]
    
    # create direction
    date <- as.character(iday)
    year = format(iday, "%Y")
    month = format(iday, "%m")
    day = format(iday, "%d")
    ncdir <- paste(repo, product_info$service, product_info$layer, product_info$var_name, year, month, day, sep="/")
    
    # open netcdf matching(d)
    product_files <- list.files(paste(ncdir), full.names=TRUE, recursive=TRUE)
    if (length(product_files) == 0) {
      print("No product files found in the specified repository path.")
      next  # <<<< Skips to the next iteration of the loop
    }
    ncfile <- product_files[1]
    nc <- nc_open(ncfile)
    
    # select first file and get dimensions
    nclon <- nc$dim$lon$vals # ncvar_get(nc, varid="lon")
    nclat <- nc$dim$lat$vals # ncvar_get(nc, varid="lat")
    ncdepth <- nc$dim$depth$vals
    maxZ <- if (is.null(maxZ)) nc$dim$depth$len else maxZ
    nctime <- nc$dim$time$vals
    ncday <- as.POSIXct(nctime, origin = "1970-01-01", tz = "UTC")
    
    # identify nearest neighbour locations
    minlon <- which.min(abs(nclon - ilon))
    minlat <- which.min(abs(nclat - ilat))
    mintime <- which.min(abs(ncday - iday))
    mindepth <- which.min(abs(ncdepth - idepth))
    
    
    # get variable
    ncdata <- ncvar_get(nc, varid=var, start=c(minlon, minlat, 1, mintime), count=c(1,1,maxZ,1))
    
    # close nc
    nc_close(nc)
    
    # Find the column index in results that corresponds to the current idn
    col_idx <- which(idns$idns == idn)
    results[1:maxZ, col_idx] <- ncdata
    
    # Surface Data: Data from the first depth level
    unique_surface[i] <- ncdata[1]
    
    # Bottom Data: Data from the last depth level
    last_non_na_idx <- max(which(!is.na(ncdata)))
    unique_bottom[i] <- if (length(last_non_na_idx) > 0) ncdata[last_non_na_idx] else NA
    
    # Nearest Depth Data: Data closest to the target depth
    min_depth_diff <- which.min(abs(ncdepth - idepth))
    unique_nearest[i] <- ncdata[min_depth_diff]
    
  }
  
  # Add the data to your dataframe
  data[[paste0("seasurface_", product_info$variable, "_", product_info$product_type)]] <- unique_surface
  data[[paste0("seabottom_", product_info$variable, "_", product_info$product_type)]] <- unique_bottom
  data[[paste0("nearest_", product_info$variable, "_", product_info$product_type)]] <- unique_nearest
  
  return(data)
}



