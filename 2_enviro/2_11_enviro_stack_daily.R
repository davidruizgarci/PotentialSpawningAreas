# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

# This script generates a daily multiband raster to then make model predictions.
#-------------------------------------------------------------------------------
# 2.11. Stack environmental data for predict
#-------------------------------------------------------------------------------
library(sf)
library(doParallel)
library(dplyr)
library(lubridate)
library(raster)
library(ncdf4)
library(stringr)
library(beepr)

# 1. Set static data repository -------------------------------------------------------
# path to environmental static data
static_data <- paste0(input_data, "/summary_static/")

# path to output
outdir <- paste0(temp_data, "/stack_daily/") 
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)


# 2. Create oceanmask-----------------------------------------------------------
# Set raster resolution and extent
res <- 0.042
e <- extent(-4, 8, 34, 45) 


# create base raster
m <- raster(e, res = res, crs = crs("+proj=longlat +datum=WGS84"))
m[] <- 1





# 3. Stack static environmental data-------------------------------------------
# import static maps
depth <- raster(paste0(static_data, "/bathy.tif"))  # bathymetry
depth <- depth+0

slope <- raster(paste0(static_data, "/slope.tif"))  # slope
slope <- slope+0

fishingEffort <- raster(paste0(static_data, "/FishingEffort.tif"))  # distance to shore
fishingEffort <- fishingEffort+0

SD_bottomT <- raster(paste0(static_data, "/SD_bottomT.tif"))  # distance to shore
SD_bottomT <- fishingEffort+0

SD_o2 <- raster(paste0(static_data, "/SD_o2.tif"))  # distance to shore
SD_o2 <- fishingEffort+0

#substrate_folk5 <- raster(paste0(static_data, "/substrate_folk5.tif"))  # distance to shore
#substrate_folk5 <- substrate_folk5+0
#crs_substrate <- crs(substrate_folk5)
#crs_slope <- crs(slope)
#substrate_folk5 <- crop(substrate_folk5, extent(slope))
#substrate_folk5 <- resample(substrate_folk5, slope, method = "bilinear")
#writeRaster(substrate_folk5, "substrate_folk5.tif", format = "GTiff")

substrate_folk5 <- raster(paste0(static_data, "/substrate_folk5.tif"))  # distance to shore
substrate_folk5 <- substrate_folk5+0

# prepare function for stack
prepareGrid <- function(r, m, method, name){
  rc <- raster::crop(r, m)  # crop by extent
  rs <- raster::resample(r, m, method=method)  # resample
  rm <- raster::mask(rs, m)  # mask
  names(rm) <- name
  return(rm)
}


# create stack with static variables
dept <- prepareGrid(depth, m, method="bilinear", name="depth")
slp <- prepareGrid(slope, m, method="bilinear", name="slope")
fishingEff <- prepareGrid(fishingEffort, m, method="bilinear", name="fishingEffort")
SD_o2 <- prepareGrid(SD_o2, m, method="bilinear", name="SD_o2")
SD_bottomT <- prepareGrid(SD_bottomT, m, method="bilinear", name="SD_bottomT")
substrate_folk5 <- prepareGrid(substrate_folk5, m, method="ngb", name="substrate")

stack_static <- stack(dept, slp, fishingEff, SD_o2, SD_bottomT, substrate_folk5) 







# 4. Prepare dynamic variables for stack --------------------------------------------------
# Function to prepare and stack raster files for each day
prepareStackForDay <- function(day_folder, variables, res, e, output_folder) {
  # Define extent and resolution
  e <- extent(e)
  
  # Create an empty stack
  stack_dynamic <- stack()
  
  # Mapping of original variable names to new names
  variable_names_map <- list(
    bottomT = "bottom_temp",
    o2 = "bottom_oxygen",
    nppv = "bottom_nppv",
    so = "bottom_so",
    po4 = "bottom_po4",
    uo = "bottom_uo",
    eke = "bottom_eke"
  )
  
  for (variable in variables) {
  # example to test code: variable <- variables[2]
    
    # Construct the file pattern for the variable
    file_pattern <- paste0("*_", variable, "_3d.nc")
    
    # List netCDF files for the given variable
    nc_files <- list.files(path = day_folder, pattern = file_pattern, full.names = TRUE)
    
    if (length(nc_files) == 0) {
      next
    }
    
    # Read each netCDF file and prepare the raster
    for (nc_file in nc_files) {
    # example for testing code: nc_file <- nc_files[1]
      
      # Open the netCDF file
      r <- raster(nc_file)

      # Calculate the number of columns and rows
      ncol <- (e@xmax - e@xmin) / res
      nrow <- (e@ymax - e@ymin) / res
      
      # Create an empty raster with the specified extent and resolution
      target_raster <- raster(ncol = ncol, nrow = nrow, 
                              xmn = e@xmin, xmx = e@xmax, 
                              ymn = e@ymin, ymx = e@ymax)
      
      r_resampled <- resample(r, target_raster, method = "bilinear")
      
      # Resamplear stack_static para que coincida con r_resampled
      stack_static_resampled <- resample(stack_static, r_resampled, method = "bilinear")
      
      # Stack the raster
      stack_dynamic <- stack(stack_dynamic, r_resampled)
      
      # Rename the latest raster layer in the stack with the desired name
      layer_name <- variable_names_map[[variable]]
      names(stack_dynamic)[nlayers(stack_dynamic)] <- layer_name
      
      
      # Close the netCDF file
      rm(r)
    }
  }
  
  # Now add the static layers once, resampled to the same resolution as the dynamic stack
  if (!is.null(stack_static)) {
    
    # Resample the static stack to match the dynamic raster
    stack_static_resampled <- resample(stack_static, target_raster, method = "bilinear")
    
    # Combine the dynamic stack with the resampled static stack (only once)
    stack_final <- stack(stack_dynamic, stack_static_resampled)
    
  } else {
    # If no static stack is provided, just return the dynamic stack
    stack_final <- stack_dynamic
  }
  
  # Save the final stack to file
  if (nlayers(stack_final) > 0) {
    # Extract the base directory and split by '/'
    components <- unlist(strsplit(day_folder, "/"))
    
    # Assumes that folder structure includes year, month, day in the specified positions
    year <- components[3]
    month <- components[4]
    day <- components[5]
    
    # Create a date string for the file name
    date_string <- paste0(year, month, day)
    
    # Define the output file path
    output_file <- file.path(output_folder, paste0("stack_", date_string, ".grd"))
    
    # Save the final stack
    writeRaster(stack_final, output_file, format = "raster", overwrite = TRUE)
    
    cat("Stack saved to", output_file, "\n")
  }
  
  # Return the final stacked raster
  return(stack_final)
}


# Function to process each dayly stack:
processDailyStacks <- function(base_folder, variables, res, e) {
  
  # Get list of all month folders
  month_folders <- list.dirs(base_folder, full.names = TRUE, recursive = FALSE)
  
  for (month_folder in month_folders) {
    
    # Get list of all day folders within the current month folder
    # example to test code: month_folder <- month_folders[1]
    day_folders <- list.dirs(month_folder, full.names = TRUE, recursive = FALSE)
    
    for (day_folder in day_folders) {
    # example to test code: day_folder <- day_folders[1]
      
      # Extract date from folder name
      date_folder <- basename(day_folder)
      
      # Define output folder for the stack
      output_folder <- day_folder
      if (!dir.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE)
      }
      
      # Prepare and stack rasters
      prepareStackForDay(day_folder, variables, res, e, output_folder)
    }
  }
}




# Stack you files:
# General path:
base_folder <- "input/cmems_predict_3d/2021"

# Select the dynamic variables to extract (same names as catalog):
catalog <- read.csv2("input/catalog_stack.csv", sep=";")
catalog$variable
variables <- c("bottomT", "o2", "nppv", "so", "po4", "eke", "uo", "vo")

# Set the resolution and extent:
res <- 0.042
e <- extent(-4, 8, 34, 45)  


# Process the stacks
processDailyStacks(base_folder, variables, res, e)
beep()


# Check some:
stack <- brick("input/cmems_predict_3d/2021/01/01/stack_20210101.grd")
print(stack)
plot(stack)
