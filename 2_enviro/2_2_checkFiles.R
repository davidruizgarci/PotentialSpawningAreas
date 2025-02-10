# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 2.2. Check downloaded data 
#-------------------------------------------------------------------------------
library(dplyr)
library(beepr)

# Import data catalog
catalog <- read.csv("input/Catalog_CMEMS.csv", sep=";")
head(catalog)

cat <- catalog %>%
  mutate(
    xmin = as.numeric(gsub(",", ".", xmin)),
    xmax = as.numeric(gsub(",", ".", xmax)),
    ymin = as.numeric(gsub(",", ".", ymin)),
    ymax = as.numeric(gsub(",", ".", ymax)),
    depth_min = as.numeric(gsub(",", ".", depth_min)),
    depth_max = as.numeric(gsub(",", ".", depth_max)))

# 1) Prepare data required for checking (same as download CMEMS) ---------------
# Import our data to extract our dates
tb <- read.csv("temp/env_data2D.csv", sep = ";")

# extract the dates in which there is data:
tb$date <- as.Date(tb$date)
dates <- unique(tb$date)
dates <- data.frame(dates)
dates$dates <- as.Date(dates$dates)

# Add a new column with the year information
dates <- dates %>%
  mutate(year = format(dates, "%Y"),
         month = format(dates, "%m"),
         day = format(dates, "%d"))

# add mins and secs:
# Note: 11:00:00 if you use 12:00:00 CMEMS use the next day
dates$date_time <- paste0(dates$dates, " 11:00:00")
head(dates)



# 2) Create function to check whether there is a file or not -------------------

# Quality control of files names during download
QControl_ncFiles <- function(main_dir, dates, cat) {
  
  # Ensure required packages are installed and loaded
  required_packages <- c("foreach", "tools")
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
  # Initialize an empty dataframe to store the results
  df <- data.frame()
  
  # Loop through each date in the dates dataframe
  foreach(j = 1:nrow(dates)) %do% {
    
    # Loop through each combination of service, layer, variable, and product_type in cat
    for (i in 1:nrow(cat)) {
      # Construct the directory path
      dir_path <- file.path(main_dir, "input", "cmems", cat$service[i], cat$layer[i], cat$var_name[i], dates$year[j], dates$month[j], dates$day[j])
      
      # Construct the expected file name
      file_name <- paste0(cat$var_name[i], "_", dates$dates[j], ".nc")
      
      # Full path to the .nc file
      nc_file_path <- file.path(dir_path, file_name)
      
      # Debugging output
      cat("Checking file path: ", nc_file_path, "\n")
      
      # Check if the .nc file exists
      if (file.exists(nc_file_path)) {
        result_nc_file <- "OK"
      } else {
        result_nc_file <- "ERROR"
      }
      
      # Add results to the dataframe
      nrow <- data.frame(
        date         = dates$dates[j],
        service      = cat$service[i],
        layer        = cat$layer[i],
        variable     = cat$variable[i],
        product_type = cat$product_type[i],
        nc_file_name = file_name,
        nc_file_status = result_nc_file,
        full_path    = nc_file_path  # Adding the full path for debugging
      )
      
      df <- rbind(df, nrow)
    }
  }
  
  # End
  print("-- Quality control finished --")
  return(df)
}



# 3) Apply function to check which files are missing ---------------------------

# Check data:
quality_control_results <- QControl_ncFiles(main_dir, dates, cat)
beep()
head(quality_control_results)


# Add a new column with the year information
quality_control_results <- quality_control_results %>%
  mutate(year = format(date, "%Y"),
         month = format(date, "%m"),
         day = format(date, "%d"))

# Filter those rows which are missing:
missing <- quality_control_results %>%
  filter(nc_file_status == "ERROR")
head(missing)
