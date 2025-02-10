# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 2.8. Download environmental data from CMEMS for predict
#-------------------------------------------------------------------------------
library(reticulate)
library(lubridate)
library(dplyr)
library(beepr)

# Import data catalog
catalog <- read.csv2("input/Catalog_pred.csv", sep=";")
str(catalog) #ensure numerical variables are numeric

catalog <- catalog %>%
  mutate(
    xmin = as.numeric(gsub(",", ".", xmin)),
    xmax = as.numeric(gsub(",", ".", xmax)),
    ymin = as.numeric(gsub(",", ".", ymin)),
    ymax = as.numeric(gsub(",", ".", ymax)),
    depth_min = as.numeric(gsub(",", ".", depth_min)),
    depth_max = as.numeric(gsub(",", ".", depth_max)))




# 1. Create a dates dataframe for the dates you want to get data from ----------
# Create a sequence of dates from January 1, 2021, to December 31, 2021
dates <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day")

# Convert the dates to the desired format with the time "11:00:00"
date_times <- paste(dates, "11:00:00")

# Create a dataframe
dates <- data.frame(date = dates, date_time = date_times)
dates$date <- as.Date(dates$date)

# Add a new column with the year, month and day information
dates <- dates %>%
  mutate(Year = format(date, "%Y"),
         Month = format(date, "%m"),
         Day = format(date, "%d"))
head(dates)





# 2) Load CMEMS package through python -----------------------------------------
# install python 
#install_python() 
#virtualenv_create(envname = "cmems")
#virtualenv_install("cmems", packages = c("copernicusmarine"))
use_virtualenv("cmems", required = TRUE)

# load package / import library (py)
cm <- import("copernicusmarine")

# log in in your CMEMS user (you should have another script with this info)
cm$login(username, password)
# for session yes == y
y




# 3) Download data -------------------------------------------------------------
# Define the time subset you want:
df <- dates 

# Define the catalog subset you want:
#cat <- catalog
cat <- catalog %>%
  filter(variable %in% c("po4"), product_type %in% c("Reanalysis")) 


# Define the name of the file and the destination
destination_folder <- paste0(input_data, "/cmems_predict")
if (!dir.exists(destination_folder)) dir.create(destination_folder, recursive = TRUE)

t <- Sys.time()
for(i in 1:nrow(cat)){ 
  
  # Calculate remaining products
  remaining_products <- nrow(cat) - i
  
    #If you need a folder per each date:
  for(j in 1:nrow(df)){
    # Calculate remaining dates
    remaining_dates <- nrow(df) - j
    
    # Print the current product and remaining products
    print(paste("Processing product", i, "of", nrow(cat), "-", remaining_products, "remaining"))
    # Print the current date and remaining dates
    print(paste("Processing date", j, "of", nrow(df), "-", remaining_dates, "remaining"))
    
    # Create the folder for each product if it doesn't exist already 
    dir_path <- file.path(destination_folder, dates$Year[j], dates$Month[j], dates$Day[j])
    if (!file.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)}
    # Define the file name using the current date
    file_name <- paste0(format(as.Date(dates$date[j], origin = "1970-01-01"), "%Y%m%d"),"_", cat$variable[i], ".nc")
    
    # download data
    cm$subset(
      dataset_id = cat$layer[i],
      start_datetime = df$date_time[j], #format example "1994-05-16 12:00:00"
      end_datetime = df$date_time[j],
      variables = list(cat$variable[i]), # attention - variable must be a list
      minimum_longitude = cat$xmin[i],
      maximum_longitude =  cat$xmax[i],
      minimum_latitude =  cat$ymin[i],
      maximum_latitude = cat$ymax[i],
      minimum_depth = cat$depth_min[i],
      maximum_depth = cat$depth_max[i],
      output_filename = file_name,
      output_directory = dir_path,
      force_download = TRUE)
  }
}
Sys.time() - t 
beep()


#check one of each:
bottomT <- nc_open("input/cmems_predict/2021/01/01/20210101_bottomT.nc")
nppv <- nc_open("input/cmems_predict/2021/01/01/20210101_nppv.nc")
o2 <- nc_open("input/cmems_predict/2021/01/01/20210101_o2.nc")
po4 <- nc_open("input/cmems_predict/2021/01/01/20210101_po4.nc")
so <- nc_open("input/cmems_predict/2021/01/01/20210101_so.nc")




# 2. Remove unwanted files------------------------------------------------------
# I had to remove a copy of all the eke files generated:
# List all files in the directory ending with "_eke_3d.nc"
# Define the base directory
base_folder <- paste0(input_data, "/cmems_predict_3d/2021")

# Get list of all month folders
month_folders <- list.dirs(base_folder, full.names = TRUE, recursive = FALSE)

# Loop through each month folder
for (month_folder in month_folders) {
  #month_folder <- month_folders[1]
  
  # Get list of all day folders within the current month folder
  day_folders <- list.dirs(month_folder, full.names = TRUE, recursive = FALSE)
  
  # Loop through each day folder
  for (day_folder in day_folders) {
    #day_folder <- day_folders[1]
    
    # List all files in the day folder ending with "_eke_3d.nc"
    #files_to_remove <- list.files(path = day_folder, pattern = "_EKE\\.nc$|_nh4\\.nc$|_no3\\.nc$|_ph\\.nc$|_po4\\.nc$|_uo\\.nc$|_vo\\.nc$", full.names = TRUE)
    # List all files in the day folder
    all_files <- list.files(path = day_folder, full.names = TRUE)
    # Filter files to eliminate those that start with "stack_" and end with ".gri"
    files_to_remove <- all_files[grepl("^stack_.*\\.grd$", basename(all_files))]
    
    # Print files to be removed (for confirmation)
    print(files_to_remove)
    
    # Remove all matching files
    file.remove(files_to_remove)
  }
}




# 2. Rename files------------------------------------------------------
#nc <- nc_open("input/cmems_predict/2021/01/05/20210105_bottomT_(1).nc")

# Define the base directory
base_folder <- paste0(input_data, "/cmems_predict/2021")

# Get list of all month folders
month_folders <- list.dirs(base_folder, full.names = TRUE, recursive = FALSE)

# Loop through each month folder
for (month_folder in month_folders) {
  #month_folder <- month_folders[1]
  
  # Get list of all day folders within the current month folder
  day_folders <- list.dirs(month_folder, full.names = TRUE, recursive = FALSE)
  
  # Loop through each day folder
  for (day_folder in day_folders) {
    #day_folder <- day_folders[1]
    
    # List all files in the day folder
    files_to_rename <- list.files(path = day_folder, full.names = TRUE)
    
    # Loop through each file
    for (file in files_to_rename) {
      #file <- files_to_rename[1]
      
      # Get the base name of the file (without the full path)
      file_base_name <- basename(file)
      
      # Check if the file contains "_(1)" and remove it
      if (grepl("_\\(1\\)", file_base_name)) {
        # New name without "_(1)"
        new_file_base_name <- gsub("bottomT_\\(1\\)", "po4", file_base_name)

        # Create the full path for the new file name
        new_file <- file.path(dirname(file), new_file_base_name)
        
        # Rename the file
        file.rename(from = file, to = new_file)
        
        # Optionally print the renamed file
        print(paste("Renamed:", file, "->", new_file))
      }
    }
  }
}
