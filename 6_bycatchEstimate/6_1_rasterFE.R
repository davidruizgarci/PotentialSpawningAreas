# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 6.1. Prepare daily fishing effort maps
#-------------------------------------------------------------------------------
library(gfwr)
library(tidyverse)
library(qdapRegex)
library(readr)
library(sf)
library(ggplot2)
library(raster)

# 1. Download daily fishing effort data from GFW for 2021-----------------------
years <- c(2021)

# Loop through years
for(i in seq_along(years)){
  y <- years[i]  
  
  # Loop through months (1 to 12)
  for(month in 1:12) {
    # Create start and end dates for the current month
    start_date <- as.Date(sprintf("%d-%02d-01", y, month))
    end_date <- as.Date(sprintf("%d-%02d-%02d", y, month, days_in_month(start_date)))  # End date of the month
    
    # get information: GFW base function to get raster from API and convert response to data frame
    raw <- tryCatch({
      get_raster(spatial_resolution = 'HIGH', # Can be "low" = 0.1 degree or "high" = 0.01 degree
                 temporal_resolution = 'DAILY', # Can be 'daily','monthly','yearly'
                 group_by = 'GEARTYPE', # Can be 'vessel_id', 'flag', 'geartype', 'flagAndGearType
                 start_date = start_date,
                 end_date = end_date,
                 region = 'GFCM', # geojson or GFW region code (i.e. option 2) or sf object (i.e. option 1)
                 region_source = 'RFMO', #source of the region ('EEZ','MPA', 'RFMO' or 'USER_JSON')
                 key = key) #Authorization token. Can be obtained with gfw_auth function
    }, error = function(e) {
      cat("Request failed:", e$message, "\nRetrying...\n")
      Sys.sleep(5)  # Wait before retrying
      NULL
    })
    
    # save data per year and month
    dir_output <- paste0(input_data, "/gfwr/rawdata/monthly")
    if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)
    file_name <- paste0(dir_output, "/", y, "-", sprintf("%02d", month), ".csv")
    write.csv(raw, file_name, row.names = FALSE)
    
    Sys.sleep(5)  # Pause for 5 seconds between requests to avoid rate limits
  }
}

#2. Save daily fishing effort summary data--------------------------------------
dir_output <- paste0(input_data, "/gfwr/rawdata/monthly")
setwd(dir_output)
# List all CSV files in the directory
file_list <- list.files(pattern = "\\.csv$")
# Read all CSV files into a list of data frames
data_list <- lapply(file_list, read_csv)
# Convert each tibble in the list to a data frame
data_list <- lapply(data_list, as.data.frame)

# Check data:
summary(data_list)
head(data_list[1])
str(as.data.frame(data_list[1]))

# Merge all dataframes: 
outputraw <- do.call(rbind, data_list)

# Check gear types:
gears <- unique(outputraw$geartype)
gears

# filter trawlers
outputraw <- outputraw %>%
  dplyr::filter(geartype %in% "trawlers")
head(outputraw)
str(outputraw)

# summarize total fishing effort per area:
summary_data <- outputraw %>%
  dplyr::rename('FishingHours' = 'Apparent Fishing Hours',
                'Date' = 'Time Range',
                'Vessel_IDs' = 'Vessel IDs') %>%
  group_by(Lat, Lon, Date) %>%
  summarize(
    TotalVessel_IDs = sum(Vessel_IDs),         # Sum of vessel IDs per Lat, Lon, and Date
    TotalFishingHours = sum(FishingHours),    # Sum of fishing hours per Lat, Lon, and Date
    .groups = "drop"                           # To avoid creating a grouped tibble in the output
  )

# Save your filtered data (across years, bottom trawling):
dir_output <- paste0(input_data, "/gfwr/rawdata/monthly/summarydata")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)
file_name <- paste0(dir_output, "/Daily_summaryTrawlEffort.csv")
write.csv(summary_data, file_name, row.names = F)

#3. Split the database in days--------------------------------------------------
head(summary_data)

# Loop through each unique Date in the summary_data
unique_dates <- sort(unique(summary_data$Date))
# Create a sequence of all dates from the start to the end of the range in the data
all_dates <- seq(from = min(unique_dates), to = max(unique_dates), by = "day")
# Check if there are any missing dates by comparing the full sequence with the unique dates in the data
missing_dates <- setdiff(all_dates, unique_dates)
missing_dates <- as.Date(missing_dates)
# Print the missing dates if any
missing_dates

# Loop over each date
for (date in unique_dates) {
  # Filter the data for the current date
  #date <- unique_dates[1]
  date <- as.Date(date)
  date_data <- summary_data %>% filter(Date == date)
  
  # Create a file name based on the date
  dir <- paste0(input_data, "/gfwr/rawdata/monthly/summarydata/daily/")
  file_name <- paste0(dir, format(date, "%Y-%m-%d"), ".csv")
  
  # Save the filtered data to a CSV file
  write.csv(date_data, file_name, row.names = FALSE)
  
  # Optionally, print a message indicating the file was saved
  cat("Saved data for", date, "as", file_name, "\n")
}

#4. Save as raster -------------------------------------------------------------
# Define the directory containing your data files
dir <- paste0("input/gfwr/rawdata/monthly/summarydata/daily/")
out_dir <- "input/gfwr/rawdata/monthly/summarydata/daily/maps/"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# List all files in the directory (adjust the pattern if needed)
files <- list.files(dir, full.names = TRUE)

# Loop through all files in the directory
for (file in files) {
  # Read in the fishing effort data (assuming each file is in CSV format)
  # file <- files[1]
  fishingEffort <- read.csv(file)
  #head(fishingEffort)
  
  # Define the extent based on xlim and ylim
  ext <- extent(-2, 2, 36, 45)
  
  # Create a raster with the specified extent and resolution
  r <- raster(ext, ncol=length(seq(ext@xmin, ext@xmax, by=0.01)),
              nrow=length(seq(ext@ymin, ext@ymax, by=0.01)),
              crs="+proj=longlat +datum=WGS84")
  # Set the resolution
  res(r) <- 0.01
  
  # Convert fishingEffort to a raster
  fishing_effort_raster <- rasterFromXYZ(fishingEffort[, c("Lon", "Lat", "TotalFishingHours")], crs=crs(r))
  #plot(fishing_effort_raster)
  
  # Define the target resolution (0.042 degrees in both x and y directions)
  new_resolution <- 0.042
  # Create a new empty raster with the desired resolution, maintaining the same extent and CRS
  raster_template <- raster(fishing_effort_raster)
  res(raster_template) <- c(new_resolution, new_resolution)
  
  # Resample the fishing effort raster to the new resolution using bilinear interpolation
  raster_resampled <- resample(fishing_effort_raster, raster_template, method = "bilinear")
  #plot(raster_resampled)
  
  # Optionally save the resampled raster (e.g., as a new file)
  output_file <- paste0(out_dir, basename(file))
  writeRaster(raster_resampled, output_file, format="GTiff", overwrite=TRUE)
}


#5. Quick checking map-----------------------------------------------------------
# Convert the raster to a dataframe with cell coordinates
path <- paste0(out_dir, "2021-01-01.tif")
raster_resampled
raster_df <- as.data.frame(raster_resampled, xy = TRUE)
head(raster_df)

# Make zoomed in map 
#Mask
mask<- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
print(mask)
mask <- st_transform(mask, crs = 4326)

# Filter out zero or negative values
raster_df <- raster_df[raster_df$TotalFishingHours > 0, ]

# Create a ggplot object
p <- ggplot() +
  # land mask
  geom_sf(data = mask) +
  # add tracks
  geom_tile(data = raster_df, aes(x = x, y = y, fill = TotalFishingHours))+
  
  #Set spatial bounds
  coord_sf(xlim = c(-6, 40), ylim = c(30, 46), expand = TRUE) + #change by extent if you wish to fit it to data
  # Add scale bar
  #annotation_scale(location = "bl", width_hint = 0.2) +  
  # theme
  theme_bw() +
  # Use a viridis color scale with log transformation
  scale_fill_viridis_c(trans="log10")+ 
  # Remove grids
  theme(panel.grid = element_blank())

plot(p)

# export plot as png:
# p_png <- paste0(dir_output, "/FishingEffort.png")
# ggsave(p_png, p, width=23, height=17, units="cm", dpi=300)



# 6. Year overall fishing effort------------------------------------------------
FE_2021 <- read.csv("input/gfwr/rawdata/high_resolution/2021.csv")
head(FE_2021)

# Check gear types:
gears <- unique(FE_2021$geartype)
gears

# filter trawlers
FE_2021 <- FE_2021 %>%
  dplyr::filter(geartype %in% "trawlers")
head(FE_2021)
str(FE_2021)

# summarize total fishing effort per area:
summary_data <- FE_2021 %>%
  dplyr::rename('FishingHours' = 'Apparent.Fishing.Hours',
                'Date' = 'Time.Range',
                'Vessel_IDs' = 'Vessel.IDs') %>%
  group_by(Lat, Lon) %>%
  summarize(
    TotalVessel_IDs = sum(Vessel_IDs),         # Sum of vessel IDs per Lat, Lon, and Date
    TotalFishingHours = sum(FishingHours),    # Sum of fishing hours per Lat, Lon, and Date
    .groups = "drop"                           # To avoid creating a grouped tibble in the output
  )

# Save your filtered data (across years, bottom trawling):
dir_output <- paste0(input_data, "/gfwr/rawdata/high_resolution/summarydata")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)
file_name <- paste0(dir_output, "/2021_summaryTrawlEffort.csv")
write.csv(summary_data, file_name, row.names = F)

#4. Save as raster -------------------------------------------------------------
# Define the directory containing your data files
out_dir <- "input/gfwr/rawdata/high_resolution/summarydata"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # Define the extent based on xlim and ylim
  ext <- extent(-3, 7, 35, 43)
  
  # Create a raster with the specified extent and resolution
  r <- raster(ext, ncol=length(seq(ext@xmin, ext@xmax, by=0.01)),
              nrow=length(seq(ext@ymin, ext@ymax, by=0.01)),
              crs="+proj=longlat +datum=WGS84")
  # Set the resolution
  res(r) <- 0.01
  
  # Convert fishingEffort to a raster
  fishing_effort_raster <- rasterFromXYZ(summary_data[, c("Lon", "Lat", "TotalFishingHours")], crs=crs(r))
  #plot(fishing_effort_raster)
  
  # Define the target resolution (0.042 degrees in both x and y directions)
  new_resolution <- 0.042
  # Create a new empty raster with the desired resolution, maintaining the same extent and CRS
  raster_template <- raster(fishing_effort_raster)
  res(raster_template) <- c(new_resolution, new_resolution)
  
  # Resample the fishing effort raster to the new resolution using bilinear interpolation
  raster_resampled <- resample(fishing_effort_raster, raster_template, method = "bilinear")
  #plot(raster_resampled)
  
  # Optionally save the resampled raster (e.g., as a new file)
  output_file <- paste0(out_dir, "/2021_FE.tif")
  writeRaster(raster_resampled, output_file, format="GTiff", overwrite=TRUE)

  
  #Make a plot to check it out
  raster_df <- as.data.frame(raster_resampled, xy = TRUE)
  head(raster_df)
  
  # Make zoomed in map 
  #Mask
  mask<- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
  print(mask)
  mask <- st_transform(mask, crs = 4326)
  
  # Filter out zero or negative values
  raster_df <- raster_df %>% filter(!is.na(TotalFishingHours))
  head(raster_df)
  
  # Create a ggplot object
  p <- ggplot() +
    # land mask
    geom_sf(data = mask) +
    # add tracks
    geom_tile(data = raster_df, aes(x = x, y = y, fill = TotalFishingHours))+
    
    #Set spatial bounds
    coord_sf(xlim = c(-6, 40), ylim = c(30, 46), expand = TRUE) + #change by extent if you wish to fit it to data
    # Add scale bar
    #annotation_scale(location = "bl", width_hint = 0.2) +  
    # theme
    theme_bw() +
    # Use a viridis color scale with log transformation
    scale_fill_viridis_c() + 
    # Remove grids
    theme(panel.grid = element_blank())
  
  plot(p)
