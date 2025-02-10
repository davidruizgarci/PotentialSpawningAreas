# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 2.3. Download GFW data - calculate fishing effort within a customized area
#-------------------------------------------------------------------------------
# @LeiaNH repository - https://github.com/LeiaNH/GFW-tools/tree/main - taken as reference

library(tidyverse)
library(qdapRegex)
library(readr)
library(sf)
library(ggplot2)
library(raster)

# install rgfw
#devtools::install_github("GlobalFishingWatch/gfwr")
# load GFWr
library(gfwr)

# 1. Set study area and period--------------------------------------------------

# Option 1:
# One of the options that rGFW package provides is to get summaries of fishing effort of an specific area such as EEZ or MPA. 
# Nevertheless there is an option to include your customed area in json format. 
# For that, a quick way to do it is to draw your own study area as a polygon in the next link: 
# https://geojson.io/#map=0.82/24.5/-7.7
# Set the areas (the name of the geojson files) that you want to process
# geojson_file <- paste0("input/gfwr/map.geojson")
# Read the geojson file as an sf object
# area <- st_read(geojson_file)

#Option 2:
# The list of available COUNTRIES:
# eez_regions <- get_regions(region_source = 'EEZ', key = key)
# The list of available RFMOs:
# rfmo_regions <- get_regions(region_source = 'RFMO', key = key)
# The list of available MPAs:
# mpa_regions <- get_regions(region_source = 'MPA', key = key)

# select your own:
#area <- get_region_id(region_name = 'GFCM', region_source = 'RFMO', key = key)

# 1.2. Set study years:
years <- c(2014:2022)

#2. Download fishing effort summary data-----------------------------------------

# let's loop it across years
for(i in seq_along(years)){
  #i = 1
  y <- years[i]  
  # Loop through months (1 to 12)
  for(month in 1:12) {
    # Create start and end dates for the current month
    # month <- 1
    start_date <- as.Date(sprintf("%d-%02d-01", y, month))
    end_date <- as.Date(sprintf("%d-%02d-%02d", y, month, days_in_month(start_date)))  # End date of the month
    
  
  # get information: GFW base function to get raster from API and convert response to data frame
  raw <- get_raster(spatial_resolution = 'HIGH', # Can be "low" = 0.1 degree or "high" = 0.01 degree
                    temporal_resolution = 'DAILY', # Can be 'daily','monthly','yearly'
                    group_by = 'GEARTYPE', # Can be 'vessel_id', 'flag', 'geartype', 'flagAndGearType
                    start_date = start_date,
                    end_date = end_date,
                    region = 'GFCM', # geojson or GFW region code (i.e. option 2) or sf object (i.e. option 1)
                    region_source = 'RFMO', #source of the region ('EEZ','MPA', 'RFMO' or 'USER_JSON')
                    key = key) #Authorization token. Can be obtained with gfw_auth function

    # save data per year if you wish:
  dir_output <- paste0(input_data, "/gfwr/rawdata/monthly")
  if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)
  file_name <- paste0(dir_output, "/", y, "-", sprintf("%02d", month), ".csv")
  write.csv(raw, file_name, row.names = FALSE)
  
  Sys.sleep(5)  # Pause for 5 seconds between requests to avoid rate limits
}
}

#3. Save fishing effort summary data--------------------------------------------
dir_output <- paste0(input_data, "/gfwr/rawdata/high_resolution")
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
                'Year' = 'Time Range',
                'Vessel_IDs' = 'Vessel IDs') %>%
  group_by(Lat, Lon) %>%
  summarize(
    TotalVessel_IDs = sum(Vessel_IDs),
    TotalFishingHours = sum(FishingHours))

# Save your filtered data (across years, bottom trawling):
dir_output <- paste0(input_data, "/gfwr/summarydata")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)
file_name <- paste0(dir_output, "/summaryTrawlEffort.csv")
write.csv(summary_data, file_name, row.names = F)


#3. Quick checking map-----------------------------------------------------------
# First read the file with gridded data already saved
setwd(main_dir)
fishingEffort <- read.csv("input/gfwr/summarydata/summaryTrawlEffort.csv")
str(fishingEffort)

# Make zoomed in map 
#Mask
mask<- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
print(mask)
mask <- st_transform(mask, crs = 4326)

# Bounding box if you rather setting it to the raster limits:
#extent <- coord_sf(xlim = c(min(fishingeffort$Lon), max(fishingeffort$Lon)), 
#                   ylim = c(min(fishingeffort$Lat), max(fishingeffort$Lat)))

# Create a ggplot object
p <- ggplot() +
  # land mask
  geom_sf(data = mask) +
  # add tracks
  geom_tile(data = fishingEffort, aes(x = Lon, y = Lat, fill = TotalFishingHours))+
              
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
p_png <- paste0(dir_output, "/FishingEffort.png")
ggsave(p_png, p, width=23, height=17, units="cm", dpi=300)


#4. Save as raster -------------------------------------------------------------

# Define the extent based on xlim and ylim
ext <- extent(-6, 40, 30, 46)

# Create a raster with the specified extent and resolution
r <- raster(ext, ncol=length(seq(ext@xmin, ext@xmax, by=0.1)),
            nrow=length(seq(ext@ymin, ext@ymax, by=0.1)),
            crs="+proj=longlat +datum=WGS84")
# Set the resolution
res(r) <- 0.1

# Convert fishingEffort to a raster
fishing_effort_raster <- rasterFromXYZ(fishingEffort[, c("Lon", "Lat", "TotalFishingHours")], crs=crs(r))
plot(fishing_effort_raster)

# Save:
dir_output <- paste0(input_data, "/gfwr/summarydata")
geotiff_file <- paste0(dir_output, "/FishingEffort.tif")
writeRaster(fishing_effort_raster, filename = geotiff_file, format = "GTiff", overwrite = TRUE)
