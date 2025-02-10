# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 4. Fit prediction Map
#-------------------------------------------------------------------------------
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(raster)
library(sp)
library(beepr)

season <- "2021"

# 1. Set data repository and load rasters---------------------------------------

# 1.1. Bathymetry
bathy<- raster("input/gebco/Bathy.tif")
print(bathy)
# plot(bathy)
# Convert bathy raster to data frame
bathy_df <- as.data.frame(bathy, xy = TRUE)
#Colour for bathymetry:
# Create a mask if you dont want to plot all the bathymetricla range
#bathy_bb <-  bathy_df$Bathy <= 5 #only upper break
bathy_filtered <- bathy_df %>%
  filter(Bathy >= -800 & Bathy <= 5)
# Apply the mask
print(bathy_filtered)



# 1.2. Landmask
mask <- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
#print(mask)
mask <- st_transform(mask, crs = 4326)
# crop it:
e <- c(-3, 7, 35, 43)
e <- extent(e)
bbox <- st_as_sfc(st_bbox(e))
# Set the CRS of bbox to match the mask
st_crs(bbox) <- st_crs(mask) 
#Crop the mask using the bounding box
mask <- st_intersection(mask, bbox)
print(mask)



# 1.3. GSAs
GSA <- st_read("input/GSAs/GSAs_simplified.shp")
#print(GSA)
# Filter the sf object to keep only the features where SECT_COD is "GSA06"
GSA_filtered <- GSA %>%
  filter(SECT_COD == "GSA06")
print(GSA_filtered)


# 2.Adjust fishing effort with potential spawning areas and MPAs-----------------
# RUN ONLY ONCE:
# 5.1. Clean PSAs:
#genus <- "Scyliorhinus" #Scyliorhinus, Raja
#
#path <- paste0("output/shapefiles/", genus, "/", genus, "MASK_habitat_raster.tif")
#PSAr <- raster(path)
#plot(PSAr)
#print(PSAr)
#
## Create contour lines for the raster
#polygon_layer <- rasterToPolygons(PSAr, dissolve = TRUE)
## Convert contour lines to spatial polygons (sf object)
#contour_sf <- st_as_sf(polygon_layer)
## Simplify to the outer boundary by merging polygons
#outer_boundary <- st_union(contour_sf)
## Plot the contours
#plot(outer_boundary)
#print(outer_boundary)
#
## Smoothing the polygon
#library(rmapshaper)  # Optional for simplification
#library(smoothr)     # For smoothing
#outer_boundary <- st_as_sf(outer_boundary)
#smoothed_boundary <- smooth(outer_boundary, method = "ksmooth", smoothness = 3)
#plot(smoothed_boundary)
#
## Define the output shapefile path
#output_shapefile <- paste0("output/shapefiles/", genus, "_outer_boundary.shp")
## Ensure the output directory exists
#if (!dir.exists(dirname(output_shapefile))) {
#  dir.create(dirname(output_shapefile), recursive = TRUE)
#}
## Save the outer boundary as a shapefile
#st_write(smoothed_boundary, output_shapefile, append = FALSE)

# 1.4. Load PSAs-------------------------------------------------------------
#path <- paste0("output/shapefiles/", season, "_ALL_PSAs.tif") 
#PSA <- raster(path)
#plot(PSA)
#
## 1.4.1. Transform PSAs to polygons-----------------------------------------------
## Step 1: Clump the raster cells to group connected areas
#clumped_raster <- clump(PSA, directions = 8)  # Use 8 directions for diagonal connectivity
#plot(clumped_raster)
## Reproject the filtered polygons to WGS 84 (EPSG:4326)
#crs(clumped_raster) <- "+proj=longlat +datum=WGS84 +no_defs"
#
## Step 2: Convert the clumped raster to polygons, merging connected areas
#library(terra)
## Convert RasterLayer (raster) to SpatRaster (terra)
#clumped_raster_terra <- rast(clumped_raster)
## Convert clumped raster to polygons (terra)
#polygon_vect <- as.polygons(clumped_raster_terra, dissolve = TRUE)
## Convert terra polygons to sf object
#polygon_sf <- st_as_sf(polygon_vect)
#plot(polygon_sf)
#head(polygon_sf)
#
## Step 3: Calculate the area of each polygon and filter
## Calculate the area of each polygon in the original coordinate system's units (square degrees)
#polygon_sf <- polygon_sf %>% 
#  mutate(area_native_units = as.numeric(st_area(.)))  # Area in square degrees if using EPSG:4326
## View(polygon_sf)
## Approximate area threshold for filtering (change as needed based on CRS)
#area_threshold_native_units <- 30000000  
#
## Filter polygons based on the threshold in native units
#polygon_sf_filtered <- polygon_sf %>% 
#  filter(area_native_units >= area_threshold_native_units)
#
## Plot the filtered polygons
#plot(st_geometry(polygon_sf_filtered))
#polygon_sf_filtered <- st_set_crs(polygon_sf_filtered, 4326)  # Assume it's WGS 84
#
## Step 3: Smooth
#library(rmapshaper)  # Optional for simplification
#library(smoothr)     # For smoothing
#
## Method 1:
#outer_boundary <- st_as_sf(polygon_sf)
#smoothed_boundary <- smooth(outer_boundary, method = "ksmooth", smoothness = 1)
#plot(smoothed_boundary)
#
## Method 2:
## Apply smoothing to the polygon shape (e.g., using the 'chaikin' method)
#p_smooth_chaikin <- smooth(polygon_sf_filtered, method = "chaikin", refinements = 1) #
#plot(st_geometry(p_smooth_chaikin))
#
## Step 3: Calculate the area of each polygon (in km²)
## Calculate the area of each polygon in the original coordinate system's units (square degrees)
#outer_boundary <- p_smooth_chaikin %>% 
#  mutate(area_native_units = as.numeric(st_area(.)))  # Area in square degrees if using EPSG:4326
## View(outer_boundary)
## Approximate area threshold for filtering (change as needed based on CRS)
#area_threshold_native_units <- 30000000  # Approximate 70 km² in square degrees
#
## Filter polygons based on the threshold in native units
#polygon_sf_filtered <- outer_boundary %>% 
#  filter(area_native_units >= area_threshold_native_units)
#
## Plot the filtered polygons
#plot(st_geometry(polygon_sf_filtered))
#polygon_sf_filtered <- st_set_crs(polygon_sf_filtered, 4326)  # Assume it's WGS 84
#
## filter:
#filtered_clumps <- polygon_sf_filtered[polygon_sf_filtered$clumps %in% c(12, 7, 1, 9, 24, 22), ] #, 20, 19, 14, 5, 26, 32, 36, 10, 34, 2, 21, 17, 15, 8, 35, 31, 23, 30, 27, 16, 25
#
##plot(st_geometry(filtered_clumps))
## Buffer the polygons with a small distance (e.g., 0.001)
##p_smooth_chaikin_proj <- st_transform(polygon_sf_filtered, crs = 32631)  # Example for UTM zone 31N
##p_buffered_proj <- st_buffer(p_smooth_chaikin_proj, dist = 1000)  # 1000 meters
##merged_polygons_proj <- st_union(p_buffered_proj)
##merged_polygons_final <- st_transform(merged_polygons_proj, crs = 4326)
##plot(merged_polygons_final, col = 'lightgreen', main = 'Final Merged Polygons')
#
#PSAs_merged <- filtered_clumps

# Step 4: Save the filtered polygons as a shapefile
#output_shapefile <- paste0("output/shapefiles/", season, "_ALL2_hurdle_contour_90_percentile_filtered.shp")
#if (!dir.exists(dirname(output_shapefile))) dir.create(dirname(output_shapefile), recursive = TRUE)
#st_write(PSAs_merged, output_shapefile, append = FALSE)


# Load PSAs:
path <- paste0("output/shapefiles/", season, "_ALL2_hurdle_contour_90_percentile_filtered.shp")
PSAs_merged <- st_read(path)



# 2. Plot PSAs -----------------------------------------------------------------
# Define the plot
p <- ggplot() +
  
  # Plot land mask
  geom_sf(data = mask, fill = "grey80", color = "grey60") +
  
  # Plot GSAs
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Plot PSAs with bold color and solid lines
  geom_sf(data = PSAs_merged, fill = NA, color = "black", linewidth = 0.4, linetype = "solid", alpha = 1) +
  
  # Set spatial bounds (adjust these to fit your data)
  coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
  
  # Add scale bar (optional)
  #annotation_scale(location = "bl", width_hint = 0.2) + 
  
  # Theme settings
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        aspect.ratio = 1) 


p

# export plot
enviro <- "PSAs" #slope, fishingEffort, 
outdir <- paste0(output_data, "/fig/Map/overlapFE_PSA")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", enviro, "_.jpeg")
ggsave(p_png, p, width=10, height=10, units="cm", dpi=1800)



# 1.5. Load fishing effort----------------------------------------------------
fishingEffort <- raster("input/gfwr/summarydata/high_resolution/FishingEffort.tif")
fishingEffort

# Create a new raster with the desired resolution (0.042 degrees)
res <- 0.042
fishingEffort_resampled <- raster(extent(fishingEffort), resolution = res, crs = crs(fishingEffort))

# Resample using bilinear interpolation
fishingEffort <- resample(fishingEffort, fishingEffort_resampled, method = "bilinear")

# Crop to FE
# Define the extent for cropping (xmin, xmax, ymin, ymax)
#crop_extent <- extent(-1.5, 6.2, 35.5, 43)
# Crop the raster using the defined extent
#fishingEffort <- crop(fishingEffort, crop_extent)

# 1.5.1. Crop to GSA and bathy -------------------------------------------------
# Convert the clipped sf object back to a data frame (with original coordinates)
# Crop according to bathy:
bathy<- raster("input/gebco/Bathy.tif")
print(bathy)
# plot(bathy)
# Filter the values between -50 and -600 and set values outside the range to NA
bathy_filtered <- calc(bathy, function(x) {
  x[x > -1 | x < -700] <- NA  # Set values outside the range to NA
  return(x)
})

# Assign a value of 1 to the remaining (non-NA) values
bathy_mask <- calc(bathy_filtered, function(x) {
  x[!is.na(x)] <- 1
  return(x)
})

# resample:
bathy_mask_resampled <- resample(bathy_mask, fishingEffort, method = "bilinear")

# Crop using the mask:
fishingEffort_cropped <- raster::mask(fishingEffort, bathy_mask_resampled)
plot(fishingEffort_cropped)

# Ensure that we retain the original x and y coordinates
FE_df <- as.data.frame(fishingEffort_cropped, xy = TRUE)

# Convert habitat_df to an sf object (using original x, y coordinates)
fishingEffort_sf <- st_as_sf(FE_df, coords = c("x", "y"), crs = st_crs(mask), remove = FALSE)
#plot(fishingEffort_sf)

# Ensure CRS compatibility between habitat_sf and GSA_filtered
if (st_crs(fishingEffort_sf) != st_crs(GSA_filtered)) {
  fishingEffort_sf <- st_transform(fishingEffort_sf, crs = st_crs(GSA_filtered))
}

# Perform the intersection
fishingEffort_clipped_sf <- st_intersection(fishingEffort_sf, GSA_filtered)
#plot(fishingEffort_clipped_sf)

# convert to df
fishingEffort_df <- as.data.frame(fishingEffort_clipped_sf, xy = TRUE)

# Replace NA values in the 'FishingEffort' column with 0
#library(tidyr)
#FE_withinGSA_df <- fishingEffort_clipped_sf %>%
#  mutate(FishingEffort = replace_na(FishingEffort, 0))

# Revert the log1p() of the response variable
fishingEffort_df$ln_FishingEffort <- log1p(fishingEffort_df$FishingEffort)

# Verify the result
summary(fishingEffort_df)


# 1.6. Load MPAs----------------------------------------------------------------
# 1.6.1. Crop to GSA------------------------------------------------------------
# run only once:
#MPA <- st_read("input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_20240531_shp/ProtectedSeas_Navigator_20240531_shp.shp")
#head(MPA)
#MPA_merged <- st_make_valid(MPA_merged)
# Identify invalid geometries
#invalid_geometries <- !st_is_valid(MPA_merged)
#sum(invalid_geometries)
# Filter out invalid geometries
#MPA_clean <- MPA_merged[!invalid_geometries, ]
#beep()
#invalid_geometries <- !st_is_valid(MPA_clean)
#sum(invalid_geometries)
#beep()
#Save:
#st_write(MPA_clean, "input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_Valid.shp", append=FALSE)
#beep()
#
#MPA <- st_read("input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_Valid.shp")
#head(MPA)
#atributes <- read.csv("input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_Attributes_20240531.csv")
#
## Merge atributes into MPA based on shared 'site_id' and 'SITE_ID'
#MPA_merged <- MPA %>%
#  left_join(atributes, by = c("SITE_ID" = "site_id"))
#print(MPA_merged)
#
## Check CRS of GSA_filtered and MPA
#print(st_crs(GSA_filtered))  # Check the CRS of GSA_filtered
#print(st_crs(MPA_merged))           # Check the CRS of MPA
#
#if (st_crs(MPA_merged) != st_crs(GSA_filtered)) {
#  MPA_merged <- st_transform(MPA_merged, st_crs(GSA_filtered))
#}
#
# Filter to remove unwanted
# Keep only MPAs that intersect with GSA_filtered
#MPA_cropped <- st_intersection(MPA_merged, GSA_filtered)
#beep()
##plot(MPA_cropped)
#
#ggplot() +
#  geom_sf(data = MPA_cropped, fill = "lightblue", color = "darkblue") +
#  theme_minimal() +
#  ggtitle("MPA Cropped Areas") +
#  theme(plot.title = element_text(hjust = 0.5))
#
##Save
#st_write(MPA_cropped, "input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_Valid_Cropped.shp", append=FALSE)
#beep()

# 1.6.2. Crop to Bathymetry-----------------------------------------------------
#Load cropped MPAs to study area:
#MPA_cropped <- st_read("input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_Valid_Cropped.shp")
#
## Check CRS of GSA_filtered and MPA
#print(st_crs(bathy_mask))  # Check the CRS of GSA_filtered
#print(st_crs(MPA_cropped))           # Check the CRS of MPA
#
#if (st_crs(MPA_cropped) != st_crs(bathy_mask)) {
#  MPA_cropped <- st_transform(MPA_cropped, st_crs(bathy_mask))
#}
#
#summary(MPA_cropped)
#print(bathy_mask)
#
## Step 1: Convert bathy_mask raster to a polygon for valid (non-NA) values
#bathy_polygon <- st_as_sf(rasterToPolygons(bathy_mask, dissolve = TRUE))
#plot(bathy_polygon)
## Step 2: Ensure CRS matches between MPA_cropped and bathy_polygon
#bathy_polygon <- st_transform(bathy_polygon, crs = st_crs(MPA_cropped))
#
## Step 3: Perform spatial intersection to get overlapping area
#MPA_cropped_clipped <- st_intersection(MPA_cropped, bathy_polygon)
#
## Step 4: Visualize the result
#plot(st_geometry(MPA_cropped_clipped))
#
##Save
#st_write(MPA_cropped_clipped, "input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_Valid_Cropped_Bathy.shp", append=FALSE)
#beep()

# 1.6.3. Filter MPAs -----------------------------------------------------------
#Load cropped MPAs to study area:
MPA_cropped_clipped <- st_read("input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_Valid_Cropped.shp")
atributes <- read.csv("input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_Attributes_20240531.csv")
MPA_merged <- MPA_cropped_clipped %>%
  left_join(atributes, by = c("SITE_ID" = "site_id"))
print(MPA_merged)

# remove not real MPAs:
summary(MPA_merged)
MPA_merged <- subset(MPA_merged, !(SITE_ID %in% c('AIDZA1', 'AIESP1', 'AIFRA1', 'AIITA1', 'AIHSMA4', 'AIESP3')))

# Save as dataframe to check bottom_trawl regulations:
# Run only once:
#MPA_merged_df <- as.data.frame(MPA_merged)
# Write to Excel
#library(writexl)
#write_xlsx(MPA_merged_df, "input/MPAs/ProtectedSeas/MPA_merged_check.xlsx")

# Load modified bottom_trawl regulations:
regu <- read.csv2("input/MPAs/ProtectedSeas/MPA_merged_check.csv")

# import to MPA vector:
head(regu)
head(MPA_merged)

# Merge the dataframes by SITE_NA
MPA_merged <- merge(MPA_merged, regu[, c("SITE_NA", "new_bottom_trawl")], by = "SITE_NA", all.x = TRUE)


# 1.6.4. Filter the MPAs types--------------------------------------------------
#fishing_mpas <- MPA_merged #if you want them all
unique_values <- unique(MPA_merged$new_bottom_trawl)
unique_values

# Loop over the unique values and filter the data for each value
for (value in unique_values) {
  # value = 0
  # Create a dynamic variable name (e.g., MPA_1, MPA_2, etc.)
  var_name <- paste0("MPA_", value)
  
  # Filter the dataset based on the current value of removal_of_marine_life_is_prohibited
  filtered_data <- MPA_merged %>%
    filter(new_bottom_trawl == value)
  
  # Assign the filtered data to the dynamically created variable
  assign(var_name, filtered_data)
  
  # Optionally, print or summarize the filtered dataset
  print(paste("Summary of", var_name))
  print(summary(filtered_data))
}


# 1.6.5. Add spatial regulations on bottom trawling ----------------------------
#Additionally, there is a bottom trawl regulation in the GSA06 to be mapped:
# The area <50 m deep or <3 nautical miles (which ever is closer) is protected

# 1.6.5.1. <3 nautical miles ---------------------------------------------------
# Create a buffer around the coast polygons at a distance of 3 nautical miles
#coast <- st_read("input/landmask/Europa/Europe_coastline.shp")
## crop to smaller area:
## Define the extent
#e <- extent(-3, 7, 35, 43)
## Convert the extent to an sf object
#e_sf <- st_as_sfc(as(e, "SpatialPolygons"))
#st_crs(e_sf) <- st_crs(GSA_filtered)  # Assign CRS from GSA_filtered
#
## Reproject the extent to match `coast` CRS
#e_sf_proj <- st_transform(e_sf, st_crs(coast))
#
## Crop the `coast` object to the extent
#coast_cropped <- st_crop(coast, e_sf_proj)
#plot(coast_cropped)
#
## Assuming 'coast' is your Simple Feature (SF) object containing the coast of Spain
## Convert 3 nautical miles to meters (3 * 1852 = 5556 meters)
#buffer_distance <- 3 * 1852  # 5556 meters
#
## Create the buffer line at 3 nautical miles from the coast
#coast_buffer <- st_buffer(coast_cropped, dist = buffer_distance)
#beep()
## Plot the original coast and the buffer to visualize
#plot(st_geometry(coast_cropped), col = 'blue', main = 'Coastline with 3 Nautical Mile Buffer')
#plot(st_geometry(coast_buffer), col = 'red', add = TRUE)
#
## Save as a shapefile
#path <- paste0("input/MPAs/polygon_dist.shp")
#st_write(coast_buffer, path, append=FALSE)

## Load it:
#coast_buffer <- st_read("input/MPAs/polygon_dist.shp")
#
## Transform coast_buffer to match the CRS of GSA_filtered
#coast_buffer <- st_transform(coast_buffer, crs = st_crs(GSA_filtered))
#print(coast_buffer)
#plot(st_geometry(coast_buffer))
## Crop to the area where it applies:
## Assuming bathy50 is already loaded as an sf object
## Define the bounding box for the crop (xmin, ymin, xmax, ymax)
#crop_box <- st_bbox(c(xmin = -1, xmax = 1, ymin = 39.7344, ymax = 40.7172), crs = st_crs(coast_buffer))
#
## Convert the bounding box to an sf object
#crop_polygon <- st_as_sfc(crop_box)
#plot(crop_polygon)
## Crop the bathy50 polygons to the bounding box
#coast_buffer_cropped <- st_intersection(coast_buffer, crop_polygon)
#
## Plot to verify
#plot(st_geometry(coast_buffer_cropped), col = "lightblue", border = "darkblue", main = "Cropped Polygons")
#print(coast_buffer_cropped)
#
## Eliminate columbretes data (xmin, xmax, ymin, ymax)
## Define the bounding box for Columbretes in WGS 84 CRS
#c_bounding_box <- st_bbox(c(xmin = 0.5, xmax = 0.8, ymin = 39.7, ymax = 40), crs = st_crs(coast_buffer_cropped))
#
## Convert the bounding box to an sf polygon
#columbretes_area <- st_as_sfc(c_bounding_box)
#
## Use st_difference to remove the Columbretes area from coast_buffer_cropped
#coast_buffer_cropped_c <- st_difference(coast_buffer_cropped, columbretes_area)
#
## Plot to verify
#plot(st_geometry(coast_buffer_cropped_c), col = "blue", add = FALSE, main = "Coast Buffer Cropped Without Columbretes")
#
## Save as a shapefile
#path <- paste0("input/MPAs/polygon_dist_FINAL.shp")
#st_write(coast_buffer_cropped_c, path, append=FALSE)

# Load it:
coast_buffer <- st_read("input/MPAs/polygon_dist_FINAL.shp")
plot(coast_buffer)

# 1.6.5.2. <50 m deep  ---------------------------------------------------------

#Run only once:
#bathy<- raster("input/gebco/Bathy.tif")
#print(bathy)
## plot(bathy)
## Filter the values between -50 and -600 and set values outside the range to NA
#bathy_filtered <- calc(bathy, function(x) {
#  x[x > -0 | x < -50] <- NA  # Set values outside the range to NA
#  return(x)
#})
#
## Assign a value of 1 to the remaining (non-NA) values
#bathy_mask <- calc(bathy_filtered, function(x) {
#  x[!is.na(x)] <- 1
#  return(x)
#})
#
## Eliminate Atlantic data (xmin, xmax, ymin, ymax)
#b <- as(extent(-6, 0, 42, 46), 'SpatialPolygons')
#crs(b) <- crs(bathy_mask)
## Use the mask function to apply the mask to bathy
#bathy <- mask(bathy_mask, b, inverse = TRUE)
#
## Eliminate Atlantic data (xmin, xmax, ymin, ymax)
#c <- as(extent(5, 10, 34, 46), 'SpatialPolygons')
#crs(c) <- crs(bathy)
## Use the mask function to apply the mask to bathy
#bathy <- mask(bathy, c, inverse = TRUE)
#
#plot(bathy)
#print(bathy)
#
## Make a polygon:
## Step 1: Clump the raster cells to group connected areas
#clumped_raster <- clump(bathy, directions = 8)  # Use 8 directions for diagonal connectivity
#plot(clumped_raster)
## Reproject the filtered polygons to WGS 84 (EPSG:4326)
##crs(clumped_raster) <- "+proj=longlat +datum=WGS84 +no_defs"
#
## Step 2: Convert the clumped raster to polygons, merging connected areas
#polygon_sf <- st_as_sf(rasterToPolygons(clumped_raster, dissolve = TRUE))
#plot(polygon_sf)
#
## Step 3: Calculate the area of each polygon (in km²)
## Calculate the area of each polygon in the original coordinate system's units (square degrees)
#polygon_sf <- polygon_sf %>% 
#  mutate(area_native_units = as.numeric(st_area(.)))  # Area in square degrees if using EPSG:4326
## View(polygon_sf)
## Approximate area threshold for filtering (change as needed based on CRS)
#area_threshold_native_units <- 200000  # Approximate 70 km² in square degrees
#
## Filter polygons based on the threshold in native units
#polygon_sf_filtered <- polygon_sf %>% 
#  filter(area_native_units >= area_threshold_native_units)
#
## Plot the filtered polygons
#plot(st_geometry(polygon_sf_filtered), col = 'red', add = TRUE)
#polygon_sf_filtered <- st_set_crs(polygon_sf_filtered, 4326)  # Assume it's WGS 84
#
### Save as a shapefile
#path <- paste0("input/MPAs/polygon_50m.shp")
#st_write(polygon_sf_filtered, path, append=FALSE)

# Load it:
bathy50 <- st_read("input/MPAs/polygon_50m.shp")
plot(bathy50)
print(bathy50)

# Define the crop box as a simple feature (polygon)
crop_box <- st_as_sfc(st_bbox(c(xmin = -1, xmax = 1, ymin = 39.7344, ymax = 40.7172), crs = st_crs(bathy50)))

# Subtract the crop box area from bathy50
bathy50_cropped <- st_difference(bathy50, crop_box)





# Plot to visualize the result
plot(st_geometry(bathy50_cropped), col = "blue", main = "bathy50 After Removing Crop Box")

# Crop to GSA:
# Ensure CRS compatibility between habitat_sf and GSA_filtered
if (st_crs(bathy50_cropped) != st_crs(GSA_filtered)) {
  bathy50_cropped <- st_transform(bathy50_cropped, crs = st_crs(GSA_filtered))
}

# Perform the intersection
bathy50_clipped <- st_intersection(bathy50_cropped, GSA_filtered)
plot(st_geometry(bathy50_clipped), col = "blue", main = "bathy50 After Removing Crop Box")


# 1.7. Load ISRAs---------------------------------------------------------------
ISRA <- st_read("input/isra_allregions/isra_allregions.shp")

# Filter the dataset to remove "recommended buffers"
ISRA_filtered <- ISRA %>%
  filter(Name != "recommended buffer")

# 1.7.1. crop them to GSA-------------------------------------------------------
head(ISRA_filtered)
head(GSA_filtered)

## remove invalid ones - run only once:
#ISRA_filtered <- st_make_valid(ISRA_filtered)
## Identify invalid geometries
#invalid_geometries <- !st_is_valid(ISRA_filtered)
#sum(invalid_geometries)
## Filter out invalid geometries
#ISRA_clean <- ISRA_filtered[!invalid_geometries, ]
#beep()
## double ckeck:
#invalid_geometries <- !st_is_valid(ISRA_clean)
#sum(invalid_geometries)
#beep()
##Save:
#st_write(ISRA_clean, "input/isra_allregions/isra_allregions_valid.shp", append=FALSE)

##Load:
#ISRA <- st_read("input/isra_allregions/isra_allregions_valid.shp")
#head(ISRA)
#
## Check CRS of GSA_filtered and MPA
#print(st_crs(GSA_filtered))  # Check the CRS of GSA_filtered
#print(st_crs(ISRA))           # Check the CRS of ISRA
#
#if (st_crs(ISRA) != st_crs(GSA_filtered)) {
#  ISRA <- st_transform(MPA_merged, st_crs(GSA_filtered))
#}
#
## Filter to remove unwanted
## Keep only MPAs that intersect with GSA_filtered
#ISRA_cropped <- st_intersection(ISRA, GSA_filtered)
#beep()
#
#
#ggplot() +
#  geom_sf(data = ISRA_cropped, fill = "lightblue", color = "darkblue") +
#  theme_minimal() +
#  ggtitle("ISRA Cropped Areas") +
#  theme(plot.title = element_text(hjust = 0.5))
#
##Save
#st_write(ISRA_cropped, "input/isra_allregions/isra_allregions_valid_cropped.shp", append=FALSE)

#Load ISRA cropped:
ISRA <- st_read("input/isra_allregions/isra_allregions_valid_cropped.shp")
head(ISRA)


# 3. Plot overlap between MPAs and PSAs ----------------------------------------
#colRamp <- colorRampPalette(c('#FFFAFA','#FFE4E1','#FFC1C1','#FF8C69','#CD5B45','#8B3E2F', '#2B130E'))(100)

# Arrange rows based on new_bottom_trawl in the specified order
MPA_merged <- MPA_merged %>%
  arrange(factor(new_bottom_trawl, levels = c(1, 2, 0)))
head(MPA_merged)

MPA_colors <- c("#FF7961","#A5D6A7", "#FFF176") #"#F44336" "#FF9800" "#FFEB3B", "#4CAF50", "#003DA5"
# value 0: red: Bottom trawling is allowed
# value 1: green: Not allowed bottom trawling (either <50 m deep, <3 nautical miles to cost, or bottom trawling is prohibited in the regulatory plan)
# value 2: yellow: Bottom trawling is not allowed in part of it (either because part of it occurs at <50 m, <3 nautical miles from coast, or is prohibitted in part of  the MPA)

#PSA_Rajac <- "steelblue"  # Dark Teal for PSA_Rajac
#PSA_Scac <- "orange"   # Coral for PSA_Scac
#PSAs <- "steelblue"

# Define the plot
p <- ggplot() +

  # Plot MPAs with varying transparency and colors to indicate restriction levels
  geom_sf(data = MPA_0, fill = MPA_colors[1], color = "#F44336", size = 0.8, alpha = 1) +
  geom_sf(data = MPA_2, fill = MPA_colors[3], color = "#FFEB3B", size = 0.8, alpha = 1) +
  geom_sf(data = MPA_1, fill = MPA_colors[2], color = "#4CAF50", size = 0.8, alpha = 1) +
  
  # Plot area < 3 nautical miles:
  geom_sf(data = coast_buffer, fill = MPA_colors[2], color = "#4CAF50", size = 0.8, alpha = 0.9) +
  
  # Plot area <50 m:
  geom_sf(data = bathy50_clipped, fill = MPA_colors[2], color = "#4CAF50", size = 0.8, alpha = 0.9) +
  
  # Plot GSAs
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Plot ISRAs
  geom_sf(data = ISRA, fill =  "#003DA5", color =  "#003DA5", size = 0.8, linetype = "solid", alpha = 0.2) +  # teal color
  
  # Plot land mask
  geom_sf(data = mask, fill = "grey80") +
  
  # Set spatial bounds
  coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
  
  # Theme and styling
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        aspect.ratio = 1,
        axis.title = element_blank())

# Display the plot
p


# export plot
enviro <- "ISRA_MPAs_PSAs" #slope, fishingEffort, 
outdir <- paste0(output_data, "/fig/Map/overlapFE_PSA")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", enviro, "2_transp.jpeg")
ggsave(p_png, p, width=10, height=10, units="cm", dpi=1800)


# 5. Plot overlap between FE and PSAs ------------------------------------------
#colRamp <- colorRampPalette(c('#FFFAFA','#FFE4E1','#FFC1C1','#FF8C69','#CD5B45','#8B3E2F', '#2B130E'))(100)
#colRamp  <- colorRampPalette(c('#E0F7FA', '#80DEEA', '#00ACC1', '#8E24AA', '#4A148C'))(100)

# Define the plot
p <- ggplot() +
  # Plot habitat raster (fishing effort)
  geom_tile(data = fishingEffort_df, aes(x = x, y = y, fill = ln_FishingEffort), alpha = 1) +
  scale_fill_viridis_c(option = "viridis", name = "fishing effort", na.value = "transparent") +
  
  # Plot PSAs with bold color and solid lines
  #geom_sf(data = PSA_Raja, fill = PSA_Rajac, color = "darkblue", size = 5, linetype = "solid", alpha = 0.2) +
  #geom_sf(data = PSA_Sca, fill = PSA_Scac, color = "darkorange", size = 5, linetype = "solid", alpha = 0.2) +
  
  # Plot GSAs
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Plot land mask
  geom_sf(data = mask, fill = "grey80") +
  
  # Apply color scale for fishing effort
  #scale_fill_gradientn(name = "fishingEffort", colors = colRamp, na.value = "transparent") +
  
  # Set spatial bounds
  coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
  
  # Theme and styling
  theme_bw() +
  #theme(panel.grid = element_blank(),
  #      legend.position = "right",
  #      legend.box = "vertical",
  #      aspect.ratio = 1)
  
  #If you dont want legend:
  theme(panel.grid = element_blank(),
     legend.position = "none",     # Remove legend
      aspect.ratio = 1,
     axis.title = element_blank())
# Display the plot
p

# export plot
enviro <- "FE_PSAs" #slope, fishingEffort, 
outdir <- paste0(output_data, "/fig/Map/overlapFE_PSA")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", enviro, "2.jpeg")
ggsave(p_png, p, width=10, height=10, units="cm", dpi=1800)




# 6. Check change in fishing effort in relation to bathymetry----------------------
# Extract bathymetry values at fishing effort locations


fishing_effort_bathy <- raster::extract(bathy, FE_withinGSA_df[, c("x", "y")])
# Add bathymetry values to fishing effort data
FE_withinGSA_df$bathy <- fishing_effort_bathy

# model relationship:
library(mgcv)
FE_withinGSA_df <- na.omit(FE_withinGSA_df) 
FE_withinGSA_df$bathy <- abs(FE_withinGSA_df$bathy)
summary(FE_withinGSA_df$bathy) 
hist(FE_withinGSA_df$ln_FishingEffort)
summary(FE_withinGSA_df$ln_FishingEffort)
ks.test(FE_withinGSA_df$ln_FishingEffort, "pnorm", mean = mean(FE_withinGSA_df$ln_FishingEffort), sd = sd(FE_withinGSA_df$ln_FishingEffort))

# GAM:
#gam_model <- gam(ln_FishingEffort ~ s(bathy), data = FE_withinGSA_df, family = gaussian(link = "identity"))
#summary(gam_model)  # Check model summary and significance
#
## Plot the GAM
#png("output/fig/Map/overlapFE_PSA/gam_bathymetry_effect.png", width = 800, height = 800, res = 120)
#plot(gam_model, se = TRUE, shade = TRUE, main = "Effect of Bathymetry on Fishing Effort")
#dev.off()

# BRT:
library(gbm)
brt_model <- gbm(ln_FishingEffort ~ bathy, data = FE_withinGSA_df, distribution = "laplace", n.trees = 1000, interaction.depth = 3)
# Plot the fitted BRT curve
png("output/fig/Map/overlapFE_PSA/brt_bathymetry_effect.png", width = 800, height = 800, res = 120)
plot(brt_model, i.var = "bathy")
dev.off()

