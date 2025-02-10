# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 2. Fit prediction Map
#-------------------------------------------------------------------------------
library(dplyr)
library(sf)
library(smoothr)
library(ggplot2)
library(ggspatial)
library(raster)
library(sp)
library(beepr)

season <- "2021" #2021, spring, winter, summer, fall

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

# 1. Set data repository and load rasters---------------------------------------

# 1.1. Bathymetry---------------------------------------------------------------
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



# 1.2. Landmask-----------------------------------------------------------------
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


# 1.3. Bathymetric contour------------------------------------------------------
#Bathy_cont<- st_read("input/gebco/cont/gebco_contours4osm.shp")
#print(Bathy_cont)
#
#Bathy_cont$DEPTH <- as.numeric(Bathy_cont$DEPTH)
#unique(Bathy_cont$DEPTH)
#
##Select the bathymetrical lines that you want to plot:
#Bathy_cont1 <- Bathy_cont %>%
#  filter(DEPTH %in% c(-750)) #-100, -200, 
#unique(Bathy_cont1$DEPTH)
## Set the CRS for the raster
#st_crs(Bathy_cont1) <- st_crs(mask)
#print(Bathy_cont1)

# 1.4. GSAs---------------------------------------------------------------------
GSA <- st_read("input/GSAs/GSAs_simplified.shp")
#print(GSA)
# Filter the sf object to keep only the features where SECT_COD is "GSA06"
GSA_filtered <- GSA %>%
  filter(SECT_COD == "GSA06")
print(GSA_filtered)


# 1.5. Predicted habitat--------------------------------------------------------
path <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_hurdle_pred_median.tif")
#path <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/01/20210101_Scyliorhinus_brt_pred.tif")
habitat <- raster(path)
#habitat <- calc(habitat, function(x) 10^x)
print(habitat)


# Ensure CRS matches for all spatial data
st_crs(mask) <- 4326
st_crs(GSA_filtered) <- st_crs(mask)
#st_crs(Bathy_cont1) <- st_crs(mask)




# 2. Crop habitat to bathy 700 m---------------------------------------------------
# Filter the values between -20 and -700 and set values outside the range to NA
bathy_filtered <- calc(bathy, function(x) {
  x[x > -20 | x < -700] <- NA  # Set values outside the range to NA
  return(x)
})

# Assign a value of 1 to the remaining (non-NA) values
bathy_mask <- calc(bathy_filtered, function(x) {
  x[!is.na(x)] <- 1
  return(x)
})


# Resample bathy_mask to match the resolution of habitat
bathy_mask_resampled <- resample(bathy_mask, habitat, method = "bilinear")

# Apply the mask to the habitat raster
habitat_cropped <- raster::mask(habitat, bathy_mask_resampled)
#plot(habitat_cropped)

# Convert raster to data frame
habitat_df <- as.data.frame(habitat_cropped, xy = TRUE)
colnames(habitat_df) <- c("x", "y", "habitat")
summary(habitat_df)


# 3. Crop habitat to GSA06 area ------------------------------------------------
# Convert habitat_df to an sf object (using original x, y coordinates)
habitat_sf <- st_as_sf(habitat_df, coords = c("x", "y"), crs = st_crs(mask), remove = FALSE)
#plot(habitat_sf)

# Ensure CRS compatibility between habitat_sf and GSA_filtered
if (st_crs(habitat_sf) != st_crs(GSA_filtered)) {
  habitat_sf <- st_transform(habitat_sf, crs = st_crs(GSA_filtered))
}

# Perform the intersection
habitat_clipped_sf <- st_intersection(habitat_sf, GSA_filtered)

# Convert the clipped sf object back to a data frame (with original coordinates)
# Ensure that we retain the original x and y coordinates
habitat_clipped_df <- as.data.frame(habitat_clipped_sf) %>%
  dplyr::select(x, y, habitat)  # Keep the original x, y, and habitat columns

# Check for any remaining NAs and clean up the data
habitat_clipped_df <- habitat_clipped_df %>%
  filter(!is.na(habitat))

# Revert the log1p() of the response variable
habitat_clipped_df$habitat <- expm1(habitat_clipped_df$habitat)

# Verify the result
summary(habitat_clipped_df)

# Step 1: Calculate the 75th percentile of the habitat values
percentile_90 <- quantile(habitat_clipped_df$habitat, probs = 0.9, na.rm = TRUE)
#percentile_50 <- quantile(habitat_clipped_df$habitat, probs = 0.5, na.rm = TRUE)

# or load them if already created:
# PSA_Raja <- st_read("output/shapefiles/Raja_outer_boundary.shp")
# PSA_Sca <- st_read("output/shapefiles/Scyliorhinushurdle_contour_90_percentile_filtered.shp")
# plot(PSA_Sca)
# plot(PSA_Raja)


# 4. Crop bathymetric contours to GSA06 ------------------------------------------
# Set the CRS of Bathy_cont1 to match GSA_filtered if needed
#st_crs(Bathy_cont1) <- st_crs(GSA_filtered)
#Bathy_cropped <- st_intersection(Bathy_cont1, GSA_filtered)
#str(Bathy_cropped)


# 5. Make HABITAT zoomed in map---------------------------------------------------------
# Define the plot
p <- ggplot() +
  # Plot habitat raster
  geom_tile(data = habitat_clipped_df, aes(x = x, y = y, fill = habitat)) +
  scale_fill_viridis_c(option = "viridis", name = "Ln(density) N/km2") +
  
  # Plot land mask
  geom_sf(data = mask, fill = "grey80", color = "grey60") +
  
  # Plot GSAs
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Add a white contour line for areas above the 90th percentile of habitat
  geom_contour(data = habitat_clipped_df, aes(x = x, y = y, z = habitat), 
               breaks = percentile_90, color = "white", linewidth = 0.4) +
  
  # Plot PSAs with bold color and solid lines
  #geom_sf(data = PSA_Raja, fill = NA, color = "white", linewidth = 0.4, linetype = "solid", alpha = 1) +
  #geom_sf(data = merged_polygons_final, fill = NA, color = "white", linewidth = 0.4, linetype = "solid", alpha = 1) +
  
  # Plot bathymetric contours
  #geom_sf(data = Bathy_cropped, color = "black", size = 0.1, alpha = 0.5) +
  
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
  
  # Title and labels
  #ggtitle(paste(genus, "   Model:", mod_code, "\n", season)) +
  #xlab("Longitude") +
  #ylab("Latitude")

p

# export plot
outdir <- paste0(output_data, "/fig/Map/", paste0(genus, type, "_", family))
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", season, "_", mod_code, "_", paste0(genus, type, "_", family), "PSA_hurdle_habitat_Map_vars.png")
ggsave(p_png, p, width=20, height=20, units="cm", dpi=1800)


# 6. extract PSA contour as a polygon-------------------------------------------
# 6.1. Adjustments on habitat for extraction------------------------------------
# Convert habitat data to a raster object
habitat_raster <- rasterFromXYZ(habitat_clipped_df[, c("x", "y", "habitat")])
crs(habitat_raster) <- st_crs(habitat_raster)$proj4string
plot(habitat_raster)

#Save it:
output_path <- paste0("output/shapefiles/", genus, "/", season, "_", genus,  "_hurdle_habitat_raster.tif")  # Windows with double backslashes
writeRaster(habitat_raster, filename = output_path, format = "GTiff", overwrite = TRUE)

# Set the CRS to WGS 84 (EPSG:4326)
crs(habitat_raster) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Define the bounding box coordinates (in the format of xmin, ymin, xmax, ymax)
bbox_coords <- c(0.827, 40.606, 0.891, 40.687)

# Create a polygon from the bounding box coordinates
bounding_box <- st_polygon(list(rbind(
  c(bbox_coords[1], bbox_coords[2]),  # Bottom left
  c(bbox_coords[3], bbox_coords[2]),  # Bottom right
  c(bbox_coords[3], bbox_coords[4]),  # Top right
  c(bbox_coords[1], bbox_coords[4]),  # Top left
  c(bbox_coords[1], bbox_coords[2])   # Closing the box
)))

# Convert to sf object
bbox_sf <- st_sfc(bounding_box, crs = 4326)  # Set CRS to WGS 84

# Transform the bounding box to match the raster's CRS
# First, get the raster's CRS
raster_crs <- crs(habitat_raster)

# Transform the bounding box to the raster's CRS
bbox_transformed <- st_transform(bbox_sf, crs = raster_crs)

# Create a mask using the raster and the transformed bounding box
# We need to create a raster mask
mask_raster <- mask(habitat_raster, as(raster::extent(st_bbox(bbox_transformed)), "SpatialPolygons"))

# Optionally, set masked areas to NA
masked_habitat_raster <- habitat_raster
masked_habitat_raster[mask_raster[] != 0] <- NA  # Set the areas within the mask to NA

plot(masked_habitat_raster)

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

bathy_mask_resampled <- resample(bathy_mask, masked_habitat_raster, method = "bilinear")
#bathy_mask_resampled <- crop(bathy_mask_resampled, e)
#plot(bathy_mask_resampled)
#masked_habitat_raster <- crop(masked_habitat_raster, e)
#plot(masked_habitat_raster)
masked_habitat_raster_crop <- raster::mask(masked_habitat_raster, bathy_mask_resampled)
plot(masked_habitat_raster_crop)

# 6.2. Raster of the potential spawning area------------------------------------
# Calculate the 90th percentile of the habitat values
percentile_90

# Convert the clipped sf object back to a data frame (with original coordinates)
# Convert raster to data frame
masked_habitat_raster_crop <- as.data.frame(masked_habitat_raster_crop, xy = TRUE)
colnames(masked_habitat_raster_crop) <- c("x", "y", "habitat")
summary(masked_habitat_raster_crop)

# Filter the DataFrame to keep values above the 90th percentile
filtered_data <- masked_habitat_raster_crop %>% 
  filter(habitat > percentile_90)

# Get the extent of the filtered data
ext <- extent(min(filtered_data$x), max(filtered_data$x), min(filtered_data$y), max(filtered_data$y))
# Create a raster with the specified resolution and the extent of the filtered data
raster_output <- raster(ext, res = 0.042)  # Adjust the resolution as needed

# Create a new column for values in filtered_data corresponding to raster cells
# Assign values to the raster at corresponding coordinates
values(raster_output) <- NA  # Initialize raster values to NA

# Assign habitat values to raster cells
coordinates(filtered_data) <- ~x + y
raster_output <- rasterize(filtered_data, raster_output, field = "habitat", fun = mean)  # Using mean or another aggregation function
plot(raster_output)

# Make a mask:
raster_output[!is.na(raster_output)] <- 1
crs(raster_output) <- st_crs(raster_output)$proj4string

plot(raster_output)

#Save it:
output_path <- paste0("output/shapefiles/", genus, "/", season, "_", genus, "_hurdle_MASK_habitat_raster.tif")  # Windows with double backslashes
writeRaster(raster_output, filename = output_path, format = "GTiff", overwrite = TRUE)


# 6.3. Transform PSAs to polygons-----------------------------------------------
# Step 1: Clump the raster cells to group connected areas
clumped_raster <- clump(raster_output, directions = 8)  # Use 8 directions for diagonal connectivity
plot(clumped_raster)
# Reproject the filtered polygons to WGS 84 (EPSG:4326)
#crs(clumped_raster) <- "+proj=longlat +datum=WGS84 +no_defs"

# Step 2: Convert the clumped raster to polygons, merging connected areas
polygon_sf <- st_as_sf(rasterToPolygons(clumped_raster, dissolve = TRUE))
plot(polygon_sf)

# Step 3: Calculate the area of each polygon (in km²)
# Calculate the area of each polygon in the original coordinate system's units (square degrees)
polygon_sf <- polygon_sf %>% 
  mutate(area_native_units = as.numeric(st_area(.)))  # Area in square degrees if using EPSG:4326
# View(polygon_sf)
# Approximate area threshold for filtering (change as needed based on CRS)
area_threshold_native_units <- 0.003  # Approximate 70 km² in square degrees

# Filter polygons based on the threshold in native units
polygon_sf_filtered <- polygon_sf %>% 
  filter(area_native_units >= area_threshold_native_units)

# Plot the filtered polygons
plot(st_geometry(polygon_sf_filtered))
polygon_sf_filtered <- st_set_crs(polygon_sf_filtered, 4326)  # Assume it's WGS 84

# Step 4: Smooth
library(rmapshaper)  # Optional for simplification
library(smoothr)     # For smoothing

# Method 1:
outer_boundary <- st_as_sf(polygon_sf_filtered)
smoothed_boundary <- smooth(outer_boundary, method = "ksmooth", smoothness = 1)
plot(smoothed_boundary)

# Method 2:
# Apply smoothing to the polygon shape (e.g., using the 'chaikin' method)
p_smooth_chaikin <- smooth(polygon_sf_filtered, method = "chaikin", refinements = 1) #
plot(st_geometry(p_smooth_chaikin))

# Buffer the polygons with a small distance (e.g., 0.001)
#p_smooth_chaikin_proj <- st_transform(smoothed_boundary, crs = 32631)  # Example for UTM zone 31N
#p_buffered_proj <- st_buffer(p_smooth_chaikin_proj, dist = 1000)  # 1000 meters
#merged_polygons_proj <- st_union(p_buffered_proj)
#merged_polygons_final <- st_transform(merged_polygons_proj, crs = 4326)
#plot(merged_polygons_final, col = 'lightgreen', main = 'Final Merged Polygons')


# Step 5: Save the filtered polygons as a shapefile
output_shapefile <- paste0("output/shapefiles/", season, "_", genus, "_ksmooth_hurdle_contour_90_percentile_filtered.shp")
if (!dir.exists(dirname(output_shapefile))) dir.create(dirname(output_shapefile), recursive = TRUE)
st_write(smoothed_boundary, output_shapefile, append = FALSE)


# 7. repeat HABITAT zoomed in map---------------------------------------------------------
# Define the plot
p <- ggplot() +
  # Plot habitat raster
  geom_tile(data = habitat_clipped_df, aes(x = x, y = y, fill = habitat)) +
  scale_fill_viridis_c(option = "viridis", name = "Ln(density) N/km2") +
  
  # Plot land mask
  geom_sf(data = mask, fill = "grey80", color = "grey60") +
  
  # Plot GSAs
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Plot PSAs with bold color and solid lines
  #geom_sf(data = p_smooth_chaikin, fill = NA, color = "white", linewidth = 0.4, linetype = "solid", alpha = 1) +
  geom_sf(data = smoothed_boundary, fill = NA, color = "white", linewidth = 0.4, linetype = "solid", alpha = 1) +
  
  # Plot bathymetric contours
  #geom_sf(data = Bathy_cropped, color = "black", size = 0.1, alpha = 0.5) +
  
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

# Title and labels
#ggtitle(paste(genus, "   Model:", mod_code, "\n", season)) +
#xlab("Longitude") +
#ylab("Latitude")

p

# export plot
outdir <- paste0(output_data, "/fig/Map/", paste0(genus, type, "_", family))
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", season, "_", mod_code, "_", paste0(genus, type, "_", family), "PSA_hurdle_habitat_Map_vars.png")
ggsave(p_png, p, width=20, height=20, units="cm", dpi=1800)

# save dataframe:
write.csv2(habitat_clipped_df, paste0("output/fig/Map/", genus, type, "_", family, "/", season, ".csv"), row.names = FALSE)


# 9. Load error map ------------------------------------------------------------
# 8.1. 95 CI map

path <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "hurdle_pred_CIR_median.tif")
error <- raster(path)
#error <- calc(error, expm1)
print(error)
#plot(error)



# 7. Crop error to bathy 800 m------------------------------------------------
# Filter the values between -50 and -600 and set values outside the range to NA
bathy_filtered <- calc(bathy, function(x) {
  x[x > -20 | x < -700] <- NA  # Set values outside the range to NA
  return(x)
})

# Assign a value of 1 to the remaining (non-NA) values
bathy_mask <- calc(bathy_filtered, function(x) {
  x[!is.na(x)] <- 1
  return(x)
})

# Resample bathy_mask to match the resolution of habitat
bathy_mask_resampled <- resample(bathy_mask, error, method = "bilinear")

# Apply the mask to the habitat raster
error_cropped <- mask(error, bathy_mask_resampled)
#plot(error_cropped)

# Convert raster to data frame
error_df <- as.data.frame(error_cropped, xy = TRUE)
colnames(error_df) <- c("x", "y", "error")
summary(error_df)




# Crop habitat to GSA06 area ------------------------------------------------
# Convert habitat_df to an sf object (using original x, y coordinates)
error_sf <- st_as_sf(error_df, coords = c("x", "y"), crs = st_crs(mask), remove = FALSE)
#plot(error_sf)

# Ensure CRS compatibility between error_sf and GSA_filtered
if (st_crs(error_sf) != st_crs(GSA_filtered)) {
  error_sf <- st_transform(error_sf, crs = st_crs(GSA_filtered))
}

# Perform the intersection
error_clipped_sf <- st_intersection(error_sf, GSA_filtered)

# Convert the clipped sf object back to a data frame (with original coordinates)
# Ensure that we retain the original x and y coordinates
error_clipped_df <- as.data.frame(error_clipped_sf) %>%
  dplyr::select(x, y, error)  # Keep the original x, y, and error columns

# Check for any remaining NAs and clean up the data
error_clipped_df <- error_clipped_df %>%
  filter(!is.na(error))

# Revert the log1p() of the response variable
error_clipped_df$error <- expm1(error_clipped_df$error)

# Verify the result
summary(error_clipped_df)



# 5. Make 95% CI zoomed in map---------------------------------------------------------
#colors <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026", "#000000")
colors <- c("#FFFFB2", "#FECC5C", "#FDBF6F", "#F03B20", "#BD0026", "#8B0000")
# Create a function to generate a color palette
color_palette <- colorRampPalette(colors)
# Generate a gradient with a specified number of colors
num_colors <- 100  # Adjust this to the number of colors you need
gradient_colors <- color_palette(num_colors)


# Define the plot
p <- ggplot() +
  # Plot error raster
  geom_tile(data = error_clipped_df, aes(x = x, y = y, fill = error)) +
  scale_fill_gradientn(colors = gradient_colors, name = "95% CI") +
  
  # Plot land mask
  geom_sf(data = mask, fill = "grey80") +
  
  # Plot GSAs
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Plot bathymetric contours
  #geom_sf(data = Bathy_cropped, color = "black", size = 0.1, alpha = 0.5) +
  
  # Set spatial bounds (adjust these to fit your data)
  coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
  
  # Add scale bar (optional)
  annotation_scale(location = "bl", width_hint = 0.2) + 
  
  # Theme settings
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        aspect.ratio = 1) 

# Title and labels
#ggtitle(paste(genus, "   Model:", mod_code, "\n", season)) +
#xlab("Longitude") +
#ylab("Latitude")

p

# export plot
outdir <- paste0(output_data, "/fig/Map/", paste0(genus, type, "_", family))
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/hurdle_EXP_CIR_Map_scale_vars.png")
ggsave(p_png, p, width=20, height=20, units="cm", dpi=1800)




# 9. Include it in a zoom out map ------------------------------------------------------
pacman::p_load(dplyr, data.table, rnaturalearth, rnaturalearthdata, 
               ggplot2, raster, terra, tidyr, stringr, gridExtra, 
               plotly, sf, ggshadow, ggforce, giscoR, cowplot, install = FALSE)

# Custom global ortohraphic proyection from Western-Mediterranean
ortho_crs <-'+proj=ortho +lat_0=20 +lon_0=0.5 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs'

# world coastlines
world_poly <- gisco_get_coastallines(year = "2016", epsg = "4326", resolution = "10")

# global graticule
grid <- st_graticule()

# ocean mask 
ocean <- st_point(x = c(0,0)) |>
  st_buffer(dist = 6371000) |> # Planet earth radius (m)
  st_sfc(crs = ortho_crs)
# plot(ocean)

# Select visible area and project
world <- world_poly |>
  st_intersection(st_transform(ocean, 4326)) |>
  st_transform(crs = ortho_crs) # 
# plot(world)

# delete grid trough continents to create a clean grid
grid_crp <- st_difference(grid, st_union(world_poly))

# select visible area
grid_crp <- st_intersection(grid_crp, st_transform(ocean, 4326)) |>
  st_transform(crs = ortho_crs)
# plot(grid_crp)

# cover globe limit into df - datframe
ocean_df <- st_cast(ocean, "LINESTRING") |> st_coordinates() |> as.data.frame()

# build shadow 
ggplot() + 
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.05,
                shadowsize = 1.5) +
  coord_sf() +
  theme_void()

# add more shadows
g <- ggplot() +
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.06,
                shadowsize = 1.5) +
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.02,
                shadowsize = 1) +
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.01,
                shadowsize = .5)

# Convert habitat_clipped_df to an sf object
habitat_clipped_sf <- st_as_sf(habitat_clipped_df, coords = c("x", "y"), crs = 4326)  # Assuming it's in WGS84 (EPSG:4326)

# Transform to the orthographic CRS (ortho_crs)
habitat_clipped_transformed <- st_transform(habitat_clipped_sf, crs = ortho_crs)

# Get the bounding box of the habitat data
bbox <- st_bbox(habitat_clipped_transformed)

# Step 3: Modify the original 'g2' plot to include this habitat data
g2 <- g + 
  # Add ocean, world, and grid layers (same as before)
  geom_sf(data = ocean, fill = "white", color = NA) +
  geom_sf(data = grid_crp, colour = "grey85", linewidth = .15) +
  geom_sf(data = world, colour = "grey35", linewidth = .2) +
  
  # Add the habitat raster as a tile layer
  geom_raster(data = habitat_clipped_df, aes(x = x, y = y, fill = habitat)) +
  
  # Add the appropriate color scale for habitat
  scale_fill_viridis_c(option = "viridis", name = "density (N/km2)") +
  
  # Add other layers (mask, GSA, contours, etc.)
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  geom_contour(data = habitat_clipped_df, aes(x = x, y = y, z = habitat), 
               breaks = percentile_75, color = "white", size = 0.4) +
  
  #Set the zoom level using the bounding box limits
coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
         ylim = c(bbox["ymin"], bbox["ymax"]),
         expand = TRUE) +
  
  # Final theme settings
  theme_void()

# Print the final map with the habitat data
print(g2)


# export plot
outdir <- paste0(output_data, "/fig/Map")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/_global_Map.png")
ggsave(p_png, g2, width=17, height=17, units="cm", dpi=300)



## 7. Load ERROR map ------------------------------------------------------------
## SD and SE
#
#path <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_pred_SD_median.tif")
#SD <- raster(path)
#print(SD)
#path <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_pred_SE_median.tif")
#SE <- raster(path)
#print(SE)
#
#
#
## 8. Crop SD and SE to bathy 800 m------------------------------------------------
## Filter the values between -50 and -600 and set values outside the range to NA
#bathy_filtered <- calc(bathy, function(x) {
#  x[x > -20 | x < -600] <- NA  # Set values outside the range to NA
#  return(x)
#})
#
## Assign a value of 1 to the remaining (non-NA) values
#bathy_mask <- calc(bathy_filtered, function(x) {
#  x[!is.na(x)] <- 1
#  return(x)
#})
#
## Resample bathy_mask to match the resolution of habitat
#bathy_mask_resampled <- resample(bathy_mask, SD, method = "bilinear")
#
## Apply the mask to the habitat raster
#SD_cropped <- mask(SD, bathy_mask_resampled)
#SE_cropped <- mask(SE, bathy_mask_resampled)
#
## Convert raster to data frame
#SD_df <- as.data.frame(SD_cropped, xy = TRUE)
#colnames(SD_df) <- c("x", "y", "SD")
#summary(SD_df)
#
#SE_df <- as.data.frame(SE_cropped, xy = TRUE)
#colnames(SE_df) <- c("x", "y", "SE")
#summary(SE_df)
#
#
## 3. Crop habitat to GSA06 area ------------------------------------------------
## Convert habitat_df to an sf object (using original x, y coordinates)
#SD_sf <- st_as_sf(SD_df, coords = c("x", "y"), crs = st_crs(mask), remove = FALSE)
#SE_sf <- st_as_sf(SE_df, coords = c("x", "y"), crs = st_crs(mask), remove = FALSE)
#
## Ensure CRS compatibility between error_sf and GSA_filtered
#if (st_crs(SD_sf) != st_crs(GSA_filtered)) {
#  SD_sf <- st_transform(SD_sf, crs = st_crs(GSA_filtered))
#}
#if (st_crs(SE_sf) != st_crs(GSA_filtered)) {
#  SE_sf <- st_transform(SE_sf, crs = st_crs(GSA_filtered))
#}
#
## Perform the intersection
#SD_clipped_sf <- st_intersection(SD_sf, GSA_filtered)
#SE_clipped_sf <- st_intersection(SE_sf, GSA_filtered)
#
## Convert the clipped sf object back to a data frame (with original coordinates)
## Ensure that we retain the original x and y coordinates
#SD_clipped_df <- as.data.frame(SD_clipped_sf) %>%
#  dplyr::select(x, y, SD)  # Keep the original x, y, and error columns
#SE_clipped_df <- as.data.frame(SE_clipped_sf) %>%
#  dplyr::select(x, y, SE)  # Keep the original x, y, and error columns
#
## Check for any remaining NAs and clean up the data
#SD_clipped_df <- SD_clipped_df %>%
#  filter(!is.na(SD))
#SE_clipped_df <- SE_clipped_df %>%
#  filter(!is.na(SE))
#
## Verify the result
#summary(SD_clipped_df)
#summary(SE_clipped_df)
#
## 5. Make SD zoomed in map---------------------------------------------------------
##colors <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026", "#000000")
#colors <- c("#FFFFB2", "#FECC5C", "#FDBF6F", "#F03B20", "#BD0026", "#8B0000")
## Create a function to generate a color palette
#color_palette <- colorRampPalette(colors)
## Generate a gradient with a specified number of colors
#num_colors <- 100  # Adjust this to the number of colors you need
#gradient_colors <- color_palette(num_colors)
#
#
## Define the plot
#p <- ggplot() +
#  # Plot error raster
#  geom_tile(data = SD_clipped_df, aes(x = x, y = y, fill = SD)) +
#  scale_fill_gradientn(colors = gradient_colors, name = "SD") +
#  
#  # Plot land mask
#  geom_sf(data = mask, fill = "grey80") +
#  
#  # Plot GSAs
#  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
#  
#  # Plot bathymetric contours
#  #geom_sf(data = Bathy_cropped, color = "black", size = 0.1, alpha = 0.5) +
#  
#  # Set spatial bounds (adjust these to fit your data)
#  coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
#  
#  # Add scale bar (optional)
#  annotation_scale(location = "bl", width_hint = 0.2) + 
#  
#  # Theme settings
#  theme_bw() +
#  theme(panel.grid = element_blank(),
#        legend.position = "right",
#        legend.box = "vertical",
#        aspect.ratio = 1) 
#
## Title and labels
##ggtitle(paste(genus, "   Model:", mod_code, "\n", season)) +
##xlab("Longitude") +
##ylab("Latitude")
#
#p
#
## export plot
#outdir <- paste0(output_data, "/fig/Map/", paste0(genus, type, "_", family))
#if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
#p_png <- paste0(outdir, "/", mod_code, "_", paste0(genus, type, "_", family), "_SD_Map_scale_vars.png")
#ggsave(p_png, p, width=20, height=20, units="cm", dpi=1800)
#
#
#
## Define the plot
#p <- ggplot() +
#  # Plot error raster
#  geom_tile(data = SE_clipped_df, aes(x = x, y = y, fill = SE)) +
#  scale_fill_gradientn(colors = gradient_colors, name = "SE") +
#  
#  # Plot land mask
#  geom_sf(data = mask, fill = "grey80") +
#  
#  # Plot GSAs
#  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
#  
#  # Plot bathymetric contours
#  #geom_sf(data = Bathy_cropped, color = "black", size = 0.1, alpha = 0.5) +
#  
#  # Set spatial bounds (adjust these to fit your data)
#  coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
#  
#  # Add scale bar (optional)
#  annotation_scale(location = "bl", width_hint = 0.2) + 
#  
#  # Theme settings
#  theme_bw() +
#  theme(panel.grid = element_blank(),
#        legend.position = "right",
#        legend.box = "vertical",
#        aspect.ratio = 1) 
#
## Title and labels
##ggtitle(paste(genus, "   Model:", mod_code, "\n", season)) +
##xlab("Longitude") +
##ylab("Latitude")
#
#p
#
## export plot
#outdir <- paste0(output_data, "/fig/Map/", paste0(genus, type, "_", family))
#if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
#p_png <- paste0(outdir, "/", mod_code, "_", paste0(genus, type, "_", family), "_SE_Map_scale_vars.png")
#ggsave(p_png, p, width=15, height=22, units="cm", dpi=1200)
#