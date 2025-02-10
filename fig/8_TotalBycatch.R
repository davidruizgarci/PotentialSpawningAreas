# ------------------------------------------------------------------------------

# Title:  Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 8. Plot total bycatch rates
#-------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(raster)
library(viridis)
library(beepr)

# 1. Load rasters------------------------------------------------------------------
# 1.1. Landmask
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


# 1.2. GSAs
GSA <- st_read("input/GSAs/GSAs_simplified.shp")
#print(GSA)
# Filter the sf object to keep only the features where SECT_COD is "GSA06"
GSA_filtered <- GSA %>%
  filter(SECT_COD == "GSA06")
print(GSA_filtered)


# Ensure CRS matches for all spatial data
st_crs(mask) <- 4326
st_crs(GSA_filtered) <- st_crs(mask)
#st_crs(Bathy_cont1) <- st_crs(mask)


# 2. Add all BPUE values across days--------------------------------------------
# Raja
genus <- "Raja" 
family <- "LN_gaussian_Final2" 
type <- "_NKm2" 
mod_code <- "brt"
dataset <- "ALL" 

# Scyliorhinus
genus <- "Scyliorhinus" 
family <- "bernuilli_Final2" 
type <- "_PA" 
mod_code <- "brt"
dataset <- "ALL" 

# Load total bycatch files
path <- paste0("output/", mod_code, "/hurdle_stacks/", genus, "/total_bycatch/processed/bycatch_sum.tif")
bycatch <- raster(path)
plot(bycatch)


# 3. Prepare raster-------------------------------------------------------------
bathy<- raster("input/gebco/Bathy.tif")
print(bathy)
# plot(bathy)

# Create a mask if you dont want to plot all the bathymetrical range
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
bathy_mask_resampled <- resample(bathy_mask, bycatch, method = "bilinear")

# Apply the mask to the habitat raster
print(bathy_mask_resampled)
#head(aggregated_df)


# Inspect the cropped dataframe
bathy_resampled <- resample(x = bathy_mask_resampled, y = bycatch, method = "bilinear")
crs(bycatch) <- crs(bathy_mask_resampled)
crs(bathy_resampled) <- crs(bathy_mask_resampled)
print(bycatch)
print(bathy_resampled)
aggregated_raster_cropped <- raster::mask(bycatch, bathy_resampled)
#plot(aggregated_raster_cropped)

# Convert raster to data frame
aggregated_raster_cropped_df <- as.data.frame(aggregated_raster_cropped, xy = TRUE)
colnames(aggregated_raster_cropped_df) <- c("x", "y", "bycatch")
summary(aggregated_raster_cropped_df)

# Crop to GSA:
# Convert habitat_df to an sf object (using original x, y coordinates)
habitat_sf <- st_as_sf(aggregated_raster_cropped_df, coords = c("x", "y"), crs = st_crs(mask), remove = FALSE)
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
  dplyr::select(x, y, bycatch)  # Keep the original x, y, and habitat columns

# Check for any remaining NAs and clean up the data
habitat_clipped_df <- habitat_clipped_df %>%
  filter(!is.na(bycatch))

# Verify the result
summary(habitat_clipped_df)
#habitat_clipped_df$LNbycatch <- log1p(habitat_clipped_df$bycatch)


# 4. Plot results---------------------------------------------------------------
p <- ggplot() +
  # Plot habitat raster
  geom_tile(data = habitat_clipped_df, aes(x = x, y = y, fill = bycatch)) + #LN
  scale_fill_viridis_c(option = "viridis", name = "N") +
  
  # Plot land mask
  geom_sf(data = mask, fill = "grey80", color = "grey60") +
  
  # Plot GSAs
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
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

p

# export plot
outdir <- paste0(output_data, "/fig/Map/", paste0(genus, type, "_", family))
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/total_bycatch_dailyOverall_scale_final.png") #LN
ggsave(p_png, p, width=20, height=20, units="cm", dpi=1800)






# 5. Plot total bycatch rates---------------------------------------------------
# 5.1. Skates-------------------------------------------------------------------
genus <- "Raja"

# Total bycatch
bycatch_skates <- data.frame(
  Metier = c("Hake", "Norway Lobster", "Red Shrimp"),
  Bycatch = c(207498, 16591, 9150)
)
total_bycatch <- sum(bycatch_skates$Bycatch)

# Donut
donut_bycatch <-ggplot(bycatch_skates, aes(x = 2, y = Bycatch, fill = Metier)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(1, 2.5) +
  theme_void() +
  labs(title = "Bycatch Proportions - Skates", subtitle = paste("Total Bycatch: ", total_bycatch)) +
  theme(legend.position = "right") +
  scale_fill_brewer(palette = "Set3")

# Total trawl-------------------------------------------------------------------
trawled_area_skates <- data.frame(
  Metier = c("Hake", "Norway Lobster", "Red Shrimp"),
  Trawled_Area = c(23372, 7288, 2689)
)

total_trawled_area <- sum(trawled_area_skates$Trawled_Area)

# Donut
donut_area <-ggplot(trawled_area_skates, aes(x = 2, y = Trawled_Area, fill = Metier)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(1, 2.5) +
  theme_void() +
  labs(title = "Trawled Area Proportions - Skates", subtitle = paste("Total Area: ", total_trawled_area, "km²")) +
  theme(legend.position = "right") +
  scale_fill_brewer(palette = "Set3")

# Bycatch rate per km2 and metier-----------------------------------------------
bycatch_per_km2 <- data.frame(
  Metier = c("Hake", "Norway Lobster", "Red Shrimp"),
  N_km2 = c(8.88, 2.28, 3.40))

# Bars
bar_nkm2 <-ggplot(bycatch_per_km2, aes(x = Metier, y = N_km2, fill = Metier)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  theme_minimal() +
  labs(
    title = "Bycatch per km² - Skates",
    x = "Metier",
    y = "N/km²"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")

# Combine-----------------------------------------------------------------------
library(patchwork)
p <- (donut_bycatch | donut_area) / bar_nkm2 +
  plot_annotation(title = "Summary of Bycatch, Trawled Area, and Bycatch per km²")
p
# export plot
outdir <- paste0(output_data, "/fig/Map/bycatch")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", genus, "_bycatch.png")
ggsave(p_png, p, width=20, height=20, units="cm", dpi=300)



# 5.2. Catshark-------------------------------------------------------------------
genus <- "Scyliorhinus"

# Total bycatch
bycatch_catshark <- data.frame(
  Metier = c("Hake", "Norway Lobster", "Red Shrimp"),
  Bycatch = c(152491,	41615,	34829)
)
total_bycatch <- sum(bycatch_catshark$Bycatch)

# Donut
donut_bycatch <-ggplot(bycatch_catshark, aes(x = 2, y = Bycatch, fill = Metier)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(1, 2.5) +
  theme_void() +
  labs(title = "Bycatch Proportions - Catshark", subtitle = paste("Total Bycatch: ", total_bycatch)) +
  theme(legend.position = "right") +
  scale_fill_brewer(palette = "Set3")

# Bycatch rate per km2 and metier-----------------------------------------------
bycatch_per_km2 <- data.frame(
  Metier = c("Hake", "Norway Lobster", "Red Shrimp"),
  N_km2 = c(6.52,	5.71,	12.95))

# Bars
bar_nkm2 <-ggplot(bycatch_per_km2, aes(x = Metier, y = N_km2, fill = Metier)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  theme_minimal() +
  labs(
    title = "Bycatch per km² - Skates",
    x = "Metier",
    y = "N/km²"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")

# Combine-----------------------------------------------------------------------
p <- (donut_bycatch) / bar_nkm2 +
  plot_annotation(title = "Summary of Bycatch, Trawled Area, and Bycatch per km²")
p

# export plot
outdir <- paste0(output_data, "/fig/Map/bycatch")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", genus, "_bycatch.png")
ggsave(p_png, p, width=20, height=20, units="cm", dpi=300)
