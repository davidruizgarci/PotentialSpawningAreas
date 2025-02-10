# ------------------------------------------------------------------------------

# Title:  Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 7. Plot seasonal persistence PSAs
#-------------------------------------------------------------------------------
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(raster)
library(viridis)
library(ncdf4)

# 1. Load data---------------------------------------------------------------------
# 1.1. Landmask-----------------------------------------------------------------
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

# 1.2. GSAs---------------------------------------------------------------------
GSA <- st_read("input/GSAs/GSAs_simplified.shp")
#print(GSA)
# Filter the sf object to keep only the features where SECT_COD is "GSA06"
GSA_filtered <- GSA %>%
  filter(SECT_COD == "GSA06")
print(GSA_filtered)

# 1.3. Seasonal stacks ---------------------------------------------------------
genus <- "Raja"
family <- "LN_gaussian_Final2" 
type <- "_NKm2" 

#genus <- "Scyliorhinus"
#family <- "bernuilli_Final2" 
#type <- "_PA" 

path <- paste0("output/shapefiles/", genus, "/", "persistence_", genus,  ".tif")
persis <- raster(path)
print(persis)
plot(persis)

# Ensure CRS matches for all spatial data
st_crs(mask) <- 4326
st_crs(GSA_filtered) <- st_crs(mask)

# Convert to a data frame (with original coordinates)
persis_df <- as.data.frame(persis, xy = TRUE)

# Filter out rows where persistence_Raja is 0 or NA
persis_df <- persis_df %>%
  filter(!is.na(persistence_Raja) & persistence_Raja != 0)

# Filter out rows where persistence_Raja is 0 or NA
persis_df <- persis_df %>%
  filter(!is.na(persistence_Scyliorhinus) & persistence_Scyliorhinus != 0)

summary(persis_df)

# 5. Make HABITAT zoomed in map---------------------------------------------------------
# Convert persistence_Raja to a factor with specified levels
persis_df$persistence_Raja <- factor(persis_df$persistence_Raja, levels = c(0.25, 0.5, 0.75, 1))
persis_df$persistence_Scyliorhinus <- factor(persis_df$persistence_Scyliorhinus, levels = c(0.25, 0.5, 0.75, 1))

# Generate colors from the viridis palette for the specified levels
viridis_colors <- viridis_pal(option = "viridis", direction = -1)(4)  # Get 4 colors
custom_colors <- setNames(viridis_colors, c("0.25", "0.5", "0.75", "1"))  # Map to levels

# Define the plot
p <- ggplot() +
  # Plot habitat raster
  geom_tile(data = persis_df, aes(x = x, y = y, fill = persistence_Raja)) +
  scale_fill_manual(values = custom_colors, name = "Persistence") +  # Use manual scale with viridis colors
  
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
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    aspect.ratio = 1
  )

# Display the plot
p

# export plot
outdir <- paste0(output_data, "/fig/Map/Persistence")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", genus, "_persistence_scale.png")
ggsave(p_png, p, width=20, height=20, units="cm", dpi=1800)
