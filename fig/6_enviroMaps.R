# ------------------------------------------------------------------------------

# Title:  Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 6. Plot environmental variables
#-------------------------------------------------------------------------------
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(raster)
library(viridis)



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




# 2. Plot enviromental variables -----------------------------------------------
# 2.1. Slope--------------------------------------------------------------------
raster <- raster("input/emodnet/slope/slope.tif")
# plot(raster)
print(raster)

# Crop raster to the bounding box of the vector layer
cropped_raster <- crop(raster, extent(GSA_filtered))

# Mask the cropped raster to the exact geometry of the vector layer
masked_raster <- mask(cropped_raster, GSA_filtered)

# Convert bathy raster to data frame
raster_df <- as.data.frame(masked_raster, xy = TRUE)
head(raster_df)


#Create colour ramp
#Slope:
color_palette_raster <- colorRampPalette(c('#edf8fb','#ccece6','#99d8c9','#66c2a4','#41ae76','#238b45','#005824','#7f3b08'))(100)

# Create a ggplot object
p <- ggplot() +
  geom_tile(data = raster_df, aes(x = x, y = y, fill = slope)) +  #X2021bottomT_mean, filling_color, Bathy, slope, layer; Use the 'layer' name for fill
  
  # depth contour
  #geom_sf(data = Bathy_cont1,  lwd = 0.05) +
  
  # land mask (if you have it, otherwise remove this line)
  geom_sf(data = mask) +
  
  #GSA
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Set spatial bounds
  coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
  
  # Add scale bar
  #annotation_scale(location = "bl", width_hint = 0.2) +
  
  # Apply viridis color scale for fill
  #scale_fill_viridis(name = "Values", option = "D", na.value = "transparent") +  # Viridis palette
  scale_fill_gradientn(name = "Slope", colors = color_palette_raster, na.value = "transparent") +
  
  # theme
  theme_bw() +
  # Directly map colors without scaling
  #scale_fill_identity()+
  
  # Remove grids
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        aspect.ratio = 1) 

#p

# export plot
enviro <- "slope_crop" #slope, fishingEffort, BathyCont
outdir <- paste0(output_data, "/fig/Map/enviro")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", enviro, ".jpeg")
ggsave(p_png, p, width=10, height=10, units="cm", dpi=300)





# 2.2. Bathymetry---------------------------------------------------------------
raster <- raster("input/gebco/Bathy.tif")

# Crop raster to the bounding box of the vector layer
cropped_raster <- crop(raster, extent(GSA_filtered))

# Mask the cropped raster to the exact geometry of the vector layer
masked_raster <- mask(cropped_raster, GSA_filtered)

# Convert bathy raster to data frame
raster_df <- as.data.frame(masked_raster, xy = TRUE)
head(raster_df)

# For bathymetry:
# Create a mask if you dont want to plot all the bathymetric range
bathy_bb <-  raster_df$Bathy <= 5 #if you want to put a below limit: bathy_df$Bathy >= -800 &
# Apply the mask
raster_df <- raster_df[bathy_bb, ]
print(raster_df)

# colour map:
color_palette_raster <- colorRampPalette(rev(c('#ecf9ff','#BFEFFF','#97C8EB','#4682B4','#264e76','#162e46')))(100)



# 1.3. Bathymetric contour
Bathy_cont<- st_read("input/gebco/cont/gebco_contours4osm.shp")
print(Bathy_cont)

Bathy_cont$DEPTH <- as.numeric(Bathy_cont$DEPTH)
unique(Bathy_cont$DEPTH)

#Select the bathymetrical lines that you want to plot:
Bathy_cont1 <- Bathy_cont %>%
  filter(DEPTH %in% c(-100, -200, -750)) # 
unique(Bathy_cont1$DEPTH)
# Set the CRS for the raster
st_crs(Bathy_cont1) <- st_crs(mask)
print(Bathy_cont1)

# Ensure both layers have the same CRS
if (st_crs(Bathy_cont1) != st_crs(GSA_filtered)) {
  GSA_filtered <- st_transform(GSA_filtered, st_crs(Bathy_cont1))
}

# Crop Bathy_cont1 to the overlapping area with GSA_filtered
Bathy_cont1_cropped <- st_intersection(Bathy_cont1, GSA_filtered)


# Create a ggplot object
p <- ggplot() +
  geom_tile(data = raster_df, aes(x = x, y = y, fill = Bathy)) +  #X2021bottomT_mean, filling_color, Bathy, slope, layer; Use the 'layer' name for fill
  
  # depth contour
  geom_sf(data = Bathy_cont1_cropped,  lwd = 0.05) +
  
  # land mask (if you have it, otherwise remove this line)
  geom_sf(data = mask) +
  
  #GSA
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Set spatial bounds
  coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
  
  # Add scale bar
  #annotation_scale(location = "bl", width_hint = 0.2) +
  
  # Apply viridis color scale for fill
  #scale_fill_viridis(name = "Values", option = "D", na.value = "transparent") +  # Viridis palette
  scale_fill_gradientn(name = "BAT", colors = color_palette_raster, na.value = "transparent") +
  
  # theme
  theme_bw() +
  # Directly map colors without scaling
  #scale_fill_identity()+
  
  # Remove grids
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        aspect.ratio = 1) 

#p

# export plot
enviro <- "bathy_crop" #slope, fishingEffort, BathyCont
outdir <- paste0(output_data, "/fig/Map/enviro")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", enviro, ".jpeg")
ggsave(p_png, p, width=10, height=10, units="cm", dpi=300)







# 2.3. Sea bottom temperature---------------------------------------------------
raster <- raster("input/Mean_enviro/2021bottomT_mean.tif")

# Crop raster to the bounding box of the vector layer
cropped_raster <- crop(raster, extent(GSA_filtered))

# Mask the cropped raster to the exact geometry of the vector layer
masked_raster <- mask(cropped_raster, GSA_filtered)

# Convert bathy raster to data frame
raster_df <- as.data.frame(masked_raster, xy = TRUE)
head(raster_df)


# colour map:
color_palette_raster <- colorRampPalette(c('#6CA6CD','#B0E2FF','#FFFFE0','#FFFACD','#FFE4B5','#FFDAB9','#FF7F50','#FF6347','#FF4500','#8B3626'))(100)


# Create a ggplot object
p <- ggplot() +
  geom_tile(data = raster_df, aes(x = x, y = y, fill = X2021bottomT_mean)) +  #X2021bottomT_mean, filling_color, Bathy, slope, layer; Use the 'layer' name for fill
  
  # land mask (if you have it, otherwise remove this line)
  geom_sf(data = mask) +
  
  #GSA
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Set spatial bounds
  coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
  
  # Add scale bar
  #annotation_scale(location = "bl", width_hint = 0.2) +
  
  # Apply viridis color scale for fill
  scale_fill_gradientn(name = "SBT", colors = color_palette_raster, na.value = "transparent") +
  
  # theme
  theme_bw() +
  # Directly map colors without scaling
  #scale_fill_identity()+
  
  # Remove grids
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        aspect.ratio = 1) 

#p

# export plot
enviro <- "SBT_crop" #slope, fishingEffort, BathyCont
outdir <- paste0(output_data, "/fig/Map/enviro")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", enviro, ".jpeg")
ggsave(p_png, p, width=10, height=10, units="cm", dpi=1800)







# 2.4. Oxygen-------------------------------------------------------------------
raster <- raster("input/Mean_enviro/2021o2_mean.tif")
raster <- log1p(raster) #Only for fishingEffort

# Crop raster to the bounding box of the vector layer
cropped_raster <- crop(raster, extent(GSA_filtered))

# Mask the cropped raster to the exact geometry of the vector layer
masked_raster <- mask(cropped_raster, GSA_filtered)

# Convert bathy raster to data frame
raster_df <- as.data.frame(masked_raster, xy = TRUE)
head(raster_df)

# colour map: viridis
# Create a ggplot object
p <- ggplot() +
  geom_tile(data = raster_df, aes(x = x, y = y, fill = layer)) +  #X2021bottomT_mean, filling_color, Bathy, slope, layer; Use the 'layer' name for fill
  

  # land mask (if you have it, otherwise remove this line)
  geom_sf(data = mask) +
  
  #GSA
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Set spatial bounds
  coord_sf(xlim = c(-1.5, 4.5), ylim = c(37, 42.2), expand = TRUE) +
  
  # Add scale bar
  #annotation_scale(location = "bl", width_hint = 0.2) +
  
  # Apply viridis color scale for fill
  scale_fill_viridis(name = "Values", option = "D", na.value = "transparent") +  # Viridis palette
  
  # theme
  theme_bw() +
  # Directly map colors without scaling
  #scale_fill_identity()+
  
  # Remove grids
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        aspect.ratio = 1) 

#p

# export plot
enviro <- "oxygen_crop" 
outdir <- paste0(output_data, "/fig/Map/enviro")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", enviro, ".jpeg")
ggsave(p_png, p, width=10, height=10, units="cm", dpi=1800)





# 2.5. Fishing effort-----------------------------------------------------------
fishingEffort <- raster("input/gfwr/summarydata/high_resolution/FishingEffort.tif")
fishingEffort

# Create a new raster with the desired resolution (0.042 degrees)
res <- 0.042
fishingEffort_resampled <- raster(extent(fishingEffort), resolution = res, crs = crs(fishingEffort))

# Resample using bilinear interpolation
fishingEffort <- resample(fishingEffort, fishingEffort_resampled, method = "bilinear")

# resample:
masked_raster_resampled <- resample(masked_raster, fishingEffort, method = "bilinear")

# Crop using the mask:
fishingEffort_cropped <- raster::mask(fishingEffort, masked_raster_resampled)
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
library(tidyr)
FE_withinGSA_df <- fishingEffort_clipped_sf %>%
  mutate(FishingEffort = replace_na(FishingEffort, 0))

# Revert the log1p() of the response variable
fishingEffort_df$ln_FishingEffort <- log1p(fishingEffort_df$FishingEffort)

# Verify the result
summary(fishingEffort_df)


# colour map: viridis
# Create a ggplot object
p <- ggplot() +
  geom_tile(data = fishingEffort_df, aes(x = x, y = y, fill = ln_FishingEffort)) +  
  
  # land mask (if you have it, otherwise remove this line)
  geom_sf(data = mask) +
  
  #GSA
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Set spatial bounds
  coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
  
  # Add scale bar
  #annotation_scale(location = "bl", width_hint = 0.2) +
  
  # Apply viridis color scale for fill
  scale_fill_viridis(name = "Values", option = "D", na.value = "transparent") +  # Viridis palette
  
  # theme
  theme_bw() +
  # Directly map colors without scaling
  #scale_fill_identity()+
  
  # Remove grids
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        aspect.ratio = 1) 

p

# export plot
enviro <- "fishingEffort_crop" 
outdir <- paste0(output_data, "/fig/Map/enviro")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", enviro, ".jpeg")
ggsave(p_png, p, width=10, height=10, units="cm", dpi=1800)






# 2.6. Sea bottom temperature---------------------------------------------------
raster <- raster("input/Mean_enviro/2021so_mean.tif")

# Crop raster to the bounding box of the vector layer
cropped_raster <- crop(raster, extent(GSA_filtered))

# Mask the cropped raster to the exact geometry of the vector layer
masked_raster <- mask(cropped_raster, GSA_filtered)

# Convert bathy raster to data frame
raster_df <- as.data.frame(masked_raster, xy = TRUE)
head(raster_df)


# colour map:
color_palette_raster <- colorRampPalette(c(
  "#F0F9FF",  # Very pale pastel blue
  "#CCE7F5",  # Soft sky blue
  "#99D3EB",  # Gentle blue
  "#66BFE0",  # Dark muted cyan (low salinity, darkest blue)
  #"#E0F2E0",  # Very light green (transition point, lightest green)
  "#C8E6C9",  # Pale green
  "#A9D9AC",  # Light mint green
  "#89CBA7",  # Pastel green
  "#33ABA6"   # Soft teal (high salinity, darkest green)
))(100)

# Create a ggplot object
p <- ggplot() +
  geom_tile(data = raster_df, aes(x = x, y = y, fill = X2021so_mean)) +  #X2021bottomT_mean, filling_color, Bathy, slope, layer; Use the 'layer' name for fill
  
  # land mask (if you have it, otherwise remove this line)
  geom_sf(data = mask) +
  
  #GSA
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Set spatial bounds
  coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
  
  # Add scale bar
  #annotation_scale(location = "bl", width_hint = 0.2) +
  
  # Apply viridis color scale for fill
  scale_fill_gradientn(name = "SAL", colors = color_palette_raster, na.value = "transparent") +
  
  # theme
  theme_bw() +
  # Directly map colors without scaling
  #scale_fill_identity()+
  
  # Remove grids
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        aspect.ratio = 1) 

#p

# export plot
enviro <- "SAL_crop" #slope, fishingEffort, BathyCont
outdir <- paste0(output_data, "/fig/Map/enviro")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", enviro, ".jpeg")
ggsave(p_png, p, width=10, height=10, units="cm", dpi=1800)


