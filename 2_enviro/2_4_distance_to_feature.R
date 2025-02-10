# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 2.4. Calculate distances from features to study points
#-------------------------------------------------------------------------------
library(geosphere)
library(sf)
library(gdistance)
library(sp)
library(raster)

# 1. Make an ocean mask --------------------------------------------------------
# Make an ocean mask so you calculate distance only without accounting for the land
bathy <- raster("input/gebco/gebco_bathy.tif")
print(bathy)

# transform 0 to NA
bathy[bathy >= 0] <- NA
print(bathy)

# Eliminate Atlantic data (xmin, xmax, ymin, ymax)
b <- as(extent(-6, 0, 42, 46), 'SpatialPolygons')
crs(b) <- crs(bathy)
# Use the mask function to apply the mask to bathy
bathy <- mask(bathy, b, inverse = TRUE)

# Eliminate Atlantic data (xmin, xmax, ymin, ymax)
c <- as(extent(5, 10, 34, 46), 'SpatialPolygons')
crs(c) <- crs(bathy)
# Use the mask function to apply the mask to bathy
bathy <- mask(bathy, c, inverse = TRUE)

plot(bathy)

# create ocean mask using the bathymetry
mask <- bathy/bathy

# change to a coarser resolution
mask_ag <- aggregate(mask, fact = 10)
print(mask_ag)
#plot(mask_ag)

# 4. Load polygon features: ------------------------------------------------------
# 4.1) Canyons
canyons <- st_read("input/global_seafloor_features/Canyons.shp")
canyons
# Clean the geometries
canyons_valid <- st_make_valid(canyons)
canyons_valid

# crop to the Mediterranean extent
mediterranean_bbox <- st_bbox(c(xmin = -6, xmax = 5, ymin = 34, ymax = 45), crs = st_crs(4326))
mediterranean_extent <- st_as_sfc(mediterranean_bbox)
cropped_canyons <- st_intersection(canyons_valid, mediterranean_extent)
cropped_canyons
#plot(st_geometry(cropped_canyons))

# eliminate accessory canyons:
atlantic <- st_bbox(c(xmin = -7, xmax = 0, ymin = 42, ymax = 48), crs = st_crs(4326))

# Convert bboxes to sfc geometry
sfc_atlantic <- st_as_sfc(atlantic)
print(sfc_atlantic)

#crop:
cropped_canyons <- st_difference(cropped_canyons, sfc_atlantic)
plot(st_geometry(cropped_canyons))
print(cropped_canyons)

# rasterize features
rcanyons <- rasterize(cropped_canyons, mask_ag)
plot(rcanyons)

# distance to the canyons
rcandist <- raster::distance(rcanyons)
rcandist <- rcandist * mask_ag
rcandist <- rcandist / 1000  # convert to km
plot(rcandist)

# save
outdir <- paste0(input_data, "/distance_to")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
writeRaster(rcandist, paste0(outdir, "/distance_canyons.nc"), format="CDF", overwrite=TRUE)
writeRaster(rcandist, filename=paste0(outdir, "/distance_canyons.tif"), overwrite=TRUE)  


# 5.2) Fans
fans <- st_read("input/global_seafloor_features/fans.shp")
fans

# Clean the geometries
fans_valid <- st_make_valid(fans)
fans_valid

# crop to the Mediterranean extent
mediterranean_bbox <- st_bbox(c(xmin = -6.25, xmax = 36.25, ymin = 30.0, ymax = 46.0), crs = st_crs(4326))
mediterranean_extent <- st_as_sfc(mediterranean_bbox)
cropped_fans <- st_intersection(fans_valid, mediterranean_extent)
cropped_fans

# 5.2) ditance to the fans
rfansdist <- raster::distance(rfans)
rfansdist <- rfansdist * mask
rfansdist <- rfansdist / 1000  # convert to km

writeRaster(rfansdist, paste0(outdir, "/distance_fans.nc"), format="CDF", overwrite=TRUE)
