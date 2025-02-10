# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 5. Overall study maps
#-------------------------------------------------------------------------------
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(raster)


# 1.Load rasters----------------------------------------------------------------
# 1.1. Bathymetry
bathy<- raster("input/gebco/Bathymetry.tif")

# Convert bathy raster to data frame
bathy_df <- as.data.frame(bathy, xy = TRUE)
print(bathy_df)


# 1.2. Landmask
mask <- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
#print(mask)
mask <- st_transform(mask, crs = 4326)
# crop it:
e <- c(-6, 37, 24, 47)
e <- extent(e)
bbox <- st_as_sfc(st_bbox(e))
# Set the CRS of bbox to match the mask
st_crs(bbox) <- st_crs(mask) 
#Crop the mask using the bounding box
mask <- st_intersection(mask, bbox)
print(mask)
#plot(mask)

# 1.3. GSAs
GSA <- st_read("input/GSAs/GSAs_simplified.shp")


# 2. Colour for bathymetry -----------------------------------------------------
# Filter bathymetry values (e.g., focus on values <= 5m if needed)
bathy_df <- bathy_df[bathy_df$Bathy <= 5, ]

# Ensure no missing values for bathymetry
bathy_df <- na.omit(bathy_df)

#Create colour ramp
#color_palette_bathy <- colorRampPalette(c("lightblue", "white")) 
color_palette_bathy <- colorRampPalette(rev(c('#ecf9ff','#BFEFFF','#97C8EB','#4682B4','#264e76','#162e46')))(100)
cuts_bathy <- cut(bathy_df$Bathy, breaks = 100)
color_indices_bathy <- as.numeric(cuts_bathy) * 100 / length(levels(cuts_bathy))
bathy_df$filling_color <- color_palette_bathy[color_indices_bathy]

# Ensure CRS matches for all spatial data
st_crs(mask) <- 4326
st_crs(GSA) <- st_crs(mask)

# 3. Zoomed out map ------------------------------------------------------------
# Create a ggplot object
p <- ggplot() +
  #geom_tile(data = bathy_df, aes(x = x, y = y, fill = Bathymetry)) +
  
  # land mask (if you have it, otherwise remove this line)
  geom_sf(data = mask, color = "black", size = 3) +
  
  # Plot GSAs
  geom_sf(data = GSA, fill = NA, color = "black", size = 3, linetype = "dashed") +
  
  # Set spatial bounds
  coord_sf(xlim = c(-4, 35), ylim = c(45.5, 34), expand = TRUE) +
  
  # Add scale bar
  #annotation_scale(location = "bl", width_hint = 0.2) +
  
  # Smooth color gradient for bathymetry
  #scale_fill_gradientn(
  #  colors = c('#162e46', '#264e76', '#4682B4', '#97C8EB', '#BFEFFF', '#ecf9ff'),
  #  values = scales::rescale(c(-5000, -3000, -1000, -500, -100, 0)),
  #  breaks = c(-5000, -3000, -1000, -500, -100, 0),
  #  labels = c("-5000", "-3000", "-1000", "-500", "-100", "0"),
  #  name = "Bathymetry (m)"
  #) +
  
  # theme
  theme_bw() +
  # Remove grids
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical") 

p

# export plot
outdir <- paste0(output_data, "/fig/Map/density")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/Mediterranean_closeup.jpeg")
ggsave(p_png, p, width=20, height=20, units="cm", dpi=300)






# 4. Make zoom out map ------------------------------------------------------
pacman::p_load(dplyr, data.table, rnaturalearth, rnaturalearthdata, 
               ggplot2, raster, terra, tidyr, stringr, gridExtra, 
               plotly, sf, ggshadow, ggforce, giscoR, install = FALSE)

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

# adding other layers to the base shadow
g2 <- g +
  
  # Add the ocean mask and fill with white
  geom_sf(data = ocean, fill = "white", color = NA) +
  # add grid
  geom_sf(data = grid_crp, 
          colour = "grey85", 
          linewidth = .15) +
  # add sea-turtle SSM points
  #geom_sf(data = ssm_sf, size = 0.15, color = "deepskyblue3") +
  # add 3D globe land
  geom_sf(data = world, 
          #fill = "#FFE4B2",
          colour = "grey35",
          linewidth = .2) +
  # theme
  theme_void()

print(g2)

# export plot
outdir <- paste0(output_data, "/fig/Map")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/_global_Map.png")
ggsave(p_png, g2, width=17, height=17, units="cm", dpi=300)
