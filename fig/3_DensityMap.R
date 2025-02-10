# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 3. Abundance Map
#-------------------------------------------------------------------------------
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(raster)

genus <- "Dipturus" #"Raja" #"Scyliorhinus" #"Dipturus"

# 1. Set data repository and load rasters---------------------------------------
file <- paste0(temp_data, "/data_subsets/", genus, "_dataset_log_pred.csv")
data <- read.csv2(file)

#transform response variable:
#data_Sca$ln_N_km2 <- log1p(data_Sca$N_km2)


# 1.1. Bathymetry
bathy<- raster("input/gebco/Bathy.tif")
#extent(bathy) <- c(-3, 7, 35, 43)
#print(bathy)

# Convert bathy raster to data frame
bathy_df <- as.data.frame(bathy, xy = TRUE)
print(bathy_df)


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


# 1.4. GSAs
GSA <- st_read("input/GSAs/GSAs_simplified.shp")
#print(GSA)
# Filter the sf object to keep only the features where SECT_COD is "GSA06"
GSA_filtered <- GSA %>%
  filter(SECT_COD == "GSA06")
print(GSA_filtered)


# Ensure CRS matches for all spatial data
st_crs(mask) <- 4326
st_crs(GSA_filtered) <- st_crs(mask)
st_crs(Bathy_cont1) <- st_crs(mask)



# 2. Crop bathymetric contours to GSA06 ----------------------------------------
# Set the CRS of Bathy_cont1 to match GSA_filtered if needed
#st_crs(Bathy_cont1) <- st_crs(GSA_filtered)
#Bathy_cropped <- st_intersection(Bathy_cont1, GSA_filtered)
#str(Bathy_cropped)


# 3. Colour for bathymetry -----------------------------------------------------
# Create a mask if you dont want to plot all the bathymetricla range
bathy_bb <-  bathy_df$Bathy <= 5 #if you want to put a below limit: bathy_df$Bathy >= -800 &
# Apply the mask
bathy_df <- bathy_df[bathy_bb, ]
print(bathy_df)

#Create colour ramp
#color_palette_bathy <- colorRampPalette(c("lightblue", "white")) 
color_palette_bathy <- colorRampPalette(rev(c('#ecf9ff','#BFEFFF','#97C8EB','#4682B4','#264e76','#162e46')))(100)
cuts_bathy <- cut(bathy_df$Bathy, breaks = 100)
color_indices_bathy <- as.numeric(cuts_bathy) * 100 / length(levels(cuts_bathy))
bathy_df$filling_color <- color_palette_bathy[color_indices_bathy]




# 4. Make map ------------------------------------------------------------------
dataP <- data %>%
  filter(presence_absence != 0)

dataA <- data %>%
  filter(presence_absence == 0)

# Sort data by N_km2
dataP <- dataP[order(-dataP$N_km2), ]


# Create a ggplot object
p <- ggplot() +
  geom_tile(data = bathy_df, aes(x = x, y = y, fill = filling_color)) +
  
  # depth contour
  geom_sf(data = Bathy_cont1,  lwd = 0.05) +
  
  # land mask
  geom_sf(data = mask) +
  
  # Add absences with cross shape and black color
  geom_point(data = dataA, aes(x = lon, y = lat), shape = 4, color = "black", size = 0.45, alpha = 0.6) +
  
  # add presence points
  geom_jitter(data = dataP, aes(x = lon, y = lat, fill = ifelse(N_km2 == 0, NA, "#6B8E23"), #"steelblue" for skates, "orange" for catsharks "green" for Dipturus
                               size = N_km2), shape = 21, color = "black", alpha = 0.6, stroke = 0.3, width = 0.02, height = 0.02) + 
  
  # Plot GSAs
  #geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  #Set spatial bounds
  coord_sf(xlim = c(-1.5, 4.5), ylim = c(37, 42.2), expand = TRUE) +
  
  # Add scale bar
  #annotation_scale(location = "bl", width_hint = 0.2) +  
  
  # theme
  theme_bw() +
  # Directly map colors without scaling
  scale_fill_identity()+
  scale_size(range = c(0.5, 4)) +
  
  # Remove grids
  theme(panel.grid = element_blank(),
      legend.position = "right",
      legend.box = "vertical",
      aspect.ratio = 1) 
 p

# export plot
outdir <- paste0(output_data, "/fig/Map/density")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", genus, "CONT_density_Map.png")
ggsave(p_png, p, width=10, height=10, units="cm", dpi=300)

# export plot
#outdir <- paste0(output_data, "/fig/Map/density")
#if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
#p_png <- paste0(outdir, "/", genus, "SCALE_density_Map.png")
#ggsave(p_png, p, width=20, height=20, units="cm", dpi=300)



# 4. Make study map ------------------------------------------------------------------
head(data)

dataECEME <- data %>%
  filter(substr(code, 3, 3) == "_")

#dataICM<- data %>%
#  filter(substr(code, 3, 3) != "_")

# List of codes to exclude
excluded_codes <- c(
  "20200630_64500_ESP000026756_01", "20200630_64500_ESP000026756_01",
  "20200630_64500_ESP000026756_01", "20200630_64500_ESP000026756_02", 
  "20200630_64500_ESP000026756_02", "20200630_64500_ESP000026756_02", 
  "20200630_64500_ESP000026756_03", "20200805_64500_ESP000026756_01", 
  "20200805_64500_ESP000026756_01", "20200805_64500_ESP000026756_01", 
  "20200805_64500_ESP000026756_01", "20200805_64500_ESP000026756_02", 
  "20200805_64500_ESP000026756_02", "20200805_64500_ESP000026756_02", 
  "20200805_64500_ESP000026756_03", "20200805_64500_ESP000026756_03", 
  "20200805_64500_ESP000026756_03", "20200805_64500_ESP000026756_03", 
  "20200805_64500_ESP000026756_03", "20201111_64500_ESP000026756_01", 
  "20201111_64500_ESP000026756_01", "20201111_64500_ESP000026756_01", 
  "20201111_64500_ESP000026756_02", "20201111_64500_ESP000026756_02", 
  "20201111_64500_ESP000026756_03", "20201111_64500_ESP000026756_03", 
  "20210324_64500_ESP000026756_01", "20210324_64500_ESP000026756_01", 
  "20210324_64500_ESP000026756_01", "20210324_64500_ESP000026756_01", 
  "20210324_64500_ESP000026756_01", "20210324_64500_ESP000026756_02", 
  "20210324_64500_ESP000026756_02", "20210324_64500_ESP000026756_02", 
  "20210324_64500_ESP000026756_03", "20210324_64500_ESP000026756_03", 
  "20210527_64500_ESP000026756_01", "20210527_64500_ESP000026756_01", 
  "20210527_64500_ESP000026756_02", "20210527_64500_ESP000026756_02", 
  "20210527_64500_ESP000026756_03", "20210527_64500_ESP000026756_03"
)

# Filter data with both conditions
dataICM <- data %>%
  filter(substr(code, 3, 3) != "_" & !(code %in% excluded_codes))

dataPalamos <- data %>%
  filter(code %in% excluded_codes)

# Create a ggplot object 
p <- ggplot() +
  geom_tile(data = bathy_df, aes(x = x, y = y, fill = filling_color)) +
  
  # depth contour
  geom_sf(data = Bathy_cont1,  lwd = 0.1) +
  
  # land mask
  geom_sf(data = mask) +
  
  # Add Palamos data
  geom_point(data = dataPalamos, aes(x = lon, y = lat), fill = "#4cb8cf", shape = 24, color = "black", size = 1.5, alpha = 0.6) + ##f28d7c
  
  # Add ECEME data
  geom_point(data = dataECEME, aes(x = lon, y = lat), fill = "#F9B233", shape = 21, color = "black", size = 1.5, alpha = 0.6) +
  
  # add ICM data
  geom_point(data = dataICM, aes(x = lon, y = lat), fill = "#4cb8cf",  shape = 21, color = "black", size = 1.5, alpha = 0.6) +
  
  # Plot GSAs
  #geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  #Set spatial bounds
  coord_sf(xlim = c(-1.5, 4.5), ylim = c(37, 42.2), expand = TRUE) +
  
  # Add scale bar
  annotation_scale(location = "bl", width_hint = 0.2) +  
  
  # theme
  theme_bw() +
  # Directly map colors without scaling
  scale_fill_identity()+
  scale_size(range = c(1.5, 10)) +
  
  # Remove grids
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        aspect.ratio = 1) 
p

# export plot
outdir <- paste0(output_data, "/fig/Map/density")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/SurveyType_ALL_cont2.png")
ggsave(p_png, p, width=10, height=10, units="cm", dpi=300)






# 6. Zoomed out map ------------------------------------------------------------
# Create a ggplot object
p <- ggplot() +
  #geom_tile(data = raster_df, aes(x = x, y = y, fill = Bathy)) +  #X2021bottomT_mean, filling_color, Bathy, slope, layer; Use the 'layer' name for fill
  
  # depth contour
  #geom_sf(data = Bathy_cont1,  lwd = 0.05) +
  
  # land mask (if you have it, otherwise remove this line)
  geom_sf(data = mask) +
  
  # Set spatial bounds
  coord_sf(xlim = c(-4, 35), ylim = c(45.5, 30.5), expand = TRUE) +
  
  # Add scale bar
  #annotation_scale(location = "bl", width_hint = 0.2) +
  
  # Apply viridis color scale for fill
  #scale_fill_viridis(name = "Values", option = "D", na.value = "transparent") +  # Viridis palette
  #scale_fill_gradientn(name = "Bathymetry", colors = color_palette_raster, na.value = "transparent") +
  
  # theme
  theme_bw() +
  # Directly map colors without scaling
  #scale_fill_identity()+
  
  # Remove grids
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical") 

p

# export plot
outdir <- paste0(output_data, "/fig/Map/density")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/Mediterranean.jpeg")
ggsave(p_png, p, width=10, height=10, units="cm", dpi=300)

