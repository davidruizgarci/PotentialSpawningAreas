# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 5.1. Calculate parameters within and outised potensial spawining areas (PSAs)
#-------------------------------------------------------------------------------
library(terra)
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(sp)



# 1. Load areas-----------------------------------------------------------------
# 1.1. PSAs:

# Scyliorhinus
genus <- "Scyliorhinus" 
family <- "bernuilli_Final2" 
type <- "_PA" 
mod_code <- "brt"
dataset <- "ALL" 
season <- "2021"

path <- paste0("output/shapefiles/", genus, "/Final_", season, "_", genus, "_hurdle_MASK_habitat_raster.tif")  
PSA_scy <- raster(path)
plot(PSA_scy)
print(PSA_scy)

# Raja
genus <- "Raja" 
family <- "LN_gaussian_Final2" 
type <- "_NKm2" 
mod_code <- "brt"
dataset <- "ALL" 

path <- paste0("output/shapefiles/", genus, "/Final_", season, "_", genus, "_hurdle_MASK_habitat_raster.tif")  
PSA_raja <- raster(path)
plot(PSA_raja)
print(PSA_raja)

# All area 
path <- paste0("output/shapefiles/", genus, "/", season, "_", genus,  "_hurdle_habitat_raster.tif") 
habitat <- raster(path)
print(habitat)
plot(habitat)



# 2. Crop habitat to PSAs ------------------------------------------------------

# RUN ONLY ONCE:
#PSA_maks_resampled <- resample(PSA_scy, habitat, method = "bilinear")
#cropped_habitat <- mask(habitat, PSA_maks_resampled, inverse = TRUE)
#
#PSA_maks_resampled <- resample(PSA_raja, habitat, method = "bilinear")
#cropped_habitat <- mask(cropped_habitat, PSA_maks_resampled, inverse = TRUE)
#plot(cropped_habitat)
#
## Make a mask:
#cropped_habitat[!is.na(cropped_habitat)] <- 1
#plot(cropped_habitat)
#
##Save it:
##output_path <- paste0("output/shapefiles/", season, "_ALL_hurdle_MASK_rest_habitat_raster.tif")  # Windows with double backslashes
##writeRaster(cropped_habitat, filename = output_path, format = "GTiff", overwrite = TRUE)

# 3. Calculate overlap (IOR) ---------------------------------------------------
# The following raster were made in QGIS
# Calculate the intersection (cells where both rasters are 1)
# Step 1: Overlap
path <- paste0("output/shapefiles/OverlapPSAs.tif")  #Overlapping area
overlap <- raster(path)
plot(overlap)
print(overlap)

# To count non-NA and non-zero cells
overlap_count_non_zero <- sum(!is.na(overlap[])) # This will give non-NA cells
print(overlap_count_non_zero)  # Non-NA cells

# Step 2: Union:
# Load PSAs merged polygon
path  <- paste0("output/shapefiles/2021_ALL2_hurdle_contour_90_percentile_filtered.shp") # Union area
PSAs_all <- st_read(path)

# rasterise:
extent_overlap <- extent(overlap)  # Get the extent from overlap
resolution_overlap <- res(overlap)  # Get the resolution from overlap
crs_overlap <- crs(overlap)  # Get CRS from overlap

# Create an empty raster with the same extent, resolution, and CRS as 'overlap'
rasterized_PSA <- raster(xmn = extent_overlap[1], xmx = extent_overlap[2], 
                         ymn = extent_overlap[3], ymx = extent_overlap[4], 
                         resolution = resolution_overlap, 
                         crs = crs_overlap)

# Rasterize the polygons from the SimpleFeatureCollection (PSAs_all)
# You can specify what field you want to use as the raster value (e.g., 'clumps' or 'ar_ntv_')
rasterized_PSA <- rasterize(PSAs_all, rasterized_PSA, field = "clumps", background = NA)
plot(rasterized_PSA)

# To count non-NA and non-zero cells
union_count_non_zero <- sum(!is.na(rasterized_PSA[])) # This will give non-NA cells
print(union_count_non_zero)  # Non-NA cells

# Compute Index of Overlap (IOR)
IOR <- (overlap_count_non_zero / union_count_non_zero) * 100
IOR


## 3. Add PSAs-----------------------------------------------------------------
## Load areas:
## nonPSAs:
#path <- paste0("output/shapefiles/", season, "_ALL_hurdle_MASK_rest_habitat_raster.tif")
#nonPSA <- raster(path)
#plot(nonPSA)

#PSAs:
# Resample PSA_raja and PSA_scy to match the habitat raster's extent and resolution
#PSA_raja_resampled <- resample(PSA_raja, habitat, method = "bilinear")
#PSA_scy_resampled <- resample(PSA_scy, habitat, method = "bilinear")

## remove NAs
#PSA_raja_noNA <- calc(PSA_raja_resampled, fun = function(x) ifelse(is.na(x), 0, x))
#PSA_scy_noNA <- calc(PSA_scy_resampled, fun = function(x) ifelse(is.na(x), 0, x))

#print(PSA_raja_noNA)
#print(PSA_scy_noNA)
#plot(PSA_raja_noNA)
#plot(PSA_scy_noNA)

## Add PSAs
#PSA <- PSA_raja_noNA + PSA_scy_noNA

# Load PSAs merged polygon
path  <- paste0("output/shapefiles/2021_ALL2_hurdle_contour_90_percentile_filtered.shp")
PSAs_all <- st_read(path)
#plot(PSAs_all)

# Crop PSAs raster -------------------------------------------------------------
print(PSAs_all)
print(habitat)

# Ensure CRS of PSAs_all matches the habitat raster's CRS (WGS84)
PSAs_all <- st_transform(PSAs_all, crs = st_crs(habitat))
# Create an empty raster with the same extent and resolution as the habitat raster
mask_raster <- raster(habitat)
# Rasterize the polygons in PSAs_all (create a binary mask)
# Set the field to 1 to create a binary mask (inside the polygons = 1, outside = NA)
PSAs <- rasterize(PSAs_all, mask_raster, field = 1)
plot(PSAs)

# Crop non PSAs raster ---------------------------------------------------------
print(PSAs)
print(habitat)

#resample:
PSAs_resempled <- resample(PSAs, habitat, method = "bilinear")

# Reclassify PSAs_resempled: set 1 to NA (areas to exclude) and NA to 1 (areas to keep)
PSAs_inverted <- calc(PSAs_resempled, fun = function(x) ifelse(is.na(x), 1, NA))

# Now mask the habitat raster using the inverted PSAs raster
nonPSA <- mask(habitat, PSAs_inverted)

# Plot the result to check
plot(nonPSA)

# Resample both to the same extent:
PSAs <- resample(PSAs, nonPSA, method = "bilinear")

# transform all to 1 values:
PSAs <- calc(PSAs, function(x) {
  x[!is.na(x)] <- 1
  return(x)
})

print(PSAs)
plot(PSAs)

#Save PSAs:
output_path <- paste0("output/shapefiles/", season, "_ALL_PSAs.tif")  
writeRaster(PSAs, filename = output_path, format = "GTiff", overwrite = TRUE)


# Crop nonPSAs to GSA ----------------------------------------------------------
# Convert raster to data frame
habitat_df <- as.data.frame(nonPSA, xy = TRUE)
colnames(habitat_df) <- c("x", "y", "habitat")
summary(habitat_df)


# 3. Crop habitat to GSA06 area ------------------------------------------------
# Load GSAs
GSA <- st_read("input/GSAs/GSAs_simplified.shp")
#print(GSA)
# Filter the sf object to keep only the features where SECT_COD is "GSA06"
GSA_filtered <- GSA %>%
  filter(SECT_COD == "GSA06")
print(GSA_filtered)

# Convert habitat_df to an sf object (using original x, y coordinates)
habitat_sf <- st_as_sf(habitat_df, coords = c("x", "y"), crs = st_crs(mask), remove = FALSE)
# Assign CRS to habitat_sf (assume it's WGS 84 based on bounding box and PSAs)
habitat_sf <- st_set_crs(habitat_sf, 4326)

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
head(habitat_clipped_df)

print(PSAs)
# Create an empty raster template
raster_template <- raster(nrows = nrow(PSAs), ncols = ncol(PSAs))

# Set the extent and CRS of the template to match PSA
extent(raster_template) <- extent(PSAs)
crs(raster_template) <- crs(PSAs)

# Rasterize the data frame to the raster template
nonPSA <- rasterize(habitat_clipped_df[, c("x", "y")], 
                            raster_template, 
                            field = habitat_clipped_df$habitat, 
                            fun = mean)  # Use mean to handle overlaps

# transform all to 1 values:
nonPSA <- calc(nonPSA, function(x) {
  x[!is.na(x)] <- 1
  return(x)
})

# Check the output
print(nonPSA)
plot(nonPSA)

#Save nonPSAs:
output_path <- paste0("output/shapefiles/", season, "_ALL_nonPSAs.tif")  
writeRaster(nonPSA, filename = output_path, format = "GTiff", overwrite = TRUE)



# 4. Overlap Fishing Effort ----------------------------------------------------
# Load fishing effort:
fishingEffort <- raster("input/gfwr/summarydata/high_resolution/FishingEffort.tif")
fishingEffort

# Create a new raster with the desired resolution (0.042 degrees)
res <- 0.042
fishingEffort_resampled <- raster(extent(fishingEffort), resolution = res, crs = crs(fishingEffort))

# Resample using bilinear interpolation
fishingEffort <- resample(fishingEffort, fishingEffort_resampled, method = "bilinear")

# 4.1. Crop fishing effort to within and to outside the PSAs--------------------
fishingEffort_cropped <- crop(fishingEffort, extent(PSAs))
extent(fishingEffort_cropped) <- extent(PSAs)
FE_withinPSA <- mask(fishingEffort_cropped, PSAs)
plot(FE_withinPSA)

# 4.2. Crop fishing effort to within and to outside the PSAs- - - - - - - - - - 
fishingEffort_cropped <- crop(fishingEffort, extent(nonPSA))
extent(fishingEffort_cropped) <- extent(nonPSA)
FE_outsidePSA <- mask(fishingEffort_cropped, nonPSA)
plot(FE_outsidePSA)

# 4.3. Transfor to dataframes- - - - - - - - - - - - - - - - - - - - - - - - - - 
print(FE_withinPSA)
print(FE_outsidePSA)
print(fishingEffort)

# Convert to data frame
df_withinPSA <- as.data.frame(FE_withinPSA, xy = TRUE, na.rm = TRUE)  # Include xy coordinates and remove NAs
df_outsidePSA <- as.data.frame(FE_outsidePSA, xy = TRUE, na.rm = TRUE)  # Include xy coordinates and remove NAs
df_FE <- as.data.frame(fishingEffort, xy = TRUE, na.rm = TRUE)  # Include xy coordinates and remove NAs

df_withinPSA$type <- "within"
df_outsidePSA$type <- "outside"
df_outsidePSA$type <- "all"

df_withinPSA$logFE <- log1p(df_withinPSA$FishingEffort)
df_outsidePSA$logFE <- log1p(df_outsidePSA$FishingEffort)
df_FE$logFE <- log1p(df_FE$FishingEffort)


# Remove rows with zero values from df_withinPSA
df_withinPSA <- df_withinPSA %>%
  filter(FishingEffort > 0, logFE > 0)

# Remove rows with zero values from df_outsidePSA
df_outsidePSA <- df_outsidePSA %>%
  filter(FishingEffort > 0, logFE > 0)

# Remove rows with zero values from df_FE
df_FE <- df_FE %>%
  filter(FishingEffort > 0, logFE > 0)


# Print the first few rows of the data frames
print(head(df_withinPSA))
print(head(df_outsidePSA))
print(head(df_FE))
summary(df_withinPSA)
summary(df_outsidePSA)
summary(df_FE)

# 4.4. Violin plot showing fishing effort inside and outside the PSAs-----------
library(smplot2)
#available colors:
#"blue", "crimson", "green", "purple", "orange", "skyblue", "pink", "limegreen", "lightpurple", "brown", "red", "lightbrown", "asparagus", "viridian", "darkred", "lightblue","wine", "yellow", "lightgreen"

p1 <- ggplot(data = df_FE, mapping = aes(x = type, y = logFE, fill = type)) +
  sm_raincloud(data = df_withinPSA, position = position_nudge(x = -1),
               show.legend = FALSE,
               point.params = list(size = 2, shape = 21,
                                   color = 'transparent', 
                                   show.legend = TRUE,
                                   alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = 1.04, seed = 10,
                                                                          jitter.width = 0.06))) +
  sm_raincloud(data = df_outsidePSA, which_side = 'left',
               show.legend = FALSE,
               position = position_nudge(x = 0),
               point.params = list(size = 2, shape = 21,
                                   show.legend = TRUE,
                                   color = 'transparent', alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = -0.06, seed = 10,
                                                                          jitter.width = 0.06))) +
  # color groups
  scale_fill_manual(values = sm_color("purple", "yellow")) +
  # y lab
  ylab('FishingEffort') +
  # Positioning Y-axis values on the right side
  scale_y_continuous(position = "right") +
  # theme 
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(angle = 90),
        axis.line.y = element_blank(),   # Remove the Y-axis line
        axis.ticks.y = element_blank(),  # Remove Y-axis ticks
        panel.background = element_blank(),  # Make panel background transparent
        plot.background = element_blank(),   # Make plot background transparent
        panel.grid.major.y = element_blank(),  # Remove horizontal major grid lines
        panel.grid.minor.y = element_blank())  # Remove horizontal minor grid lines



print(p1)


# export plot
outdir <- paste0(output_data, "/fig/raincloud")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", "ALL_FE.png")
ggsave(p_png, p1, width=17, height=10, units="cm", dpi=300)




# 4.4. Statistically compare groups ----------------------------------------------
# Shapiro-Wilk normality test
shapiro_test_within <- shapiro.test(df_withinPSA$logFE)
shapiro_test_outside <- shapiro.test(df_outsidePSA$logFE)

# Print results
print(shapiro_test_within)
print(shapiro_test_outside)

# If both groups are normally distributed, use t-test
if (shapiro_test_within$p.value > 0.05 && shapiro_test_outside$p.value > 0.05) {
  t_test_result <- t.test(df_withinPSA$logFE, df_outsidePSA$logFE)
  print(t_test_result)
} else {
  # If not normally distributed, use Mann-Whitney U test
  wilcox_test_result <- wilcox.test(df_withinPSA$logFE, df_outsidePSA$logFE)
  print(wilcox_test_result)
}





# 5. Overlap with MPAs----------------------------------------------------------
# 5.1. Load MPAs shapefile -----------------------------------------------------
#Load cropped MPAs to study area:
MPA_cropped_clipped <- st_read("input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_Valid_Cropped.shp")
atributes <- read.csv("input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_Attributes_20240531.csv")
MPA_merged <- MPA_cropped_clipped %>%
  left_join(atributes, by = c("SITE_ID" = "site_id"))
print(MPA_merged)

# remove not real MPAs:
summary(MPA_merged)
MPA_merged <- subset(MPA_merged, !(SITE_ID %in% c('AIDZA1', 'AIESP1', 'AIFRA1', 'AIITA1', 'AIHSMA4', 'AIESP3')))

# Load modified bottom_trawl regulations:
regu <- read.csv2("input/MPAs/ProtectedSeas/MPA_merged_check.csv")

# import to MPA vector:
head(regu)
head(MPA_merged)

# Merge the dataframes by SITE_NA
MPA_merged <- merge(MPA_merged, regu[, c("SITE_NA", "new_bottom_trawl")], by = "SITE_NA", all.x = TRUE)


# 5.2. Calculate overlap -------------------------------------------------------
# List of unique values for new_bottom_trawl
MPA_levels <- unique(MPA_merged$new_bottom_trawl)

# Initialize an empty data frame to store the results
overlap_results <- data.frame(MPA_level = integer(), percent_overlap = numeric(), stringsAsFactors = FALSE)

# Loop over each MPA level (1, 2, 3, 4, 5)
library(exactextractr)
# Ensure raster values are numeric and handle NA
values(PSAs) <- as.numeric(values(PSAs))

# Loop through MPA levels
for (level in MPA_levels) {
  # Filter polygons for the current MPA level
  # level = 1
  fishing_mpas_filtered <- MPA_merged %>%
    filter(new_bottom_trawl == level)
  
  # Skip levels without polygons
  if (nrow(fishing_mpas_filtered) == 0) {
    next
  }
  
  # Reproject polygons to the raster CRS
  fishing_mpas_projected <- st_transform(fishing_mpas_filtered, crs = st_crs(PSAs))
  
  # Convert bounding boxes to sf geometries and check overlap
  fishing_bbox <- st_as_sfc(st_bbox(fishing_mpas_projected))
  raster_bbox <- st_as_sfc(st_bbox(PSAs))
  
  # Check if bounding boxes overlap
  if (!st_intersects(fishing_bbox, raster_bbox, sparse = FALSE)[1, 1]) {
    warning(paste("No overlap for level", level))
    next
  }
  
  
  # Calculate total valid raster area
  valid_cells <- sum(values(PSAs) == 1, na.rm = TRUE)
  cell_area <- prod(res(PSAs))  # Calculate area of one cell
  total_valid_area <- valid_cells * cell_area
  
  # Extract overlap areas using exact_extract
  overlap_areas <- exact_extract(PSAs, fishing_mpas_projected, function(values, coverage_fraction) {
    sum(coverage_fraction[values == 1], na.rm = TRUE)
  })
  
  # Calculate the total overlap area
  total_overlap_area <- sum(overlap_areas) * cell_area
  
  # Compute percentage overlap
  if (total_valid_area > 0) {
    percent_overlap <- (total_overlap_area / total_valid_area) * 100
  } else {
    percent_overlap <- NA
  }
  
  # Store results
  overlap_results <- rbind(overlap_results, data.frame(MPA_level = as.integer(level), percent_overlap = percent_overlap))
}

# View final results
print(overlap_results)




# 5.3. Plot particular Marine Protected Areas ----------------------------------
fishing_mpas <- MPA_merged %>%
  filter(new_bottom_trawl == 1) #!=
print(fishing_mpas)

ggplot(data = fishing_mpas) +
  geom_sf(fill = "skyblue", color = "darkblue") +  # Fill color and border color
  labs(title = "Distribution of Marine Protected Areas",
       subtitle = "Marine MPAs where MARINE == 1",
       caption = "Source: Protected Seas") +
  theme_minimal()  # Use a minimal theme


# 5.4. Make a bar chart --------------------------------------------------------
overlap_results$MPA_level <- factor(overlap_results$MPA_level, levels = rev(c(0, 1, 2)))
levels(overlap_results$MPA_level)

# Now, create the bar plot
p_MPA <- ggplot(overlap_results, aes(x = "", y = percent_overlap, fill = MPA_level)) + 
  geom_bar(stat = "identity", width = 0.5) +  # Use stat = "identity" for exact values
  scale_fill_manual(values = c("0" = "blue", "1" = "green", "2" = "yellow")) + #"1" = "blue", "2" = "green", "3" = "yellow", "4" = "orange", "5" = "red"
  coord_flip() +  # Flip the bar to make it horizontal
  labs(
    title = "Bar Plot with Color Breaks",
    x = "",
    y = "Percentage"
  ) +
  scale_y_continuous(limits = c(0, 100)) +  # Ensure the bar maxes out at 100%
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12)) +  # Customize x-axis text size
  theme(axis.text.y = element_text(size = 12))   # Customize y-axis text size

p_MPA

# export plot
outdir <- paste0(output_data, "/fig/MPAbarchart")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", "ALL_MPA_trawl.png")
ggsave(p_png, p_MPA, width=17, height=10, units="cm", dpi=300)





# 6. Overlap with ISRAs --------------------------------------------------------
# 6.1. Load ISRAs shapefile -----------------------------------------------------
ISRA <- st_read("input/isra_allregions/isra_allregions.shp")

# Filter the dataset to remove "recommended buffers"
ISRA_filtered <- ISRA %>%
  filter(Name != "recommended buffer")
head(ISRA_filtered)

# Print raster and check CRS
print(PSAs)
st_crs(ISRA_filtered)  # Check the CRS of ISRA_filtered (polygon dataset)
crs(PSAs)  # Check the CRS of the raster
# Reproject raster to match the polygon CRS
PSA_reprojected <- projectRaster(PSAs, crs = st_crs(ISRA_filtered)$proj4string)

# 6.2. Calculate overall overlap -----------------------------------------------
# Create a mask for the raster where values are not NA
PSA_masked <- PSA_reprojected
PSA_masked[is.na(PSA_masked)] <- 0  # Set NA values to 0 or another irrelevant value

# Calculate the total area of valid raster cells (where the value is 1)
valid_cells <- sum(values(PSA_masked) == 1)
cell_area <- prod(res(PSA_masked))  # Calculate cell area in square meters
total_valid_area <- valid_cells * cell_area

# Extract coverage fraction for valid cells overlapping with ISRA polygons
overlap_areas <- exact_extract(PSA_masked, ISRA_filtered, function(values, coverage_fraction) {
  sum(coverage_fraction[values == 1])  # Only consider valid raster areas (values == 1)
})

# Count the number of valid overlapping cells
valid_overlap_cells <- sum(overlap_areas)

# Calculate the percentage of valid raster cells that are overlapped by ISRA polygons
percent_overlap <- (valid_overlap_cells / valid_cells) * 100

# Print the result
print(paste("Percentage of valid raster cells overlapped by ISRA polygons:", percent_overlap))

# 6.3. Calculate overlap with each ISRA ----------------------------------------
#Loop over each ISRA feature and calculate the overlap
# Initialize an empty data frame to store results
#overlap_results_isra <- data.frame(ISRA_code = character(), percent_overlap = numeric(), stringsAsFactors = FALSE)
#
#for (i in 1:nrow(ISRA_filtered)) {
#  
#  # Extract the current ISRA feature (polygon)
#  isra_feature <- ISRA_filtered[i, ]
#  
#  # Calculate the area of overlap between the ISRA polygons and the raster
#  overlap_areas <- exact_extract(PSA_masked, isra_feature, function(values, coverage_fraction) {
#    sum(coverage_fraction[values == 1])  # Only consider valid raster areas (where values == 1)
#  })
#  
#  # Calculate the total overlap area in square meters
#  total_overlap_area <- sum(overlap_areas) * cell_area
#  
#  # Calculate the percentage of overlap
#  percent_overlap <- (total_overlap_area / total_valid_area) * 100
#  
#  # Add the result to the results data frame
#  overlap_results_isra <- rbind(overlap_results_isra, data.frame(ISRA_code = isra_feature$code, percent_overlap = percent_overlap))
#}
#
## View the final results
#head(overlap_results_isra)


# 6.4. Make a bar chart --------------------------------------------------------
# Create a data frame with Overlap and Non-Overlap categories
overlap_data <- data.frame(
  Category = c("Overlap", "Non-Overlap"),
  Percentage = c(percent_overlap, 100 - percent_overlap)
)

# Convert Category to factor to control the order in the plot
overlap_data$Category <- factor(overlap_data$Category, levels = c("Non-Overlap", "Overlap"))

# Plot the bar chart
p_overlap <- ggplot(overlap_data, aes(x = "", y = Percentage, fill = Category)) + 
  geom_bar(stat = "identity", width = 0.5) +  # Use stat = "identity" to plot exact values
  scale_fill_manual(values = c("Overlap" = "blue", "Non-Overlap" = "gray")) +  # Colors for overlap and non-overlap
  coord_flip() +  # Flip the bar to make it horizontal
  labs(
    title = "Percentage of Valid Raster Cells Overlapped by ISRA Polygons",
    x = "",
    y = "Percentage"
  ) +
  scale_y_continuous(limits = c(0, 100)) +  # Ensure the bar maxes out at 100%
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12)) +  # Customize x-axis text size
  theme(axis.text.y = element_text(size = 12))   # Customize y-axis text size

# Print the plot
print(p_overlap)

# export plot
outdir <- paste0(output_data, "/fig/MPAbarchart")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", "ALL_ISRA.png")
ggsave(p_png, p_overlap, width=17, height=10, units="cm", dpi=300)






