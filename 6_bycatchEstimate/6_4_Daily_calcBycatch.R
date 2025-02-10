# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#---------------------------------------------------------------------------------------------------
# 6.4. Estimate total bycatch for a each day based on fishing effort and bycatch prediction maps
#---------------------------------------------------------------------------------------------------
# You need to combine the daily trawl fishing effort maps with the daily bycatch per unit of effort maps. 
library(raster)
library(sf)
library(dplyr)
library(exactextractr)

# 1.Load stacks-----------------------------------------------------------------
# Daily fishing effort rasters:
input_dir <- "input/gfwr/rawdata/monthly/summarydata/daily/maps/resampled"

# List all the .tif files in the directory (you can adjust the pattern if needed)
FE_list <- list.files(input_dir, pattern = "*.tif", full.names = TRUE)
summary(FE_list)

#effort_stack <- stack("input/gfwr/rawdata/monthly/summarydata/daily/maps/stacked_raster.tif")  # Daily fishing effort maps
#print(effort_stack[[3]])
#plot(effort_stack[[3]])


# Daily BPUE
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

# List all the .tif files in the directory (you can adjust the pattern if needed)
input_dir <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/hurdle_crop")
bpue_list <- list.files(input_dir, pattern = "*.tif", full.names = TRUE)
bpue_list <- bpue_list[!grepl("cir", bpue_list)]
#bpue_list <- list.files(input_dir, pattern = "^raster_cir.*\\.tif$", full.names = TRUE)
summary(bpue_list)


# 2. Ensure both stacks have the same dimensions and alignment------------------
# Load all rasters and create a stack
raster_stack <- stack(FE_list)

# Load the first geometry from bpue_list (assuming all geometries have the same CRS)
reference_geometry <- raster(bpue_list[1])
print(reference_geometry)
plot(reference_geometry)

print(raster_stack[[8]])
plot(raster_stack[[8]])
print(raster_stack)

# Ensure both stacks have the same dimensions and alignment
# Vector to store the results of the comparison
alignment_issues <- vector("list", length = nlayers(raster_stack))

# Loop through all the rasters in the raster_stack
for (i in 1:nlayers(raster_stack)) {
  # Get the current raster from the stack
  # i=1
  raster_layer <- raster_stack[[i]]
  
  # Load the corresponding geometry from bpue_list
  geometry <- raster(bpue_list[i])
  
  # Check if the raster and geometry are aligned (same CRS, extent, and resolution)
  is_aligned <- compareRaster(raster_layer, raster(raster_layer), extent = TRUE, res = TRUE, crs = TRUE)
  
  # Store the result (TRUE or FALSE) and details if not aligned
  if (!is_aligned) {
    alignment_issues[[i]] <- list(raster = i, geometry = bpue_list[i])
  }
}

# Summary of alignment checks
alignment_problems <- Filter(Negate(is.null), alignment_issues)

if (length(alignment_problems) > 0) {
  # Print out the summary of the issues
  print(paste(length(alignment_problems), "rasters are not aligned with their corresponding geometries:"))
  for (issue in alignment_problems) {
    print(paste("Raster", issue$raster, "with geometry", issue$geometry, "is not aligned."))
  }
} else {
  print("All rasters are aligned with their corresponding geometries.")
}


# 2.1. Add all hours trawled across all days to understand the total effort-----
print(raster_stack[[8]])
# Sum the cell values across all layers in the stack
total_effort_raster <- calc(raster_stack, sum, na.rm = TRUE)

# Sum all cell values of the resulting raster to get the total fishing effort in hours
total_fishing_effort <- cellStats(total_effort_raster, stat = 'sum', na.rm = TRUE)

# Print the total fishing effort
cat("Total fishing effort across all days:", total_fishing_effort, "hours\n")
#Total fishing effort across all days: 372,187 hours



# 3. Transform hours trawled per unit of areas to number of tows-------------------
#tow_dur <- read.csv2("C:/Users/david/OneDrive/Escritorio/Survival/chondrichthyan_mortality/output/data_env_all.csv")
#head(tow_dur)

# Extract the first row for each unique tripID
#first_row_per_trip <- tow_dur[!duplicated(tow_dur$tripID), ]

# View the resulting data frame
#head(first_row_per_trip)

# 3.1. Tow duration-------------------------------------------------------------
# Calculate and print mean and standard deviation for depth < 200
#mean_trawl_duration_below_200 <- mean(first_row_per_trip$Trawl_duration[first_row_per_trip$depth < 200], na.rm = TRUE)
#std_dev_trawl_duration_below_200 <- sd(first_row_per_trip$Trawl_duration[first_row_per_trip$depth < 200], na.rm = TRUE)
#cat("Mean Trawl Duration (Depth < 200):", mean_trawl_duration_below_200, 
#    "\nStandard Deviation (Depth < 200):", std_dev_trawl_duration_below_200, "\n\n")
#3.381164 +- 1.122123 

# Calculate and print mean and standard deviation for depth > 200
#mean_trawl_duration_above_200 <- mean(first_row_per_trip$Trawl_duration[first_row_per_trip$depth > 200], na.rm = TRUE)
#std_dev_trawl_duration_above_200 <- sd(first_row_per_trip$Trawl_duration[first_row_per_trip$depth > 200], na.rm = TRUE)
#cat("Mean Trawl Duration (Depth > 200):", mean_trawl_duration_above_200, 
#    "\nStandard Deviation (Depth > 200):", std_dev_trawl_duration_above_200, "\n\n")

# Calculate and print mean and standard deviation for depth between 200 and 500
#mean_trawl_duration_200_to_500 <- mean(first_row_per_trip$Trawl_duration[first_row_per_trip$depth >= 200 & first_row_per_trip$depth <= 500], na.rm = TRUE)
#std_dev_trawl_duration_200_to_500 <- sd(first_row_per_trip$Trawl_duration[first_row_per_trip$depth >= 200 & first_row_per_trip$depth <= 500], na.rm = TRUE)
#cat("Mean Trawl Duration (200 <= Depth <= 500):", mean_trawl_duration_200_to_500, 
#    "\nStandard Deviation (200 <= Depth <= 500):", std_dev_trawl_duration_200_to_500, "\n\n")
#3.263058 +- 1.160529


# Calculate and print mean and standard deviation for depth > 500
#mean_trawl_duration_above_500 <- mean(first_row_per_trip$Trawl_duration[first_row_per_trip$depth > 500], na.rm = TRUE)
#std_dev_trawl_duration_above_500 <- sd(first_row_per_trip$Trawl_duration[first_row_per_trip$depth > 500], na.rm = TRUE)
#cat("Mean Trawl Duration (Depth > 500):", mean_trawl_duration_above_500, 
#    "\nStandard Deviation (Depth > 500):", std_dev_trawl_duration_above_500, "\n\n")
#3.373981 +- 2.122087 

# Calculate and print overall mean and standard deviation
#mean_trawl_duration <- mean(first_row_per_trip$Trawl_duration, na.rm = TRUE)
#std_dev_trawl_duration <- sd(first_row_per_trip$Trawl_duration, na.rm = TRUE)
#cat("Mean Trawl Duration (Overall):", mean_trawl_duration, 
#    "\nStandard Deviation (Overall):", std_dev_trawl_duration, "\n")
# A tow represents 3.340268+-1.468432 h

# 3.2. Speed--------------------------------------------------------------------
#head(first_row_per_trip)

# Calculate and print mean and standard deviation for depth < 200
#mean_speed_below_200 <- mean(first_row_per_trip$Average_speed[first_row_per_trip$depth < 200], na.rm = TRUE)
#SD_speed_below_200 <- sd(first_row_per_trip$Average_speed[first_row_per_trip$depth < 200], na.rm = TRUE)
#cat("Mean Trawl Duration (Depth < 200):", mean_speed_below_200, 
#    "\nStandard Deviation (Depth < 200):", SD_speed_below_200, "\n\n")
# <200m: 3.191429 +- 0.3146775 

# Calculate and print mean and standard deviation for depth > 200
#mean_speed_above_200 <- mean(first_row_per_trip$Average_speed[first_row_per_trip$depth > 200], na.rm = TRUE)
#SD_speed_above_200 <- sd(first_row_per_trip$Average_speed[first_row_per_trip$depth > 200], na.rm = TRUE)
#cat("Mean Trawl Duration (Depth > 200):", mean_speed_above_200, 
#    "\nStandard Deviation (Depth > 200):", SD_speed_above_200, "\n\n")
# >200m: 2.693167  +- 0.2948538

# Calculate and print mean and standard deviation for depth between 200 and 500
#mean_speed_200_to_500 <- mean(first_row_per_trip$Average_speed[first_row_per_trip$depth >= 200 & first_row_per_trip$depth <= 500], na.rm = TRUE)
#SD_speed_200_to_500 <- sd(first_row_per_trip$Average_speed[first_row_per_trip$depth >= 200 & first_row_per_trip$depth <= 500], na.rm = TRUE)
#cat("Mean Trawl Duration (200 <= Depth <= 500):", mean_speed_200_to_500, 
#    "\nStandard Deviation (200 <= Depth <= 500):", SD_speed_200_to_500, "\n\n")
# 200-500m: 2.813438 +- 0.2681395

# Calculate and print mean and standard deviation for depth > 500
#mean_speed_above_500 <- mean(first_row_per_trip$Average_speed[first_row_per_trip$depth > 500], na.rm = TRUE)
#SD_speed_above_500 <- sd(first_row_per_trip$Average_speed[first_row_per_trip$depth > 500], na.rm = TRUE)
#cat("Mean Trawl Duration (Depth > 500):", mean_speed_above_500, 
#    "\nStandard Deviation (Depth > 500):", SD_speed_above_500, "\n\n")
# 200-500m: 2.555714 +- 0.2671811 

# Calculate and print overall mean and standard deviation
#mean_speed <- mean(first_row_per_trip$Average_speed, na.rm = TRUE)
#SD_speed <- sd(first_row_per_trip$Average_speed, na.rm = TRUE)
#cat("Mean Speed (Overall):", mean_speed, 
#    "\nStandard Deviation (Overall):", SD_speed, "\n")
# Overall mean: 2.884281+-0.3870117

# Speed per metier:
speed_hake <- 3.191 * 1.852
speed_norwaylobs <- 2.813 * 1.852
speed_red <- 2.556 * 1.852

# 3.3. Net opening--------------------------------------------------------------
#net_opening <- read.csv2("input/Original/DatosCatalunyaECEME_Final.csv")
#head(net_opening)

# Create the new dataframe `net_op` with the first value of NetHorizontalOpening for each unique code
#net_op <- net_opening %>%
#  group_by(code) %>%                # Group by the 'code' column
#  summarise(
#    NetOpening = first(NetHorizontalOpenening),
#   depth = first(depth))  # Get the first value

# Merge both dataframes:
#head(net_op)  
#head(first_row_per_trip)

# Merge the two dataframes
# Create the dataframe for net_op values
#net_op_part <- data.frame(
#  ID = net_op$code,
#  Net_Opening = net_op$NetOpening,
#  depth = net_op$depth)

# Create the dataframe for first_row_per_trip values
#trip_part <- data.frame(
#  ID = first_row_per_trip$tripID,
#  Net_Opening = first_row_per_trip$Net_horizontal_opening,
#  depth = first_row_per_trip$depth
#)

# Combine the two dataframes
#combined_df <- rbind(net_op_part, trip_part)
#head(combined_df)

# Calculate and print mean and standard deviation for depth < 200
#mean_net_below_200 <- mean(combined_df$Net_Opening[combined_df$depth < 200], na.rm = TRUE)
#SD_net_below_200 <- sd(combined_df$Net_Opening[combined_df$depth < 200], na.rm = TRUE)
#cat("Mean net opening (Depth < 200):", mean_net_below_200, 
#    "\nStandard Deviation (Depth < 200):", SD_net_below_200, "\n\n")
# <200m: 21.04615 +- 7.096282

# Calculate and print mean and standard deviation for depth > 200
#mean_net_above_200 <- mean(combined_df$Net_Opening[combined_df$depth > 200], na.rm = TRUE)
#SD_net_above_200 <- sd(combined_df$Net_Opening[combined_df$depth > 200], na.rm = TRUE)
#cat("Mean net opening (Depth > 200):", mean_net_above_200, 
#    "\nStandard Deviation (Depth > 200):", SD_net_above_200, "\n\n")
# >200m: 26.6557   +- 8.74487 

# Calculate and print mean and standard deviation for depth between 200 and 500
#mean_net_200_to_500 <- mean(combined_df$Net_Opening[combined_df$depth >= 200 & combined_df$depth <= 500], na.rm = TRUE)
#SD_net_200_to_500 <- sd(combined_df$Net_Opening[combined_df$depth >= 200 & combined_df$depth <= 500], na.rm = TRUE)
#cat("Mean net opening (200 <= Depth <= 500):", mean_net_200_to_500, 
#    "\nStandard Deviation (200 <= Depth <= 500):", SD_net_200_to_500, "\n\n")
# 200-500m: 26.86809 +- 9.893369

# Calculate and print mean and standard deviation for depth > 500
#mean_net_above_500 <- mean(combined_df$Net_Opening[combined_df$depth > 500], na.rm = TRUE)
#SD_net_above_500 <- sd(combined_df$Net_Opening[combined_df$depth > 500], na.rm = TRUE)
#cat("Mean net opening (Depth > 500):", mean_net_above_500, 
#    "\nStandard Deviation (Depth > 500):", SD_net_above_500, "\n\n")
# 200-500m: 26.34375  +- 6.856169 

# Calculate and print overall mean and standard deviation
#mean_net <- mean(combined_df$Net_Opening, na.rm = TRUE)
#SD_net <- sd(combined_df$Net_Opening, na.rm = TRUE)
#cat("Mean net opening (Overall):", mean_net, 
#    "\nStandard Deviation (Overall):", SD_net, "\n")
# Overall mean: 24.12361 +- 8.490982

# Horizontal net opening per metier in km:
opening_hake <- 21.046 / 1000
opening_norwaylobs <- 26.868 / 1000
opening_red <- 26.344 / 1000




# 4. Add them to calculate BPUE per day in relation to fishing effort-----------
# Parameters for each metier
metiers <- list(
  hake = list(opening = opening_hake, speed = speed_hake, depth_range = c(0, -199.99)),
  norwaylobs = list(opening = opening_norwaylobs, speed = speed_norwaylobs, depth_range = c(-200, -499.99)),
  red = list(opening = opening_red, speed = speed_red, depth_range = c(-500, -700))
)
print(metiers)

# Load bathymetry data
bathy <- raster("input/gebco/Bathy.tif")

# Filter the values between -20 and -700 and set values outside the range to NA
bathy <- calc(bathy, function(x) {
  x[x > -20 | x < -700] <- NA  # Set values outside the range to NA
  return(x)
})

# resample:
path <- paste0("output/", mod_code, "/hurdle_stacks/", genus, "/total_bycatch/sum/")
output_file <- paste0(path, "aggregated_bycatch_map.tiff")
totalbycatch_raster <- raster(output_file)
print(totalbycatch_raster)
bathy <- resample(bathy, totalbycatch_raster, method = "bilinear")
print(bathy)

# Function to assign metier based on depth
assign_metier <- function(depth) {
  if (depth >= - 200) {
    return("hake")
  } else if (depth < - 200 & depth > - 500) {
    return("norwaylobs")
  } else {
    return("red")
  }
}

# Saving directory:
path <- paste0("output/", mod_code, "/hurdle_stacks/", genus, "/total_bycatch/")
if (!dir.exists(path)) dir.create(path , recursive = TRUE)

# Preprocess the metiers to ensure numeric parameters
metiers <- lapply(metiers, function(params) {
  params$opening <- as.numeric(params$opening)
  params$speed <- as.numeric(params$speed)
  return(params)
})


# Function to transform fishing effort to area units and calculate total bycatch:
process_raster <- function(i) {
  # i=8
  FE_layer <- raster_stack[[i]]
  #plot(FE_layer)
  #print(FE_layer)
  
  raster_date <- substr(names(FE_layer), 19, 22) %>% 
    paste0(substr(names(FE_layer), 24, 25)) %>% 
    paste0(substr(names(FE_layer), 27, 28))
  
  bpue_file <- bpue_list[grep(raster_date, bpue_list)]
  
  if (length(bpue_file) == 1) {
    print(paste("Processing BPUE file:", bpue_file))
    bpue <- raster(bpue_file)
    #plot(bpue)
    #print(bpue)
    
    # Mask the raster to the exact shape of bpue_cleaned
    FE_layer_mask <- raster::mask(FE_layer, bpue)
    #plot(FE_layer_mask)
    #print(FE_layer_mask)
    
    # Add depth values as a new layer in bpue
    # Crop bathy to the extent of bpue
    bathy <- resample(bathy, bpue, method = "bilinear")
    bathy <- crop(bathy, extent(bpue))
    #plot(bathy)
    bpue_with_depth <- stack(bpue, bathy)
    names(bpue_with_depth) <- c("bpue_values", "depth_values")
    
    # Initialize total bycatch and metier-specific columns
    bpue_with_depth$total_bycatch <- 0
    
    # Process metiers
    # metier_name <- names(metiers)[3]
    for (metier_name in names(metiers)) {
      params <- metiers[[metier_name]]
      # Get depth values as a numeric vector
      depth_values <- values(bpue_with_depth$depth_values)
      # Create the logical mask based on depth values
      metier_mask <- (depth_values < params$depth_range[1]) & (depth_values >= params$depth_range[2])
      cat("Number of cells in the metier mask:", sum(metier_mask, na.rm = TRUE), "\n")
      # Create a masked raster (set non-matching cells to NA)
      depth_masked_raster <- bpue_with_depth$depth_values
      values(depth_masked_raster)[!metier_mask] <- NA
      #plot(depth_masked_raster, main = "Masked Depth Values")

      if (sum(metier_mask, na.rm = TRUE) > 0) {
        # Extract fishing effort for the metier from FE_layer_mask
        fishing_effort <- values(FE_layer_mask)
        fishing_effort[!metier_mask] <- 0  # Zero out values outside the mask
        fishing_effort[is.na(fishing_effort)] <- 0  # Replace NA with 0
        
        # Compute trawled area
        trawled_area <- params$opening * params$speed * fishing_effort

        # Add the trawled area as a new raster layer
        trawled_area_raster <- raster(bpue_with_depth)
        values(trawled_area_raster) <- trawled_area
        bpue_with_depth <- addLayer(bpue_with_depth, trawled_area_raster)
        names(bpue_with_depth)[nlayers(bpue_with_depth)] <- paste0("trawled_area_", metier_name)
        # bpue_with_depth
        # plot(FE_layer_mask)
        # plot(bpue_with_depth$trawled_area_hake)
        # plot(bpue_with_depth$trawled_area_norwaylobs) 
        # plot(bpue_with_depth$trawled_area_red) 

    # Calculate bycatch for the current metier
    metier_bycatch <- trawled_area * values(bpue_with_depth$bpue_values)
    
    # Add bycatch_<metier> as a new layer
    bycatch_layer <- raster(bpue_with_depth)
    values(bycatch_layer) <- metier_bycatch
    bpue_with_depth <- addLayer(bpue_with_depth, bycatch_layer)
    names(bpue_with_depth)[nlayers(bpue_with_depth)] <- paste0("bycatch_", metier_name)
    bpue_with_depth
    
    # Update total_bycatch layer
    total_bycatch_layer <- bpue_with_depth$total_bycatch
    values(total_bycatch_layer) <- values(total_bycatch_layer) + metier_bycatch
    bpue_with_depth <- dropLayer(bpue_with_depth, which(names(bpue_with_depth) == "total_bycatch"))
    bpue_with_depth <- addLayer(bpue_with_depth, total_bycatch_layer)
    names(bpue_with_depth)[nlayers(bpue_with_depth)] <- "total_bycatch"
    #plot(bpue_with_depth$total_bycatch)
    #plot(bpue_with_depth$trawled_area_hake)
    #plot(bpue_with_depth$bycatch_hake)
    #plot(bpue_with_depth$total_bycatch)
      }
    }
    
    # Save the updated BPUE with depth and bycatch as a GeoTIFF file
    path <- paste0("output/", mod_code, "/hurdle_stacks/", genus, "/total_bycatch/processed")
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    output_file <- paste0("total_bycatch_", raster_date, ".tif")
    #output_file <- paste0("total_bycatch_cir_", raster_date, ".tif")
    writeRaster(bpue_with_depth, file.path(path, output_file), format = "GTiff", overwrite = TRUE)
    
    # Calculate total bycatch for this raster and update bycatchdata
    total_bycatch_sum <- cellStats(bpue_with_depth$total_bycatch, stat = "sum", na.rm = TRUE)
    # Calculate sums for trawled_area and bycatch for each metier
    trawled_area_hake_sum <- cellStats(bpue_with_depth$trawled_area_hake, stat = "sum", na.rm = TRUE)
    trawled_area_norwaylobs_sum <- cellStats(bpue_with_depth$trawled_area_norwaylobs, stat = "sum", na.rm = TRUE)
    trawled_area_red_sum <- cellStats(bpue_with_depth$trawled_area_red, stat = "sum", na.rm = TRUE)
    bycatch_hake_sum <- cellStats(bpue_with_depth$bycatch_hake, stat = "sum", na.rm = TRUE)
    bycatch_norwaylobs_sum <- cellStats(bpue_with_depth$bycatch_norwaylobs, stat = "sum", na.rm = TRUE)
    bycatch_red_sum <- cellStats(bpue_with_depth$bycatch_red, stat = "sum", na.rm = TRUE)
    
    # Append these results to the bycatchdata dataframe
    bycatchdata <<- rbind(bycatchdata, data.frame(
      date = raster_date,
      total_bycatch = total_bycatch_sum,
      trawled_area_hake = trawled_area_hake_sum,
      trawled_area_norwaylobs = trawled_area_norwaylobs_sum,
      trawled_area_red = trawled_area_red_sum,
      bycatch_hake = bycatch_hake_sum,
      bycatch_norwaylobs = bycatch_norwaylobs_sum,
      bycatch_red = bycatch_red_sum
    ))
    print(paste("Processed and saved BPUE for date:", raster_date))
  } else {
    print(paste("No matching BPUE file for raster with date:", raster_date))
  }
}

# Sequential processing of raster layers
bycatchdata <- data.frame(date = character(), total_bycatch = numeric(), stringsAsFactors = FALSE)  # Initialize bycatch data
fishing_effort_values <- vector("list", length = length(raster_stack)) # Fishing effort vector

for (i in 1:nlayers(raster_stack)) {
  process_raster(i)  # Call the function for each raster layer
}

# Save the bycatch summary as a CSV
head(bycatchdata)
bycatch_csv_file <- file.path(path, "bycatch_summary_cir.csv")
#bycatch_csv_file <- file.path(path, "bycatch_summary.csv")
write.csv(bycatchdata, bycatch_csv_file, row.names = FALSE)


    
# 6. Overall egg case bycatch for 2021------------------------------------------
path <- paste0("output/", mod_code, "/hurdle_stacks/", genus, "/total_bycatch/")
bycatch_csv_file <- file.path(path, "bycatch_summary.csv")
#bycatch_csv_file <- file.path(path, "bycatch_summary_cir.csv")
bycatch <- read.csv(bycatch_csv_file)
head(bycatch)

# Calculate the total sum of total_bycatch
total_bycatch_sum <- sum(bycatch$total_bycatch, na.rm = TRUE)
print(total_bycatch_sum)
# Total bycatch for 2021: 233,239 egg cases for Raja
# Total bycatch for 2021: 228,935 egg cases for Scyliorhinus

# Total 95% CI for 2021: 43,169 egg cases for Raja
# Total 95% CI for 2021: 25,570 egg cases for Scyliorhinus
# Exclude the date column and calculate the sum for all other columns
total_sums <- colSums(bycatch[, -1], na.rm = TRUE)
print(total_sums)

# Select the trawled area columns and calculate their total sum
total_trawled_area <- sum(
  total_sums["trawled_area_hake"],
  total_sums["trawled_area_norwaylobs"],
  total_sums["trawled_area_red"])

# Print the total trawled area
print(total_trawled_area)

#95% CI Raja:
#total_bycatch       trawled_area_hake trawled_area_norwaylobs        trawled_area_red            bycatch_hake 
#43169.482               22980.475                8070.905                2402.344               26085.388 
#bycatch_norwaylobs             bycatch_red 
#12962.913                4121.181

#95% CI Scyliorhinus:
#total_bycatch       trawled_area_hake trawled_area_norwaylobs        trawled_area_red            bycatch_hake 
#25569.899               23396.891                7327.269                2698.409               15766.427 
#bycatch_norwaylobs             bycatch_red 
#6621.830                3181.642 

#6. Bootstrap total bycatch-----------------------------------------------------

#6.1. Make  a simulation--------------------------------------------------------
# Bootstrap Sampling
n_sim <- 1000  # Number of bootstrap iterations
bootstrap_results <- replicate(n_sim, {
  # Randomly sample with replacement
  sampled_bycatch <- sample(bycatch, size = length(bycatch), replace = TRUE)
  # Calculate total bycatch for the sampled population
  sum(sampled_bycatch, na.rm = TRUE)
})

# Calculate 95% Confidence Interval
ci_95 <- quantile(bootstrap_results, probs = c(0.025, 0.975))
cat("95% Confidence Interval for Total Bycatch:", ci_95, "\n")
