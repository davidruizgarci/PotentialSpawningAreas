# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#---------------------------------------------------------------------------------------------------
# 6.5. Estimate total bycatch inside and outside PSAs
#---------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggspatial)
library(raster)
library(pbapply)
library(beepr)

# 1. Load total bycatch raster--------------------------------------------------
genus <- "Raja" 
family <- "LN_gaussian_Final2" 
type <- "_NKm2" 
mod_code <- "brt"
season <- "2021"

genus <- "Scyliorhinus" 
family <- "bernuilli_Final2" 
type <- "_PA" 
mod_code <- "brt"
dataset <- "ALL" 

# Total bycatch
path <- paste0("output/", mod_code, "/hurdle_stacks/", genus, "/total_bycatch/processed")
bycatch_list <- list.files(path, pattern = "^total_bycatch.*\\.tif$", full.names = TRUE)
bycatch_list <- bycatch_list[!grepl("cir", bycatch_list)]

# check one of them>
example <- raster(bycatch_list[250])
#example_stack <- stack(file.path(bycatch_list[1]))
print(example)
plot(example)

# Load all rasters into a stack
# Initialize an empty list to store the first rasters
bycatch_stack <- stack()

# Loop through each raster file in bycatch_list
for (file in bycatch_list) {
  # Load the full raster stack from the file
  raster_stack <- stack(file)  # Load as a stack
  # Extract the last raster layer from the stack
  last_raster <- raster_stack[[nlayers(raster_stack)]]
  # Stack iteratively
  bycatch_stack <- stack(bycatch_stack, last_raster)
}

# Print summary
print(bycatch_stack)
plot(bycatch_stack[[15]], main = "First Raster in Bycatch Stack")

# Function to sum rasters and print progress
# Enable progress bar inside calc()
pboptions(type = "txt")

# Define function with progress tracking
sum_rasters <- function(x) {
  return(sum(x, na.rm = TRUE))
}

# Apply function across raster stack
bycatch_sum <- calc(bycatch_stack, fun = sum_rasters)
print("Summation complete!")
plot(bycatch_sum, main = "Total Bycatch Sum Across Rasters")
print(bycatch_sum)

#Save the sum:
output_file <- file.path(path, "bycatch_sum.tif")
writeRaster(bycatch_sum, filename = output_file, format = "GTiff", overwrite = TRUE)


# PSA
path <- paste0("output/shapefiles/", genus, "/Final_2021_", genus, "_hurdle_MASK_habitat_raster.tif")
PSA <- raster(path)
plot(PSA)
print(PSA)
PSA_resampled <- resample(PSA, bycatch_sum, method = "bilinear")

# Convert PSA to a strict binary mask (1 = inside PSA, NA = outside)
PSA_resampled[PSA_resampled >= 0.5] <- 1  # Set all values >= 0.5 to 1
PSA_resampled[PSA_resampled < 0.5] <- NA  # Set lower values to NA (outside PSA)

# Mask the bycatch sum raster
bycatch_in_PSA <- mask(bycatch_sum, PSA_resampled)  # Keep only inside PSA
bycatch_outside_PSA <- mask(bycatch_sum, PSA_resampled, inverse = TRUE)  # Keep only outside PSA
plot(bycatch_in_PSA)
plot(bycatch_outside_PSA)

# Calculate total sums
total_bycatch_in_PSA <- cellStats(bycatch_in_PSA, sum, na.rm = TRUE)
total_bycatch_outside_PSA <- cellStats(bycatch_outside_PSA, sum, na.rm = TRUE)

# Verify consistency
total_bycatch_overall <- cellStats(bycatch_sum, sum, na.rm = TRUE)

# Print results
print(paste("Total Bycatch Within PSAs:", total_bycatch_in_PSA))
#Raja: 97,847 (42.0%)
#Scyliorhinus: 19,052 (8.3%)
print(paste("Total Bycatch Outside PSAs:", total_bycatch_outside_PSA))
#Raja: 135,392 (58.0%)
#Scyliorhinus: 209,883
print(paste("Total Bycatch Overall:", total_bycatch_overall))
# Raja: 233,239
# Scyliorhinus: 228,935


# Check if the total sum matches the original 233,239 egg cases
if (round(total_bycatch_in_PSA + total_bycatch_outside_PSA, 2) == round(total_bycatch_overall, 2)) {
  print("✅ Sum check passed: Inside + Outside matches overall total!")
} else {
  print("❌ Sum mismatch! Check alignment and missing values.")
}

# Total bycatch for 2021: 233,239 egg cases for Raja
# Total bycatch for 2021: 228,935 egg cases for Scyliorhinus