# ------------------------------------------------------------------------------
# 6.4. Integrate conservative GFW v4 hours with corrected daily hurdle predictions
# ------------------------------------------------------------------------------
library(raster)
library(dplyr)

# Check that the local setup has been run before looking for the helper files.
if (!exists("code_dir")) stop("Run the local 0_setup.R first")
source(file.path(code_dir,"fun","training_smearing.R"))
source(file.path(code_dir,"fun","effort_grid_checks.R"))


# 1. Select one genus and the exact calendar to integrate -----------------------
# Choose "Raja" or "Scyliorhinus" below and run the script once for each genus.
# Choose the genus for this run; run the script separately for Raja and Scyliorhinus.
genus <- "Raja"
if (!genus %in% c("Raja","Scyliorhinus")) stop("Unknown genus")

# Use the full 2021 calendar; every date must have its corresponding input files.
dates <- seq(as.Date("2021-01-01"),as.Date("2021-12-31"),by="day")

# Keep this new effort series in its own folder, separate from the historical data.
effort_dir <- file.path(input_data,"gfwr/rawdata/monthly_v4_2021")
study_dir <- file.path(effort_dir,"summarydata/daily/maps_conservative/study_domain")

# Construct one expected effort filename per date and stop if any day is missing.
FE_list <- daily_files(study_dir,dates,function(day) paste0(day,".tif"))

# Read the corrected joint maps from the presence folder, where 4.6 saves them for both
# genera.
bpue_dir <- file.path(output_data,"brt",paste0(genus,"_PA_bernuilli_Final2"),"predict_boost")

# Build the matching BPUE filename from the same date, excluding interval maps and sidecar
# files.
bpue_list <- daily_files(bpue_dir,dates,function(day) {
  file.path(format(day,"%Y/%m"),paste0("hurdle_X",format(day,"%Y%m%d"),"_",genus,"_brt_pred.tif"))
})


# 2. Use the verified grid and depth from 6.2, without old bycatch dependencies ---
# Read the reference raster to keep its exact cell size, cell positions and map extent.
reference <- raster(FE_list[1])

# Reuse the depth file prepared in 6.2 instead of loading any previous bycatch result.
bathy_file <- file.path(study_dir,"study_bathymetry.tif")

# Read those aligned signed depths for assigning the original depth-specific parameters.
bathy <- raster(bathy_file)

# Stop if these grids differ; matching the number of cells alone would not be enough.
compareRaster(reference,bathy,extent=TRUE,rowcol=TRUE,res=TRUE,orig=TRUE,crs=TRUE)


# 3. Transform hours trawled per unit of areas to number of tows-------------------
#tow_dur <- read.csv2("C:/Users/david/OneDrive/Escritorio/Survival/chondrichthyan_mortality/output/data_env_all.csv")
#head(tow_dur)

# Extract the first row for each tripID to describe the original trip parameters.
#first_row_per_trip <- tow_dur[!duplicated(tow_dur$tripID), ]

# Inspect the resulting data frame.
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

# Retain the original speed for each metier.
# Retain the original mean speeds and convert knots to kilometres per hour using 1.852.
speed_hake <- 3.191 * 1.852
speed_norwaylobs <- 2.813 * 1.852
speed_red <- 2.556 * 1.852

# 3.3. Net opening--------------------------------------------------------------
#net_opening <- read.csv2("input/Original/DatosCatalunyaECEME_Final.csv")
#head(net_opening)

# Take the first net-opening measurement for each original sampling code.
#net_op <- net_opening %>%
#  group_by(code) %>%                # Group by the 'code' column
#  summarise(
#    NetOpening = first(NetHorizontalOpenening),
#   depth = first(depth))  # Get the first value

# Bring both sources of net-opening measurements together.
#head(net_op)  
#head(first_row_per_trip)

# Combine the two tables of measurements.
# Prepare the table of net-opening measurements.
#net_op_part <- data.frame(
#  ID = net_op$code,
#  Net_Opening = net_op$NetOpening,
#  depth = net_op$depth)

# Prepare the table with the first measurement from each trip.
#trip_part <- data.frame(
#  ID = first_row_per_trip$tripID,
#  Net_Opening = first_row_per_trip$Net_horizontal_opening,
#  depth = first_row_per_trip$depth
#)

# Combine the two tables of measurements.
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

# Express the horizontal net opening for each metier in kilometres.
# Retain the original net openings and convert metres to kilometres.
opening_hake <- 21.046 / 1000
opening_norwaylobs <- 26.868 / 1000
opening_red <- 26.344 / 1000




# 4. Pair dates explicitly and integrate the original swept-area equation -------
# Calculate hours x speed (km/hour) x net width (km) x BPUE (capsules/km2) to obtain capsules.
# Keep the original depth classes and make their 200 m and 500 m boundaries meet without gaps.
# Put the new GFW v4 results in their own folder, preserving the historical bycatch outputs.
output_dir <- file.path(output_data,"brt/hurdle_stacks",genus,"total_bycatch/gfw_v4_2021")

# Keep the daily multi-layer bycatch files in this subfolder.
processed_dir <- file.path(output_dir,"processed")

# Create the output folder if it does not already exist.
dir.create(processed_dir,recursive=TRUE,showWarnings=FALSE)

# Reserve one coverage record per day so missing predictions cannot disappear from the report.
audit_rows <- vector("list",length(dates))

# Reserve one daily results row for total catches and the three metiers.
bycatch_rows <- vector("list",length(dates))

# Create an empty raster on the same grid to accumulate the daily results.
annual <- raster(reference)

# Leave the annual raster empty until it is clear which cells have usable predictions.
values(annual) <- NA_real_

# Create a numeric accumulator for adding daily central catch estimates.
annual_values <- rep(0,ncell(reference))

# Track cells with at least one usable day so cells never estimated remain NA.
annual_available <- rep(FALSE,ncell(reference))

# Turn signed elevation into positive water depth to make the class boundaries easier to read.
depth <- -getValues(bathy)

# Assign 20-<200 m to hake, 200-<500 m to Norway lobster and 500-700 m to red shrimp.
classes <- list(hake=is.finite(depth) & depth>=20 & depth<200,
                norwaylobs=is.finite(depth) & depth>=200 & depth<500,
                red=is.finite(depth) & depth>=500 & depth<=700)

# Collect the unchanged speed parameters under the corresponding metier names.
speeds <- c(hake=speed_hake,norwaylobs=speed_norwaylobs,red=speed_red)

# Collect the unchanged net openings in the same named order.
openings <- c(hake=opening_hake,norwaylobs=opening_norwaylobs,red=opening_red)

for (i in seq_along(dates)) {

  # Take the same calendar date for the effort, prediction and output below.
  day <- dates[i]

  # Select the effort raster for this date from the checked calendar.
  effort_file <- FE_list[i]

  # Recover its saved checksums before reading values for the calculation.
  a <- readRDS(paste0(effort_file,".effort.rds"))

  # Stop if either the effort raster or the depth file differs from the audited inputs.
  if (!identical(a$method,"conservative_study_mask") ||
      !identical(a$md5,unname(tools::md5sum(effort_file))) ||
      !identical(a$depth_md5,unname(tools::md5sum(bathy_file)))) stop("Effort audit mismatch")

  # Load fishing hours already restricted to the selected study cells.
  fe <- raster(effort_file)

  # Take the BPUE map for exactly the same day as the effort.
  bpue_file <- bpue_list[i]

  # Verify that BPUE is already in capsules/km2; do not undo the logarithm again.
  check_prediction_scale(bpue_file,"capsules/km2")

  # Load the corrected central hurdle prediction for this date.
  bpue <- raster(bpue_file)

  # Stop if these grids differ; matching the number of cells alone would not be enough.
  compareRaster(reference,fe,bpue,extent=TRUE,rowcol=TRUE,res=TRUE,orig=TRUE,crs=TRUE)

  # Read hours per cell and day as a vector for the swept-area calculation.
  h <- getValues(fe)

  # Read predicted capsules per km2 in the same cell order.
  b <- getValues(bpue)

  # Reject negative hours or predicted densities before multiplying them.
  if (any(is.finite(h) & h<0) || any(is.finite(b) & b<0)) stop("Negative effort or BPUE")

  # Identify cells where the effort map includes the study area; zero hours still count as
  # valid.
  in_study <- is.finite(h)

  # Calculate catches only where both the study effort and a finite BPUE prediction exist.
  available <- in_study & is.finite(b)

  # Separately mark study cells without BPUE so their fishing hours are reported rather than
  # treated as zero catch.
  missing <- in_study & !is.finite(b)

  # Retain the input BPUE and depth together with the calculated layers for inspection.
  layers <- list(bpue_values=bpue,depth_values=bathy)

  # Start with NA so locations without predictions cannot look like zero catches.
  total <- rep(NA_real_,length(h))

  # Start the catch sum at zero only in cells where the calculation is possible.
  total[available] <- 0

  # Begin a results row labelled with this date, rather than extract a date from a raster
  # layer name.
  row <- data.frame(date=format(day,"%Y%m%d"))

  # Create all three metier layers even if one of them has no effort on this day.
  for (metier in names(classes)) {

    # Select the usable cells that belong to the current metier depth range.
    included <- available & classes[[metier]]

    # Keep swept area undefined outside cells where the calculation is available.
    area <- rep(NA_real_,length(h))

    # Start the metier area at zero in usable cells before adding this metier contribution.
    area[available] <- 0

    # Multiply hours by speed and net width: hours x km/hour x km gives swept km2.
    area[included] <- h[included]*speeds[metier]*openings[metier]

    # Multiply swept km2 by predicted capsules/km2 to obtain the expected number of capsules.
    catch <- area*b

    # Set this metier contribution to zero in usable cells belonging to a different depth
    # class.
    catch[available & !included] <- 0

    # Put the swept areas back on the original grid without changing their cell positions.
    r_area <- raster(reference); values(r_area) <- area

    # Put the metier catches on that same grid.
    r_catch <- raster(reference); values(r_catch) <- catch

    # Retain the swept area layer for this metier in the daily output.
    layers[[paste0("trawled_area_",metier)]] <- r_area

    # Retain its catch layer separately, so the total remains traceable.
    layers[[paste0("bycatch_",metier)]] <- r_catch

    # Add this metier contribution to the daily catch total in usable cells.
    total[available] <- total[available]+catch[available]

    # Add the metier swept area across the usable cells for the daily table.
    row[[paste0("trawled_area_",metier)]] <- sum(area,na.rm=TRUE)

    # Add the corresponding expected catches for that same day.
    row[[paste0("bycatch_",metier)]] <- sum(catch,na.rm=TRUE)
  }

  # Create the total catch layer on the same reference grid.
  r_total <- raster(reference); values(r_total) <- total

  # Include the sum across all three metiers in the daily file.
  layers$total_bycatch <- r_total

  # Combine inputs, swept areas and catches into one daily raster stack.
  daily_stack <- stack(layers)

  # Name the layers explicitly so their contents are identifiable.
  names(daily_stack) <- names(layers)

  # Save the daily stack in double precision, separately from the older results.
  writeRaster(daily_stack,file.path(processed_dir,paste0("total_bycatch_",format(day,"%Y%m%d"),".tif")),
              overwrite=TRUE,datatype="FLT8S")
  # Store the names separately because raster/GDAL may rename the saved GeoTIFF bands.
  # Also save the layer names because some GeoTIFF readers replace them with numbered bands.
  saveRDS(names(daily_stack),file.path(processed_dir,paste0("total_bycatch_",format(day,"%Y%m%d"),".tif.layers.rds")))

  # Sum the estimated cells for this day; the coverage table records what could not be
  # estimated.
  row$total_bycatch <- sum(total,na.rm=TRUE)

  # Retain this daily row for the final results table.
  bycatch_rows[[i]] <- row

  # Record study hours, integrated hours and hours lacking BPUE beside each daily result.
  audit_rows[[i]] <- data.frame(date=day,study_hours=sum(h,na.rm=TRUE),
      integrated_hours=sum(h[available]),hours_without_BPUE=sum(h[missing]),
      cells_without_BPUE=sum(missing),bpue_md5=unname(tools::md5sum(bpue_file)))

  # Add this day only where an estimate exists, without filling missing predictions with zero.
  annual_values[available] <- annual_values[available]+total[available]

  # Track cells with at least one usable day so cells never estimated remain NA.
  annual_available <- annual_available | available
}


# 5. Save central totals and coverage separately; do not sum daily CI widths -----
# Join the individual daily results into one table.
bycatchdata <- bind_rows(bycatch_rows)

# Keep the original summary column names and order for easier comparison with previous runs.
bycatchdata <- bycatchdata[c("date","total_bycatch","trawled_area_hake",
   "trawled_area_norwaylobs","trawled_area_red","bycatch_hake","bycatch_norwaylobs","bycatch_red")]

# Join the daily coverage records into a separate table.
coverage <- bind_rows(audit_rows)

# Save daily central catches and swept areas, not confidence-interval widths.
write.csv(bycatchdata,file.path(output_dir,"bycatch_summary.csv"),row.names=FALSE)

# Save the coverage table so excluded effort is visible when interpreting the annual total.
write.csv(coverage,file.path(output_dir,"bycatch_coverage_audit.csv"),row.names=FALSE)

# Keep cells with no usable predictions on any day as NA in the annual map.
annual_values[!annual_available] <- NA_real_

# Leave the annual raster empty until it is clear which cells have usable predictions.
values(annual) <- annual_values

# Create the output folder if it does not already exist.
dir.create(file.path(output_dir,"sum"),showWarnings=FALSE)

# Save the sum of available daily catches per cell for later maps and PSA comparisons.
writeRaster(annual,file.path(output_dir,"sum/aggregated_bycatch_map.tiff"),overwrite=TRUE,datatype="FLT8S")

# Sum each numeric daily results column to obtain its annual central total.
total_sums <- colSums(bycatchdata[,-1],na.rm=TRUE)

# Save the annual catches and swept areas as a small table.
write.csv(data.frame(quantity=names(total_sums),total=unname(total_sums)),
          file.path(output_dir,"annual_totals.csv"),row.names=FALSE)

# Check that summing the annual catch map gives the same result as summing the daily table.
if (abs(cellStats(annual,"sum")-sum(bycatchdata$total_bycatch))>1e-6) stop("Annual catch mismatch")
print(total_sums)

# Print the effort that could not be integrated because BPUE was missing.
message("Study hours without BPUE: ",sum(coverage$hours_without_BPUE))

# Report central estimates for the cells with model coverage; do not label these as annual CI
# bounds.
# Do not use the former data-frame resampling, because it did not propagate model uncertainty.
# Leave annual uncertainty for a separately agreed calculation rather than invent an interval
# here.


