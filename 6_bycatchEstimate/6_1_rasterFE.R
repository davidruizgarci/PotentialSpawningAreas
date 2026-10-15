# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 6.1. Prepare daily fishing effort maps
#-------------------------------------------------------------------------------
library(gfwr)

# Load the downloader that requests a fixed GFW version from the laptop code folder.
# Check that the local setup has been run before looking for the helper files.
if (!exists("code_dir")) stop("Run the local 0_setup.R first")
source(file.path(code_dir, "fun", "complete_effort_month.R"))
source(file.path(code_dir, "fun", "download_effort_version.R"))

# Preserve the historical effort used in my models and store the new bycatch effort
# separately.
# Fix the GFW data version so a later run does not silently use a newer product.
gfw_dataset <- "public-global-fishing-effort:v4.0"

# Keep this new effort series in its own folder, separate from the historical data.
effort_dir <- file.path(input_data, "gfwr", "rawdata", "monthly_v4_2021")

library(tidyverse)
library(qdapRegex)
library(readr)
library(sf)
library(ggplot2)
library(raster)

# 1. Download daily fishing effort data from GFW for 2021-----------------------
# Use 2021 here, because this is the year of the bycatch calculation.
years <- c(2021)

# Request complete months, including the final day of each month.
# Reuse a saved month only after its version, dates and file checksum have been checked.
for (y in years) {
  for (month in 1:12) {

    # Start the request on the first day of this month.
    start_date <- as.Date(sprintf("%d-%02d-01", y, month))

    # Stop at the first day of the next month: GFW excludes this end date, so the last day is
    # included.
    end_date <- next_month_start(start_date)

    # Download this complete month, or check and reuse it if it is already saved.
    download_effort_version(
      file = file.path(effort_dir, sprintf("%d-%02d.csv", y, month)),
      start_date = start_date,
      end_date = end_date,
      key = key,
      dataset = gfw_dataset
    )
  }
}

# 2. Summarize the complete daily series for trawlers ----------------------------
# Read the twelve verified monthly reports using full paths, without changing the working
# directory.
# List the twelve monthly CSVs explicitly, to avoid accidentally reading other summaries.
monthly_files <- file.path(effort_dir, sprintf("2021-%02d.csv", 1:12))

# Stop if a monthly file is missing rather than build an incomplete year.
if (!all(file.exists(monthly_files))) stop("Complete the twelve downloads first")

# Write out the complete calendar to detect missing dates, including at the ends of the year.
expected_dates <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by="day")

# Read each month after checking the information saved with its download.
data_list <- lapply(monthly_files, function(file) {

  # Recover the saved data version and file checksum for this month.
  audit <- readRDS(paste0(file, ".source.rds"))

  # Check both the requested version and the file contents before using the report.
  if (!identical(audit$dataset, gfw_dataset) ||
      !identical(audit$md5, unname(tools::md5sum(file)))) stop("Monthly source audit mismatch")

  # Keep the original column names when reading the report into a data frame.
  as.data.frame(readr::read_csv(file, show_col_types=FALSE))
})

# Join the monthly reports into one table; their rows still describe individual days.
outputraw <- dplyr::bind_rows(data_list)

# Convert the GFW date column into R dates so comparisons use dates rather than text.
outputraw$Date <- as.Date(outputraw[["Time Range"]])

# Check that all 365 dates are represented in the raw reports before selecting trawlers.
if (!setequal(as.character(unique(outputraw$Date)),as.character(expected_dates))) {
  stop("Raw downloads do not cover the complete expected calendar")
}

# Prepare one record per location and day for the trawl fleet.
summary_data <- outputraw %>%

  # Keep trawlers because these are the fishing operations used in my bycatch calculation.
  filter(geartype == "trawlers") %>%

  # Put together records that share the same position and day.
  group_by(Lat, Lon, Date) %>%

  # Sum fishing hours within each group. The vessel-count sum is not a count of unique boats.
  summarize(TotalVessel_IDs=sum(.data[["Vessel IDs"]]),
            TotalFishingHours=sum(.data[["Apparent Fishing Hours"]]), .groups="drop")

# Retain the sum of reported vessel counts as auxiliary information; do not interpret it as
# unique boats.
# Reject missing, infinite or negative hours before creating the daily files.
if (any(!is.finite(summary_data$TotalFishingHours) | summary_data$TotalFishingHours < 0)) {
  stop("Invalid daily fishing hours")
}

# Put the daily summary and annual checks together under the new data folder.
summary_dir <- file.path(effort_dir,"summarydata")

# Keep the individual daily CSVs in this subfolder.
daily_dir <- file.path(summary_dir,"daily")

# Create the output folder if it does not already exist.
dir.create(daily_dir,recursive=TRUE,showWarnings=FALSE)

# Save the complete daily table to inspect it without repeating the aggregation.
write.csv(summary_data,file.path(summary_dir,"Daily_summaryTrawlEffort.csv"),row.names=FALSE)


# 3. Export one CSV per calendar day, including verified days without trawlers ---
for (i in seq_along(expected_dates)) {

  # Take one calendar day at a time, including days with no recorded trawling.
  day <- expected_dates[i]

  # Select only this date. An empty table means no trawler rows in the verified raw report.
  day_data <- summary_data[summary_data$Date == day,]

  # Save a dated CSV even if it has no rows, so the calendar stays complete.
  write.csv(day_data,file.path(daily_dir,paste0(day,".csv")),row.names=FALSE)
}


# 4. Transfer hours by overlap onto the actual prediction grid ------------------
source(file.path(code_dir,"fun","effort_grid_checks.R"))

# Use a corrected hurdle map as the reference grid; do not create an approximate 0.042-degree
# grid.
reference_file <- file.path(output_data,"brt","Raja_PA_bernuilli_Final2",
                  "predict_boost/2021/01/hurdle_X20210101_Raja_brt_pred.tif")

# Read the reference raster to keep its exact cell size, cell positions and map extent.
reference <- raster(reference_file)

# Also read the other genus to check that the same effort grid can serve both.
other <- raster(file.path(output_data,"brt","Scyliorhinus_PA_bernuilli_Final2",
                  "predict_boost/2021/01/hurdle_X20210101_Scyliorhinus_brt_pred.tif"))

# Stop if these grids differ; matching the number of cells alone would not be enough.
compareRaster(reference,other,extent=TRUE,rowcol=TRUE,res=TRUE,orig=TRUE,crs=TRUE)

# Reuse the overlap fractions across days and keep the original GFW cell-centre convention.
# Collect each original GFW position once, rather than recalculate its overlap on every day.
coords <- unique(as.data.frame(summary_data[c("Lon","Lat")]))

# Give each coordinate pair a text key; rounding avoids tiny differences in stored decimal
# numbers.
coord_key <- function(x) paste(round(x$Lon,6),round(x$Lat,6),sep="_")

# Work out what fraction of each GFW cell overlaps each prediction cell.
mapping <- effort_overlap(coords,reference)

# Retain the coordinate keys in the same order as the overlap calculation.
keys <- coord_key(coords)

# Save these rasters separately from the older bilinearly interpolated maps.
map_dir <- file.path(daily_dir,"maps_conservative")

# Create the output folder if it does not already exist.
dir.create(map_dir,recursive=TRUE,showWarnings=FALSE)

# Create an empty raster on the same grid to accumulate the daily results.
annual <- raster(reference)

# Start the annual hours at zero and add each daily raster once.
values(annual) <- 0

# Reserve one entry per day to record hours before and after the transfer.
audits <- vector("list",length(expected_dates))

# Move hours by overlap fractions; do not smooth or average these totals.
for (i in seq_along(expected_dates)) {

  # Take one calendar day at a time, including days with no recorded trawling.
  day <- expected_dates[i]

  # Select this day from the complete daily effort table.
  d <- summary_data[summary_data$Date == day,]

  # Start every known source position at zero for this verified day.
  hours <- numeric(nrow(coords))

  # Insert the reported hours at their matching GFW positions, without relying on row order.
  hours[match(coord_key(d),keys)] <- d$TotalFishingHours

  # Split hours by cell overlap and check that the assigned hours are conserved inside the
  # rectangle.
  transferred <- transfer_effort(hours,mapping,reference)

  # Take the daily hours raster returned by that calculation.
  r <- transferred$raster

  # Give the hours layer an explicit name before saving it.
  names(r) <- "TotalFishingHours"

  # Use the calendar date in the filename so the next scripts can match the correct day.
  file <- file.path(map_dir,paste0(day,".tif"))

  # Save hours per cell and day with double precision to limit rounding differences.
  writeRaster(r,file,overwrite=TRUE,datatype="FLT8S")

  # Save the method, units, version, date and checksum beside this raster for later checks.
  saveRDS(list(method="spherical_cell_overlap",units="hours/cell/day",
              dataset=gfw_dataset,date=day,source_cell_resolution=0.01,
              md5=unname(tools::md5sum(file))),paste0(file,".effort.rds"))

  # Add this day to the annual raster; these are hours, sum them rather than average them.
  annual <- annual + r

  # Record all reported hours, the hours inside the rectangle, and the hours outside it
  # separately.
  audits[[i]] <- data.frame(date=day,input_hours=transferred$input_hours,
      rectangular_domain_hours=transferred$domain_hours,
      outside_rectangle_hours=transferred$outside_hours,
      raster_hours=transferred$raster_hours)
}

# Save the daily conservation table to check totals without reopening every raster.
write.csv(bind_rows(audits),file.path(summary_dir,"daily_conservation_audit.csv"),row.names=FALSE)


# 5. Check an explicitly selected day, independent of the previous loop ----------
# Choose the date to inspect; changing this does not alter the calculations above.
day <- as.Date("2021-01-15")

# Build the landmask path with directory separators supplied by file.path.
land <- file.path(input_data,"landmask/Europa/Europe_coastline_poly.shp")

# Read the coastline and put it in longitude and latitude for plotting.
mask <- st_transform(st_read(land,quiet=TRUE),4326)

# Explicitly load the chosen day, rather than reuse the last raster left by the loop.
raster_day <- raster(file.path(map_dir,paste0(day,".tif")))

# Turn the raster into coordinates and hours for ggplot.
raster_df <- as.data.frame(raster_day,xy=TRUE)

# Set predictable column names because a raster reader may use the filename as its layer name.
names(raster_df) <- c("x","y","TotalFishingHours")

# Only remove zeros and invalid values from this plotting table; keep the saved effort rasters
# unchanged.
raster_df <- raster_df[is.finite(raster_df$TotalFishingHours) & raster_df$TotalFishingHours>0,]

# Plot the selected day on the study-area scale so the effort cells are visible.
p <- ggplot() +
  geom_tile(data=raster_df,aes(x=x,y=y,fill=TotalFishingHours)) +
  geom_sf(data=mask,fill="grey80") +
  coord_sf(xlim=c(-1.5,5),ylim=c(37,43),expand=FALSE) +

  # Use logarithmic colours only for display; the saved hours remain on their original scale.
  scale_fill_viridis_c(trans="log10",name="Fishing hours") +
  theme_bw() + labs(title=as.character(day))
print(p)


# 6. Summarize the same new series across the complete year ---------------------
# Keep the full GFCM annual CSV separate from the annual raster for the prediction rectangle.
# Sum all dates at each original GFW position to describe the same new series for the whole
# year.
annual_summary <- summary_data %>% group_by(Lat,Lon) %>%
  summarize(TotalFishingHours=sum(TotalFishingHours),.groups="drop")

# Save the annual table for the full downloaded region, before the later GSA06 selection.
write.csv(annual_summary,file.path(summary_dir,"2021_summaryTrawlEffort.csv"),row.names=FALSE)

# Save the annual hours already accumulated on the prediction rectangle.
writeRaster(annual,file.path(summary_dir,"2021_FE_conservative.tif"),overwrite=TRUE,datatype="FLT8S")

# Check that the sum of the annual raster matches the sum of the daily raster totals.
if (abs(cellStats(annual,"sum")-sum(bind_rows(audits)$raster_hours)) > 1e-6) {
  stop("Annual raster and daily hours disagree")
}
message("Daily and annual effort ready. Continue with 6_2_cropFE.R.")
