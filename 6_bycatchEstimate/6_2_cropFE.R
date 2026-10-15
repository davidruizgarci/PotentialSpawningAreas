# ------------------------------------------------------------------------------
# 6.2. Select the original GSA06 and 20-700 m study domain without resampling hours
# ------------------------------------------------------------------------------
library(raster)
library(sf)

# Check that the local setup has been run before looking for the helper files.
if (!exists("code_dir")) stop("Run the local 0_setup.R first")
source(file.path(code_dir,"fun","effort_grid_checks.R"))


# 1. Locate the conservative daily maps from 6.1 ---------------------------------
# Keep this new effort series in its own folder, separate from the historical data.
effort_dir <- file.path(input_data,"gfwr/rawdata/monthly_v4_2021")

# Read the conservative maps produced by 6.1, not the earlier interpolated maps.
map_dir <- file.path(effort_dir,"summarydata/daily/maps_conservative")

# Keep the study-area maps in a separate subfolder so the full rectangle remains available.
study_dir <- file.path(map_dir,"study_domain")

# Create the output folder if it does not already exist.
dir.create(study_dir,recursive=TRUE,showWarnings=FALSE)

# Use the full 2021 calendar; every date must have its corresponding input files.
dates <- seq(as.Date("2021-01-01"),as.Date("2021-12-31"),by="day")

# Require exactly the expected dated files, rather than trust their directory order.
files <- daily_files(map_dir,dates,function(day) paste0(day,".tif"))

# Read the reference raster to keep its exact cell size, cell positions and map extent.
reference <- raster(files[1])


# 2. Prepare bathymetry on this fixed grid; interpolate depth, never fishing hours
# Keep the original GEBCO source and the 20-700 m depth range for integrating bycatch.
# Load the original GEBCO elevations, where underwater values are negative.
bathy <- raster(file.path(input_data,"gebco/Bathy.tif"))

# Align this continuous depth surface to the reference grid. Do not interpolate fishing hours.
bathy <- resample(bathy,reference,method="bilinear")

# Change signed elevation into positive water depth for the 20-700 m test below.
depth <- -getValues(bathy)

# Read the management-area polygons used in the original analysis.
GSA <- st_read(file.path(input_data,"GSAs/GSAs_simplified.shp"),quiet=TRUE)

# Select GSA06 and put its polygon in the reference raster coordinate system.
GSA <- st_transform(GSA[GSA$SECT_COD=="GSA06",],crs(reference))

# Stop if GSA06 cannot be found, rather than continue with the wrong region.
if (!nrow(GSA)) stop("GSA06 polygon missing")

# Mark cells whose centres fall in GSA06, retaining the rasterized polygon convention.
gsa_mask <- rasterize(as(GSA,"Spatial"),reference,field=1,background=NA)

# Select cells by their centres, following the polygon rasterization used in the study maps.
# Identify which prediction cells belong to the selected management area.
gsa_valid <- is.finite(getValues(gsa_mask))

# Retain only finite water depths between 20 and 700 metres, including both limits.
depth_valid <- is.finite(depth) & depth>=20 & depth<=700

# Keep cells only when they pass both the GSA06 and depth tests.
selected <- gsa_valid & depth_valid

# Create a mask on the reference grid; its values will select cells without changing their
# hours.
domain <- raster(reference)

# Mark retained cells with 1 and cells outside the study area with NA.
values(domain) <- ifelse(selected,1,NA_real_)

# Save the study mask to inspect precisely which cells are included.
writeRaster(domain,file.path(study_dir,"study_mask.tif"),overwrite=TRUE)

# Save the aligned signed depths for the later assignment of trawling parameters.
writeRaster(bathy,file.path(study_dir,"study_bathymetry.tif"),overwrite=TRUE,datatype="FLT8S")


# 3. Mask and audit hours, retaining zeros inside the selected domain ------------
# Reserve one row per day to account for hours kept and excluded by each selection.
audits <- vector("list",length(dates))

# Create an empty raster on the same grid to accumulate the daily results.
annual <- raster(reference)

# Start at zero inside the study area and keep NA outside it.
values(annual) <- ifelse(selected,0,NA_real_)
for (i in seq_along(dates)) {

  # Take the same calendar date for the effort, prediction and output below.
  day <- dates[i]

  # Select the effort file already matched to this date.
  file <- files[i]

  # Read the record saved beside the source effort raster.
  a <- readRDS(paste0(file,".effort.rds"))

  # Check that the file comes from the conservative transfer and has not changed since its
  # audit.
  if (!identical(a$method,"spherical_cell_overlap") ||
      !identical(a$md5,unname(tools::md5sum(file)))) stop("Effort source audit mismatch")

  # Load the original daily rectangle before applying the study mask.
  r <- raster(file)

  # Stop if these grids differ; matching the number of cells alone would not be enough.
  compareRaster(reference,r,extent=TRUE,rowcol=TRUE,res=TRUE,orig=TRUE,crs=TRUE)

  # Read the original cell hours to account for the effect of the study-area selection.
  hours <- getValues(r)

  # Expect finite non-negative hours across this verified rectangle, including genuine
  # recorded zeros.
  if (any(!is.finite(hours) | hours<0)) stop("Invalid rectangular effort grid")

  # Exclude cells outside the study area without redistributing or smoothing their hours.
  kept <- mask(r,domain)

  # Retain the date as the filename for direct matching in 6.4.
  outfile <- file.path(study_dir,paste0(day,".tif"))

  # Save the selected hours with the same cell positions and double precision.
  writeRaster(kept,outfile,overwrite=TRUE,datatype="FLT8S")

  # Save the effort and depth checksums so 6.4 can verify exactly which files it is using.
  saveRDS(list(method="conservative_study_mask",date=day,units="hours/cell/day",
     dataset=a$dataset,md5=unname(tools::md5sum(outfile)),
     depth_md5=unname(tools::md5sum(file.path(study_dir,"study_bathymetry.tif")))),
     paste0(outfile,".effort.rds"))

  # Separate hours outside GSA06, hours excluded by depth, and hours retained for integration.
  audits[[i]] <- data.frame(date=day,rectangle_hours=sum(hours),
     outside_GSA06_hours=sum(hours[!gsa_valid]),
     outside_depth_hours=sum(hours[gsa_valid & !depth_valid]),
     study_hours=sum(hours[selected]))

  # Add this day to the annual effort in the selected study cells.
  annual <- annual + kept
}

# Save the daily breakdown of retained and excluded fishing hours.
write.csv(do.call(rbind,audits),file.path(study_dir,"study_effort_audit.csv"),row.names=FALSE)

# Save total annual study-area hours, not an average across days.
writeRaster(annual,file.path(study_dir,"2021_study_effort.tif"),overwrite=TRUE,datatype="FLT8S")
message("Study effort ready; 6_4 reads corrected daily hurdle maps directly.")



# 4. Plot daily fishing effort within the study area ----------------------------

# Choose the day to inspect.
day <- as.Date("2021-01-15")

# Load the selected daily map produced by 6_2.
effort_file <- file.path(study_dir, paste0(day, ".tif"))
effort_map <- raster::raster(effort_file)

# Convert the raster to a table and use predictable column names.
effort_df <- as.data.frame(effort_map, xy = TRUE)
names(effort_df) <- c("Lon", "Lat", "FishingHours")

# Keep positive hours for the logarithmic colour scale.
# Zeros remain unchanged in the saved raster.
effort_df <- effort_df[
  is.finite(effort_df$FishingHours) & effort_df$FishingHours > 0,]

# Load the coastline for the background.
land <- sf::st_read(
  file.path(input_data, "landmask/Europa/Europe_coastline_poly.shp"),
  quiet = TRUE)
land <- sf::st_transform(land, 4326)

# Read GSA06 to show the study-area boundary.
gsa <- sf::st_read(
  file.path(input_data, "GSAs/GSAs_simplified.shp"),
  quiet = TRUE)
gsa <- sf::st_transform(gsa[gsa$SECT_COD == "GSA06", ], 4326)

# Show hours per prediction cell, after the GSA06 and 20–700 m selection.
p <- ggplot2::ggplot() +
  ggplot2::geom_tile(
    data = effort_df,
    ggplot2::aes(x = Lon, y = Lat, fill = FishingHours),
    width = raster::res(effort_map)[1],
    height = raster::res(effort_map)[2]) +
  ggplot2::geom_sf(
    data = land, fill = "grey85", colour = "grey50",
    linewidth = 0.2) +
  ggplot2::geom_sf(
    data = gsa, fill = NA, colour = "black",
    linewidth = 0.4, linetype = "dashed" ) +
  ggplot2::coord_sf(
    xlim = c(-1.5, 5), ylim = c(37, 43),
    expand = FALSE ) +
  ggplot2::scale_fill_viridis_c(
    trans = "log10",
    name = "Fishing hours\nper cell" ) +
  ggplot2::labs(
    title = paste("Daily trawling effort —", day),
    subtitle = "GFW v4 · GSA06 · 20–700 m",
    x = "Longitude",
    y = "Latitude" ) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid = ggplot2::element_blank())

print(p)

# Print the total recorded hours retained in the study area for this day.
raster::cellStats(effort_map, stat = "sum", na.rm = TRUE)
