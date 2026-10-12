# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management

# ------------------------------------------------------------------------------
# Shared input checks for model fitting and spatial prediction
# ------------------------------------------------------------------------------


# 1. Parse dates with an explicit format and reject silent partial conversions ---
strict_date <- function(x, format = "%d/%m/%Y") {
  y <- as.Date(x, format = format)
  if (anyNA(y) || any(base::format(y, format) != x)) stop("Invalid or ambiguous date")
  y
}


# 2. Extract the sampling date encoded in the two supported haul-code formats ---
date_from_code <- function(code) {
  iso <- ifelse(grepl("^[0-9]{8}_", code), substr(code, 1, 8),
    ifelse(grepl("^[0-9]{2}_[0-9]{2}_[0-9]{2}_", code),
      paste0("20", substr(code, 1, 2), substr(code, 4, 5), substr(code, 7, 8)), NA
    )
  )
  strict_date(iso, "%Y%m%d")
}


# 3. Match dates by unique haul code, never by row position -------------------
# Retain every other field of the final corrected model table.
restore_extraction_dates <- function(final, upstream) {
  if (anyDuplicated(final$code) || anyDuplicated(upstream$code)) stop("Duplicate haul code")
  # Join the extraction reference by haul identifier rather than row position.
  j <- match(final$code, upstream$code)
  if (anyNA(j)) stop("Unmatched haul code")
  dates <- strict_date(upstream$date[j])
  if (any(dates != date_from_code(final$code))) stop("Extraction date/code disagreement")
  before <- strict_date(final$date)
  # Record only date corrections; retain the other corrected final-table fields.
  changed <- before != dates
  log <- data.frame(
    code = final$code[changed], before = as.character(before[changed]),
    after = as.character(dates[changed])
  )
  final$date <- format(dates, "%d/%m/%Y")
  list(data = final, changes = log)
}


# 4. Translate known predictor aliases while detecting duplicate names --------
# Column order is irrelevant; numerical values are not changed.
normalise_model_columns <- function(data) {
  # List recognised historical names and their common prediction-time names.
  aliases <- c(
    LN_fishingEffort = "ln_fishingEffort", LN_slope = "ln_slope",
    bottomT_Reanalysis = "bottom_temp", seabottom_o2Reanalysis = "bottom_oxygen",
    seabottom_nppvReanalysis = "bottom_nppv", seabottom_phReanalysis = "bottom_ph",
    seabottom_nh4Reanalysis = "bottom_nh4", seabottom_no3Reanalysis = "bottom_no3",
    seabottom_po4Reanalysis = "bottom_po4", seabottom_soReanalysis = "bottom_so",
    seabottom_uoReanalysis = "bottom_uo", seabottom_voReanalysis = "bottom_vo",
    eke = "bottom_eke", subs = "substrate"
  )
  for (old in names(aliases)) {
    new <- aliases[[old]]
    if (old %in% names(data) && new %in% names(data)) stop("Duplicate predictor aliases: ", new)
    names(data)[names(data) == old] <- new
  }
  data
}


# 5. Read the final CSV and its environmental extraction reference ------------
# Restore keyed dates, standardise names and check the model response and depth.
read_model_data <- function(file, extraction_file) {
  data <- read.csv2(file, stringsAsFactors = FALSE)
  upstream <- read.csv2(extraction_file, stringsAsFactors = FALSE)
  fixed <- restore_extraction_dates(data, upstream)
  data <- normalise_model_columns(fixed$data)
  assert_positive_depth(data$depth, "Model training data")
  if (any(!is.finite(data$N_km2)) || any(data$N_km2 < 0)) stop("Invalid BPUE")
  if (any(data$presence_absence != as.integer(data$N > 0))) stop("Inconsistent occurrence response")
  attr(data, "date_changes") <- fixed$changes
  data
}

# 3. Convert signed elevation once; do not use abs(), which would include land ---

# 6. Convert negative ocean elevation to positive depth in metres -------------
# Positive or zero elevations are land and remain missing; never use abs().
depth_from_elevation <- function(elevation) {
  if (any(!is.na(elevation) & !is.finite(elevation))) stop("Non-finite bathymetry")
  ifelse(!is.na(elevation) & elevation < 0, -elevation, NA_real_)
}


# 7. Reject missing-only, non-finite, negative and implausible depth values ---
assert_positive_depth <- function(depth, context = "Depth layer") {
  # Check observed depth values while allowing missing cells outside the ocean domain.
  available <- depth[!is.na(depth)]
  if (!length(available) || any(!is.finite(available)) || any(available <= 0) ||
    any(available > 12000)) {
    stop(context, ": expected positive ocean depth in metres")
  }
  invisible(TRUE)
}


# 8. Check every block of the saved depth raster, not just cached extrema -----
assert_depth_raster <- function(depth, context = "Prediction stack") {
  # Scan all raster blocks rather than checking only a sample or cached min/max.
  blocks <- raster::blockSize(depth)
  present <- FALSE
  for (i in seq_len(blocks$n)) {
    z <- raster::getValues(depth, row = blocks$row[i], nrows = blocks$nrows[i])
    if (any(!is.na(z))) {
      assert_positive_depth(z, context)
      present <- TRUE
    }
  }
  if (!present) stop(context, ": no valid ocean depth cells")
  invisible(TRUE)
}


# 9. Identify ocean from source elevation at each target cell centre ----------
# Do not average land and sea while transferring this binary domain mask.
ocean_mask_for_grid <- function(elevation, grid) {
  mask <- raster::raster(grid)
  # Locate target cell centres for direct sampling of the signed source elevation.
  xy <- raster::xyFromCell(grid, seq_len(raster::ncell(grid)))
  z <- raster::extract(elevation, xy, method = "simple")
  raster::values(mask) <- ifelse(!is.na(z) & z < 0, 1, NA_real_)
  mask
}
