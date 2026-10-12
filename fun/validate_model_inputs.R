# ------------------------------------------------------------------------------
# Shared input checks for model fitting and spatial prediction
# ------------------------------------------------------------------------------

# 1. Read dates explicitly and match extraction dates by unique haul code --------
strict_date <- function(x, format = '%d/%m/%Y') {
  y <- as.Date(x, format = format)
  if (anyNA(y) || any(base::format(y, format) != x)) stop('Invalid or ambiguous date')
  y
}

date_from_code <- function(code) {
  iso <- ifelse(grepl('^[0-9]{8}_', code), substr(code, 1, 8),
    ifelse(grepl('^[0-9]{2}_[0-9]{2}_[0-9]{2}_', code),
      paste0('20', substr(code, 1, 2), substr(code, 4, 5), substr(code, 7, 8)), NA))
  strict_date(iso, '%Y%m%d')
}

restore_extraction_dates <- function(final, upstream) {
  if (anyDuplicated(final$code) || anyDuplicated(upstream$code)) stop('Duplicate haul code')
  j <- match(final$code, upstream$code)
  if (anyNA(j)) stop('Unmatched haul code')
  dates <- strict_date(upstream$date[j])
  if (any(dates != date_from_code(final$code))) stop('Extraction date/code disagreement')
  before <- strict_date(final$date)
  changed <- before != dates
  log <- data.frame(code = final$code[changed], before = as.character(before[changed]),
    after = as.character(dates[changed]))
  final$date <- format(dates, '%d/%m/%Y')
  list(data = final, changes = log)
}

# 2. Rename predictors by name, never by column position ------------------------
normalise_model_columns <- function(data) {
  aliases <- c(LN_fishingEffort = 'ln_fishingEffort', LN_slope = 'ln_slope',
    bottomT_Reanalysis = 'bottom_temp', seabottom_o2Reanalysis = 'bottom_oxygen',
    seabottom_nppvReanalysis = 'bottom_nppv', seabottom_phReanalysis = 'bottom_ph',
    seabottom_nh4Reanalysis = 'bottom_nh4', seabottom_no3Reanalysis = 'bottom_no3',
    seabottom_po4Reanalysis = 'bottom_po4', seabottom_soReanalysis = 'bottom_so',
    seabottom_uoReanalysis = 'bottom_uo', seabottom_voReanalysis = 'bottom_vo',
    eke = 'bottom_eke', subs = 'substrate')
  for (old in names(aliases)) {
    new <- aliases[[old]]
    if (old %in% names(data) && new %in% names(data)) stop('Duplicate predictor aliases: ', new)
    names(data)[names(data) == old] <- new
  }
  data
}

read_model_data <- function(file, extraction_file) {
  data <- read.csv2(file, stringsAsFactors = FALSE)
  upstream <- read.csv2(extraction_file, stringsAsFactors = FALSE)
  fixed <- restore_extraction_dates(data, upstream)
  data <- normalise_model_columns(fixed$data)
  assert_positive_depth(data$depth, 'Model training data')
  if (any(!is.finite(data$N_km2)) || any(data$N_km2 < 0)) stop('Invalid BPUE')
  if (any(data$presence_absence != as.integer(data$N > 0))) stop('Inconsistent occurrence response')
  attr(data, 'date_changes') <- fixed$changes
  data
}

# 3. Convert signed elevation once; do not use abs(), which would include land ---
depth_from_elevation <- function(elevation) {
  if (any(!is.na(elevation) & !is.finite(elevation))) stop('Non-finite bathymetry')
  ifelse(!is.na(elevation) & elevation < 0, -elevation, NA_real_)
}

assert_positive_depth <- function(depth, context = 'Depth layer') {
  available <- depth[!is.na(depth)]
  if (!length(available) || any(!is.finite(available)) || any(available <= 0) ||
      any(available > 12000)) stop(context, ': expected positive ocean depth in metres')
  invisible(TRUE)
}

assert_depth_raster <- function(depth, context = 'Prediction stack') {
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
  if (!present) stop(context, ': no valid ocean depth cells')
  invisible(TRUE)
}

# 4. Identify ocean at target cell centres without averaging land and sea -------
ocean_mask_for_grid <- function(elevation, grid) {
  mask <- raster::raster(grid)
  xy <- raster::xyFromCell(grid, seq_len(raster::ncell(grid)))
  z <- raster::extract(elevation, xy, method = 'simple')
  raster::values(mask) <- ifelse(!is.na(z) & z < 0, 1, NA_real_)
  mask
}
