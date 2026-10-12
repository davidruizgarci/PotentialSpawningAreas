# ------------------------------------------------------------------------------
# Shared checks for positive ocean depth in the original prediction workflow
# ------------------------------------------------------------------------------

# 1. Convert signed source elevation before building the environmental stack ----
ocean_depth <- function(elevation) {

  # Negative source elevations are ocean. Zero and positive values are land.
  # Do not use abs(): it would turn positive land elevations into ocean depths.
  if (any(!is.na(elevation) & !is.finite(elevation))) stop("Invalid source elevation")
  ifelse(!is.na(elevation) & elevation < 0, -elevation, NA_real_)
}

# 2. Inspect every block instead of relying on cached raster extrema ------------
check_ocean_depth <- function(depth, context) {
  blocks <- raster::blockSize(depth)
  available <- FALSE

  for (i in seq_len(blocks$n)) {
    values <- raster::getValues(depth, row = blocks$row[i], nrows = blocks$nrows[i])
    values <- values[!is.na(values)]
    if (length(values)) {
      available <- TRUE
      if (any(!is.finite(values) | values <= 0 | values > 12000)) {
        stop(context, ": expected positive ocean depth in metres; rebuild the stack")
      }
    }
  }

  if (!available) stop(context, ": no ocean depth values")
  invisible(TRUE)
}

# 3. Identify ocean independently at the centres of the final prediction cells ---
ocean_on_grid <- function(elevation, grid) {

  # Direct sampling avoids averaging a binary mask across land and sea.
  xy <- raster::xyFromCell(grid, seq_len(raster::ncell(grid)))
  values <- raster::extract(elevation, xy, method = "simple")
  result <- raster::raster(grid)
  raster::values(result) <- ifelse(!is.na(values) & values < 0, 1, NA_real_)
  result
}
