# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management

# ------------------------------------------------------------------------------
# Predict joint hurdle means and retain replicate identity for later integration
# ------------------------------------------------------------------------------

# 1. Read a rebuilt daily environmental stack and match predictor names -------
# Check depth sign and ocean membership independently before selecting the map domain.
# Return the study-area grid restricted to ocean depths greater than 20 and at most 700 m.
prepare_hurdle_grid <- function(stack_file, elevation_file) {
  s <- raster::stack(stack_file)
  dummy <- as.data.frame(setNames(rep(list(numeric(0)), raster::nlayers(s)), names(s)))
  names(s) <- names(normalise_model_columns(dummy))
  if (!"depth" %in% names(s)) stop("Missing depth layer")
  assert_depth_raster(s$depth, "Joint prediction input")
  elevation <- raster::raster(elevation_file)
  ocean <- ocean_mask_for_grid(elevation, s$depth)
  bad <- raster::overlay(s$depth, ocean, fun = function(d, z) as.integer(!is.na(d) & is.na(z)))
  if (raster::cellStats(bad, "sum", na.rm = TRUE) > 0) stop("Depth values on land; rebuild stacks")
  # Restrict the grid to the study extent before applying the depth domain.
  s <- raster::crop(s, raster::extent(-1.5, 5, 37, 43))
  # Retain ocean cells deeper than 20 m and no deeper than 700 m.
  mask <- raster::calc(s$depth, function(z) ifelse(z > 20 & z <= 700, 1, NA_real_))
  s <- raster::mask(s, mask)
  if (!"ln_fishingEffort" %in% names(s)) {
    if (!"fishingEffort" %in% names(s)) stop("Missing fishing effort predictor")
    s$ln_fishingEffort <- raster::calc(s$fishingEffort, function(z) {
      if (any(!is.na(z) & (!is.finite(z) | z < 0))) stop("Invalid effort predictor")
      log1p(z)
    })
  }
  s
}


# 2. Write the fitted mean and uncertainty for one genus and day --------------
# Every uncertainty draw multiplies the two components from the same bootstrap object.
# Keep each replicate band and its identity so annual totals can be summed correctly.
write_joint_hurdle_day <- function(ensemble, s, out_prefix) {
  predictors <- ensemble$main$predictors
  if (!all(predictors %in% names(s))) stop("Missing model predictors")
  ids <- vapply(ensemble$replicates, `[[`, integer(1), "replicate_id")
  if (!identical(ids, seq_along(ids))) stop("Missing, duplicated or unordered replicate IDs")
  B <- length(ids)
  empty_brick <- function(n) {
    raster::brick(
      nrows = nrow(s), ncols = ncol(s),
      xmn = raster::xmin(s), xmx = raster::xmax(s), ymn = raster::ymin(s), ymx = raster::ymax(s),
      crs = raster::crs(s), nl = n
    )
  }
  # Allocate bands for the fitted mean, bootstrap mean, interval and occurrence.
  summary <- empty_brick(5L)
  names(summary) <- c("mean", "bootstrap_mean", "lower_025", "upper_975", "occurrence")
  # Allocate one band per paired bootstrap replicate for later integration.
  draws <- empty_brick(B)
  names(draws) <- sprintf("rep_%04d", ids)
  summary <- raster::writeStart(summary, paste0(out_prefix, "_summary.tif"), overwrite = FALSE)
  draws <- raster::writeStart(draws, paste0(out_prefix, "_replicates.tif"), overwrite = FALSE)
  blocks <- raster::blockSize(s)
  n_complete <- 0L
  n_domain <- 0L
  for (i in seq_len(blocks$n)) {
    values <- as.data.frame(raster::getValues(s, row = blocks$row[i], nrows = blocks$nrows[i]))
    # Predict only cells with all required predictors; retain missing cells as NA.
    valid <- complete.cases(values[predictors])
    n_domain <- n_domain + sum(!is.na(values$depth))
    n_complete <- n_complete + sum(valid)
    # Start summaries as missing so incomplete cells cannot become false zero densities.
    a <- matrix(NA_real_, nrow(values), 5)
    b <- matrix(NA_real_, nrow(values), B)
    if (any(valid)) {
      x <- values[valid, , drop = FALSE]
      main <- predict_hurdle_choice(ensemble$main, x)
      if (any(!is.finite(main$prediction) | main$prediction < 0) || any(!is.finite(main$p) | main$p < 0 | main$p > 1)) {
        stop("Invalid full-data predictions")
      }
      # Apply each paired hurdle model to the same complete prediction cells.
      z <- vapply(
        ensemble$replicates, function(model) predict_hurdle_choice(model, x)$prediction,
        numeric(nrow(x))
      )
      z <- matrix(z, nrow = nrow(x), ncol = B)
      if (any(!is.finite(z) | z < 0)) stop("Invalid joint predictions")
      # Calculate cell-wise conditional mean uncertainty from the joint replicate
      # distribution.
      q <- t(apply(z, 1, stats::quantile, probs = c(.025, .975), names = FALSE))
      a[valid, ] <- cbind(main$prediction, rowMeans(z), q, main$p)
      b[valid, ] <- z
    }
    summary <- raster::writeValues(summary, a, blocks$row[i])
    draws <- raster::writeValues(draws, b, blocks$row[i])
  }
  # Close raster writers before recording paths and file fingerprints.
  summary <- raster::writeStop(summary)
  draws <- raster::writeStop(draws)
  domain <- raster::calc(s$depth, function(z) ifelse(!is.na(z), 1, NA_real_))
  domain <- raster::writeRaster(domain, paste0(out_prefix, "_domain.tif"), overwrite = FALSE)
  if (n_complete == 0L) stop("No complete prediction cells")
  list(
    summary = normalizePath(raster::filename(summary)),
    replicates = normalizePath(raster::filename(draws)), domain = normalizePath(raster::filename(domain)), domain_cells = n_domain,
    complete_cells = n_complete, missing_predictor_cells = n_domain - n_complete,
    replicate_ids = ids, replicates_md5 = unname(tools::md5sum(raster::filename(draws))), units = "capsules/km2",
    interval = "Conditional bootstrap uncertainty for the fitted mean, not new-catch prediction intervals"
  )
}
