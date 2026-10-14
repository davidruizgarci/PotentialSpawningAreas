# ------------------------------------------------------------------------------
# Approximate joint hurdle uncertainty using existing independent ensembles
# ------------------------------------------------------------------------------

# 1. Summarize all cross-products without refitting or saving individual maps ----
joint_hurdle_uncertainty <- function(genus, dates, data_dir, output_dir,
                                     overwrite = FALSE) {
  if (!genus %in% c("Raja", "Scyliorhinus")) stop("Unknown genus")
  if (!inherits(dates, "Date") || anyDuplicated(dates)) stop("Supply unique dates")
  requireNamespace("gbm", quietly = TRUE)
  density_folder <- if (genus == 'Raja') 'Raja_NKm2_LN_gaussian_Final2' else 'Scyliorhinus_PA_gaussian_Final2'
  presence_folder <- paste0(genus, '_PA_bernuilli_Final2')
  load_models <- function(folder) {
    files <- list.files(file.path(data_dir, 'output/brt', folder, 'bootstrap', folder),
                        pattern = '_boot_Nkm2\\.rds$', full.names = TRUE)
    if (length(files) != 100L) stop('Expected 100 saved models: ', folder)
    list(models = lapply(files, readRDS), files = files)
  }
  den <- load_models(density_folder)
  pa <- load_models(presence_folder)

  # Calculate each saved model's training-residual correction only once.
  corrections <- vapply(den$models, training_smearing, numeric(1))
  hashes <- tools::md5sum(c(den$files, pa$files))
  signed <- raster::raster(file.path(data_dir, "input/summary_static/bathy.tif"))

  # 2. Process one day at a time; retain the original grid and 1-700 m domain ----
  for (i in seq_along(dates)) {
    day <- dates[i]
    stamp <- format(day, "%Y%m%d")
    folder <- file.path(output_dir, format(day, "%Y"), format(day, "%m"))
    files <- file.path(folder, paste0("hurdle_X", stamp, "_", genus,
                                    "_brt_pred_", c("cil", "ciu", "cir"), ".tif"))

    # Resume only outputs certified for this method and these exact models.
    audit_file <- paste0(files[3], ".joint.rds")
    if (!overwrite && all(file.exists(files)) && file.exists(audit_file)) {
      audit <- readRDS(audit_file)
      if (identical(audit$model_md5, hashes) &&
          identical(audit$method, "independent_cross_products") &&
          identical(audit$output_md5, tools::md5sum(files))) {
        message("Joint uncertainty already completed: ", genus, " ", day)
        next
      }
    }
    message("Joint uncertainty: ", genus, " ", day)
    started <- proc.time()[3]
    s <- raster::stack(file.path(data_dir, "input/cmems_predict_3d",
          format(day, "%Y/%m/%d"), paste0("stack_", stamp, ".grd")))
    check_ocean_depth(s$depth, paste("Joint uncertainty", day))
    s <- raster::crop(s, raster::extent(-1.5, 5, 37, 43))
    s <- raster::mask(s, ocean_on_grid(signed, s$depth))
    s <- raster::mask(s, raster::calc(s$depth, function(z)
                     ifelse(z >= 1 & z <= 700, 1, NA_real_)))
    s$ln_fishingEffort <- log1p(s$fishingEffort)
    values <- as.data.frame(raster::getValues(s))
    predictors <- unique(c(den$models[[1]]$gbm.call$predictor.names,
                           pa$models[[1]]$gbm.call$predictor.names))
    if (!length(predictors) || !all(predictors %in% names(values))) stop("Missing predictors")
    valid <- which(complete.cases(values[, predictors, drop = FALSE]))
    if (!length(valid)) stop("No valid prediction cells")
    x <- values[valid, , drop = FALSE]

    # 3. Predict in original units before combining the two components ---------
  D <- vapply(seq_along(den$models), function(k) {
    m <- den$models[[k]]
    positive_bpue_mean(as.numeric(predict(m, x, n.trees = m$gbm.call$best.trees,
                                         type = 'response')), corrections[k])
  }, numeric(nrow(x)))
  P <- vapply(pa$models, function(m) {
    as.numeric(predict(m, x, n.trees = m$gbm.call$best.trees, type = 'response'))
  }, numeric(nrow(x)))
  if (any(!is.finite(D)) || any(!is.finite(P)) || any(P < 0 | P > 1)) stop('Invalid predictions')

    # 4. Calculate 10,000 products per cell, assuming component independence ----
    # These are combinations of 100 existing models, not 10,000 new replicates.
  result <- t(vapply(seq_len(nrow(x)), function(k) {
    products <- as.vector(outer(P[k, ], D[k, ], '*'))
    q <- quantile(products, c(.025, .975), names = FALSE)
    c(central = median(P[k, ]) * median(D[k, ]), lower = q[1], upper = q[2],
      width = q[2] - q[1], old_width_product = diff(quantile(P[k, ], c(.025, .975))) *
        diff(quantile(D[k, ], c(.025, .975))))
  }, numeric(5)))
  colnames(result) <- c('central', 'lower', 'upper', 'width', 'old_width_product')

    # 5. Store bounds and width; do not replace the existing central map --------
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    for (j in seq_along(files)) {
      if (file.exists(files[j])) {
        backup <- sub("\\.tif$", paste0(".before_joint_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".bak"), files[j])
        if (!file.copy(files[j], backup, overwrite = FALSE)) stop("Cannot back up output")
      }
      r <- raster::raster(s)
      r[valid] <- result[, c("lower", "upper", "width")[j]]
      raster::writeRaster(r, files[j], overwrite = TRUE)
      write_prediction_scale(files[j], "capsules/km2", "independent_cross_products")
    }
    saveRDS(list(method = "independent_cross_products", model_md5 = hashes,
                 output_md5 = tools::md5sum(files), date = day,
                 elapsed_seconds = proc.time()[3] - started), audit_file)
    message("Completed in ", round(proc.time()[3] - started, 1), " seconds")
  }
  if (!identical(hashes, tools::md5sum(c(den$files, pa$files)))) stop("Source models changed")
  invisible(NULL)
}

