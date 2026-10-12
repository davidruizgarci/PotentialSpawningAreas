# ------------------------------------------------------------------------------
# Recover positive arithmetic BPUE means from the author's existing log1p models
# ------------------------------------------------------------------------------

# 1. Estimate one smearing correction from the model's own training residuals ----
training_smearing <- function(model) {

  # Load the prediction method even when this helper is sourced on its own.
  if (!requireNamespace("gbm", quietly = TRUE)) stop("Install gbm to read existing models")

  # Use the data saved inside this exact model, including bootstrap resamples.
  # Do not substitute another corrected table or fit a replacement model.
  training <- model$gbm.call$data
  response <- model$gbm.call$gbm.y
  if (model$distribution$name != "gaussian" || !is.data.frame(training) ||
      length(response) != 1L || !response %in% names(training)) {
    stop("The saved Gaussian model must contain its original training response")
  }

  # Confirm that the response really is log1p(capsules/km2), not a different log.
  observed <- training[[response]]
  if (!"N_km2" %in% names(training) || any(!is.finite(training$N_km2)) ||
      any(training$N_km2 <= 0) ||
      !isTRUE(all.equal(as.numeric(observed), log1p(training$N_km2), tolerance = 1e-7))) {
    stop("Saved response is not positive-BPUE log1p; inspect the model before retransformation")
  }

  # Predict the same training rows with the exact saved tree count and predictors.
  fitted <- as.numeric(stats::predict(model, training,
                        n.trees = model$gbm.call$best.trees, type = "response"))
  if (length(fitted) != length(observed) || any(!is.finite(fitted))) {
    stop("Invalid fitted training predictions")
  }

  # The author selected in-sample residuals. This can underestimate correction
  # because these observations were also used to fit the model.
  correction <- mean(exp(observed - fitted))
  if (!is.finite(correction) || correction <= 0) stop("Invalid smearing factor")
  correction
}

# 2. Return to original units BEFORE multiplying by occurrence probability ------
positive_bpue_mean <- function(log_prediction, correction) {

  # For log1p(y), the corrected mean is exp(mu) * S - 1, not expm1(mu * S).
  answer <- exp(log_prediction) * correction - 1
  if (any(!is.na(answer) & (!is.finite(answer) | answer < 0))) {
    stop("Invalid corrected BPUE; review the model rather than silently clipping values")
  }
  answer
}

# 3. Keep a scale audit so old log maps cannot be mistaken for corrected BPUE ----
write_prediction_scale <- function(file, units, method) {
  saveRDS(list(units = units, method = method,
               md5 = unname(tools::md5sum(file))), paste0(file, ".scale.rds"))
}

check_prediction_scale <- function(file, units) {
  audit_file <- paste0(file, ".scale.rds")
  if (!file.exists(audit_file)) stop("Regenerate this map with the corrected workflow: ", file)
  audit <- readRDS(audit_file)
  if (!identical(audit$units, units) ||
      !identical(audit$md5, unname(tools::md5sum(file)))) {
    stop("Map scale or fingerprint differs from its audit: ", file)
  }
  invisible(TRUE)
}
