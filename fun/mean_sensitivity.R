# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management

# ------------------------------------------------------------------------------
# Focused exploratory sensitivity after the original mean-validation experiment
# ------------------------------------------------------------------------------

# Gamma with a log link models the positive arithmetic mean directly. Standardise
# predictors using training statistics only; retain every valid positive catch.

# 1. Fit a positive Gamma mean using the same physical predictors -------------
# Standardisation uses positive training observations only.
# The numerical optimiser fits the Gamma log-link mean; no residual smearing is used.
fit_gamma_mean <- function(data, predictors) {
  positive <- data[data$N > 0, , drop = FALSE]
  # Store training means for applying the same centring to future observations.
  centre <- vapply(positive[predictors], mean, numeric(1))
  # Store training standard deviations; assessment data never determine scaling.
  scale <- vapply(positive[predictors], stats::sd, numeric(1))
  if (any(!is.finite(scale) | scale <= 0)) stop("Constant or invalid Gamma predictor")
  z <- as.data.frame(scale(positive[predictors], center = centre, scale = scale))
  z$N_km2 <- positive$N_km2
  # Build an intercept and the standardised physical predictor columns.
  X <- stats::model.matrix(stats::reformulate(predictors), z)
  y <- z$N_km2
  # Gamma log-link IRLS can oscillate for highly skewed training subsets. Minimise
  # its convex coefficient-dependent negative log likelihood with a line search.
  # Dispersion does not affect these fitted mean coefficients.
  objective <- function(beta) {
    eta <- as.numeric(X %*% beta)
    value <- sum(exp(log(y) - eta) + eta)
    if (is.finite(value)) value else Inf
  }
  gradient <- function(beta) as.numeric(crossprod(X, 1 - exp(log(y) - X %*% beta)))
  # Start the optimiser at a constant positive mean with zero predictor effects.
  start <- c(log(mean(y)), rep(0, ncol(X) - 1L))
  fit <- stats::optim(start, objective, gradient,
    method = "BFGS",
    control = list(maxit = 2000, reltol = 1e-11)
  )
  if (fit$convergence != 0L || max(abs(gradient(fit$par))) / nrow(X) > 1e-4) {
    stop("Gamma fit did not converge to a stationary solution")
  }
  list(
    coefficients = fit$par, centre = centre, scale = scale, predictors = predictors,
    convergence = fit$convergence, score = max(abs(gradient(fit$par))) / nrow(X)
  )
}


# 2. Apply the training standardisation and fitted Gamma mean coefficients ----
# The exponential is a mean-link inverse, not an inverse of logged observations.
predict_gamma_mean <- function(model, data) {
  z <- as.data.frame(scale(data[model$predictors], center = model$centre, scale = model$scale))
  p <- as.numeric(exp(stats::model.matrix(stats::reformulate(model$predictors), z) %*% model$coefficients))
  if (any(!is.finite(p) | p <= 0)) stop("Invalid Gamma positive mean")
  p
}

# Use the original occurrence configuration, selected only from outer training.
# The focused BRT grid is fixed at additive trees and the lower learning rate.

# 3. Run the limited exploratory comparison on one original outer assessment ---
# Reuse the original training-only occurrence configuration and identical haul groups.
# These reused test observations are sensitivity evidence, not independent validation.
run_mean_sensitivity <- function(job) {
  data <- job$data
  hold <- data$validation_fold == job$fold
  train <- data[!hold, , drop = FALSE]
  test <- data[hold, , drop = FALSE]
  seed <- job$seed + 100 * job$fold
  original <- readRDS(job$config_file)[["raw_gaussian"]]
  stopifnot(
    setequal(original$training_codes, train$code),
    setequal(original$assessment_codes, test$code)
  )
  occurrence <- original$occurrence
  simple <- tune_positive_means(
    train, job$predictors, occurrence,
    expand.grid(tc = 1L, lr = .005), c(250L, 750L, 1500L), seed + 10000, job$scheme
  )
  inner_seed <- seed + 10000
  inner <- validation_folds(train, 3L, inner_seed, job$scheme)
  gamma_sse <- 0
  for (k in 1:3) {
    a <- train[inner != k, , drop = FALSE]
    b <- train[inner == k, , drop = FALSE]
    occ <- fit_mean_brt(
      a, job$predictors, "presence_absence", "bernoulli",
      occurrence$config, inner_seed + 10000 + k, occurrence$trees
    )
    p <- as.numeric(stats::predict(occ, b, n.trees = occurrence$trees, type = "response"))
    gamma <- fit_gamma_mean(a, job$predictors)
    # Add Gamma hurdle squared errors on the original density scale.
    gamma_sse <- gamma_sse + sum((b$N_km2 - p * predict_gamma_mean(gamma, b))^2)
  }
  # Compare all candidate hurdle means using the same inner assessment observations.
  inner_scores <- c(
    simple_raw = simple$raw_gaussian$mse,
    simple_log = simple$log_smearing$mse, gamma_log = gamma_sse / nrow(train)
  )
  selected <- names(which.min(inner_scores))
  predictions <- list()
  models <- list()
  for (family in c("raw_gaussian", "log_smearing")) {
    model <- fit_hurdle_choice(train, job$predictors, occurrence, simple[[family]], seed + 20000, job$scheme)
    name <- if (family == "raw_gaussian") "simple_raw" else "simple_log"
    predictions[[name]] <- predict_hurdle_choice(model, test)$prediction
    models[[name]] <- model
  }
  gamma <- fit_gamma_mean(train, job$predictors)
  # Occurrence fit and seed match both BRT candidates exactly.
  occ <- models$simple_raw$occurrence
  p <- as.numeric(stats::predict(occ, test, n.trees = occurrence$trees, type = "response"))
  predictions$gamma_log <- p * predict_gamma_mean(gamma, test)
  result <- do.call(rbind, lapply(names(predictions), function(family) {
    data.frame(
      taxon = job$taxon, scheme = job$scheme, repeat_seed = job$seed, fold = job$fold,
      code = test$code, lon = test$lon, lat = test$lat, observed = test$N_km2, p = p,
      predicted = predictions[[family]], family = family, chosen_family = selected,
      inner_MSE = inner_scores[[family]]
    )
  }))
  prefix <- file.path(job$out, paste(job$taxon, job$scheme, job$seed, job$fold, sep = "_"))
  write.csv(result, paste0(prefix, ".csv"), row.names = FALSE)
  saveRDS(list(
    inner_scores = inner_scores, selected = selected,
    training_codes = train$code, assessment_codes = test$code
  ), paste0(prefix, "_config.rds"))
  result
}
