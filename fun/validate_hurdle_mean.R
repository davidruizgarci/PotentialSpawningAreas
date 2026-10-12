# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management

# ------------------------------------------------------------------------------
# Nested validation of hurdle means on the original BPUE scale
# ------------------------------------------------------------------------------


# 1. Assign complete sampling locations to reproducible assessment groups -----
# Inputs are haul coordinates, the number of folds, a seed and the partition type.
# Observed and predicted abundance do not influence the assignments.
validation_folds <- function(data, k, seed, scheme = "geographic") {
  # Identify repeated coordinates so hauls from one location stay in the same fold.
  location <- paste(sprintf("%.6f", data$lon), sprintf("%.6f", data$lat), sep = "_")
  # Keep one coordinate pair per sampling location before allocating groups.
  sites <- data[!duplicated(location), c("lon", "lat")]
  sites$id <- unique(location)
  sites <- sites[order(sites$id), ]
  set.seed(seed)
  if (scheme == "geographic") {
    # Randomly rotate a geographic axis; hold out whole contiguous bands.
    angle <- runif(1, 0, pi)
    projection <- sites$lon * cos(mean(sites$lat) * pi / 180) * cos(angle) + sites$lat * sin(angle)
    order <- order(projection, sites$id)
  } else if (scheme == "random") order <- sample(nrow(sites)) else stop("Unknown fold scheme")
  fold <- integer(nrow(sites))
  fold[order] <- pmin(k, ceiling(seq_len(nrow(sites)) * k / nrow(sites)))
  # Return group labels in the original haul order, including repeated locations.
  fold[match(location, sites$id)]
}


# 2. Fit one candidate BRT using only the training observations supplied here ---
# The response/distribution identify occurrence, direct density or log density.
# Tree count, complexity and learning rate are supplied by the tuning routine.
fit_mean_brt <- function(data, predictors, response, distribution, config, seed, max_trees) {
  if (nrow(data) < 8L) stop("Too few training observations")
  if (distribution == "bernoulli" && length(unique(data[[response]])) < 2L) {
    stop("Occurrence training set has only one class")
  }
  set.seed(seed)
  gbm::gbm(stats::reformulate(predictors, response),
    data = data, distribution = distribution,
    n.trees = max_trees, interaction.depth = config$tc, shrinkage = config$lr,
    bag.fraction = .7, n.minobsinnode = min(5L, max(2L, floor(nrow(data) * .7 / 3))), train.fraction = 1, cv.folds = 0,
    keep.data = FALSE, verbose = FALSE
  )
}


# 3. Predict the same observations at each candidate tree count ---------------
# Return a rows-by-tree-count matrix, including the one-count edge case.
predict_tree_grid <- function(model, data, trees) {
  matrix(vapply(
    trees, function(n) as.numeric(stats::predict(model, data, n.trees = n, type = "response")),
    numeric(nrow(data))
  ), nrow = nrow(data), ncol = length(trees))
}


# 4. Estimate the correction needed to recover a positive arithmetic mean -----
# Each positive observation is predicted by a model that excluded its location.
# Average exp(log1p(observed density) - predicted log density) over those residuals.
training_smear <- function(data, predictors, config, trees, seed, scheme) {
  # Third-level holdouts keep each calibration residual out of its own model fit.
  fold <- validation_folds(data, 3L, seed, scheme)
  # Reserve one held-out log prediction per positive haul and tree count.
  mu <- matrix(NA_real_, nrow(data), length(trees))
  for (k in 1:3) {
    train <- data[fold != k, , drop = FALSE]
    model <- fit_mean_brt(train, predictors, "log_bpue", "gaussian", config, seed + k, max(trees))
    mu[fold == k, ] <- predict_tree_grid(model, data[fold == k, , drop = FALSE], trees)
  }
  # Convert held-out log residuals to multiplicative correction factors.
  residual <- exp(data$log_bpue - mu)
  if (any(!is.finite(residual))) stop("Non-finite retransformation correction")
  colMeans(residual)
}


# 5. Compare occurrence settings within the current training set --------------
# Brier score is the mean squared error of the predicted occurrence probability.
# Outer assessment catches never enter this function.
tune_occurrence <- function(data, predictors, grid, trees, seed, scheme) {
  fold <- validation_folds(data, 3L, seed, scheme)
  # Accumulate squared errors separately for every configuration and tree count.
  scores <- matrix(0, nrow(grid), length(trees))
  for (g in seq_len(nrow(grid))) {
    for (k in 1:3) {
      model <- fit_mean_brt(
        data[fold != k, , drop = FALSE], predictors, "presence_absence",
        "bernoulli", grid[g, ], seed + 100 * g + k, max(trees)
      )
      p <- predict_tree_grid(model, data[fold == k, , drop = FALSE], trees)
      scores[g, ] <- scores[g, ] + colSums((data$presence_absence[fold == k] - p)^2)
    }
  }
  # Find the minimum score; exact ties follow the supplied grid order.
  index <- which(scores == min(scores), arr.ind = TRUE)[1, ]
  # Expand the parameter indices into an exportable table of candidate scores.
  table <- expand.grid(g = seq_len(nrow(grid)), t = seq_along(trees))
  table <- data.frame(grid[table$g, ], trees = trees[table$t], Brier = as.vector(scores) / nrow(data))
  list(config = grid[index[1], , drop = FALSE], trees = trees[index[2]], score = min(scores) / nrow(data), scores = table)
}


# 6. Compare direct and corrected-log means within the current training set ---
# Score p(occurrence) times the positive mean against ALL inner assessment hauls.
# Use capsules/km2 MSE, rather than fit on the transformed response scale.
tune_positive_means <- function(data, predictors, occurrence, grid, trees, seed, scheme) {
  fold <- validation_folds(data, 3L, seed, scheme)
  families <- c("raw_gaussian", "log_smearing")
  scores <- array(0, c(nrow(grid), length(trees), 2), dimnames = list(NULL, NULL, families))
  # Refit occurrence inside each inner training set. Its chosen configuration
  # belongs to the outer training data, never to the outer assessment set.
  for (k in 1:3) {
    train <- data[fold != k, , drop = FALSE]
    test <- data[fold == k, , drop = FALSE]
    # Fit occurrence using every haul in the current training subset.
    a <- fit_mean_brt(
      train, predictors, "presence_absence", "bernoulli", occurrence$config,
      seed + 10000 + k, occurrence$trees
    )
    p <- as.numeric(stats::predict(a, test, n.trees = occurrence$trees, type = "response"))
    positive <- train[train$N > 0, , drop = FALSE]
    for (g in seq_len(nrow(grid))) {
      raw <- fit_mean_brt(positive, predictors, "N_km2", "gaussian", grid[g, ], seed + 100 * g + k, max(trees))
      log <- fit_mean_brt(positive, predictors, "log_bpue", "gaussian", grid[g, ], seed + 100 * g + k, max(trees))
      # Truncate negative Gaussian density predictions at zero before combining
      # components.
      raw_prediction <- pmax(predict_tree_grid(raw, test, trees), 0)
      mu <- predict_tree_grid(log, test, trees)
      S <- training_smear(positive, predictors, grid[g, ], trees, seed + 1000 * g + k, scheme)
      # Recover the arithmetic positive mean with the training-only smearing correction.
      log_prediction <- pmax(sweep(exp(mu), 2, S, "*") - 1, 0)
      # Score the full hurdle prediction in capsules/km2, including zero-catch hauls.
      scores[g, , 1] <- scores[g, , 1] + colSums((test$N_km2 - p * raw_prediction)^2)
      scores[g, , 2] <- scores[g, , 2] + colSums((test$N_km2 - p * log_prediction)^2)
    }
  }
  # Find the best configuration within each family before comparing families.
  choices <- lapply(seq_along(families), function(i) {
    family_scores <- matrix(scores[, , i], nrow = nrow(grid), ncol = length(trees))
    index <- which(family_scores == min(family_scores), arr.ind = TRUE)[1, ]
    list(
      family = families[i], config = grid[index[1], , drop = FALSE], trees = trees[index[2]],
      mse = min(scores[, , i]) / nrow(data)
    )
  })
  names(choices) <- families
  choices$selected <- families[which.min(vapply(choices, function(x) x$mse, numeric(1)))]
  # Expand the parameter indices into an exportable table of candidate scores.
  table <- expand.grid(g = seq_len(nrow(grid)), t = seq_along(trees), f = seq_along(families))
  choices$scores <- data.frame(grid[table$g, ],
    trees = trees[table$t], family = families[table$f],
    MSE = as.vector(scores) / nrow(data)
  )
  choices
}


# 7. Fit both hurdle components with the supplied configuration ---------------
# For corrected-log models, estimate the correction again from training-only folds.
# The returned object stores both models and their exact prediction settings.
fit_hurdle_choice <- function(data, predictors, occurrence, positive, seed, scheme) {
  # Fit occurrence using every haul in the current training subset.
  a <- fit_mean_brt(
    data, predictors, "presence_absence", "bernoulli", occurrence$config, seed,
    occurrence$trees
  )
  # Fit positive abundance using only hauls with capsules present.
  d <- data[data$N > 0, , drop = FALSE]
  response <- if (positive$family == "raw_gaussian") "N_km2" else "log_bpue"
  b <- fit_mean_brt(d, predictors, response, "gaussian", positive$config, seed + 1, positive$trees)
  # Estimate smearing only for the log family; direct means need no correction.
  S <- if (positive$family == "log_smearing") {
    training_smear(
      d, predictors, positive$config,
      positive$trees, seed + 2, scheme
    )
  } else {
    1
  }
  list(
    occurrence = a, positive = b, family = positive$family, smearing = as.numeric(S),
    predictors = predictors, occurrence_config = occurrence, positive_config = positive
  )
}


# 8. Predict occurrence and positive mean from the SAME fitted hurdle object ---
# Convert corrected log predictions with exp(mu) * correction - 1.
# Multiply by occurrence only after recovering the positive arithmetic mean.
predict_hurdle_choice <- function(model, data) {
  p <- as.numeric(stats::predict(model$occurrence, data, n.trees = model$occurrence_config$trees, type = "response"))
  z <- as.numeric(stats::predict(model$positive, data, n.trees = model$positive_config$trees, type = "response"))
  # Undo the transformed response using the stored correction, not expm1 alone.
  mean_positive <- if (model$family == "log_smearing") exp(z) * model$smearing - 1 else z
  list(
    p = p, mean_positive = pmax(mean_positive, 0), prediction = p * pmax(mean_positive, 0),
    clipped = mean_positive < 0, mu = if (model$family == "log_smearing") z else rep(NA_real_, length(z))
  )
}

# 3. Report errors in original units; repeated partitions are not independent ----

# 9. Calculate descriptive prediction error in original density units ---------
# Repeated assessments of the same haul do not create independent observations.
bpue_metrics <- function(observed, predicted) {
  data.frame(
    n = length(observed), observed_mean = mean(observed), predicted_mean = mean(predicted),
    bias = mean(predicted - observed), RMSE = sqrt(mean((predicted - observed)^2)),
    MAE = mean(abs(predicted - observed))
  )
}


# 10. Reserve one complete outer group before tuning or calibration -----------
# Fit both candidate families on the remaining hauls and predict reserved hauls.
# Save scores/configurations locally for reproducibility without publishing rows.
run_outer_assessment <- function(job) {
  data <- job$data
  assessment <- data$validation_fold == job$fold
  train <- data[!assessment, , drop = FALSE]
  test <- data[assessment, , drop = FALSE]
  stopifnot(!any(train$code %in% test$code))
  predictors <- job$predictors
  seed <- job$seed + 100 * job$fold
  occurrence <- tune_occurrence(train, predictors, job$grid, job$trees, seed, job$scheme)
  positive <- tune_positive_means(train, predictors, occurrence, job$grid, job$trees, seed + 10000, job$scheme)
  predictions <- list()
  configs <- list()
  for (family in c("raw_gaussian", "log_smearing")) {
    model <- fit_hurdle_choice(train, predictors, occurrence, positive[[family]], seed + 20000, job$scheme)
    z <- predict_hurdle_choice(model, test)
    row <- data.frame(
      taxon = job$taxon, scheme = job$scheme, repeat_seed = job$seed, fold = job$fold,
      code = test$code, lon = test$lon, lat = test$lat, observed = test$N_km2, p = z$p,
      mean_positive = z$mean_positive, predicted = z$prediction, family = family,
      chosen_family = positive$selected, smearing = model$smearing, clipped = z$clipped,
      inner_MSE = positive[[family]]$mse, tc = positive[[family]]$config$tc,
      lr = positive[[family]]$config$lr, trees = positive[[family]]$trees
    )
    # Define the high-catch diagnostic threshold from training hauls only.
    cutoff <- as.numeric(quantile(train$N_km2[train$N > 0], .75))
    row$assessment_stratum <- ifelse(test$N == 0, "zero", ifelse(test$N_km2 > cutoff, "high_positive", "other_positive"))
    # Diagnostics use test outcomes only after folds, tuning and calibration are fixed.
    row$high_positive_threshold_training <- cutoff
    # Use the training mean as the abundance benchmark for reserved hauls.
    row$baseline_mean <- mean(train$N_km2)
    predictions[[family]] <- row
    configs[[family]] <- list(
      occurrence = occurrence, positive = positive[[family]], smearing = model$smearing,
      training_codes = train$code, assessment_codes = test$code
    )
  }
  result <- do.call(rbind, predictions)
  file <- file.path(job$out, paste(job$taxon, job$scheme, job$seed, job$fold, sep = "_"))
  write.csv(result, paste0(file, ".csv"), row.names = FALSE)
  saveRDS(configs, paste0(file, "_config.rds"))
  list(predictions = result, configs = configs)
}
