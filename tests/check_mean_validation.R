# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management

# ------------------------------------------------------------------------------
# Check response-independent folds and original-unit mean calculations
# ------------------------------------------------------------------------------
source("fun/validate_hurdle_mean.R")
# 1. Keep repeated locations together without using catches to assign folds -----
set.seed(42)
x <- data.frame(code = sprintf("haul_%03d", 1:90), lon = runif(90), lat = 38 + runif(90), N_km2 = rexp(90))
x <- rbind(x, x[1:5, ])
x$code <- sprintf("haul_%03d", 1:nrow(x))
f <- validation_folds(x, 5, 123)
stopifnot(identical(f[1:5], tail(f, 5)))
# Change only the response: geographic group assignments must remain identical.
y <- x
y$N_km2 <- rev(y$N_km2)
stopifnot(identical(f, validation_folds(y, 5, 123)))
# Reorder the input rows: each original haul must retain its assigned group.
o <- sample(nrow(x))
stopifnot(identical(f[o], validation_folds(x[o, ], 5, 123)))
g <- validation_folds(x, 5, 456)
stopifnot(!identical(outer(f, f, "=="), outer(g, g, "==")))

# 2. Verify arithmetic-mean recovery rather than a naive inverse log -----------
# Mean retransformation: a valid correction recovers the arithmetic mean.
positive <- c(1, 99)
mu <- mean(log1p(positive))
S <- mean(exp(log1p(positive) - mu))
stopifnot(
  abs(.5 * (exp(mu) * S - 1) - 25) < 1e-12,
  abs(.5 * expm1(mu) - 25) > 1
)
metrics <- bpue_metrics(c(0, 2), c(1, 1))
stopifnot(metrics$RMSE == 1, metrics$bias == 0, metrics$MAE == 1)
message("Site grouping, response-independent repeated folds and arithmetic means passed")

# 3. Verify that held-out catches cannot influence fitting or calibration ------
# Changing reserved responses must not affect fitted choices or predictions.
if (requireNamespace("gbm", quietly = TRUE)) {
  set.seed(72)
  d <- data.frame(code = sprintf("synthetic_%03d", 1:180), lon = runif(180), lat = 38 + runif(180))
  d$N <- rep(c(0, 1, 1), 60)
  d$N_km2 <- d$N * exp(rnorm(180, 2, 1))
  d$presence_absence <- as.integer(d$N > 0)
  d$log_bpue <- log1p(d$N_km2)
  d$validation_fold <- validation_folds(d, 5, 123)
  output <- tempfile("mean_validation_")
  dir.create(output)
  job <- list(
    data = d, taxon = "synthetic", scheme = "geographic", seed = 123L, fold = 1L,
    predictors = c("lon", "lat"), grid = expand.grid(tc = 1L, lr = .02), trees = c(20L, 40L), out = output
  )
  first <- run_outer_assessment(job)$predictions
  held <- job$data$validation_fold == 1
  job$data$N_km2[held] <- 10 * job$data$N_km2[held]
  job$data$log_bpue[held] <- log1p(job$data$N_km2[held])
  second <- run_outer_assessment(job)$predictions
  stopifnot(
    identical(first$predicted, second$predicted),
    identical(first$chosen_family, second$chosen_family),
    identical(first$smearing, second$smearing)
  )
  unlink(output, recursive = TRUE)
  message("Nested fitting excludes reserved response values: passed")
} else {
  message("Install gbm to run the nested-fitting leakage check")
}
