# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management

# ------------------------------------------------------------------------------
# 4.2.2.4. Complete the focused exploratory mean sensitivity and hotspot audit
# ------------------------------------------------------------------------------
library(gbm)
source("fun/workflow_settings.R")
source("fun/validate_model_inputs.R")
source("fun/validate_hurdle_mean.R")
source("fun/mean_sensitivity.R")

# 1. Fix the limited follow-up before running it; do not search until a model wins
# Rscript script.R local_data_root completed_validation_folder new_output_folder
args <- workflow_arguments(c("data_root", "validation_folder", "sensitivity_output"))
root <- args[1]
previous <- args[2]
out <- args[3]
if (!file.exists(file.path(previous, "validation_complete.rds"))) stop("Original validation incomplete")
if (dir.exists(out)) stop("Use a new output folder")
dir.create(out, recursive = TRUE)
manifest <- readRDS(file.path(previous, "validation_manifest.rds"))
# Require the same source data as the original comparison before reusing its configurations.
original_hashes <- readRDS(file.path(previous, "source_hashes.rds"))
predictors <- manifest$predictors
saveRDS(
  list(
    status = "Exploratory follow-up using the same assessment data",
    previous = normalizePath(previous), predictors = predictors,
    BRT_grid = list(tc = 1L, lr = .005, trees = c(250L, 750L, 1500L)),
    alternative = "Positive Gamma GLM, log link, linear standardised predictors",
    selection = "Inner complete-hurdle MSE; original training-only occurrence configuration"
  ),
  file.path(out, "sensitivity_manifest.rds")
)
jobs <- list()
inputs <- list()
hashes <- list()
for (taxon in c("Scyliorhinus", "Raja")) {
  file <- file.path(root, "temp/folds_dataset", paste0(taxon, "_ALL_folds_dataset.csv"))
  extraction <- file.path(root, "temp/data_subsets", paste0(taxon, "_dataset_log_pred.csv"))
  hashes[[taxon]] <- tools::md5sum(c(file, extraction))
  stopifnot(identical(hashes[[taxon]], original_hashes[[taxon]]))
  data <- read_model_data(file, extraction)
  data <- data[order(data$code), ]
  rownames(data) <- NULL
  data$log_bpue <- log1p(data$N_km2)
  inputs[[taxon]] <- data
  for (scheme in c("geographic", "random")) {
    seeds <- if (scheme == "geographic") manifest$spatial_seeds else manifest$random_seeds
    for (seed in seeds) {
      data$validation_fold <- validation_folds(data, 5L, seed, scheme)
      for (k in 1:5) {
        jobs[[length(jobs) + 1L]] <- list(
          data = data, taxon = taxon, scheme = scheme,
          seed = seed, fold = k, predictors = predictors, out = out,
          config_file = file.path(previous, paste0(paste(taxon, scheme, seed, k, sep = "_"), "_config.rds"))
        )
      }
    }
  }
}
saveRDS(hashes, file.path(out, "source_hashes.rds"))

# 2. Reuse identical assessment locations; this is sensitivity, not new validation
cluster <- parallel::makePSOCKcluster(min(4L, max(1L, parallel::detectCores() - 1L)),
  outfile = file.path(out, "worker_log.txt")
)
parallel::clusterCall(cluster, function(paths, repository) {
  setwd(repository)
  .libPaths(paths)
  library(gbm)
  source("fun/validate_hurdle_mean.R")
  source("fun/mean_sensitivity.R")
  NULL
}, .libPaths(), normalizePath("."))
result <- tryCatch(parallel::parLapplyLB(cluster, jobs, run_mean_sensitivity),
  finally = parallel::stopCluster(cluster)
)
predictions <- do.call(rbind, result)
selected <- predictions[predictions$family == predictions$chosen_family, ]
selected$family <- "sensitivity_selected"
predictions <- rbind(predictions, selected)
write.csv(predictions, file.path(out, "outer_predictions.csv"), row.names = FALSE)
summarise <- function(columns, filename) {
  rows <- lapply(split(predictions, interaction(predictions[columns], drop = TRUE)), function(x) {
    cbind(x[1, columns, drop = FALSE], bpue_metrics(x$observed, x$predicted))
  })
  result <- do.call(rbind, rows)
  write.csv(result, file.path(out, filename), row.names = FALSE)
  result
}
# Report original-unit errors for each fixed family and the internal selector.
metrics <- summarise(c("taxon", "scheme", "family"), "pooled_metrics.csv")
summarise(c("taxon", "scheme", "repeat_seed", "family"), "repeat_metrics.csv")
summarise(c("taxon", "scheme", "repeat_seed", "fold", "family"), "regional_metrics.csv")

# 3. Diagnose concentration without removing high captures or changing folds ----
# Use the first experiment to describe influential errors without deleting any hauls.
original <- read.csv(file.path(previous, "outer_predictions.csv"))
hotspots <- list()
influence <- list()
latitude <- list()
for (taxon in names(inputs)) {
  d <- inputs[[taxon]]
  # Identify the five highest observed haul densities for descriptive concentration statistics.
  top <- order(d$N_km2, decreasing = TRUE)[1:5]
  # Use this explicit latitude band as a diagnostic, not an administrative Castellon boundary.
  band <- d$lat >= 39.5 & d$lat < 40.5
  hotspots[[taxon]] <- data.frame(
    taxon = taxon, n = nrow(d), positive = sum(d$N > 0),
    top5_density_sum_share = sum(d$N_km2[top]) / sum(d$N_km2),
    top5_captured_count_share = sum(d$N[top]) / sum(d$N),
    top5_min_lat = min(d$lat[top]), top5_max_lat = max(d$lat[top]),
    diagnostic_band_min_lat = 39.5, diagnostic_band_max_lat = 40.5,
    band_n = sum(band), band_positive = sum(d$N[band] > 0),
    band_density_sum_share = sum(d$N_km2[band]) / sum(d$N_km2)
  )
  x <- original[original$taxon == taxon & original$scheme == "geographic" &
    original$family %in% c("raw_gaussian", "log_smearing"), ]
  # Assess which hauls dominate original geographic squared prediction error.
  x$squared_error <- (x$predicted - x$observed)^2
  a <- aggregate(squared_error ~ taxon + family + code, x, sum)
  influence[[taxon]] <- a[order(a$family, -a$squared_error), ]
  x <- predictions[predictions$taxon == taxon & predictions$scheme == "geographic", ]
  # Summarise spatial bias in half-degree latitude bands after all predictions are fixed.
  x$latitude_band <- floor(x$lat * 2) / 2
  groups <- split(x, interaction(x$family, x$latitude_band, drop = TRUE))
  latitude[[taxon]] <- do.call(rbind, lapply(groups, function(g) {
    cbind(g[1, c("taxon", "family", "latitude_band")], bpue_metrics(g$observed, g$predicted))
  }))
}
write.csv(do.call(rbind, hotspots), file.path(out, "hotspot_summary.csv"), row.names = FALSE)
write.csv(do.call(rbind, influence), file.path(out, "haul_influence.csv"), row.names = FALSE)
write.csv(do.call(rbind, latitude), file.path(out, "latitude_band_metrics.csv"), row.names = FALSE)
for (taxon in names(hashes)) stopifnot(identical(hashes[[taxon]], tools::md5sum(names(hashes[[taxon]]))))
saveRDS(list(completed = Sys.time(), metrics = metrics), file.path(out, "sensitivity_complete.rds"))
print(metrics)
set_workflow_output("sensitivity_folder", normalizePath(out, winslash = "/"))
