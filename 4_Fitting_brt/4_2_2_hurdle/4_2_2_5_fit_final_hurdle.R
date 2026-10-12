# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management

# ------------------------------------------------------------------------------
# 4.2.2.5. Fit the locked exploratory formulations and paired-location bootstrap
# ------------------------------------------------------------------------------
library(gbm)
source("fun/workflow_settings.R")
source("fun/validate_model_inputs.R")
source("fun/validate_hurdle_mean.R")

# 1. Lock the scientific decision; keep its exploratory provenance explicit -----
# Rscript script.R data_root original_validation sensitivity_output new_output B
# Use at least 200 replicates for production; 500 improves tail resolution.
# Use command-line paths, or the named settings from the local 0_setup.R.
args <- workflow_arguments(c(
  "data_root", "validation_folder", "sensitivity_folder",
  "model_output", "bootstrap_replicates"
))
# Read final data from Dropbox while keeping the working directory in the code repository.
root <- args[1]
previous <- args[2]
sensitivity <- args[3]
out <- args[4]
# B is the number of paired bootstrap replicates, not the number of fitted trees.
B <- suppressWarnings(as.integer(args[5]))
if (is.na(B) || B < 100L) stop("Use at least 100 bootstrap replicates")
if (!file.exists(file.path(sensitivity, "sensitivity_complete.rds"))) stop("Sensitivity incomplete")
if (dir.exists(out)) stop("Use a new output folder")
dir.create(out, recursive = TRUE)
# Reuse the original predictor recipe and partition seeds; this stage does not repeat
# variable screening.
manifest <- readRDS(file.path(previous, "validation_manifest.rds"))
# Compare file fingerprints with the completed sensitivity run before refitting.
hashes <- readRDS(file.path(sensitivity, "source_hashes.rds"))
# Use the documented exploratory family decision: corrected log for catshark, direct BPUE
# for skates.
locked <- c(Scyliorhinus = "log_smearing", Raja = "raw_gaussian")
decision <- list(
  families = locked, positive_tc = 1L, positive_lr = .005,
  positive_trees = c(250L, 750L, 1500L), B = B,
  provenance = "Exploratory fixed-family decision after original and focused spatial assessments",
  limits = "Weak Scyliorhinus spatial abundance skill; no independent validation of post-hoc choice",
  bootstrap = "Paired sampling locations; conditional on locked families, predictors and tuned parameters"
)
saveRDS(decision, file.path(out, "decision.rds"))
inputs <- list()
models <- list()
# Fit both genera using their complete final model tables.
for (taxon in names(locked)) {
  file <- file.path(root, "temp/folds_dataset", paste0(taxon, "_ALL_folds_dataset.csv"))
  extraction <- file.path(root, "temp/data_subsets", paste0(taxon, "_dataset_log_pred.csv"))
  # Stop if the source data differ from the data used in the completed analysis.
  stopifnot(identical(hashes[[taxon]], tools::md5sum(c(file, extraction))))
  # Read and check the authoritative final table; preserve its counts and effort values.
  d <- read_model_data(file, extraction)
  # Sort haul keys so fixed seeds give reproducible fits and paired resamples.
  d <- d[order(d$code), ]
  rownames(d) <- NULL
  # Prepare the transformed response for the corrected-log candidate.
  d$log_bpue <- log1p(d$N_km2)
  inputs[[taxon]] <- d
  # Read the full-data OCCURRENCE tuning scores from the original comparison.
  a <- read.csv(file.path(previous, paste0(taxon, "_deployment_occurrence_scores.csv")))
  # Take the occurrence setting with the smallest Brier score.
  a <- a[order(a$Brier, a$trees, a$tc, a$lr), ]
  occurrence <- list(config = a[1, c("tc", "lr")], trees = a$trees[1], score = a$Brier[1])
  # Compare positive tree counts inside each full-data geographic partition.
  # Complexity is fixed at 1 and learning rate at 0.005 for this production decision.
  scores <- lapply(manifest$spatial_seeds, function(seed) {
    tune_positive_means(
      d, manifest$predictors, occurrence, expand.grid(tc = 1L, lr = .005),
      c(250L, 750L, 1500L), seed + 10000, "geographic"
    )$scores
  })
  scores <- do.call(rbind, scores)
  # Keep only the locked family; do not reopen family selection after viewing its tests.
  scores <- scores[scores$family == locked[[taxon]], ]
  # Average inner MSE across the three geographic partitions for each tree count.
  b <- aggregate(MSE ~ tc + lr + trees, scores, mean)
  # Choose minimum MSE; an exact tree-count tie favours fewer trees.
  # This is not a one-standard-error rule or the historical deviance/AUC selection.
  b <- b[order(b$MSE, b$trees), ]
  positive <- list(family = locked[[taxon]], config = b[1, c("tc", "lr")], trees = b$trees[1], mse = b$MSE[1])
  # Fit both components on all final hauls and recalibrate the log mean when needed.
  model <- fit_hurdle_choice(d, manifest$predictors, occurrence, positive, 13579L, "geographic")
  model$status <- "Locked exploratory formulation; interpret with spatial-validation limits"
  model$decision <- decision
  model$source_hashes <- hashes[[taxon]]
  models[[taxon]] <- model
  # Save the full-data model separately from its bootstrap ensemble.
  saveRDS(model, file.path(out, paste0(taxon, "_hurdle_model.rds")))
  write.csv(b, file.path(out, paste0(taxon, "_within_family_scores.csv")), row.names = FALSE)
  message(taxon, ": ", positive$family, "; trees=", positive$trees, "; correction=", model$smearing)
}
stopifnot(identical(inputs$Raja[c("code", "lon", "lat")], inputs$Scyliorhinus[c("code", "lon", "lat")]))

# 2. Resample the same locations for both components and genera -----------------
# Duplicate copies of a source location stay together in correction cross-fitting.
# Do not tune again or independently resample occurrence and positive abundance.
bootstrap_one <- function(job) {
  d <- job$data
  # Group repeated coordinates before resampling; retain all hauls from a selected location.
  site <- paste(sprintf("%.6f", d$lon), sprintf("%.6f", d$lat), sep = "_")
  # Store original row indices for every sampling location.
  groups <- split(seq_len(nrow(d)), site)
  set.seed(90000L + job$replicate)
  # Draw locations with replacement; the same replicate seed is used for both genera.
  sampled <- sample(seq_along(groups), length(groups), replace = TRUE)
  # Expand selected location groups into the paired set of haul rows.
  index <- unlist(groups[sampled], use.names = FALSE)
  # Both hurdle components see exactly these resampled observations.
  boot <- d[index, , drop = FALSE]
  # Refit both components on this resample and re-estimate its log correction.
  model <- fit_hurdle_choice(
    boot, job$model$predictors, job$model$occurrence_config,
    job$model$positive_config, 100000L + job$replicate, "geographic"
  )
  # Keep a stable identity for combining cells, days and genera later.
  model$replicate_id <- job$replicate
  list(model = model, sampled_codes = boot$code)
}
jobs <- list()
# Build one job per genus and replicate, using matching replicate seeds.
for (taxon in names(locked)) {
  for (b in seq_len(B)) {
    jobs[[length(jobs) + 1L]] <-
      list(data = inputs[[taxon]], model = models[[taxon]], replicate = b, taxon = taxon)
  }
}
# Use at most four workers to fit independent bootstrap jobs.
cluster <- parallel::makePSOCKcluster(min(4L, max(1L, parallel::detectCores() - 1L)),
  outfile = file.path(out, "worker_log.txt")
)
parallel::clusterCall(cluster, function(paths, repository) {
  setwd(repository)
  .libPaths(paths)
  library(gbm)
  source("fun/validate_hurdle_mean.R")
  NULL
}, .libPaths(), normalizePath("."))
parallel::clusterExport(cluster, "bootstrap_one", envir = environment())
# Stop and report any failed replicate; do not discard or replace it silently.
result <- tryCatch(parallel::parLapplyLB(cluster, jobs, bootstrap_one),
  finally = parallel::stopCluster(cluster)
)
# Save one complete, ordered bootstrap ensemble per genus.
for (taxon in names(locked)) {
  selected <- which(vapply(jobs, function(j) j$taxon == taxon, logical(1)))
  ensemble <- list(
    main = models[[taxon]], replicates = lapply(result[selected], `[[`, "model"),
    resampled_codes = lapply(result[selected], `[[`, "sampled_codes"), decision = decision
  )
  stopifnot(identical(vapply(ensemble$replicates, `[[`, integer(1), "replicate_id"), seq_len(B)))
  # Save matching models, source haul resamples and the decision metadata together.
  saveRDS(ensemble, file.path(out, paste0(taxon, "_joint_bootstrap.rds")))
}
for (taxon in names(hashes)) stopifnot(identical(hashes[[taxon]], tools::md5sum(names(hashes[[taxon]]))))
saveRDS(list(completed = Sys.time(), decision = decision), file.path(out, "final_fit_complete.rds"))
# Point the next RStudio stage to this completed model folder.
set_workflow_output("final_model_folder", normalizePath(out, winslash = "/"))
