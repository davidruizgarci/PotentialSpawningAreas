# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management

# ------------------------------------------------------------------------------
# 3.7. Validate final model inputs and restore environmental-extraction dates
# ------------------------------------------------------------------------------
source("fun/validate_model_inputs.R")

# 1. Set the local data repository ---------------------------------------------
# Run from the repository root and provide the data root as the only argument.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 1L) temp_data <- file.path(args[1], "temp")
if (!exists("temp_data")) stop("Set temp_data or provide the local data root")
outdir <- file.path(temp_data, "validated_model_inputs")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# 2. Validate inputs without changing the source tables ------------------------
for (genus in c("Scyliorhinus", "Raja")) {
  # Use the final ALL table; do not rebuild corrected measurements from earlier versions.
  file <- file.path(temp_data, "folds_dataset", paste0(genus, "_ALL_folds_dataset.csv"))
  extraction_file <- file.path(temp_data, "data_subsets", paste0(genus, "_dataset_log_pred.csv"))
  # Check keys, dates, columns, response consistency and positive depth.
  data <- read_model_data(file, extraction_file)
  # Write a derived checked copy; leave the source table untouched.
  write.csv2(data, file.path(outdir, paste0(genus, "_validated.csv")), row.names = FALSE)
  write.csv(attr(data, "date_changes"), file.path(outdir, paste0(genus, "_date_changes.csv")),
    row.names = FALSE
  )
  message(genus, ": ", nrow(data), " hauls; ", nrow(attr(data, "date_changes")), " dates restored")
}
