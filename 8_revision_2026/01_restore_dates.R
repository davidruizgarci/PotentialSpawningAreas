# Run from the repository root:
# Rscript 8_revision_2026/01_restore_dates.R /path/to/EggCase_Distribution
source('8_revision_2026/revision_helpers.R')
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop('Supply the local data root as the only argument')
out <- '8_revision_2026/local_outputs'
dir.create(out, recursive = TRUE, showWarnings = FALSE)
for (taxon in c('Scyliorhinus', 'Raja')) {
  final <- read.csv2(file.path(args[1], 'temp/folds_dataset', paste0(taxon, '_ALL_folds_dataset.csv')),
    stringsAsFactors = FALSE)
  upstream <- read.csv2(file.path(args[1], 'temp/data_subsets', paste0(taxon, '_dataset_log_pred.csv')),
    stringsAsFactors = FALSE)
  fixed <- restore_extraction_dates(final, upstream)
  stopifnot(identical(final[names(final) != 'date'], fixed$data[names(final) != 'date']))
  write.csv2(fixed$data, file.path(out, paste0(taxon, '_date_corrected.csv')), row.names = FALSE)
  write.csv(fixed$changes, file.path(out, paste0(taxon, '_date_changes.csv')), row.names = FALSE)
  message(taxon, ': ', nrow(fixed$changes), ' dates restored; all other fields preserved')
}
