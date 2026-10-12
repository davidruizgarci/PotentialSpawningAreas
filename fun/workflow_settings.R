# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management

# ------------------------------------------------------------------------------
# Shared execution settings for RStudio and command-line runs
# ------------------------------------------------------------------------------
# Personal paths and credentials belong in the author's local 0_setup.R.
# This public helper contains no personal settings and never reads credentials.

# 1. Read the settings required by the selected analysis script -----------------
workflow_arguments <- function(setting_names) {
  # Command-line arguments take precedence when a script is run with Rscript.
  arguments <- commandArgs(trailingOnly = TRUE)

  if (length(arguments) > 0L) {
    # Reject partial argument lists rather than guessing which path is missing.
    if (length(arguments) != length(setting_names)) {
      stop("Provide these arguments in order: ", paste(setting_names, collapse = ", "))
    }

    return(arguments)
  }

  # RStudio's Source button supplies no command-line arguments. Read the named
  # options set by the local setup instead, keeping the calculation identical.
  settings <- getOption("PotentialSpawningAreas.workflow")

  if (is.null(settings)) {
    stop("Run your local 0_setup.R first, or provide the documented Rscript arguments.")
  }

  values <- vapply(setting_names, function(name) {
    value <- settings[[name]]

    # Report the precise setting to edit; do not start a run with an empty path.
    if (is.null(value) || length(value) != 1L || is.na(value) || !nzchar(as.character(value))) {
      stop("Set '", name, "' in your local 0_setup.R before running this script.")
    }

    as.character(value)
  }, character(1))

  message("Using local setup settings for: ", paste(setting_names, collapse = ", "))
  unname(values)
}

# 2. Connect a completed stage to the next stage in the same RStudio session ----
set_workflow_output <- function(name, value) {
  settings <- getOption("PotentialSpawningAreas.workflow")

  # Command-line runs remain independent and receive their paths explicitly.
  if (!is.null(settings)) {
    settings[[name]] <- value
    options(PotentialSpawningAreas.workflow = settings)
  }

  invisible(value)
}
