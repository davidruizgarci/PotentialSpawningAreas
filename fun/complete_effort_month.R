# ------------------------------------------------------------------------------
# Complete monthly effort files without replacing existing corrected records
# ------------------------------------------------------------------------------

# 1. Include the last day by using the exclusive start of the next month ---------
next_month_start <- function(start_date) {
  seq(as.Date(start_date), by = "month", length.out = 2L)[2L]
}

# 2. Download only absent dates, keeping the original monthly filename -----------
complete_effort_month <- function(file, start_date, end_date, download,
                                  source_compatibility_verified = FALSE) {
  expected <- seq(as.Date(start_date), as.Date(end_date) - 1, by = "day")
  existing <- NULL
  present <- as.Date(character())

  if (file.exists(file)) {
    existing <- read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)
    if (!"Time Range" %in% names(existing)) stop("Unexpected historical GFW schema: ", file)
    present <- as.Date(existing[["Time Range"]])
    if (anyNA(present) || any(!present %in% expected)) stop("Invalid dates in ", file)
  }

  # An absent date is a download candidate, not proof of zero fishing effort.
  missing <- expected[!expected %in% present]
  if (!length(missing)) {
    message("Already complete: ", basename(file))
    return(invisible(existing))
  }

  # Historical CSVs do not declare a source version. Do not silently mix releases.
  if (!is.null(existing) && !source_compatibility_verified) {
    stop("Missing dates in ", basename(file), ": ", paste(missing, collapse = ", "),
         ". Verify historical/API dataset compatibility before completing this file.")
  }

  additions <- list()
  for (i in seq_along(missing)) {
    day <- missing[i]

    # A one-day query ends at the next day's exclusive boundary.
    item <- as.data.frame(download(day, day + 1))
    if (!nrow(item) || !"Time Range" %in% names(item)) {
      stop("Empty or incompatible GFW response for ", day, "; original file unchanged")
    }
    if (anyNA(as.Date(item[["Time Range"]])) || any(as.Date(item[["Time Range"]]) != day)) {
      stop("Unexpected response dates for ", day)
    }
    if (!is.null(existing) && !setequal(names(item), names(existing))) {
      stop("GFW response schema differs from historical data; original file unchanged")
    }
    if (!is.null(existing)) item <- item[names(existing)]
    additions[[i]] <- item
    Sys.sleep(5)
  }

  # Append only new dates. Never redownload or replace existing corrected values.
  completed <- do.call(rbind, c(list(existing), additions))
  if (!all(expected %in% as.Date(completed[["Time Range"]]))) stop("Incomplete month")
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

  # Keep an exact backup before replacing the CSV at its original location.
  if (file.exists(file)) {
    backup <- paste0(file, ".before_completion_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".bak")
    if (!file.copy(file, backup, overwrite = FALSE)) stop("Could not back up ", file)
  }
  write.csv(completed, file, row.names = FALSE)
  invisible(completed)
}
