# ------------------------------------------------------------------------------
# Download complete monthly reports from one explicitly pinned GFW dataset
# ------------------------------------------------------------------------------

# 1. Download or verify one month without mixing historical source versions ------
download_effort_version <- function(file, start_date, end_date, key,
                                    dataset = "public-global-fishing-effort:v4.0") {
  if (!requireNamespace("httr2", quietly = TRUE)) stop("Install httr2")
  if (!requireNamespace("readr", quietly = TRUE)) stop("Install readr")
  audit_file <- paste0(file, ".source.rds")

  # Resume only verified reports from the same version and period.
  if (file.exists(file) || file.exists(audit_file)) {
    if (!file.exists(file) || !file.exists(audit_file)) stop("Incomplete download: ", file)
    audit <- readRDS(audit_file)
    if (!identical(audit$dataset, dataset) ||
        !identical(audit$start, start_date) || !identical(audit$end, end_date) ||
        !identical(audit$md5, unname(tools::md5sum(file)))) stop("Report audit mismatch: ", file)
    message("Verified existing report: ", basename(file))
    return(invisible(audit))
  }

  # The end boundary is exclusive: use the first day of the following month.
  message("Downloading ", basename(file), " using ", dataset)
  req <- httr2::request("https://gateway.api.globalfishingwatch.org/v3/4wings/report")
  req <- httr2::req_url_query(req, `datasets[0]` = dataset,
     `spatial-resolution` = "HIGH", `temporal-resolution` = "DAILY",
     `group-by` = "GEARTYPE", `date-range` = paste(start_date, end_date, sep = ","),
     format = "CSV")
  req <- httr2::req_headers(req, Authorization = paste("Bearer", key),
                            `Content-Type` = "application/json")
  req <- httr2::req_body_raw(req, '{"region":{"dataset":"public-rfmo","id":"GFCM"}}')
  req <- httr2::req_timeout(req, 180)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  response <- httr2::req_perform(req)
  if (httr2::resp_status(response) != 200L) {
    stop("GFW returned HTTP ", httr2::resp_status(response),
         "; no report saved. Retry later; do not replace historical data.")
  }
  resolved <- httr2::resp_header(response, "x-datasets")
  if (is.null(resolved) || !grepl(dataset, resolved, fixed = TRUE)) {
    stop("API did not confirm the requested dataset version")
  }

  # Validate the archive and daily coverage before publishing the local CSV.
  archive <- tempfile(fileext = ".zip")
  on.exit(unlink(archive), add = TRUE)
  writeBin(httr2::resp_body_raw(response), archive)
  entries <- utils::unzip(archive, list = TRUE)$Name
  csv <- entries[grepl("\\.csv$", entries)]
  if (length(csv) != 1L) stop("Expected one CSV in GFW report")
  data <- as.data.frame(readr::read_csv(unz(archive, csv), show_col_types = FALSE))
  required <- c("Lat", "Lon", "Time Range", "geartype", "Vessel IDs", "Apparent Fishing Hours")
  if (!setequal(names(data), required)) stop("Unexpected GFW schema")
  data <- data[required]
  dates <- as.Date(data[["Time Range"]])
  expected <- seq(start_date, end_date - 1, by = "day")
  if (anyNA(dates) || any(!dates %in% expected) ||
      !setequal(as.character(unique(dates)), as.character(expected))) {
    stop("Unexpected or missing represented dates; inspect rather than assuming zero effort")
  }
  hours <- data[["Apparent Fishing Hours"]]
  if (any(!is.finite(hours) | hours < 0)) stop("Invalid fishing hours")
  # Preserve all reported rows; downstream aggregation sums hours by cell and day.
  repeated_keys <- sum(duplicated(data[c("Lat", "Lon", "Time Range", "geartype")]))
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(tmpdir = dirname(file), fileext = ".partial")
  write.csv(data, stage, row.names = FALSE)
  if (!file.rename(stage, file)) stop("Cannot finalize downloaded report")
  audit <- list(dataset = dataset, resolved = resolved,
                start = start_date, end = end_date, region = "GFCM",
                spatial_resolution = "HIGH", temporal_resolution = "DAILY",
                group_by = "GEARTYPE", downloaded_utc = format(Sys.time(), tz = "UTC"),
                md5 = unname(tools::md5sum(file)), rows = nrow(data),
                days = length(unique(dates)), repeated_keys = repeated_keys, trawl_hours = sum(hours[data$geartype == "trawlers"]))
  saveRDS(audit, audit_file)
  message("Saved ", audit$days, " days; ", audit$rows, " rows")
  invisible(audit)
}


