# Optional API helper; calling source() does not start a download.
# Use a fixed dataset version and a NEW output directory for a complete series.
download_gfw_window <- function(start, end_exclusive, dataset, token_file, output_zip) {
  if (!requireNamespace('httr2', quietly = TRUE) || !requireNamespace('jsonlite', quietly = TRUE))
    stop('Install httr2 and jsonlite')
  if (!grepl('^public-global-fishing-effort:v[0-9.]+$', dataset)) stop('Use an explicit dataset version')
  if (as.Date(end_exclusive) <= as.Date(start)) stop('Invalid date window')
  if (file.exists(output_zip)) stop('Output already exists; preserve the previous download')
  secret <- paste(readLines(token_file, warn = FALSE), collapse = '')
  matched <- regmatches(secret, regexpr('eyJ[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+', secret))
  token <- if (length(matched) && nzchar(matched)) matched else trimws(secret)
  coords <- list(list(c(-1.5,37), c(5,37), c(5,43), c(-1.5,43), c(-1.5,37)))
  polygon <- list(type = 'FeatureCollection', features = list(list(type = 'Feature',
    properties = list(), geometry = list(type = 'Polygon', coordinates = coords))))
  params <- list('spatial-resolution' = 'HIGH', 'temporal-resolution' = 'DAILY',
    'group-by' = 'GEARTYPE', format = 'CSV', 'datasets[0]' = dataset,
    'filters[0]' = "geartype IN ('trawlers')", 'date-range' = paste(start,end_exclusive,sep=','),
    'spatial-aggregation' = 'false')
  req <- httr2::request('https://gateway.api.globalfishingwatch.org/v3/4wings/report')
  req <- do.call(httr2::req_url_query, c(list(req), params))
  req <- httr2::req_headers(req, Authorization = paste('Bearer', token))
  req <- httr2::req_body_json(req, list(geojson = polygon), auto_unbox = TRUE)
  req <- httr2::req_timeout(req, 110)
  dir.create(dirname(output_zip), recursive = TRUE, showWarnings = FALSE)
  response <- httr2::req_perform(req)
  writeBin(httr2::resp_body_raw(response), output_zip)
  # Manifest contains public parameters only, never the request/authentication header.
  jsonlite::write_json(list(dataset = dataset, start = start, end_exclusive = end_exclusive,
    bbox = c(-1.5,37,5,43), status = httr2::resp_status(response)),
    paste0(output_zip,'.json'), auto_unbox = TRUE, pretty = TRUE)
  invisible(output_zip)
}
