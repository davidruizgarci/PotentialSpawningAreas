# ------------------------------------------------------------------------------
# Transfer extensive fishing hours using source-cell overlap, never interpolation
# ------------------------------------------------------------------------------

# 1. Build overlap weights once for the fixed source and prediction grids -------
effort_overlap <- function(coords, template, source_resolution = 0.01) {
  if (!raster::isLonLat(template)) stop("Expected the original longitude/latitude grid")
  half <- source_resolution / 2
  e <- raster::extent(template)
  dx <- raster::res(template)[1]
  dy <- raster::res(template)[2]
  output <- vector("list", nrow(coords))

  # Preserve the original rasterFromXYZ convention: coordinates are cell centres.
  # Assume uniform effort within each GFW cell and use spherical area fractions.
  for (i in seq_len(nrow(coords))) {
    x0 <- coords$Lon[i] - half; x1 <- coords$Lon[i] + half
    y0 <- coords$Lat[i] - half; y1 <- coords$Lat[i] + half
    if (x1 <= e@xmin || x0 >= e@xmax || y1 <= e@ymin || y0 >= e@ymax) next
    cols <- seq(max(1, floor((x0-e@xmin)/dx)+1),
                min(raster::ncol(template), ceiling((x1-e@xmin)/dx)))
    rows <- seq(max(1, floor((e@ymax-y1)/dy)+1),
                min(raster::nrow(template), ceiling((e@ymax-y0)/dy)))
    candidates <- expand.grid(row=rows, col=cols)
    west <- e@xmin + (candidates$col-1)*dx
    north <- e@ymax - (candidates$row-1)*dy
    lon_overlap <- pmax(0, pmin(x1, west+dx)-pmax(x0,west))
    lat_overlap <- pmax(0, sin(pmin(y1,north)*pi/180)-sin(pmax(y0,north-dy)*pi/180))
    weight <- lon_overlap * lat_overlap /
      ((x1-x0)*(sin(y1*pi/180)-sin(y0*pi/180)))
    keep <- weight > 0
    output[[i]] <- data.frame(source=i,
       target=raster::cellFromRowCol(template,candidates$row[keep],candidates$col[keep]),
       weight=weight[keep])
  }
  mapping <- do.call(rbind, output)
  if (is.null(mapping)) return(data.frame(source=integer(),target=integer(),weight=numeric()))
  # Explicitly check that no original cell has more than its full effort assigned.
  sums <- rowsum(mapping$weight, mapping$source)
  if (any(sums > 1+1e-8)) stop("Overlap weights exceed source cell area")
  mapping
}


# 2. Apply weights and check conservation in the same rectangular domain --------
transfer_effort <- function(hours, mapping, template) {
  if (any(!is.finite(hours) | hours < 0)) stop("Invalid source fishing hours")
  contributions <- hours[mapping$source] * mapping$weight
  values <- rep(0, raster::ncell(template))
  if (nrow(mapping)) {
    grouped <- rowsum(contributions, mapping$target)
    values[as.integer(rownames(grouped))] <- grouped[,1]
  }
  expected <- sum(contributions)
  if (abs(sum(values)-expected) > 1e-8*max(1,expected)) stop("Hours are not conserved")
  result <- raster::raster(template)
  raster::values(result) <- values
  list(raster=result, input_hours=sum(hours), domain_hours=expected,
       outside_hours=sum(hours)-expected, raster_hours=sum(values))
}


# 3. Require exactly one file per requested date, independent of file ordering ---
daily_files <- function(folder, dates, filename) {
  files <- file.path(folder, vapply(dates, function(day) filename(as.Date(day,origin="1970-01-01")), character(1)))
  if (anyDuplicated(files) || !all(file.exists(files))) {
    stop("Missing or duplicated daily files: ", paste(basename(files[!file.exists(files)]),collapse=", "))
  }
  files
}
