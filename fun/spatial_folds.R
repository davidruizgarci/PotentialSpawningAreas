# ------------------------------------------------------------------------------
# Reproducible geographic folds shared by occurrence and positive-BPUE models
# ------------------------------------------------------------------------------
make_spatial_folds <- function(data, k = 5L, seed = 123L) {
  if (anyDuplicated(data$code)) stop('Create folds from one record per haul')
  if (any(!is.finite(data$lat)) || any(!is.finite(data$lon))) stop('Missing haul coordinates')
  # Sort by identifier so input row order cannot change the geographic partition.
  order <- order(data$code)
  xy <- cbind(data$lon[order] * cos(mean(data$lat) * pi / 180), data$lat[order])
  set.seed(seed)
  groups <- kmeans(xy, centers = k, nstart = 50)$cluster
  fold <- integer(nrow(data))
  fold[order] <- groups
  if (length(unique(fold)) != k || any(tabulate(fold) < 5L)) stop('Insufficient geographic fold size')
  fold
}
