# Contracts shared by the revised workflow. No data paths or credentials here.
strict_date <- function(x, format) {
  y <- as.Date(x, format = format)
  if (anyNA(y) || any(base::format(y, format) != x)) stop('Invalid or ambiguous date')
  y
}

date_from_code <- function(code) {
  iso <- ifelse(grepl('^[0-9]{8}_', code), substr(code, 1, 8),
    ifelse(grepl('^[0-9]{2}_[0-9]{2}_[0-9]{2}_', code),
      paste0('20', substr(code, 1, 2), substr(code, 4, 5), substr(code, 7, 8)), NA))
  strict_date(iso, '%Y%m%d')
}

restore_extraction_dates <- function(final, upstream) {
  if (anyDuplicated(final$code) || anyDuplicated(upstream$code)) stop('Duplicate tow code')
  j <- match(final$code, upstream$code)
  if (anyNA(j)) stop('Unmatched tow code')
  dates <- strict_date(upstream$date[j], '%d/%m/%Y')
  if (any(dates != date_from_code(final$code))) stop('Extraction date/code disagreement')
  before <- strict_date(final$date, '%d/%m/%Y')
  changed <- before != dates
  log <- data.frame(code = final$code[changed], before = as.character(before[changed]),
    after = as.character(dates[changed]))
  final$date <- format(dates, '%d/%m/%Y')
  list(data = final, changes = log)
}

# mu is E(log1p(Y) | Y>0, X). A validated correction is needed for arithmetic means.
# S=1 is an inverse-log sensitivity, not an established mean on the original scale.
hurdle_bpue <- function(p, mu, S) {
  if (length(p) != length(mu) || !(length(S) %in% c(1L, length(p)))) stop('Lengths differ')
  if (any(!is.finite(c(p, mu, S))) || any(p < 0 | p > 1) || any(S <= 0)) stop('Invalid prediction')
  p * pmax(exp(mu) * S - 1, 0)
}

month_windows <- function(year) {
  starts <- seq(as.Date(sprintf('%d-01-01', year)), by = 'month', length.out = 13)
  data.frame(start = starts[1:12], end_exclusive = starts[2:13])
}
