# Small scientific/data-contract checks, independent of private input files.
source('8_revision_2026/revision_helpers.R')
code <- c('20210218_63210_test_03', '21_10_05_CRT_ISA_L1')
f <- data.frame(code, date = c('18/02/2020', '05/10/2020'), ln_fishingEffort = c(2.25, 1.88))
u <- data.frame(code = rev(code), date = c('05/10/2021', '18/02/2021'))
z <- restore_extraction_dates(f, u)
stopifnot(identical(z$data$date, c('18/02/2021', '05/10/2021')),
  identical(z$data$ln_fishingEffort, f$ln_fishingEffort), nrow(z$changes) == 2)
fails <- function(expr) inherits(try(expr, silent = TRUE), 'try-error')
stopifnot(fails(restore_extraction_dates(f, rbind(u,u[1,]))),
  fails(strict_date('31/02/2021', '%d/%m/%Y')))
# Positives 1 and 99: arithmetic mean=50. Probability=.5 -> unconditional mean=25.
mu <- mean(log1p(c(1, 99))); S <- mean(exp(log1p(c(1, 99)) - mu))
stopifnot(abs(hurdle_bpue(.5, mu, S) - 25) < 1e-10,
  abs(hurdle_bpue(.5, mu, 1) - 25) > 1,
  fails(hurdle_bpue(1.1, mu, 1)))
w <- month_windows(2021)
stopifnot(sum(as.integer(w$end_exclusive - w$start)) == 365,
  w$end_exclusive[12] == as.Date('2022-01-01'))
message('Date joins, effort preservation, hurdle arithmetic and 365-day windows passed')
