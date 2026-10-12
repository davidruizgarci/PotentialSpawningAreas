# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management

# ------------------------------------------------------------------------------
# Verify area units, alignment and joint replicate totals on a known small grid
# ------------------------------------------------------------------------------
library(raster)
source("fun/integrate_joint_hurdle.R")
grid <- raster(nrows = 1, ncols = 2, xmn = 0, xmx = 2, ymn = 0, ymx = 1)
summary <- stack(setValues(grid, c(2, 4)))
draws <- brick(nrows = 1, ncols = 2, nl = 2, xmn = 0, xmx = 2, ymn = 0, ymx = 1)
values(draws) <- matrix(c(1, 3, 3, 5), nrow = 2)
names(draws) <- sprintf("rep_%04d", 1:2)
domain <- setValues(grid, c(1, 1))
area <- setValues(grid, c(10, 20))
a <- integrate_joint_day(summary, draws, domain, area, 1:2)
stopifnot(a$point == 100, identical(unname(a$replicates), c(70, 130)))
fails <- function(expr) inherits(try(force(expr), silent = TRUE), "try-error")
stopifnot(
  fails(integrate_joint_day(summary, draws, domain, setValues(grid, c(NA, 20)), 1:2)),
  fails(integrate_joint_day(summary, draws, domain, setValues(grid, c(-1, 20)), 1:2)),
  fails(integrate_joint_day(summary, draws, domain, shift(area, dx = .1), 1:2)),
  fails(integrate_joint_day(summary, draws, domain, area, 2:1))
)
message("Joint replicate area integration, missing effort and alignment checks passed")
