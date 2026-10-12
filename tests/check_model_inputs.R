# ------------------------------------------------------------------------------
# Check scientific input contracts before rebuilding stacks or fitting models
# ------------------------------------------------------------------------------
source('fun/validate_model_inputs.R')
source('fun/spatial_folds.R')
source('fun/hurdle_predictions.R')
fails <- function(expr) inherits(try(expr, silent = TRUE), 'try-error')

# 1. Dates must match keys even when the extraction table is reordered ----------
code <- c('20210218_63210_test_03', '21_10_05_CRT_ISA_L1')
final <- data.frame(code, date = c('18/02/2020', '05/10/2020'), ln_fishingEffort = c(2.25,1.88))
upstream <- data.frame(code = rev(code), date = c('05/10/2021','18/02/2021'))
fixed <- restore_extraction_dates(final, upstream)
stopifnot(identical(fixed$data$date,c('18/02/2021','05/10/2021')),
  identical(final$ln_fishingEffort,fixed$data$ln_fishingEffort),
  fails(restore_extraction_dates(final,rbind(upstream,upstream[1,]))),
  fails(strict_date('31/02/2021')))

# 2. Land must not become ocean; old negative-depth stacks must fail ------------
depth <- depth_from_elevation(c(-700,-50,0,120,NA))
stopifnot(identical(depth,c(700,50,NA_real_,NA_real_,NA_real_)),
  fails(assert_positive_depth(c(-50,100))), fails(assert_positive_depth(c(NA,NA))),
  fails(assert_positive_depth(c(100,Inf))))
assert_positive_depth(depth)

# 3. Predictor renaming must preserve field identities after reordering --------
x <- data.frame(N=2, bottomT_Reanalysis=13, LN_fishingEffort=2.1)
y <- normalise_model_columns(x[c(3,1,2)])
stopifnot(y$N==2,y$bottom_temp==13,y$ln_fishingEffort==2.1)

# 4. Geographic groups must be independent of row order -------------------------
set.seed(1)
x <- data.frame(code=sprintf('haul_%03d',1:100),lon=runif(100),lat=runif(100)+38)
f <- make_spatial_folds(x)
o <- sample(nrow(x));g <- make_spatial_folds(x[o,])
stopifnot(identical(f[o],g))
# 5. Combine joint predictions before summarising their uncertainty ------------
p <- matrix(c(.1,.9),nrow=1,dimnames=list('cell_date',c('joint_1','joint_2')))
y <- matrix(c(100,1),nrow=1,dimnames=dimnames(p))
j <- combine_hurdle_replicates(p,y)
stopifnot(all(j==matrix(c(10,.9),nrow=1)), abs(summarise_joint_predictions(j)$mean-5.45)<1e-12)
colnames(y) <- rev(colnames(y))
stopifnot(fails(combine_hurdle_replicates(p,y)))
message('Date keys, columns, depth, land exclusion, geographic groups and joint predictions passed')
