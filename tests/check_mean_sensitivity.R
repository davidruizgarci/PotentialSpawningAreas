# ------------------------------------------------------------------------------
# Verify Gamma means and complete nested follow-up without private input records
# ------------------------------------------------------------------------------
source('fun/validate_hurdle_mean.R')
source('fun/mean_sensitivity.R')
set.seed(327)
d <- data.frame(code=sprintf('synthetic_%03d',1:180),lon=runif(180),lat=runif(180))
d$N <- rep(c(0,1,1),60)
d$N_km2 <- d$N*rgamma(180,shape=2,scale=exp(1+d$lon-.3*d$lat)/2)
d$presence_absence <- as.integer(d$N>0);d$log_bpue <- log1p(d$N_km2)
model <- fit_gamma_mean(d,c('lon','lat'))
reference <- glm(N_km2~lon+lat,data=d[d$N>0,],family=Gamma(link='log'))
stopifnot(max(abs(predict_gamma_mean(model,d)-predict(reference,d,type='response')))<1e-3)
if(requireNamespace('gbm',quietly=TRUE)) {
  d$validation_fold <- validation_folds(d,5L,123L)
  train <- d[d$validation_fold!=1,];test <- d[d$validation_fold==1,]
  occurrence <- tune_occurrence(train,c('lon','lat'),expand.grid(tc=1L,lr=.02),c(20L,40L),223L,'geographic')
  output <- tempfile('sensitivity_');dir.create(output)
  config <- file.path(output,'original_config.rds')
  saveRDS(list(raw_gaussian=list(occurrence=occurrence,training_codes=train$code,assessment_codes=test$code)),config)
  job <- list(data=d,taxon='synthetic',scheme='geographic',seed=123L,fold=1L,
    predictors=c('lon','lat'),out=output,config_file=config)
  a <- run_mean_sensitivity(job)
  held <- d$validation_fold==1
  job$data$N_km2[held] <- 10*job$data$N_km2[held]
  job$data$log_bpue[held] <- log1p(job$data$N_km2[held])
  b <- run_mean_sensitivity(job)
  stopifnot(identical(a$predicted,b$predicted),identical(a$inner_MSE,b$inner_MSE),
    identical(a$chosen_family,b$chosen_family))
  unlink(output,recursive=TRUE)
} else message('Install gbm to run the nested sensitivity check')
message('Gamma arithmetic means and response-excluded sensitivity checks passed')
