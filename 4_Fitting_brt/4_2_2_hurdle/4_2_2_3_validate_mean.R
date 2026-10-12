# ------------------------------------------------------------------------------
# 4.2.2.3. Select and evaluate hurdle means using nested, repeated spatial holdouts
# ------------------------------------------------------------------------------
library(gbm)
source('fun/validate_model_inputs.R')
source('fun/validate_hurdle_mean.R')

# 1. Set repositories and prespecified choices before inspecting test outcomes --
# Rscript 4_Fitting_brt/4_2_2_hurdle/4_2_2_3_validate_mean.R data_root output_folder
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=2L)stop('Provide the local data root and a new output folder')
root <- args[1];out <- args[2]
if(file.exists(file.path(out,'validation_complete.rds')))stop('Completed output exists; use a new folder')
dir.create(out,recursive=TRUE,showWarnings=FALSE)
predictors <- c('depth','slope','ln_fishingEffort','bottom_temp','bottom_so')
grid <- expand.grid(tc=c(1L,3L,5L),lr=c(.005,.02))
trees <- c(250L,750L,1500L,3000L)
spatial_seeds <- c(123L,456L,789L)
random_seeds <- 2026L
manifest <- list(predictors=predictors,grid=grid,trees=trees,spatial_seeds=spatial_seeds,
  random_seeds=random_seeds,outer_folds=5L,inner_folds=3L,calibration_folds=3L,
  selection='Minimum inner original-unit hurdle MSE; occurrence uses Brier score',
  packages=c(R=as.character(getRversion()),gbm=as.character(packageVersion('gbm'))))
saveRDS(manifest,file.path(out,'validation_manifest.rds'))
jobs <- list();inputs <- list();hashes <- list()
for(taxon in c('Scyliorhinus','Raja')) {
  file <- file.path(root,'temp/folds_dataset',paste0(taxon,'_ALL_folds_dataset.csv'))
  extraction <- file.path(root,'temp/data_subsets',paste0(taxon,'_dataset_log_pred.csv'))
  data <- read_model_data(file,extraction)
  data <- data[order(data$code),]
  rownames(data) <- NULL
  if(any(!complete.cases(data[predictors])))stop('Missing prespecified predictor')
  data$log_bpue <- log1p(data$N_km2)
  inputs[[taxon]] <- data
  hashes[[taxon]] <- tools::md5sum(c(file,extraction))
  for(scheme in c('geographic','random')) {
    seeds <- if(scheme=='geographic')spatial_seeds else random_seeds
    for(seed in seeds) {
      data$validation_fold <- validation_folds(data,5L,seed,scheme)
      if(any(sapply(1:5,function(k)sum(data$N[data$validation_fold!=k]>0))<20L))
        stop('Insufficient positive training hauls; do not drop or replace this holdout silently')
      write.csv(data[c('code','lon','lat','N','N_km2','validation_fold')],
        file.path(out,paste(taxon,scheme,seed,'folds.csv',sep='_')),row.names=FALSE)
      for(k in 1:5)jobs[[length(jobs)+1L]] <- list(data=data,taxon=taxon,scheme=scheme,
        seed=seed,fold=k,predictors=predictors,grid=grid,trees=trees,out=out)
    }
  }
}
stopifnot(identical(inputs$Scyliorhinus[c('code','lon','lat')],inputs$Raja[c('code','lon','lat')]))
saveRDS(hashes,file.path(out,'source_hashes.rds'))

# 2. Run independent outer assessments in parallel, keeping private outputs local
cores <- min(4L,max(1L,parallel::detectCores()-1L))
cluster <- parallel::makePSOCKcluster(cores,outfile=file.path(out,'worker_log.txt'))
parallel::clusterCall(cluster,function(paths,repository) {
  setwd(repository)
  .libPaths(paths)
  library(gbm)
  source('fun/validate_hurdle_mean.R')
  NULL
},.libPaths(),normalizePath('.'))
message('Running ',length(jobs),' outer assessments on ',cores,' workers')
results <- tryCatch(parallel::parLapplyLB(cluster,jobs,run_outer_assessment),
  finally=parallel::stopCluster(cluster))
predictions <- do.call(rbind,lapply(results,function(x)x$predictions))
selected <- predictions[predictions$family==predictions$chosen_family,]
selected$family <- 'nested_selected'
baseline <- predictions[predictions$family=='raw_gaussian',]
baseline$predicted <- baseline$baseline_mean;baseline$family <- 'training_mean_baseline'
all_predictions <- rbind(predictions,selected,baseline)
write.csv(all_predictions,file.path(out,'outer_predictions.csv'),row.names=FALSE)

# 3. Summarise pooled, repeat, regional and abundance-stratum performance -------
summarise <- function(columns,filename) {
  key <- interaction(all_predictions[columns],drop=TRUE)
  rows <- lapply(split(all_predictions,key),function(x)
    cbind(x[1,columns,drop=FALSE],bpue_metrics(x$observed,x$predicted)))
  result <- do.call(rbind,rows)
  write.csv(result,file.path(out,filename),row.names=FALSE)
  result
}
pooled <- summarise(c('taxon','scheme','family'),'pooled_metrics.csv')
summarise(c('taxon','scheme','repeat_seed','family'),'repeat_metrics.csv')
summarise(c('taxon','scheme','repeat_seed','fold','family'),'regional_metrics.csv')
summarise(c('taxon','scheme','family','assessment_stratum'),'abundance_stratum_metrics.csv')

# 4. Choose the deployment configuration from repeated CV using all training data
# This uses no new independent test set. Performance of the selection procedure
# is reported above using the outer held-out predictions, not these fitted models.
for(taxon in names(inputs)) {
  data <- inputs[[taxon]];occ_scores <- list();positive_scores <- list()
  for(seed in spatial_seeds) {
    occurrence <- tune_occurrence(data,predictors,grid,trees,seed,'geographic')
    positive <- tune_positive_means(data,predictors,occurrence,grid,trees,seed+10000,'geographic')
    occ_scores[[as.character(seed)]] <- occurrence$scores
    positive_scores[[as.character(seed)]] <- positive$scores
  }
  a <- aggregate(Brier~tc+lr+trees,do.call(rbind,occ_scores),mean)
  b <- aggregate(MSE~family+tc+lr+trees,do.call(rbind,positive_scores),mean)
  a <- a[order(a$Brier,a$trees,a$tc,a$lr),];b <- b[order(b$MSE,b$trees,b$tc,b$lr,b$family),]
  occurrence <- list(config=a[1,c('tc','lr')],trees=a$trees[1],score=a$Brier[1])
  positive <- list(family=b$family[1],config=b[1,c('tc','lr')],trees=b$trees[1],mse=b$MSE[1])
  model <- fit_hurdle_choice(data,predictors,occurrence,positive,13579L,'geographic')
  model$validation_manifest <- manifest
  model$source_hashes <- hashes[[taxon]]
  model$status <- 'Mean-validation candidate; joint bootstrap and final map integration pending'
  saveRDS(model,file.path(out,paste0(taxon,'_selected_hurdle_model.rds')))
  write.csv(a,file.path(out,paste0(taxon,'_deployment_occurrence_scores.csv')),row.names=FALSE)
  write.csv(b,file.path(out,paste0(taxon,'_deployment_positive_scores.csv')),row.names=FALSE)
  message(taxon,': selected ',positive$family,'; tc=',positive$config$tc,
    '; lr=',positive$config$lr,'; trees=',positive$trees,'; smearing=',model$smearing)
}
for(taxon in names(hashes))stopifnot(identical(hashes[[taxon]],tools::md5sum(names(hashes[[taxon]]))))
saveRDS(list(manifest=manifest,pooled_metrics=pooled,completed=Sys.time()),
  file.path(out,'validation_complete.rds'))
print(pooled)
