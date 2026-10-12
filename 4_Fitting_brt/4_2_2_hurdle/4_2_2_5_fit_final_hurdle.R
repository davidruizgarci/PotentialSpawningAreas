# ------------------------------------------------------------------------------
# 4.2.2.5. Fit the locked exploratory formulations and paired-location bootstrap
# ------------------------------------------------------------------------------
library(gbm)
source('fun/validate_model_inputs.R')
source('fun/validate_hurdle_mean.R')

# 1. Lock the scientific decision; keep its exploratory provenance explicit -----
# Rscript script.R data_root original_validation sensitivity_output new_output B
# Use at least 200 replicates for production; 500 improves tail resolution.
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=5L)stop('Provide data root, original validation, sensitivity, new output and B')
root <- args[1];previous <- args[2];sensitivity <- args[3];out <- args[4]
B <- suppressWarnings(as.integer(args[5]))
if(is.na(B)||B<100L)stop('Use at least 100 bootstrap replicates')
if(!file.exists(file.path(sensitivity,'sensitivity_complete.rds')))stop('Sensitivity incomplete')
if(dir.exists(out))stop('Use a new output folder')
dir.create(out,recursive=TRUE)
manifest <- readRDS(file.path(previous,'validation_manifest.rds'))
hashes <- readRDS(file.path(sensitivity,'source_hashes.rds'))
locked <- c(Scyliorhinus='log_smearing',Raja='raw_gaussian')
decision <- list(families=locked,positive_tc=1L,positive_lr=.005,
  positive_trees=c(250L,750L,1500L),B=B,
  provenance='Exploratory fixed-family decision after original and focused spatial assessments',
  limits='Weak Scyliorhinus spatial abundance skill; no independent validation of post-hoc choice',
  bootstrap='Paired sampling locations; conditional on locked families, predictors and tuned parameters')
saveRDS(decision,file.path(out,'decision.rds'))
inputs <- list();models <- list()
for(taxon in names(locked)) {
  file <- file.path(root,'temp/folds_dataset',paste0(taxon,'_ALL_folds_dataset.csv'))
  extraction <- file.path(root,'temp/data_subsets',paste0(taxon,'_dataset_log_pred.csv'))
  stopifnot(identical(hashes[[taxon]],tools::md5sum(c(file,extraction))))
  d <- read_model_data(file,extraction);d <- d[order(d$code),];rownames(d) <- NULL
  d$log_bpue <- log1p(d$N_km2);inputs[[taxon]] <- d
  a <- read.csv(file.path(previous,paste0(taxon,'_deployment_occurrence_scores.csv')))
  a <- a[order(a$Brier,a$trees,a$tc,a$lr),]
  occurrence <- list(config=a[1,c('tc','lr')],trees=a$trees[1],score=a$Brier[1])
  scores <- lapply(manifest$spatial_seeds,function(seed)
    tune_positive_means(d,manifest$predictors,occurrence,expand.grid(tc=1L,lr=.005),
      c(250L,750L,1500L),seed+10000,'geographic')$scores)
  scores <- do.call(rbind,scores);scores <- scores[scores$family==locked[[taxon]],]
  b <- aggregate(MSE~tc+lr+trees,scores,mean)
  b <- b[order(b$MSE,b$trees),]
  positive <- list(family=locked[[taxon]],config=b[1,c('tc','lr')],trees=b$trees[1],mse=b$MSE[1])
  model <- fit_hurdle_choice(d,manifest$predictors,occurrence,positive,13579L,'geographic')
  model$status <- 'Locked exploratory formulation; interpret with spatial-validation limits'
  model$decision <- decision;model$source_hashes <- hashes[[taxon]]
  models[[taxon]] <- model
  saveRDS(model,file.path(out,paste0(taxon,'_hurdle_model.rds')))
  write.csv(b,file.path(out,paste0(taxon,'_within_family_scores.csv')),row.names=FALSE)
  message(taxon,': ',positive$family,'; trees=',positive$trees,'; correction=',model$smearing)
}
stopifnot(identical(inputs$Raja[c('code','lon','lat')],inputs$Scyliorhinus[c('code','lon','lat')]))

# 2. Resample the same locations for both components and genera -----------------
# Duplicate copies of a source location stay together in correction cross-fitting.
# Do not tune again or independently resample occurrence and positive abundance.
bootstrap_one <- function(job) {
  d <- job$data
  site <- paste(sprintf('%.6f',d$lon),sprintf('%.6f',d$lat),sep='_')
  groups <- split(seq_len(nrow(d)),site)
  set.seed(90000L+job$replicate)
  sampled <- sample(seq_along(groups),length(groups),replace=TRUE)
  index <- unlist(groups[sampled],use.names=FALSE)
  boot <- d[index,,drop=FALSE]
  model <- fit_hurdle_choice(boot,job$model$predictors,job$model$occurrence_config,
    job$model$positive_config,100000L+job$replicate,'geographic')
  model$replicate_id <- job$replicate
  list(model=model,sampled_codes=boot$code)
}
jobs <- list()
for(taxon in names(locked))for(b in seq_len(B))jobs[[length(jobs)+1L]] <-
  list(data=inputs[[taxon]],model=models[[taxon]],replicate=b,taxon=taxon)
cluster <- parallel::makePSOCKcluster(min(4L,max(1L,parallel::detectCores()-1L)),
  outfile=file.path(out,'worker_log.txt'))
parallel::clusterCall(cluster,function(paths,repository) {
  setwd(repository);.libPaths(paths);library(gbm);source('fun/validate_hurdle_mean.R');NULL
},.libPaths(),normalizePath('.'))
parallel::clusterExport(cluster,'bootstrap_one',envir=environment())
result <- tryCatch(parallel::parLapplyLB(cluster,jobs,bootstrap_one),
  finally=parallel::stopCluster(cluster))
for(taxon in names(locked)) {
  selected <- which(vapply(jobs,function(j)j$taxon==taxon,logical(1)))
  ensemble <- list(main=models[[taxon]],replicates=lapply(result[selected],`[[`,'model'),
    resampled_codes=lapply(result[selected],`[[`,'sampled_codes'),decision=decision)
  stopifnot(identical(vapply(ensemble$replicates,`[[`,integer(1),'replicate_id'),seq_len(B)))
  saveRDS(ensemble,file.path(out,paste0(taxon,'_joint_bootstrap.rds')))
}
for(taxon in names(hashes))stopifnot(identical(hashes[[taxon]],tools::md5sum(names(hashes[[taxon]]))))
saveRDS(list(completed=Sys.time(),decision=decision),file.path(out,'final_fit_complete.rds'))
