# ------------------------------------------------------------------------------
# 6.4.1. Sum joint daily bycatch by replicate before computing period intervals
# ------------------------------------------------------------------------------
library(raster)
source('fun/validate_model_inputs.R')
source('fun/integrate_joint_hurdle.R')

# Effort manifest: date, swept_area_path, dataset_version. Area must already be
# km2 swept PER CELL per day, conservatively aligned to the prediction grid.
# Hours, hours/km2, and interpolated extensive effort are not valid inputs here.
# Rscript script.R joint_prediction_folder effort_manifest.csv new_output first_date last_date
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=5L)stop('Provide predictions, effort manifest, new output, first and last date')
predictions <- args[1];effort <- read.csv(args[2],stringsAsFactors=FALSE);out <- args[3]
period <- strict_date(args[4:5],'%Y-%m-%d')
if(period[1]>period[2])stop('Invalid integration period')
expected <- format(seq(period[1],period[2],by='day'),'%Y-%m-%d')
if(!all(c('date','swept_area_path','dataset_version')%in%names(effort)))stop('Invalid effort manifest')
strict_date(effort$date,'%Y-%m-%d')
if(anyDuplicated(effort$date)||!setequal(effort$date,expected))stop('Effort dates must exactly cover requested period')
if(length(unique(effort$dataset_version))!=1L||anyNA(effort$dataset_version)||any(effort$dataset_version==''))
  stop('Declare one consistent effort dataset version')
if(any(!file.exists(effort$swept_area_path)))stop('Missing swept area files')
run <- readRDS(file.path(predictions,'prediction_complete.rds'))
if(!all(expected%in%run$manifest$date))stop('Missing predicted dates')
if(dir.exists(out))stop('Use a new output folder')
dir.create(out,recursive=TRUE)
for(taxon in c('Scyliorhinus','Raja')) {
  total <- NULL;point <- 0;daily <- list();ids <- NULL
  for(day in expected) {
    prefix <- file.path(predictions,taxon,gsub('-','',day))
    audit <- readRDS(paste0(prefix,'_audit.rds'))
    if(is.null(ids))ids <- audit$replicate_ids else if(!identical(ids,audit$replicate_ids))
      stop('Replicate identities change between days')
    draws_file <- paste0(prefix,'_replicates.tif')
    if(is.null(audit$replicates_md5)||!identical(audit$replicates_md5,unname(tools::md5sum(draws_file))))
      stop('Replicate raster differs from its identity audit')
    draws <- raster::brick(draws_file)
    # GeoTIFF readers may replace band labels. Restore labels from the writer's
    # ordered IDs only after verifying that the exact raster file is unchanged.
    names(draws) <- sprintf('rep_%04d',ids)
    item <- integrate_joint_day(raster::brick(paste0(prefix,'_summary.tif')),
      draws,raster::raster(paste0(prefix,'_domain.tif')),
      raster::raster(effort$swept_area_path[match(day,effort$date)]),ids)
    if(is.null(total))total <- numeric(length(ids))
    total <- total+item$replicates;point <- point+item$point
    daily[[day]] <- data.frame(date=day,point=item$point,
      excluded_swept_area_km2=item$excluded_swept_area_km2)
  }
  write.csv(do.call(rbind,daily),file.path(out,paste0(taxon,'_daily.csv')),row.names=FALSE)
  write.csv(data.frame(replicate_id=ids,total=total),
    file.path(out,paste0(taxon,'_period_replicates.csv')),row.names=FALSE)
  summary <- data.frame(taxon=taxon,first_date=args[4],last_date=args[5],point=point,
    bootstrap_mean=mean(total),lower_025=unname(quantile(total,.025)),
    upper_975=unname(quantile(total,.975)),units='capsules',
    uncertainty='Conditional model uncertainty; effort/conversion uncertainty excluded')
  write.csv(summary,file.path(out,paste0(taxon,'_period_summary.csv')),row.names=FALSE)
}
saveRDS(list(completed=Sys.time(),effort_manifest=effort,period=period),file.path(out,'integration_complete.rds'))
