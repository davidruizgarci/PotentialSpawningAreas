# ------------------------------------------------------------------------------
# 4.5.1. Predict locked joint hurdle models from a dated environmental manifest
# ------------------------------------------------------------------------------
library(raster)
library(gbm)
source('fun/validate_model_inputs.R')
source('fun/validate_hurdle_mean.R')
source('fun/predict_joint_hurdle.R')

# Manifest columns: date (YYYY-MM-DD), stack_path (absolute path to rebuilt stack).
# Rscript script.R final_model_folder stack_manifest.csv signed_bathymetry new_output
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=4L)stop('Provide final model folder, stack manifest, signed bathymetry and new output')
models <- args[1];manifest_file <- args[2];elevation <- args[3];out <- args[4]
if(!file.exists(file.path(models,'final_fit_complete.rds')))stop('Final joint fit incomplete')
if(dir.exists(out))stop('Use a new output folder')
manifest <- read.csv(manifest_file,stringsAsFactors=FALSE)
if(!all(c('date','stack_path')%in%names(manifest)))stop('Invalid stack manifest columns')
dates <- strict_date(manifest$date,'%Y-%m-%d')
file_date <- regmatches(basename(manifest$stack_path),regexpr('[0-9]{8}',basename(manifest$stack_path)))
if(length(file_date)!=nrow(manifest)||any(file_date!=format(dates,'%Y%m%d')))
  stop('Stack filename and manifest date disagree')
if(anyDuplicated(dates)||anyDuplicated(manifest$stack_path)||any(!file.exists(manifest$stack_path)))
  stop('Missing or duplicated date/stack')
manifest <- manifest[order(dates),]
dir.create(out,recursive=TRUE)
audit <- list()
for(taxon in c('Scyliorhinus','Raja')) {
  ensemble <- readRDS(file.path(models,paste0(taxon,'_joint_bootstrap.rds')))
  folder <- file.path(out,taxon);dir.create(folder)
  for(i in seq_len(nrow(manifest))) {
    s <- prepare_hurdle_grid(manifest$stack_path[i],elevation)
    prefix <- file.path(folder,gsub('-','',manifest$date[i]))
    check <- write_joint_hurdle_day(ensemble,s,prefix)
    audit[[paste(taxon,manifest$date[i])]] <- c(list(taxon=taxon,date=manifest$date[i]),check)
    saveRDS(check,paste0(prefix,'_audit.rds'))
    message(taxon,' ',manifest$date[i],': ',check$complete_cells,' complete cells')
  }
}
saveRDS(list(completed=Sys.time(),manifest=manifest,audit=audit,
  complete_calendar_year=identical(manifest$date,format(seq(as.Date(paste0(substr(manifest$date[1],1,4),'-01-01')),
    as.Date(paste0(substr(manifest$date[1],1,4),'-12-31')),by='day')))),
  file.path(out,'prediction_complete.rds'))
