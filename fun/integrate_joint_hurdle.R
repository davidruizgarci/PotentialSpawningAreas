# ------------------------------------------------------------------------------
# Integrate densities against aligned swept area, preserving replicate identity
# ------------------------------------------------------------------------------
integrate_joint_day <- function(summary,draws,domain,swept_area,ids) {
  raster::compareRaster(summary,draws,domain,swept_area,extent=TRUE,rowcol=TRUE,
    crs=TRUE,res=TRUE,orig=TRUE,rotation=TRUE,stopiffalse=TRUE)
  if(raster::nlayers(draws)!=length(ids))stop('Replicate count differs')
  if(!identical(names(draws),sprintf('rep_%04d',ids)))stop('Replicate band identities differ')
  blocks <- raster::blockSize(draws)
  point <- 0;total <- numeric(length(ids));excluded <- 0
  for(i in seq_len(blocks$n)) {
    row <- blocks$row[i];n <- blocks$nrows[i]
    a <- raster::getValues(swept_area,row=row,nrows=n)
    d <- raster::getValues(domain,row=row,nrows=n)
    y <- raster::getValues(summary[[1]],row=row,nrows=n)
    z <- raster::getValues(draws,row=row,nrows=n)
    if(any(!is.na(a)&(!is.finite(a)|a<0)))stop('Invalid swept area')
    inside <- !is.na(d)
    if(any(inside&(!is.finite(a))))stop('Missing swept area inside prediction domain; never assume zero')
    active <- inside&a>0
    if(any(active&!is.finite(y))||any(!is.finite(z[active,,drop=FALSE])))
      stop('Positive effort in cells without complete BPUE predictions')
    point <- point+sum(y[active]*a[active])
    total <- total+colSums(z[active,,drop=FALSE]*a[active])
    excluded <- excluded+sum(a[!inside],na.rm=TRUE)
  }
  list(point=point,replicates=total,excluded_swept_area_km2=excluded)
}
