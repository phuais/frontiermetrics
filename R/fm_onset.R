calc_fm_onset <- function(x, classes_sub, ncores, silent){

  ff_onset <- function(idcell, x){
    foo <- x@data[x@data$id_cell == idcell, x@fl_cols, with = F]
    yy_sums <- colSums(foo > 0)
    onset_year <- which(yy_sums >= classes_sub[[1]])[1]
    if(!is.na(onset_year)) onset_year <- x@time_frame[1] + onset_year
    return(as.numeric(onset_year))
  }

  fm_ds_c <- unique(x@data[, "id_cell", with = F])

  if(ncores > 1){
    if(!requireNamespace("future", quietly = TRUE) ||
       !requireNamespace("future.apply", quietly = TRUE)) {
      stop("Packages 'future' and 'future.apply' must be installed for parallel processing.")
    }

    if(silent) message(paste0("Running in parallel: ", ncores, " workers"))
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = ncores)
    fm_ds_c$onset <- unlist(future.apply::future_lapply(fm_ds_c$id_cell, ff_onset, x, future.seed = TRUE))
  } else {
    fm_ds_c$onset <- sapply(fm_ds_c$id_cell, ff_onset, x)
  }

  out <- list(metrics = "onset",
              data = as.data.table(fm_ds_c))

  return(out)
}
