calc_fm_onset <- function(x, classes_sub, ncores){

  ff_onset <- function(idcell, x){
    foo <- x@data[x@data$id_cell == idcell, x@fl_cols, with = F]
    yy_sums <- colSums(foo > 0)
    onset_year <- which(yy_sums >= classes_sub[[1]])[1]
    if(!is.na(onset_year)) onset_year <- x@time_frame[1] + onset_year
    return(as.numeric(onset_year))
  }

  fm_ds_c <- unique(x@data[, "id_cell", with = F])

  if(ncores > 1){
    # Run in parallel
    message(paste0("Running in parallel: ", ncores, " CPUs"))
    snowfall::sfSetMaxCPUs(ncores)
    snowfall::sfInit(parallel = T, cpus = ncores, type = "SOCK")
    snowfall::sfLibrary(data.table)
    snowfall::sfExport("ff_onset")
    fm_ds_c$onset <- snowfall::sfSapply(fm_ds_c$id_cell, ff_onset, x)
    snowfall::sfStop()
  } else {
    fm_ds_c$onset <- sapply(fm_ds_c$id_cell, ff_onset, x)
  }

  out <- new("FrontierMetric",
             metrics = "onset",
             ud_metrics = "",
             time_frame = x@time_frame,
             data = as.data.table(fm_ds_c),
             extent = x@extent,
             grain = x@grain,
             aggregation = x@aggregation,
             min_treecover = x@min_treecover,
             min_cover = x@min_cover,
             min_rate = x@min_rate,
             window = x@window,
             archetypes = data.frame(),
             excluded_cells = x@excluded_cells)

  return(out)
}
