calc_fm_baseline_frag <- function(x, classes_sub, ncores, silent){

  dsbs <- x@data[, c("id_cell", "x_1", "y_1", x@initial_fc_col), with = F]
  uu <- unique(dsbs[[1]])

  ds_ed <- .get_ed_bs(list(dsbs, uu), grain = x@grain,
                      initial_fc_col = x@initial_fc_col,
                      ncores, silent)

  brks <- getBreaks(ds_ed$baseline_frag, classes_sub)
  ds_ed$baseline_frag.c <- cut(ds_ed$baseline_frag, brks, labels = classes_sub[[2]])
  ds_ed$baseline_frag.c <- factor(ds_ed$baseline_frag.c, levels = rev(classes_sub[[2]]))
  out <- list(metrics = "baseline_frag", data = as.data.table(ds_ed))
  return(out)
}

.get_ed_bs <- function(data_input, grain, initial_fc_col, ncores, silent) {

  process_cell <- function(cell_id, dt, grain, initial_fc_col) {
    spgdssample <- dt[which(dt$id_cell == cell_id), ]
    spgdssample <- spgdssample[order(spgdssample$y_1, decreasing = T), ]
    spgdssample <- spgdssample[order(spgdssample$x_1), ]
    coordi <- spgdssample[, 2:3]
    coordi2 <- c(min(coordi$x_1) - grain[[1]][1]/2, max(coordi$x_1) + grain[[1]][1]/2,
                 min(coordi$y_1) - grain[[1]][2]/2, max(coordi$y_1) + grain[[1]][2]/2)

    mean_lat <- mean(coordi2[3:4])
    mean_lon <- mean(coordi2[1:2])

    x_width <- geodist::geodist(cbind(x = coordi2[1], y = mean_lat),
                                cbind(x = coordi2[2], y = mean_lat),
                                measure = "cheap")
    y_width <- geodist::geodist(cbind(x = mean_lon, y = coordi2[3]),
                                cbind(x = mean_lon, y = coordi2[4]),
                                measure = "cheap")

    side_cells <- sqrt(nrow(coordi))
    spgdssample$FC_bas2 <- ifelse(spgdssample[[initial_fc_col]] == 0, 0, 1)

    list(
      res = c(x_width / side_cells, y_width / side_cells),
      mat = matrix(spgdssample$FC_bas2, ncol = side_cells, byrow = F)
    )
  }

  if(ncores > 1){
    if(!requireNamespace("future", quietly = TRUE) ||
       !requireNamespace("future.apply", quietly = TRUE)) {
      stop("Packages 'future' and 'future.apply' must be installed for parallel processing.")
    }
    if(!silent) cat(paste0(", running in parallel (", ncores, " workers)"))
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = ncores)

    ras <- future.apply::future_lapply(
      data_input[[2]],
      process_cell,
      dt = data_input[[1]],
      grain = grain,
      initial_fc_col = initial_fc_col,
      future.seed = TRUE
    )
  } else {
    ras <- lapply(data_input[[2]],
                  process_cell,
                  dt = data_input[[1]],
                  grain = grain,
                  initial_fc_col = initial_fc_col)
  }

  ed <- vapply(ras, function(cell) edge_density(list(cell$res, cell$mat)), numeric(1))
  data.table::data.table(id_cell = data_input[[2]], baseline_frag = ed)
}
