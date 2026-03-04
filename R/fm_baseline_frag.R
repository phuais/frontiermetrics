calc_fm_baseline_frag <- function(x, classes_sub, ncores, silent){

  if(ncores > 1){
    if(!requireNamespace("future", quietly = TRUE) ||
       !requireNamespace("future.apply", quietly = TRUE)) {
      stop("Packages 'future' and 'future.apply' must be installed for parallel processing.")
    }

    if(!silent) cat(paste0(", running in parallel (", ncores, " workers)"))
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = ncores)
  }

  # Functions to get fragmentation metric
  chunk <- function(x, n) split(x, cut(seq_along(x), n, labels = FALSE))

  ed_func <- edge_density

  # data_input <- listi[[1]]
  # Get edge density value
  get.ed <- function(data_input, x){
    total_cells <- length(data_input[[2]])
    ras <- vector("list", total_cells)
    #reso_vec <- vector("list", total_cells)
    for(j in 1:total_cells){
      spgdssample <- data_input[[1]][which(data_input[[1]]$id_cell == data_input[[2]][[j]]), ]
      spgdssample <- spgdssample[order(spgdssample$y_1, decreasing = T), ]
      spgdssample <- spgdssample[order(spgdssample$x_1), ]
      coordi <- spgdssample[, 2:3]
      coordi2 <- c(min(coordi$x_1)-x@grain[[1]][1]/2, max(coordi$x_1)+x@grain[[1]][1]/2,
                   min(coordi$y_1)-x@grain[[1]][2]/2, max(coordi$y_1)+x@grain[[1]][2]/2)

      # Axis X width
      x1 <- terra::distance(matrix(c(coordi2[1], coordi2[4]), ncol = 2),
                            matrix(c(coordi2[2], coordi2[4]), ncol = 2), lonlat = T)
      x2 <- terra::distance(matrix(c(coordi2[1], coordi2[3]), ncol = 2),
                            matrix(c(coordi2[2], coordi2[3]), ncol = 2), lonlat = T)
      x_width <- mean(x1, x2)

      # Axis Y width
      y1 <- terra::distance(matrix(c(coordi2[1], coordi2[4]), ncol = 2),
                            matrix(c(coordi2[1], coordi2[3]), ncol = 2), lonlat = T)
      y2 <- terra::distance(matrix(c(coordi2[2], coordi2[4]), ncol = 2),
                            matrix(c(coordi2[2], coordi2[3]), ncol = 2), lonlat = T)
      y_width <- mean(y1, y2)

      # Get resolution of cell (in meters)
      side_cells <- sqrt(nrow(coordi))
      #reso_vec[[j]] <- c(x_width/side_cells, y_width/side_cells)

      ras[[j]] <- vector("list", 2)
      ras[[j]][[1]] <- c(x_width/side_cells, y_width/side_cells)
      spgdssample$FC_bas2 <- ifelse(spgdssample[[x@initial_fc_col]] == 0, 0, 1)
      ras[[j]][[2]] <- matrix(spgdssample$FC_bas2, ncol = side_cells, byrow = F)
    }
    ed <- lapply(X = ras, FUN = ed_func)
    ed <- unlist(ed)

    return(data.table(id = data_input[[2]], ed = ed))
  }

  dsbs <- x@data[, c("id_cell", "x_1", "y_1", x@initial_fc_col), with = F]
  uu <- unique(dsbs[[1]])

  if(ncores > 1){
    subs <- split(uu, cut(seq_along(uu), ncores, labels = FALSE))
    listi <- list()
    for(i in 1:length(subs)){
      listi[[i]] <- list(dsbs[which(dsbs[[1]] %in% subs[[i]]), ], subs[[i]])
    }
    croppi <- future.apply::future_lapply(listi, get.ed, x, future.seed = TRUE)
  } else {
    subs <- list(uu)
    listi <- list()
    listi[[1]] <- list(dsbs[which(dsbs[[1]] %in% subs[[1]]), ], subs[[1]])
    croppi <- lapply(listi, get.ed, x)
  }

  ds_ed <- data.table::rbindlist(croppi)
  colnames(ds_ed) <- c("id_cell", "baseline_frag")

  # Classify cells
  brks <- getBreaks(ds_ed$baseline_frag, classes_sub)
  ds_ed$baseline_frag.c <- cut(ds_ed$baseline_frag, brks, labels = classes_sub[[2]])
  ds_ed$baseline_frag.c <- factor(ds_ed$baseline_frag.c, levels = rev(classes_sub[[2]]))

  out <- list(metrics = "baseline_frag",
              data = as.data.table(ds_ed))

  return(out)
}
