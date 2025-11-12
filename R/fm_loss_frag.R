calc_fm_loss_frag <- function(x, classes_sub, ncores){

  if(ncores > 1){
    if (!requireNamespace("snowfall", quietly = TRUE)) {
      message("Package 'snowfall' must be installed for parallel processing.")
      return(invisible(NULL))
    }
  }

  # Functions to get fragmentation metric
  chunk <- function(x, n) split(x, cut(seq_along(x), n, labels = FALSE))

  ed_func <- edge_density

  #data_input <- listi[[1]]
  get.ed <- function(data_input, x){
    total_cells <- length(data_input[[2]])
    ras <- vector("list", total_cells)
    reso_vec <- vector("list", total_cells)
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
      reso_vec[[j]] <- c(x_width/side_cells, y_width/side_cells)

      ras[[j]] <- vector("list", 2)
      ras[[j]][[1]] <- c(x_width/side_cells, y_width/side_cells)
      spgdssample$Flosssum2 <- ifelse(spgdssample$Flosssum == 0, 0, 1)
      ras[[j]][[2]] <- matrix(spgdssample$Flosssum2, ncol = side_cells, byrow = F)
    }
    ed <- lapply(X = ras, FUN = ed_func)
    ed <- unlist(ed)

    return(data.table(id = data_input[[2]], ed = ed))
  }

  if(ncores > 1){
    # Run in parallel
    message(paste0("Running in parallel: ", ncores, " CPUs"))
    snowfall::sfSetMaxCPUs(ncores)
    snowfall::sfInit(parallel = T, cpus = ncores, type = "SOCK")
    snowfall::sfLibrary(terra)
    snowfall::sfLibrary(data.table)
    snowfall::sfLibrary(landscapemetrics)
    snowfall::sfExport("ed_func")
  }

  ds_ed_years <- data.table()
  years_lab <- x@time_frame[1]:x@time_frame[2]
  for(year in 0:(x@time_frame[2]-x@time_frame[1]-1)){
    cat("\r> Processing year: ", years_lab[year+2], sep = "")
    # selects id and coordinates of smaller cells
    dsfl <- x@data[, c("id_cell", "x_1", "y_1"), with = F]
    dsfl$Flosssum <- rowSums(x@data[, 8:(8 + year)])
    uu <- unique(dsfl[[1]])
    if(ncores > 1){
      subs <- split(uu, cut(seq_along(uu), ncores, labels = FALSE))
      listi <- list()
      for(i in 1:length(subs)){
        listi[[i]] <- list(dsfl[which(dsfl[[1]] %in% subs[[i]]), ], subs[[i]])
      }
      croppi <- snowfall::sfClusterApplyLB(listi, get.ed, x)
    } else {
      subs <- list(uu)
      listi <- list()
      for(i in 1:length(subs)){
        listi[[i]] <- list(dsfl[which(dsfl[[1]] %in% subs[[i]]), ], subs[[i]])
      }
      croppi <- lapply(listi, get.ed, x)
    }

    ds_ed <- data.table::rbindlist(croppi)
    colnames(ds_ed)[2] <- paste0("ed", year)
    if(year == 0){
      ds_ed_years <- ds_ed
    } else {
      ds_ed_years <- cbind(ds_ed_years, ds_ed[, 2])
    }
  }
  if(ncores > 1) snowfall::sfStop()
  #parabar::stop_backend(backend)

  ds_ed_years$loss_frag <- apply(ds_ed_years[, 2:ncol(ds_ed_years)], 1, max)
  ds_ed_years <- ds_ed_years[, c("id", "loss_frag"), with = F]
  colnames(ds_ed_years)[1] <- "id_cell"

  # Classify cells
  brks <- getBreaks(ds_ed_years$loss_frag, classes_sub)
  ds_ed_years$loss_frag.c <- cut(ds_ed_years$loss_frag, brks, labels = classes_sub[[2]])
  ds_ed_years$loss_frag.c <- factor(ds_ed_years$loss_frag.c, levels = rev(classes_sub[[2]]))

  out <- new("FrontierMetric",
             metrics = "loss_frag",
             ud_metrics = "",
             time_frame = x@time_frame,
             data = as.data.table(ds_ed_years),
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
