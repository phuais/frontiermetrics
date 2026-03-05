calc_fm_loss_frag <- function(x, classes_sub, ncores, silent){

  if(ncores > 1){
    if(!requireNamespace("future", quietly = TRUE) ||
        !requireNamespace("future.apply", quietly = TRUE)) {
      stop("Packages 'future' and 'future.apply' must be installed for parallel processing.")
    }
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = ncores)
  }

  ds_ed_years <- data.table()
  years_lab <- x@time_frame[1]:x@time_frame[2]
  if(ncores > 1){
    if(!silent) cat(paste0("  Forest loss fragmentation, running in parallel (", ncores, " workers) - 0/", length(years_lab)-1))
  } else {
    cat(paste0("  Forest loss fragmentation - 0/", length(years_lab)-1))
  }

  dsfl <- x@data[, c("id_cell", "x_1", "y_1"), with = F]
  uu <- unique(dsfl[[1]])
  for(year in 0:(x@time_frame[2]-x@time_frame[1]-1)){
    dsfl$Flosssum <- rowSums(x@data[, 8:(8 + year)])
    ds_ed <- .get_ed_lf(list(dsfl, uu), grain = x@grain, ncores, silent)
    colnames(ds_ed)[2] <- paste0("ed", year)
    if(year == 0){
      ds_ed_years <- ds_ed
    } else {
      ds_ed_years <- cbind(ds_ed_years, ds_ed[, 2])
    }
    if(ncores > 1){
      if(!silent) cat("\r  Forest loss fragmentation, running in parallel (", ncores, " workers) - ", year+1, "/", length(years_lab)-1, sep = "")
    } else {
      if(!silent) cat("\r  Forest loss fragmentation - ", year+1, "/", length(years_lab)-1, sep = "")
    }
  }

  ds_ed_years$loss_frag <- apply(ds_ed_years[, 2:ncol(ds_ed_years)], 1, max)
  ds_ed_years <- ds_ed_years[, c("id_cell", "loss_frag"), with = F]

  # Classify cells
  brks <- getBreaks(ds_ed_years$loss_frag, classes_sub)
  ds_ed_years$loss_frag.c <- cut(ds_ed_years$loss_frag, brks, labels = classes_sub[[2]])
  ds_ed_years$loss_frag.c <- factor(ds_ed_years$loss_frag.c, levels = rev(classes_sub[[2]]))

  out <- list(metrics = "loss_frag",
              data = as.data.table(ds_ed_years))

  return(out)
}

.get_ed_lf <- function(data_input, grain, ncores, silent) {

  # cheap_distance <- function(lon1, lat1, lon2, lat2) {
  #   # MERIDIAN <- 20003930.0
  #   # EQUATOR  <- 40007862.917
  #   lat_range_rad <- c(min(lat1, lat2), max(lat1, lat2)) * pi / 180
  #   cosy <- cos(mean(lat_range_rad))
  #   dy <- 20003930.0 * (lat1 - lat2) / 180.0
  #   dx <- 40007862.917  * (lon1 - lon2) * cosy / 360.0
  #   sqrt(dx^2 + dy^2)
  # }

  process_cell <- function(cell_id, dt, grain) {
    spgdssample <- dt[which(dt$id_cell == cell_id), ]
    spgdssample <- spgdssample[order(spgdssample$y_1, decreasing = T), ]
    spgdssample <- spgdssample[order(spgdssample$x_1), ]
    coordi <- spgdssample[, 2:3]
    coordi2 <- c(min(coordi$x_1) - grain[[1]][1]/2, max(coordi$x_1) + grain[[1]][1]/2,
                 min(coordi$y_1) - grain[[1]][2]/2, max(coordi$y_1) + grain[[1]][2]/2)

    mean_lat <- mean(coordi2[3:4])
    mean_lon <- mean(coordi2[1:2])

    # This step is computationally intensive, could improve
    x_width <- geodist::geodist(cbind(x = coordi2[1], y = mean_lat),
                                cbind(x = coordi2[2], y = mean_lat),
                                measure = "cheap")
    y_width <- geodist::geodist(cbind(x = mean_lon, y = coordi2[3]),
                                cbind(x = mean_lon, y = coordi2[4]),
                                measure = "cheap")

    # x_width <- cheap_distance(coordi2[1], mean_lat, coordi2[2], mean_lat)
    # y_width <- cheap_distance(mean_lon, coordi2[3], mean_lon, coordi2[4])

    side_cells <- sqrt(nrow(coordi))
    spgdssample$Flosssum2 <- ifelse(spgdssample$Flosssum == 0, 0, 1)
    list(
      res = c(x_width / side_cells, y_width / side_cells),
      mat = matrix(spgdssample$Flosssum2, ncol = side_cells, byrow = F)
    )
  }

  if(ncores > 1){
    ras <- future.apply::future_lapply(
      data_input[[2]],
      process_cell,
      dt = data_input[[1]],
      grain = grain,
      future.seed = TRUE
    )
  } else {
    ras <- lapply(data_input[[2]], process_cell,
                  dt = data_input[[1]],
                  grain = grain)
  }

  ed <- vapply(ras, function(cell) edge_density(list(cell$res, cell$mat)), numeric(1))
  data.table::data.table(id_cell = data_input[[2]], ed = ed)
}
