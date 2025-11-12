ud_metric <- function(x, fm_ds, name){
  out <- new("FrontierMetric",
             metrics = "",
             ud_metrics = name,
             time_frame = x@time_frame,
             data = as.data.table(fm_ds),
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

ud_rasterize <- function(x, fm_ds, cell){
  #fm_ds <- x@data

  spgdssample <- fm_ds[fm_ds$id_cell == cell, ]
  spgdssample <- spgdssample[order(spgdssample$y_1, decreasing = T), ]
  spgdssample <- spgdssample[order(spgdssample$x_1), ]
  coordi <- spgdssample[, c("x_1", "y_1")]
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
  rast_cell <- terra::rast(matrix(spgdssample$FC_2000, ncol = side_cells, byrow = F), crs = "EPSG:4326", extent = coordi2)

  return(rast_cell)
}
