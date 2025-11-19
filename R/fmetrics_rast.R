#' Exports raster layers of frontier metrics
#'
#' Based on an object of class 'FrontierMetric', exports raster layers of
#' frontier metrics, both the continuous values and the
#' discrete classes, as well as frontier archetypes.
#'
#' @param x An object of class 'FrontierMetric' generated with [fmetrics()].
#' @param metrics Frontier metrics to be plotted. Options include those calculated
#' frontier metrics (including user-defined metrics), "archetypes"
#' for frontier archetypes, or "all" for all frontier metrics and archetypes available within `x`.
#' Default is "all".
#' @param dir Path to the directory where raster layers will be exported. By default,
#' the current directory.
#' @param gdal GDAL driver specific datasource creation options. See ?terra::writeRaster for details.
#' @param overwrite Logical. If `TRUE`, files will be overwritten.
#' @param silent Logical. If `TRUE`, suppresses messages. Default is `FALSE`.
#'
#' @details
#' Raster layers are exported as .tif files named after each exported metric, including
#' continuous and discrete categories within each raster layer. Raster layer of archetypes is exported
#' as "archetypes.tif". In addition, a raster layer named "is_frontier.tif" is
#' also exported, which depict cells classified as frontiers (value = 1) and those that
#' were not (value = 0).
#'
#' This function is also used by [fmetrics()] to export raster layers of frontier metrics and
#' archetypes, if a path to a directory is defined when running this function.
#'
#' @return Invisible. Exports raster layers of metrics and archetypes as .tif files.
#' A text file named README.txt with details on the discrete classes, and another text file
#' named archetypes.txt with a table with definitions of the archetypes, are also created.
#' @export
#'
#' @examples
#' \dontrun{
#' # Export all available metrics as raster layers, including archetypes
#' fmetrics_rast(copo_metrics)
#'
#' # Export a a single metric as a raster layer
#' fmetrics_rast(copo_metrics, metrics = "left")
#'
#' # Export only archetypes
#' fmetrics_rast(copo_metrics, metrics = "archetypes")
#' }
fmetrics_rast <- function(x,
                          metrics = "all",
                          dir = "",
                          gdal = NULL,
                          overwrite = TRUE,
                          silent = FALSE){

  data_to_raster <- function(dd, mm_name = NULL){
    levs_vec <- NULL
    # if categorical data is being processed
    if(!is.numeric(dd[[3]])){
      ff <- factor(dd[[3]])
      dd[, 3] <- as.numeric(factor(dd[[3]]))
      text <- paste0("\n\n", mm_name, "\n\n",
                     paste(levels(ff), seq_along(levels(ff)), sep = " = ", collapse = "\n"))
      levs_vec <- seq_along(levels(ff))
      names(levs_vec) <- levels(ff)
    }

    list(terra::rast(dd, type = "xyz", crs = "EPSG:4326"), text, levs_vec)
  }

  if(dir != ""){
    dir <- correct_path(dir)
    if(!dir.exists(dir)){
      stop("Could not find the provided directory in 'dir'.")
    }
  }

  # Export mask layers of frontiers vs no frontiers and NA as outside study area
  masc <- x@data[, c("x_cell", "y_cell", "id_cell")]
  masc$is_frontier <- 1
  no_frontiers <- x@excluded_cells[, c("x_cell", "y_cell", "id_cell")]
  no_frontiers$is_frontier <- 0
  masc <- rbind(masc, no_frontiers)
  rr <- terra::rast(masc[, c(1, 2, 4)], type = "xyz", crs = "EPSG:4326")
  # Export
  terra::writeRaster(rr, file.path(dir, "is_frontier.tif"),
                     gdal = gdal, overwrite = overwrite)

  if("all" %in% metrics){
    metrics <- x@metrics
    if(length(x@ud_metrics) > 0){
      metrics <- c(metrics, x@ud_metrics)
    }
    metrics <- c(metrics, "archetypes")
  }

  text <- "In raster layer \"is_frontier.tif\", values equal to 1 represent frontiers under user classification, whereas values equal to 0, cells that were not classified as frontiers.\n\nCode number's meaning of frontier metric's classes"

  template <- x@excluded_cells[, c("x_cell", "y_cell")]
  template$foo <- -99

  if("baseline" %in% metrics & "baseline" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "baseline")])[[1]]
    rr_class <- data_to_raster(x@data[, c("x_cell", "y_cell", "baseline.c")], "- Baseline forest -")
    rr <- c(rr, rr_class[[1]])
    terra::writeRaster(rr, file.path(dir, "baseline_forest.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr_class[[2]])
  }

  if("loss" %in% metrics & "loss" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "loss")])[[1]]
    rr_class <- data_to_raster(x@data[, c("x_cell", "y_cell", "loss.c")], "- Forest loss -")
    rr <- c(rr, rr_class[[1]])
    terra::writeRaster(rr, file.path(dir, "forest_loss.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr_class[[2]])
  }

  if("loss_frag" %in% metrics & "loss_frag" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "loss_frag")])[[1]]
    rr_class <- data_to_raster(x@data[, c("x_cell", "y_cell", "loss_frag.c")], "- Forest loss fragmentation -")
    rr <- c(rr, rr_class[[1]])
    terra::writeRaster(rr, file.path(dir, "loss_frag.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr_class[[2]])
  }

  if("speed" %in% metrics & "speed" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "speed")])[[1]]
    rr_class <- data_to_raster(x@data[, c("x_cell", "y_cell", "speed.c")], "- Speed -")
    rr <- c(rr, rr_class[[1]])
    terra::writeRaster(rr, file.path(dir, "speed.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr_class[[2]])
  }

  if("baseline_frag" %in% metrics & "baseline_frag" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "baseline_frag")])[[1]]
    rr_class <- data_to_raster(x@data[, c("x_cell", "y_cell", "baseline_frag.c")], "- Fragmentation -")
    rr <- c(rr, rr_class[[1]])
    terra::writeRaster(rr, file.path(dir, "baseline_frag.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr_class[[2]])
  }

  if("activeness" %in% metrics & "activeness" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "activeness")], "- Activeness -")
    terra::writeRaster(rr[[1]], file.path(dir, "activeness.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr[[2]])
  }

  if("left" %in% metrics & "left" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "left")])[[1]]
    rr_class <- data_to_raster(x@data[, c("x_cell", "y_cell", "left.c")], "- Forest left -")
    rr <- c(rr, rr_class[[1]])
    terra::writeRaster(rr, file.path(dir, "forest_left.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr_class[[2]])
  }

  if("onset" %in% metrics & "onset" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "onset")], "- Onset -")
    terra::writeRaster(rr[[1]], file.path(dir, "onset.tif"),
                       gdal = gdal, overwrite = overwrite)
  }

  if(any(grepl("ud_", metrics))){
    ud_metrics <- metrics[grepl("ud_", metrics)]
    for(i in 1:length(ud_metrics)){
      rr <- data_to_raster(x@data[, c("x_cell", "y_cell", ud_metrics[i]), with = F], ud_metrics[i])
      terra::writeRaster(rr[[1]], file.path(dir, paste0(ud_metrics[i], ".tif")),
                         gdal = gdal, overwrite = overwrite)
    }
  }

  ee_archs <- F
  if("archetypes" %in% metrics){
    if(nrow(x@archetypes) > 0){
      dd <- x@data[, c("x_cell", "y_cell", "archetype")]
      dd$archetype <- as.numeric(dd$archetype)
      rr <- terra::rast(dd, type = "xyz", crs = "EPSG:4326")
      terra::writeRaster(rr, file.path(dir, "archetypes.tif"),
                         gdal = gdal, overwrite = overwrite)
      write.table(x@archetypes, file = file.path(dir, "archetypes.txt"), quote = FALSE,
                  sep ="\t", row.names = F, col.names = TRUE)
      ee_archs <- T
    } else {
      warning("No archetypes for export were found in 'x'.")
      ee_archs <- F
    }
  }

  fileConn <- file(file.path(dir, "README.txt"))
  writeLines(text, fileConn)
  close(fileConn)
  if(!silent){
    if(ee_archs){
      message("Raster layers were succesfully exported. Read through \"README.txt\" and \"archetypes.txt\" for details.")
    } else {
      message("Raster layers were succesfully exported. Read through \"README.txt\" for details.")
    }
  }
}
