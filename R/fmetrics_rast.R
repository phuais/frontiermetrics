#' Exports raster layers of frontier metrics
#'
#' Based on an object of class 'FrontierMetric', exports raster layers of
#' frontier metrics, both the absolute values and the
#' discrete classes, as well as frontier archetypes.
#'
#' @param x An object of class 'FrontierMetric' generated with [fmetrics()]
#' @param metrics Frontier metrics to be exported as raster layers. Options are: "baseline", "loss",
#' "fragmentation", "speed", "activeness", and/or "left" for individual frontier
#' metrics; "severity", "spatio_temporal", and/or "development" for frontier typologies; "archetypes"
#' for frontier archetypes; "all" for all frontier metrics and archetypes available within `x`.
#' Default is "all".
#' @param dir Path to the directory where to export raster layers.
#' @param gdal GDAL driver specific datasource creation options. See the GDAL documentation.
#' @param overwrite Logical. If `TRUE`, files will be overwritten.
#'
#' @details
#' Raster layers are exported as .tif files, with the following names:
#' "baseline_forest.tif", "forest_loss.tif,"speed.tif", "fragmentation.tif",
#' "activeness.tif", and "forest_left.tif". Raster layer of archetypes is exported
#' as "archetypes.tif". In addition, a raster layer named "is_frontier.tif" is
#' also exported, which shows cells that are frontiers (value = 1) and those that
#' are not (value = 0).
#'
#' @return Invisible. Exports raster layers as .tif files. Absolute values, discrete classes
#' of each metric, and frontier archetypes are exported as separate layers within each .tif file.
#' A text file named README.txt with details on the discrete classes, and another text file
#' named ARCHETYPES.txt with a table with definitions of the archetypes, are also created.
#' @export
#'
#' @examples
#' \dontrun{
#' fmetrics_rast(copo_metrics)
#' }
fmetrics_rast <- function(x,
                          metrics = "all",
                          dir = NULL,
                          gdal = NULL,
                          overwrite = TRUE){

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

  # app_raster <- function(rast_out, ra_app){
  #   if(is.null(rast_out)){
  #     rast_out <- ra_app
  #   } else {
  #     rast_out <- c(rast_out, ra_app)
  #   }
  #   return(rast_out)
  # }

  # if(!is.null(dir)) dir <- paste0(dir, "/")

  # rast_out <- NULL
  # rast_info <- list()

  if(!is.null(dir)){

  }

  # Export mask layers of frontiers vs no frontiers and NA as outside study area
  masc <- x@data[, c("x_cell", "y_cell", "id_cell")]
  masc$is_frontier <- 1
  no_frontiers <- x@excluded_cells[, c("x_cell", "y_cell", "id_cell")]
  no_frontiers$is_frontier <- 0
  masc <- rbind(masc, no_frontiers)
  rr <- terra::rast(masc[, c(1, 2, 4)], type = "xyz", crs = "EPSG:4326")
  # rast_out <- app_raster(rast_out, rr)
  # rast_info[[length(rast_info)+1]] <- c("not frontier" = 0, "frontier" = 1)
  # names(rast_info)[[length(rast_info)]] <- "is_frontier"
  # Export
  terra::writeRaster(rr, file.path(dir, "is_frontier.tif"),
                     gdal = gdal, overwrite = overwrite)

  if("archetypes" %in% metrics || "all" %in% metrics){
    dd <- x@data[, c("x_cell", "y_cell", "archetype")]
    dd$archetype <- as.numeric(dd$archetype)
    rr <- terra::rast(dd, type = "xyz", crs = "EPSG:4326")
    # rast_out <- app_raster(rast_out, rr)
    # rast_info[[length(rast_info)+1]] <- x@archetypes
    # names(rast_info)[[length(rast_info)]] <- "archetypes"
    terra::writeRaster(rr, file.path(dir, "archetypes.tif"),
                       gdal = gdal, overwrite = overwrite)
    write.table(x@archetypes, file = file.path(dir, "archetypes.txt"), quote = FALSE,
                sep ="\t", row.names = F, col.names = TRUE)
    if(metrics == "archetypes"){
      cat("Raster layer of archetypes was succesfully exported. Read through \"archetypes.txt\" to see the definitions of each archetype.")
      return(invisible(NULL))
    }
  }

  if("all" %in% metrics){
    metrics <- x@metrics
    if(length(x@ud_metrics) > 0){
      metrics <- c(metrics, x@ud_metrics)
    }
  }

  text <- "In raster layer \"is_frontier.tif\", values equal to 1 represent frontiers under user classification, whereas values equal to 0, cells that were not classified as frontiers.\n\nCode number's meaning of frontier metric's classes"

  template <- x@excluded_cells[, c("x_cell", "y_cell")]
  template$foo <- -99

  if("baseline" %in% metrics & "baseline" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "baseline")])[[1]]
    rr_class <- data_to_raster(x@data[, c("x_cell", "y_cell", "baseline.c")], "- Baseline forest -")
    rr <- c(rr, rr_class[[1]])
    # rast_out <- app_raster(rast_out, rr)
    # rast_info[[length(rast_info)+1]] <- rr_class[[3]]
    # names(rast_info)[[length(rast_info)]] <- "baseline"
    terra::writeRaster(rr, file.path(dir, "baseline_forest.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr_class[[2]])
  }

  if("loss" %in% metrics & "loss" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "loss")])[[1]]
    rr_class <- data_to_raster(x@data[, c("x_cell", "y_cell", "loss.c")], "- Forest loss -")
    rr <- c(rr, rr_class[[1]])
    # rast_out <- app_raster(rast_out, rr)
    # rast_info[[length(rast_info)+1]] <- rr_class[[3]]
    # names(rast_info)[[length(rast_info)]] <- "loss"
    terra::writeRaster(rr, file.path(dir, "forest_loss.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr_class[[2]])
  }

  if("loss_frag" %in% metrics & "loss_frag" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "loss_frag")])[[1]]
    rr_class <- data_to_raster(x@data[, c("x_cell", "y_cell", "loss_frag.c")], "- Forest loss fragmentation -")
    rr <- c(rr, rr_class[[1]])
    # rast_out <- app_raster(rast_out, rr)
    # rast_info[[length(rast_info)+1]] <- rr_class[[3]]
    # names(rast_info)[[length(rast_info)]] <- "loss_frag"
    terra::writeRaster(rr, file.path(dir, "loss_frag.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr_class[[2]])
  }

  if("speed" %in% metrics & "speed" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "speed")])[[1]]
    rr_class <- data_to_raster(x@data[, c("x_cell", "y_cell", "speed.c")], "- Speed -")
    rr <- c(rr, rr_class[[1]])
    # rast_out <- app_raster(rast_out, rr)
    # rast_info[[length(rast_info)+1]] <- rr_class[[3]]
    # names(rast_info)[[length(rast_info)]] <- "speed"
    terra::writeRaster(rr, file.path(dir, "speed.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr_class[[2]])
  }

  if("baseline_frag" %in% metrics & "baseline_frag" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "baseline_frag")])[[1]]
    rr_class <- data_to_raster(x@data[, c("x_cell", "y_cell", "baseline_frag.c")], "- Fragmentation -")
    rr <- c(rr, rr_class[[1]])
    # rast_out <- app_raster(rast_out, rr)
    # rast_info[[length(rast_info)+1]] <- rr_class[[3]]
    # names(rast_info)[[length(rast_info)]] <- "baseline_frag"
    terra::writeRaster(rr, file.path(dir, "baseline_frag.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr_class[[2]])
  }

  if("activeness" %in% metrics & "activeness" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "activeness")], "- Activeness -")
    # rast_out <- app_raster(rast_out, rr[[1]])
    # rast_info[[length(rast_info)+1]] <- rr[[3]]
    # names(rast_info)[[length(rast_info)]] <- "activeness"
    terra::writeRaster(rr[[1]], file.path(dir, "activeness.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr[[2]])
  }

  if("left" %in% metrics & "left" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "left")])[[1]]
    rr_class <- data_to_raster(x@data[, c("x_cell", "y_cell", "left.c")], "- Forest left -")
    rr <- c(rr, rr_class[[1]])
    # rast_out <- app_raster(rast_out, rr)
    # rast_info[[length(rast_info)+1]] <- rr_class[[3]]
    # names(rast_info)[[length(rast_info)]] <- "left"
    terra::writeRaster(rr, file.path(dir, "forest_left.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr_class[[2]])
  }

  if("onset" %in% metrics & "onset" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "onset")], "- Onset -")
    # rast_out <- app_raster(rast_out, rr[[1]])
    terra::writeRaster(rr[[1]], file.path(dir, "onset.tif"),
                       gdal = gdal, overwrite = overwrite)
  }

  if(any(grepl("ud_", metrics))){
    ud_metrics <- metrics[grepl("ud_", metrics)]
    for(i in 1:length(ud_metrics)){
      rr <- data_to_raster(x@data[, c("x_cell", "y_cell", ud_metrics[i]), with = F], ud_metrics[i])
      # rast_out <- app_raster(rast_out, rr[[1]])
      terra::writeRaster(rr[[1]], file.path(dir, paste0(ud_metrics[i], ".tif")),
                         gdal = gdal, overwrite = overwrite)
    }
  }

  fileConn <- file(file.path(dir, "README.txt"))
  writeLines(text, fileConn)
  close(fileConn)
  cat("Raster layers were succesfully exported. Read through \"README.txt\" and/or \"archetypes.txt\" for details.")

#   out <- new("FrontierMetric_raster",
#              metrics = x@metrics,
#              ud_metrics = x@ud_metrics,
#              time_frame = x@time_frame,
#              data = x@data,
#              extent = x@extent,
#              grain = x@grain,
#              aggregation = x@aggregation,
#              min_treecover = x@min_treecover,
#              min_cover = x@min_cover,
#              min_rate = x@min_rate,
#              window = x@window,
#              archetypes = x@archetypes,
#              excluded_cells = x@excluded_cells,
#              raster_layers = rast_out,
#              info = rast_info,
#              time_frame = x@time_frame)
#
#   invisible(out)
}
