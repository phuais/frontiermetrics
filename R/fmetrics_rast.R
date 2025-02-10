# data_to_raster <- function(dd){
#   # # Join with coordinates and variables cleaning
#   # fm_coords <- dplyr::inner_join(fm_ds, coords, by = "id_cell")
#   # sp::coordinates(fm_coords) <- fm_coords[, c("x_cell", "y_cell")]
#   #
#   # fm_coords <- fm_coords[, 2]
#   # # Coercing to SpatialPixelDataFrame
#   # sp::gridded(fm_coords) <- TRUE
#   #
#   # # Coercing to raster
#   # raster <- terra::rast(fm_coords)
#   # terra::crs(raster) <- "+proj=longlat +datum=WGS84"
# #
# #   dd <- merge(fm_ds, coords, by = "id_cell")
# #   cols <- c("x_cell", "y_cell", value)
# #   dd <- dd[, ..cols]
#   raster <- terra::rast(dd, type = "xyz", crs = "EPSG:4326")
#
#   return(raster)
# }

#' Exports raster layers of frontier metrics
#'
#' Based on an object of class 'FrontierMetric', exports raster layers of
#' frontier metrics. Exported raster layers can depict the absolute values, the
#' defined classes, or both. Raster layers are exported as .tif files, with
#' the following names: "baseline_woodland.tif", "woodland_loss.tif,
#' "speed.tif", "fragmentation.tif", "activeness.tif", "woodland_left.tif".
#'
#' @param x An object of class 'FrontierMetric' generated with [fmetrics()]
#' @param metrics Frontier metrics to be exported as raster layers. Options are: "baseline", "loss",
#' "fragmentation", "speed", "activeness" and/or "left" for individual frontier
#' metrics; "severity", "spatio_temporal" and/or "development" for frontier typologies;
#' "all" to calculate all frontier metrics available within `x`. Default is "all".
#' @param dir Path to the directory where to export raster layers.
#' @param gdal GDAL driver specific datasource creation options. See the GDAL documentation.
#' @param overwrite logical. If `TRUE`, files will be overwritten.
#'
#' @return Invisible, it exports raster layers as .tif files.
#' @export
#'
#' @examples
#' \dontrun{
#' fmetrics_rast(copo_metrics)
#' }
fmetrics_rast <- function(x, metrics = "all", dir = NULL, gdal = "TFW=YES", overwrite = TRUE){

  data_to_raster <- function(dd, mm_name = NULL){
    if(!is.numeric(dd[[3]])){
      ff <- factor(dd[[3]])
      dd[, 3] <- as.numeric(factor(dd[[3]]))
      text <- mm_name
      text <- paste0(text, "\n\n",
                     paste(levels(ff), seq_along(levels(ff)), sep = " = ", collapse = "\n"))
    }

    list(terra::rast(dd, type = "xyz", crs = "EPSG:4326"), text)
  }

  if(!is.null(dir)){
    dir <- paste0(dir, "/")
  }

  text <- "Code number's meaning of frontier metric's classes\n\n"

  if("baseline" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "Wct0")])[[1]]
    rr_class <- data_to_raster(x@data[, c("x_cell", "y_cell", "Wct0_class")], "- Baseline -")
    rr <- c(rr, rr_class[[1]])
    terra::writeRaster(rr, paste0(dir, "baseline_woodland.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr_class[[2]], "\n\n")
  }

  if("loss" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "PTWl")])[[1]]
    rr_class <- data_to_raster(x@data[, c("x_cell", "y_cell", "PTWl_class")], "- Woodland loss -")
    rr <- c(rr, rr_class[[1]])
    terra::writeRaster(rr, paste0(dir, "woodland_loss.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr_class[[2]], "\n\n")
  }

  if("speed" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "Sp")])[[1]]
    rr_class <- data_to_raster(x@data[, c("x_cell", "y_cell", "Sp_class")], "- Speed -")
    rr <- c(rr, rr_class[[1]])
    terra::writeRaster(rr, paste0(dir, "speed.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr_class[[2]], "\n\n")
  }

  if("fragmentation" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "ED")])[[1]]
    rr_class <- data_to_raster(x@data[, c("x_cell", "y_cell", "ED_class")], "- Fragmentation -")
    rr <- c(rr, rr_class[[1]])
    terra::writeRaster(rr, paste0(dir, "fragmentation.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr_class[[2]], "\n\n")
  }

  if("activeness" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "Activeness")], "- Activeness -")
    terra::writeRaster(rr[[1]], paste0(dir, "activeness.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr[[2]], "\n\n")
  }

  if("left" %in% x@metrics){
    rr <- data_to_raster(x@data[, c("x_cell", "y_cell", "Wleft")])[[1]]
    rr_class <- data_to_raster(x@data[, c("x_cell", "y_cell", "Wleft_class")], "- Woodland left -")
    rr <- c(rr, rr_class[[1]])
    terra::writeRaster(rr, paste0(dir, "woodland_left.tif"),
                       gdal = gdal, overwrite = overwrite)
    text <- paste0(text, rr_class[[2]], "\n\n")
  }

  fileConn <- file(paste0(dir, "README.txt"))
  writeLines(text, fileConn)
  close(fileConn)

  message("Raster layers were succesfully exported")
  invisible()
}
