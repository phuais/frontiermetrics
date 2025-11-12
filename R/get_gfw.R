#' Downloads and processes Global Forest Watch tiles
#'
#' Downloads all overlapping tiles from Global Forest Watch database 2024-v1.12 (Hansen et al. 2013), based on the
#' extent of a provided study area, and exports raster layers of tree cover and forest loss.
#'
#' @param study_area Either: (1) a 'SpatExtent' object defining the study area extent
#' in latitude/longitude coordinates; (2) a 'SpatVector' object with the polygon of the
#' study area; or (3) a file path to a spatial object.
#' @param dir Path to the directory where the processed raster layers will be saved. Default is current
#' directory.
#' @param mask Logical. If TRUE (default) downloaded tiles will be masked according to the
#' boundaries of the provided extent or polygon in `study_area`. Otherwise, the whole tiles will be kept.
#' @param overwrite Logical. If `TRUE`, files will be overwritten.
#' @param timeout Maximum time (in seconds) invested in downloading the tiles. Default is 600.
#'
#' @details
#' The downloaded tiles include: (1) a raster layer of tree cover for the year 2000,
#' and (2) raster layer of forest loss for subsequent years up to 2024.
#' Tiles of the same type (tree cover or forest loss) are merged and cropped to the
#' study area extent specified in `study_area`. If `mask = TRUE`, the tiles will be masked to
#' the provided extent or polygon.
#'
#' @return Exports cropped and/or masked raster layers from GFW databases, one for tree cover
#' and another for forest loss, based on the provided study area.
#'
#' @references
#' Hansen, M. C., Potapov, P. V., Moore, R., Hancher, M., Turubanova, S. A., Tyukavina, A.,
#' ... & Townshend, J. R. (2013). High-resolution global maps of 21st-century forest cover change. science, 342(6160), 850-853.
#' @export
#'
#' @examples
#' \dontrun{
#' # Creates working directory
#' dir.create("copo_tiles/")
#'
#' # Downloads study_area of study area
#' download.file(frontiermetrics_data[1], dir = "copo.gpkg")
#'
#' # Loads study_area of study area
#' copo <- terra::vect("copo.gpkg")
#'
#' # Downloads GFW tiles and exports cropped and masked raster layers
#' get_gfw(study_area = copo, dir = "copo_tiles/")
#' }
get_gfw <- function(study_area,
                    dir = "",
                    mask = TRUE,
                    overwrite = FALSE,
                    timeout = 600){

  # Argument's checking
  environment(check_get_gfw) <- environment()
  chk <- check_get_gfw()
  if(length(chk[[1]]) > 0)
    for(w in 1:length(chk[[1]])){
      warning(strwrap(chk[[1]], prefix = "\n", initial = ""), call. = FALSE)
    }
  if(length(chk[[2]]) > 0){
    errors <- chk[[2]]
    stop(strwrap(errors, prefix = "\n", initial = "\n"))
  } else {
    objs <- names(chk)
    for(i in 3:length(chk)){ assign(objs[i], chk[[i]]) }
  }

  if(file.exists("tree_cover.tif"))
    stop("A file named 'tree_cover.tif' already exists. To overwrite it, set overwrite = TRUE.")

  if(file.exists("loss_year.tif"))
    stop("A file named 'loss_year.tif' already exists. To overwrite it, set overwrite = TRUE.")


  prev_timeout <- options()$timeout
  options(timeout = max(timeout, getOption("timeout")))

  if(is(study_area, "character")) study_area <- terra::vect(study_area)
  if(is(study_area, "SpatExtent")) exte <- study_area
  # if(is(study_area, "SpatExtent")){
  #   exte <- study_area
  #   if(mask) message("mask = TRUE was ignored")
  #   mask <- FALSE
  # }

  if(is(study_area, "SpatVector")){
    study_area <- terra::project(study_area, y = terra::crs("EPSG:4326"))
    exte <- terra::ext(study_area)
  }

  # Longitude
  # Min
  min_val_x <- round(exte[1]/10)*10
  if(min_val_x > exte[1]) min_val_x <- min_val_x - 10

  # Max
  max_val_x <- round(exte[2]/10)*10
  if(max_val_x < exte[2]) max_val_x <- max_val_x + 10

  # Latitude
  # Min
  min_val_y <- round(exte[3]/10)*10
  if(min_val_y > exte[3]) min_val_y <- min_val_y - 10

  # Max
  max_val_y <- round(exte[4]/10)*10
  if(max_val_y < exte[4]) max_val_y <- max_val_y + 10

  dir.create(tmp <- tempfile())

  # Tree cover in year 2000
  treecover2000_links <- GFL_2024_v1.12_treecover2000[GFL_2024_v1.12_treecover2000$x_min >= min_val_x &
                                                        GFL_2024_v1.12_treecover2000$x_max <= max_val_x &
                                                        GFL_2024_v1.12_treecover2000$y_min >= min_val_y &
                                                        GFL_2024_v1.12_treecover2000$y_max <= max_val_y, ]

  # Links to download
  ntiles_label <- if(nrow(treecover2000_links) > 1) "tiles" else "tile"

  # Download tiles
  message(paste0("Downloading ", nrow(treecover2000_links), " ", ntiles_label," from Global Forest Watch (2024-v1.12): Tree cover in 2000."))
  for(i in 1:nrow(treecover2000_links)){
    message(paste0("Tile ", i, "/" , nrow(treecover2000_links)))
    filename1 <- substring(treecover2000_links[i, 1], first = 74, last = nchar(treecover2000_links[i, 1])-4)
    # download.file(treecover2000_links[i, 1], destfile = paste0(tmp, "/", filename1, ".tif"))
    curl::curl_download(url = treecover2000_links[i, 1],
      destfile = paste0(normalizePath(tmp, winslash = "/"), "/", filename1, ".tif"))
  }

  # Tree loss
  treeloss_links <- GFL_2024_v1.12_loss[GFL_2024_v1.12_loss$x_min >= min_val_x &
                                          GFL_2024_v1.12_loss$x_max <= max_val_x &
                                          GFL_2024_v1.12_loss$y_min >= min_val_y &
                                          GFL_2024_v1.12_loss$y_max <= max_val_y, ]

  # Download tiles
  message(paste0("\nDownloading ", nrow(treeloss_links), " ", ntiles_label," from Global Forest Watch (2024-v1.12): Forest cover loss."))
  for(i in 1:nrow(treeloss_links)){
    message(paste0("Tile ", i, "/" , nrow(treeloss_links)))
    filename2 <- substring(treeloss_links[i, 1], first = 74, last = nchar(treeloss_links[i, 1])-4)
    # download.file(treeloss_links[i, 1], destfile = paste0(tmp, "/", filename2, ".tif"))
    curl::curl_download(url = treeloss_links[i, 1],
                        destfile = paste0(normalizePath(tmp, winslash = "/"), "/", filename2, ".tif"))
  }

  # Mergear y cropear
  files <- list.files(file.path(tmp))
  tcover_rast <- terra::vrt(paste0(tmp, "/", files[grepl("treecover", files)]))
  tcover_rast <- terra::crop(tcover_rast, study_area)
  if(mask) tcover_rast <- terra::mask(tcover_rast, study_area)
  terra::writeRaster(tcover_rast, paste0(dir, "tree_cover.tif"), overwrite = overwrite)

  tloss_rast <- terra::vrt(paste0(tmp, "/", files[grepl("lossyear", files)]))
  tloss_rast <- terra::crop(tloss_rast, study_area)
  if(mask) tloss_rast <- terra::mask(tloss_rast, study_area)
  terra::writeRaster(tloss_rast, paste0(dir, "loss_year.tif"), overwrite = overwrite)

  # Eliminar carpeta temporal
  unlink(file.path(tmp), recursive = T)

  options(timeout = max(prev_timeout, getOption("timeout")))

  if(any(abs(terra::ext(tcover_rast)[3:4]) > 35)){
    warning(paste("The study area extends beyond the latitude limits of tropical and subtropical regions.",
                  "\nFrontier metrics were primeraly designed with these regions in mind, so the results may not be applicable",
                  "or meaningful outside of these areas."))
  }

  message("Raster layers were successfully processed and downloaded")
}
