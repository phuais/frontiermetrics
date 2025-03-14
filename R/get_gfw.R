#' Downloads and processes Global Forest Watch tiles
#'
#' Downloads all overlapping tiles from Global Forest Watch database (Hansen et al. 2013), based on the
#' extent of a user-provided polygon, and exports raster layers of tree cover and woodland loss.
#'
#' @param polygon An object of class 'SpatVector' or a path to a vector file, representing the polygon
#' of the study area.
#' @param mask Logical. If TRUE (default) downloaded tiles will be cropped and masked according to the
#' boundaries of the provided polygon. Otherwise, the whole tiles will be kept.
#' @param dir Path to the directory where the processed raster layers will be saved. Default is current
#' directory.
#' @param overwrite Logical. If `TRUE`, files will be overwritten.
#' @param timeout Maximum time (in seconds) invested in downloading the tiles. Default is 600.
#'
#' @details
#' Downloaded tiles are classified as follows: (1) raster layer depicting the tree cover
#' in the year 2000, and (2) raster layer depicting the woodland loss in the following years
#' until 2023. Tiles of the same type (tree cover or woodland loss)
#' are merged, then cropped and masked based on the on the polygon provided in
#' argument `polygon`.
#'
#' @return Exports cropped and masked raster layers from GFW databases, one for tree cover
#' and another for woodland loss, based on the polygon provided in argument `polygon`.
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
#' # Downloads polygon of study area
#' download.file(frontiermetrics_data[1], dir = "copo.gpkg")
#'
#' # Loads polygon of study area
#' copo <- terra::vect("copo.gpkg")
#'
#' # Downloads GFW tiles and exports cropped and masked raster layers
#' get_gfw(polygon = copo, dir = "copo_tiles/")
#' }
get_gfw <- function(polygon,
                    mask = TRUE,
                    dir = "",
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

  if(is(polygon, "character")) polygon <- terra::vect(polygon)

  polygon <- terra::project(polygon, y = terra::crs("+proj=longlat +datum=WGS84"))
  shp_ext <- terra::ext(polygon)

  # Longitude
  # Min
  min_val_x <- round(shp_ext[1]/10)*10
  if(min_val_x > shp_ext[1]) min_val_x <- min_val_x - 10

  # Max
  max_val_x <- round(shp_ext[2]/10)*10
  if(max_val_x < shp_ext[2]) max_val_x <- max_val_x + 10

  # Latitude
  # Min
  min_val_y <- round(shp_ext[3]/10)*10
  if(min_val_y > shp_ext[3]) min_val_y <- min_val_y - 10

  # Max
  max_val_y <- round(shp_ext[4]/10)*10
  if(max_val_y < shp_ext[4]) max_val_y <- max_val_y + 10

  dir.create(tmp <- tempfile())

  # Tree cover in year 2000
  treecover2000_links <- GFL_2023_v1.11_treecover2000[GFL_2023_v1.11_treecover2000$x_min >= min_val_x &
                                                        GFL_2023_v1.11_treecover2000$x_max <= max_val_x &
                                                        GFL_2023_v1.11_treecover2000$y_min >= min_val_y &
                                                        GFL_2023_v1.11_treecover2000$y_max <= max_val_y, ]

  # Links to download
  ntiles_label <- if(nrow(treecover2000_links) > 1) "tiles" else "tile"

  # Download tiles
  message(paste0("Downloading ", nrow(treecover2000_links), " ", ntiles_label," from Global Forest Watch (2023): Tree cover in 2000."))
  for(i in 1:nrow(treecover2000_links)){
    message(paste0("Tile ", i, "/" , nrow(treecover2000_links)))
    filename1 <- substring(treecover2000_links[i, 1], first = 74, last = nchar(treecover2000_links[i, 1])-4)
    download.file(treecover2000_links[i, 1], destfile = paste0(tmp, "/", filename1, ".tif"))
  }

  # Tree loss
  treeloss_links <- GFL_2023_v1.11_loss[GFL_2023_v1.11_loss$x_min >= min_val_x &
                                          GFL_2023_v1.11_loss$x_max <= max_val_x &
                                          GFL_2023_v1.11_loss$y_min >= min_val_y &
                                          GFL_2023_v1.11_loss$y_max <= max_val_y, ]

  # Download tiles
  message(paste0("\nDownloading ", nrow(treeloss_links), " ", ntiles_label," from Global Forest Watch (2023): Forest cover loss."))
  for(i in 1:nrow(treeloss_links)){
    message(paste0("Tile ", i, "/" , nrow(treeloss_links)))
    filename2 <- substring(treeloss_links[i, 1], first = 74, last = nchar(treeloss_links[i, 1])-4)
    download.file(treeloss_links[i, 1], destfile = paste0(tmp, "/", filename2, ".tif"))
  }

  # Mergear y cropear
  files <- list.files(file.path(tmp))
  tcover_rast <- terra::vrt(paste0(tmp, "/", files[grepl("treecover", files)]))
  if(mask){
    tcover_rast <- terra::crop(tcover_rast, polygon)
    tcover_rast <- terra::mask(tcover_rast, polygon)
  }
  terra::writeRaster(tcover_rast, paste0(dir, "tree_cover.tif"), overwrite = overwrite)

  tloss_rast <- terra::vrt(paste0(tmp, "/", files[grepl("lossyear", files)]))
  if(mask){
    tloss_rast <- terra::crop(tloss_rast, polygon)
    tloss_rast <- terra::mask(tloss_rast, polygon)
  }
  terra::writeRaster(tloss_rast, paste0(dir, "loss_year.tif"), overwrite = overwrite)

  # Eliminar carpeta temporal
  unlink(file.path(tmp), recursive = T)

  options(timeout = max(prev_timeout, getOption("timeout")))

  if(any(abs(terra::ext(tcover_rast)[3:4]) > 35)){
    warning(paste("The study area extends beyond the latitude limits of tropical and subtropical regions.",
                  "\nFrontier metrics were designed with these regions in mind, so the results may not be applicable",
                  "or meaningful outside of these areas."))
  }

  message("Raster layers were successfully processed and downloaded")
}
