get_FL <- function(i, gfw_loss, cell_size, aggregation, years, ncores){
  cat("\r> Processing year: ", i , " / ", length(years), sep = "")
  gwf_loss_tmp <- gfw_loss == i
  gwf_loss_tmp <- gwf_loss_tmp * cell_size
  if(aggregation[1] > 1){
    gwf_loss_tmp_agg <- terra::aggregate(gwf_loss_tmp,
                                         fact = aggregation[1],
                                         fun = sum,
                                         cores = ncores,
                                         na.rm = T)
  } else {
    gwf_loss_tmp_agg <- gwf_loss_tmp
  }
  gwf_loss_tmp_agg <- gwf_loss_tmp_agg[1:(dim(gwf_loss_tmp_agg)[1] - dim(gwf_loss_tmp_agg)[1] %% aggregation[2]),
                                       1:(dim(gwf_loss_tmp_agg)[2] - dim(gwf_loss_tmp_agg)[2] %% aggregation[2]), drop = F]
  dat_tmp <- terra::as.data.frame(gwf_loss_tmp_agg, xy = T, cells = T, na.rm = F)
  dat_tmp <- dat_tmp[, c(1, 4)]
  colnames(dat_tmp) <- c("id_1", paste0("FL_", years[i]))
  return(dat_tmp)
}

#' Generates primary dataset to develop frontier metrics
#'
#' Based on inputs of tree cover and tree loss from Global Forest Watch database, generates
#' a structured dataset that can be used as main input to calculate frontier
#' metrics with function [fmetrics()].
#'
#' @param gfw_cover An object of class 'SpatRaster' or 'RasterLayer', or a path
#' (string) to a raster layer of tree cover from the GFW database.
#' @param gfw_loss An object of class 'SpatRaster' or 'RasterLayer', or a path
#' (string) to a raster layer of tree cover from the GFW database.
#' @param min_treecover A number (percentage) depicting the minimum percentage of tree
#' cover to be considered as "woodland". Default is 10%. See Details.
#' @param aggregation A numeric vector of length two, depicting the first and second
#' magnitude of aggregation. See Details.
#' @param year_range A numeric vector of two elements depicting the first and last year of the analyzed time-frame.
#' Default is c(2000, 2023).
#' @param min_cover A number (percentage) depicting the minimum percentage of woodland
#' cover for a cell to be considered a frontier. Default is 5%. See Details.
#' @param min_rate A number (percentage) depicting the minimum average annual woodland loss rate
#' in a five-year period for a cell to be considered a frontier. Default is 0.5%. See Details.
#' @param window A whole number depicting the number of years to consider in the calculation of activeness.
#' Default is 5. See Details.
#' @param ncores Numbers of cores to parallelize processes.
#'
#' @details
#' Global Forest Watch databases (Hansen et al. 2013) provide the percentage of tree cover in each
#' cell. One must define a minimum percentage of tree cover to a cell be considered
#' as "woodland". Usually, 10% is used for subtropical dry forests, and 5% for
#' tropical forests.
#'
#' Aggregation is done in two steps. First, cells are aggregated using the first
#' value of the aggregation argument, which represents the number of cells to merge
#' in each direction (horizontal and vertical). In this step, the values of woodland cover of all
#' included cells are summed, and the amount of woodland loss is calculated for each new aggregated cell.
#' Second, the newly aggregated cells are further
#' grouped using the second value of the aggregation argument. At this stage,
#' each cell from the initial aggregation is merged into larger cells,
#' which will serve as the basis for calculating frontier metrics. Since GFW raster layers have a resolution of approximately 30m (at the equator),
#' setting `aggregation = c(10, 10)` (default setting) will: (1) aggregate 10 cells both horizontally
#' and vertically, producing a raster layer with a resolution of
#' ~300m, and (2) further aggregate these newly aggregated cells by a factor of
#' 10, creating larger cells with a resolution of ~3000m. For instance, setting
#' `aggregation = c(5, 10)` will first agregate to ~150m cells (30m * 5), then
#' aggregate these new cells into ~1500m cells (150m * 10).
#'
#' Frontiers are defined as cells that meet two criteria: (1) they have a minimum
#' percentage of woodland cover (min_cover) and (2) they have a minimum average
#' annual woodland loss rate in a given temporal window (min_rate). By default, these
#' values take 5% and 0.5%, respectively (Buchadas et al. 2022). Those cells
#' that do not meet these criteria are excluded from the analysis. Also by default,
#' the temporal window is set to five years. However, this window could be set smaller
#' (e.g. 2 or 3 years) or bigger (e.g. 7 or 9 years).
#'
#' @return
#' An object of class 'GFW_dataset', containing the primary dataset to
#' calculate frontier metrics with [fmetrics()].
#'
#' @references
#' Buchadas, A., Baumann, M., Meyfroidt, P., & Kuemmerle, T. (2022). Uncovering major types of deforestation frontiers across the world's tropical dry woodlands. #' Nature Sustainability, 5(7), 619-627.
#'
#' Hansen, M. C., Potapov, P. V., Moore, R., Hancher, M., Turubanova, S. A., Tyukavina, A.,
#' ... & Townshend, J. R. (2013). High-resolution global maps of 21st-century forest cover change. science, 342(6160), 850-853.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Loads raster layers generated with function get_gfw()
#' gfw_cover <- terra::rast(system.file("extdata", "tree_cover.tif", package = "frontiermetrics"))
#' gfw_loss <- terra::rast(system.file("extdata", "loss_year.tif", package = "frontiermetrics"))
#'
#' # Generates primary dataset
#' copo_dataset <- init_fmetrics(gfw_cover, gfw_loss, year_range = c(2000, 2023), ncores = 2)
#' }
init_fmetrics <- function(gfw_cover,
                          gfw_loss,
                          min_treecover = 10,
                          aggregation = c(10, 10),
                          year_range = c(2000, 2023),
                          min_cover = 5,
                          min_rate = 0.5,
                          window = 5,
                          ncores = 1) {
  # Argument's checking
  environment(check_init_fmetrics) <- environment()
  chk <- check_init_fmetrics()
  if (length(chk[[1]]) > 0){
    for (w in 1:length(chk[[1]])) {
      warning(strwrap(chk[[1]], prefix = "\n", initial = ""), call. = FALSE)
    }
  }
  if (length(chk[[2]]) > 0) {
    errors <- chk[[2]]
    stop(strwrap(errors, prefix = "\n", initial = "\n"))
  } else {
    if(length(chk) > 2){
      objs <- names(chk)
      for (i in 3:length(chk)) {
        assign(objs[i], chk[[i]])
      }
    }
  }

  terra_progress_bar <- terra::terraOptions(print = FALSE)$progress
  terra::terraOptions(progress = 0, print = FALSE)

  # Load raster layers if paths are provided
  if (is.character(gfw_cover)) {
    gfw_cover <- terra::rast(gfw_cover)
  }
  if (is.character(gfw_loss)) {
    gfw_loss <- terra::rast(gfw_loss)
  }

  # Transform to epsg:4326 if necessary
  gfw_cover <- crs_transform(gfw_cover)
  gfw_loss <- crs_transform(gfw_loss)

  if(any(abs(terra::ext(gfw_cover)[3:4]) > 35)){
    warning(paste("The study area extends beyond the latitude limits of tropical and subtropical regions (35\u00b0).",
                  "\nFrontier metrics were designed with these regions in mind, so the results may not be applicable",
                  "or meaningful outside of the mentioned ranges."))
  }

  gfw_loss <- gfw_loss[gfw_cover >= min_treecover, drop = F]
  gfw_cover <- gfw_cover >= min_treecover
  cell_size <- terra::cellSize(gfw_cover, unit = "km")
  gfw_cover <- gfw_cover * cell_size
  if(aggregation[1] > 1){
    gfw_cover_1 <- terra::aggregate(
      gfw_cover,
      fact = aggregation[1],
      fun = sum,
      cores = ncores,
      na.rm = T)
  } else {
    gfw_cover_1 <- gfw_cover
  }
  # Shrink rasters to adapt to aggregation
  gfw_cover_1 <- gfw_cover_1[1:(dim(gfw_cover_1)[1] - dim(gfw_cover_1)[1] %% aggregation[2]),
                             1:(dim(gfw_cover_1)[2] - dim(gfw_cover_1)[2] %% aggregation[2]), drop = F]
  dat <- as.data.frame(gfw_cover_1,
                       xy = T,
                       cells = T,
                       na.rm = F)
  dat <- as.data.table(dat)

  x_sides <- dim(gfw_cover_1)[1] / aggregation[2]
  y_sides <- dim(gfw_cover_1)[2] / aggregation[2]
  vec <- vector("numeric")
  init <- 1
  end <- y_sides
  for (i in 1:x_sides) {
    vec <- c(vec, rep(rep(init:end, each = aggregation[2]), aggregation[2]))
    init <- init + y_sides
    end <- end + y_sides
  }
  dat$id_cell <- vec
  colnames(dat) <- c("id_1", "x_1", "y_1", paste0("FC_", year_range[1]), "id_cell")

  # X central coordinates
  mid_x <- rep(NA, y_sides)
  for (i in 1:y_sides) {
    tmp <- dat[dat$id_cell == i, ]
    ran <- range(tmp$x_1)
    mid_x[i] <- ran[1] + (ran[2] - ran[1]) / 2
  }
  # Y central coordinates
  mid_y <- rep(NA, x_sides)
  cellid <- 1
  for (i in 1:x_sides) {
    tmp <- dat[dat$id_cell == cellid, ]
    ran <- range(tmp$y_1)
    mid_y[i] <- ran[1] + (ran[2] - ran[1]) / 2
    cellid <- cellid + y_sides
  }

  ids_coords <- data.table(
    id_cell = unique(dat$id_cell),
    x_cell = rep(mid_x, x_sides),
    y_cell = rep(mid_y, each = y_sides)
  )

  dat <- merge(dat, ids_coords)
  dat <- dat[, c("id_cell",
                 "x_cell",
                 "y_cell",
                 "id_1",
                 "x_1",
                 "y_1",
                 paste0("FC_", year_range[1])), with = F]

  years <- (year_range[1] + 1):year_range[2]

  results <- lapply(1:length(years),
                    get_FL,
                    gfw_loss,
                    cell_size,
                    aggregation,
                    years,
                    ncores)

  # Merge into a dataframe
  for(i in 1:length(results)){
    dat <- merge(dat, results[[i]], by = "id_1")
  }

  # Remove cells with NAs in initial Forest Cover
  cols <- paste0("FC_", year_range[1])
  dat <- dat[!dat$id_cell %in% unique(dat[which(is.na(dat[, paste0("FC_", year_range[1]), with = F]))]$id_cell), ]

  # Rearrange columns
  dat <- dat[, c("id_cell",
                 "x_cell",
                 "y_cell",
                 "id_1",
                 "x_1",
                 "y_1",
                 colnames(dat)[7:ncol(dat)]), with = F]

  coords <- unique(dat[, c("id_cell", "x_cell", "y_cell")])
  # Get individual cell area
  rr <- terra::rast(dat[, c("x_cell", "y_cell", "id_cell")], type = "xyz", crs = "EPSG:4326")
  rr <- c(rr, terra::cellSize(rr, unit = "km"))
  rr <- as.data.table(terra::as.data.frame(rr))
  rr <- na.omit(rr, "id_cell")
  colnames(rr) <- c("id_cell", "cell_area")
  coords <- merge(coords, rr, by = "id_cell")

  out <- new(
    "GFW_dataset",
    data = dat,
    coords = coords,
    year_range = year_range,
    initial_fc_col = paste0("FC_", year_range[1]),
    fl_cols = colnames(dat)[8:ncol(dat)],
    extent = as.vector(terra::ext(gfw_cover_1)),
    grain = list(
      terra::res(gfw_cover_1),
      terra::res(gfw_cover_1) * aggregation[2]
    ),
    aggregation = aggregation,
    min_treecover = min_treecover,
    min_cover = min_cover,
    min_rate = min_rate,
    window = window,
    excluded_cells = data.table()
  )

  # Filter by frontier activeness and baseline
  foo_classes <- init_classes(x = out)
  foo_classes@baseline[[1]] <- c(-Inf, 5, Inf)
  foo_classes@baseline[[2]] <- c("Excluded", "Included")
  foo_classes@activeness$old <- year_range[2] - (window + 1)
  foo_classes@activeness$emerging <- year_range[2] - window
  foo_metrics <- fmetrics(out, c("baseline", "activeness"),
                          classes = foo_classes,
                          ncores = ncores,
                          silent = T)@data
  foo_metrics_inc <- foo_metrics[foo_metrics$Wct0_class == "Included" &
                                   foo_metrics$Activeness != "Excluded", ]
  frontier_ids <- foo_metrics_inc$id_cell
  if(length(frontier_ids) == 0){
    warning("No frontiers were found in the study area that meet the criteria for activeness and baseline. See ?fmetrics")
  }

  # Retain only those cells considered frontiers
  out@data <- out@data[out@data$id_cell %in% frontier_ids, ]

  # Retain excluded cells (for plotting purposes)
  out@excluded_cells <- unique(foo_metrics[!foo_metrics$id_cell %in% frontier_ids, 1:3])

  terra::terraOptions(progress = terra_progress_bar, print = FALSE)

  return(out)
}
