get_FL <- function(i, rast_loss, cell_size, aggregation, years, ncores, is_series, cover_series, silent){
  if(!silent) cat("\r> Processing year: ", i , " / ", length(years), sep = "")
  if(!is_series){
    gwf_loss_tmp <- rast_loss == i
  } else {
    gwf_loss_tmp <- cover_series[[i]] - cover_series[[i+1]]
  }
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

#' Generates primary structure to develop frontier metrics
#'
#' Based on inputs of tree cover and forest loss from Global Forest Watch database, or
#' on a custom forest cover time-series, generates a structured dataset that can be used as the main input to calculate frontier
#' metrics with [fmetrics()].
#'
#' @param raster If `is_series = FALSE`, a list of two objects of class 'SpatRaster',
#' or two paths to two raster layers, with tree cover and forest loss from
#' Global Forest Watch databases. If `is_series = TRUE`, an object of class 'SpatRaster', or a path
#' to a raster layer, with a cover time-series. See Details.
#' @param is_series Logical. If `FALSE` (default), it is expected that the raster
#' layers provided in `raster` were obtained from Global Forest Watch databases.
#' If `TRUE`, it is expected that a forest cover time-series is provided in `raster`. See Details.
#' @param is_continuous Logical. If `FALSE` (default) it is expected that the provided forest
#' cover series in `raster` contains binary raster layers. If `TRUE`, it is expected that
#' the provided raster series has continuous values of forest cover. Ignored if
#' `is_series = FALSE`. See Details.
#' @param min_treecover A number (percentage) depicting the minimum percentage of tree
#' cover to be considered as "forest". Default is 10. Ignored if `is_series = TRUE`. See Details.
#' @param aggregation A numeric vector of length 2, depicting the first and second
#' magnitude of aggregation. See Details.
#' @param time_frame A numeric vector of two elements depicting the first and last year of the analyzed time-frame.
#' Default is c(2000, 2024). See Details.
#' @param min_cover A number (percentage) depicting the minimum percentage of forest
#' cover for a cell to be considered a frontier. Default is 5. See Details.
#' @param min_rate A number (percentage) depicting the minimum average annual forest loss rate
#' in a five-year period (when `window = 5`) for a cell to be considered a frontier. Default is 0.5. See Details.
#' @param window A whole number depicting the number of years to consider in the calculation of
#' frontier activeness. Default is 5. See Details.
#' @param ncores Numbers of cores to parallelize processes. Default is 1.
#' @param silent Logical. If `TRUE`, suppresses messages. Default is `FALSE`.
#'
#' @details
#' Deforestation frontier metrics were initially developed to be calculated using Global Forest Watch databases
#' (GFW) (Buchadas et al. 2022; Hansen et al. 2013). If data from GFW is used, `is_series` must equal
#' `FALSE`, the option by default. Then, a list with two main raster layers from this source must be provided,
#' in the following order:
#'
#' (1) a raster layer (class 'SpatRaster' or a path to a raster layer) of initial forest cover in year 2000.
#' The values for this raster layer typically ranges between 0 and 100, depicting
#' the percentage of tree cover of each individual cell (at a resolution of ~30m).
#'
#' (2) a raster layer (class 'SpatRaster' or a path to a raster layer) of forest loss between years 2001
#' and 2024. This is a single raster layer where 0 indicates no loss, while values of 1, 2, 3, etc.,
#' represent forest loss occurring in years 2001, 2002, 2003, and so on.
#'
#' Both raster layers can be previously downloaded and processed with [get_gfw()], which will
#' access GFW sources. See `?get_gfw` for details. The user must also define a minimum
#' percentage of tree cover (in argument `min_treecover`) for a cell to be considered
#' "forest". By default, this is 10%.
#'
#' The argument `time_frame` must represent the first and last year of the time-series.
#' By default, [get_gfw()] downloads forest cover in the year 2000 (as provided by GFW datasets), so `time_frame` equals
#' `c(2000, 2024)`. If an year different than 2000 is defined, the function will calculate the
#' forest cover for the provided year, by subtracting the accumulated forest loss from the
#' layer of forest cover in 2000 until the provided year. Note that forest gain is
#' not considered in any step.
#'
#' Alternatively, a custom raster layer representing a time-series of
#' forest cover can be provided. If so, `is_series` must equal `TRUE`, and
#' a raster of multiple layers must be provided in argument `raster`.
#' Within this object, each layer must be binary, representing the amount of forest cover for
#' each consecutive year of the analyzed time series. A cell of value 1 will be
#' considered as covered by forest, while 0 will be considered as a different
#' cover. Alternatively, each layer of the raster series can be continuous,
#' representing a continuous value of forest cover in km², and argument
#' `is_continuous` must be `TRUE`. In addition, when `is_series = TRUE`, argument `time_frame` must
#' represent the first and last year of the time-series. The number of years
#' provided by this range must match the number of layers of the cover time-series
#' provided in `raster`.
#'
#' \emph{
#' Note that available metrics were initially programmed to work with Global Forest Watch
#' datasets, and excluding the process of forest gain. This could be relevant for the
#' calculation of some of these metrics, if a custom time-series of forest cover is
#' provided, and the process of forest gain is considered within this time-series. In
#' addition, these metrics where initially thought for the process of forest loss in
#' tropical and subtropiccal regions. Any other use for a different cover or region
#' should be applied with caution, and these metrics might need particular adjustments.
#' }
#'
#' Forest cover and forest loss data can be aggregated, which is recommended.
#' Aggregation is done in two steps. First, cells are aggregated using the first
#' value of the aggregation argument, which represents the number of cells to merge
#' in each direction (horizontal and vertical). In this step, the values of forest cover in the baseline year of all
#' included cells are summed, as well as the amount of forest loss in subsequent years.
#' Second, the newly aggregated cells are further
#' grouped using the second value of the aggregation argument. At this stage,
#' each cell from the initial aggregation is merged into larger cells,
#' which will serve as the basis for calculating frontier metrics. Since GFW raster layers have a resolution of approximately 30m (at the equator),
#' setting `aggregation = c(10, 10)` (default setting) will: (1) aggregate 10 cells both horizontally
#' and vertically, producing cells of
#' ~300m, and (2) further group these newly aggregated cells by a factor of
#' 10, creating larger cells with a resolution of ~3000m. For instance, setting
#' `aggregation = c(5, 10)` will first aggregate to ~150m cells (30 * 5), then
#' group these new cells into ~1500m cells (150 * 10).
#'
#' Frontiers are defined as cells that meet two criteria: (1) they have a minimum
#' percentage of forest cover (`min_cover`) and (2) they have a minimum average
#' annual forest loss rate in a given temporal window (`min_rate`). By default, these
#' values take 5% and 0.5%, respectively (as in Buchadas et al. 2022), but they could
#' take other values. Those cells
#' that do not meet these criteria are not considered frontiers and thus excluded from the analysis. Also, by default
#' the temporal window is set to 5 years (`window`), but it could also be set to a different
#' number of years (e.g. 2, 3, 7 or 9 years).
#'
#' @return
#' An object of class 'init_FrontierMetric', containing the structured dataset to
#' calculate frontier metrics with [fmetrics()].
#'
#' @references
#' Buchadas, A., Baumann, M., Meyfroidt, P., & Kuemmerle, T. (2022). Uncovering major types of deforestation frontiers across the world's tropical dry
#' woodlands. Nature Sustainability, 5(7), 619-627.
#'
#' Hansen, M. C., Potapov, P. V., Moore, R., Hancher, M., Turubanova, S. A., Tyukavina, A.,
#' ... & Townshend, J. R. (2013). High-resolution global maps of 21st-century forest cover change. science, 342(6160), 850-853.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Downloads raster layers generated with function get_gfw()
#' # Tree cover
#' curl::curl_download(frontiermetrics_data[2], "tree_cover.tif")
#' # Forest loss
#' curl::curl_download(frontiermetrics_data[3], "loss_year.tif")
#'
#' # Loads raster layers generated with function get_gfw()
#' rast_cover <- terra::rast("tree_cover.tif")
#' rast_loss <- terra::rast("loss_year.tif")
#'
#' # Generates structured object
#' copo_dataset <- init_fmetrics(raster = list(gfw_cover, gfw_loss),
#'                               time_frame = c(2000, 2024),
#'                               ncores = 1)
#'
#' # Shows basic information of the object
#' copo_dataset
#' }
init_fmetrics <- function(raster,
                          is_series = FALSE,
                          is_continuous = FALSE,
                          min_treecover = 10,
                          aggregation = c(10, 10),
                          time_frame = c(2000, 2024),
                          min_cover = 5,
                          min_rate = 0.5,
                          window = 5,
                          ncores = 1,
                          silent = FALSE) {
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

  if(!is_series){
    rast_cover <- raster[[1]]
    rast_loss <- raster[[2]]
    if(is.character(rast_cover)) rast_cover <- terra::rast(rast_cover)
    if(is.character(rast_loss)) rast_loss <- terra::rast(rast_loss)
  } else {
    rast_cover <- raster
    rast_loss <- NULL
    if(is.character(rast_cover)) rast_cover <- terra::rast(rast_cover)
    if(dim(rast_cover)[3] != length(time_frame[1]:time_frame[2])){
      stop("The number of raster layers in 'raster' does not match the number of years given the provided 'time_frame'. See ?init_fmetrics")
    }
  }

  terra_progress_bar <- terra::terraOptions(print = FALSE)$progress
  terra::terraOptions(progress = 0, print = FALSE)

  # Transform to epsg:4326 if necessary
  rast_cover <- crs_transform(rast_cover)
  if(!is_series) rast_loss <- crs_transform(rast_loss)

  if(any(abs(terra::ext(rast_cover)[3:4]) > 35)){
    warning(paste("The study area extends beyond the latitude limits of tropical and subtropical regions (35\u00b0).",
                  "\nFrontier metrics were designed with these regions in mind, so the results may not be applicable",
                  "or meaningful outside of the mentioned ranges."))
  }

  if(!is_series){
    #rast_loss <- rast_loss[rast_cover >= min_treecover, drop = F]
    rast_loss[!rast_cover >= min_treecover] <- 0
    rast_cover <- rast_cover >= min_treecover
    if(time_frame[1] != 2000){
      mask_loss <- rast_loss >= 1 & rast_loss <= (time_frame[1] - 2000)
      rast_cover <- rast_cover - mask_loss
    }
    cover_series <- NULL
  } else {
    cover_series <- rast_cover
    rast_cover <- cover_series[[1]]
  }
  cell_size <- terra::cellSize(rast_cover, unit = "km")
  # Multiplies 1s of raster with cell size
  if(!is_continuous) rast_cover <- rast_cover * cell_size
  if(aggregation[1] > 1){
    rast_cover_1 <- terra::aggregate(
      rast_cover,
      fact = aggregation[1],
      fun = sum,
      cores = ncores,
      na.rm = T)
  } else {
    rast_cover_1 <- rast_cover
  }
  # Shrink rasters to adapt to aggregation
  rast_cover_1 <- rast_cover_1[1:(dim(rast_cover_1)[1] - dim(rast_cover_1)[1] %% aggregation[2]),
                             1:(dim(rast_cover_1)[2] - dim(rast_cover_1)[2] %% aggregation[2]), drop = F]
  dat <- as.data.frame(rast_cover_1,
                       xy = T,
                       cells = T,
                       na.rm = F)
  dat <- as.data.table(dat)

  x_sides <- dim(rast_cover_1)[1] / aggregation[2]
  y_sides <- dim(rast_cover_1)[2] / aggregation[2]
  vec <- vector("numeric")
  init <- 1
  end <- y_sides
  for (i in 1:x_sides) {
    vec <- c(vec, rep(rep(init:end, each = aggregation[2]), aggregation[2]))
    init <- init + y_sides
    end <- end + y_sides
  }
  dat$id_cell <- vec
  colnames(dat) <- c("id_1", "x_1", "y_1", paste0("FC_", time_frame[1]), "id_cell")

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
                 paste0("FC_", time_frame[1])), with = F]

  # Calculating forest loss

  years <- (time_frame[1] + 1):time_frame[2]

  results <- lapply(1:length(years),
                    get_FL,
                    rast_loss,
                    cell_size,
                    aggregation,
                    years,
                    ncores,
                    is_series,
                    cover_series,
                    silent)

  # Merge into a dataframe
  for(i in 1:length(results)){
    dat <- merge(dat, results[[i]], by = "id_1")
  }

  # Remove cells with NAs in initial Forest Cover
  cols <- paste0("FC_", time_frame[1])
  dat <- dat[!dat$id_cell %in% unique(dat[which(is.na(dat[, paste0("FC_", time_frame[1]), with = F]))]$id_cell), ]

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

  if(is_series) min_treecover <- NULL

  # Year windows
  T_ranges <- data.frame(window = 1:(time_frame[2]-time_frame[1]-(window-1)),
                         #window = paste0("T", 1:(time_frame[2]-time_frame[1]-(window-1))),
                         first_year = (time_frame[1]+1):(time_frame[2]-(window-1)),
                         last_year = (time_frame[1]+window):time_frame[2])

  out <- new(
    "init_FrontierMetric",
    data = dat,
    coords = coords,
    time_frame = time_frame,
    initial_fc_col = paste0("FC_", time_frame[1]),
    fl_cols = colnames(dat)[8:ncol(dat)],
    extent = as.vector(terra::ext(rast_cover_1)),
    grain = list(
      terra::res(rast_cover_1),
      terra::res(rast_cover_1) * aggregation[2]
    ),
    aggregation = aggregation,
    is_series = is_series,
    min_treecover = min_treecover,
    min_cover = min_cover,
    min_rate = min_rate,
    window = window,
    temporal_windows = T_ranges,
    excluded_cells = data.table()
  )

  # Filter by frontier activeness and baseline
  foo_classes <- breaks_rules()
  foo_classes@baseline[[1]] <- c(-Inf, min_cover, Inf)
  foo_classes@baseline[[2]] <- c("excluded", "included")
  foo_metrics <- fmetrics(x = out,
                          metrics = c("baseline", "activeness"),
                          params = list(activeness_levels = list(included = T_ranges$window),
                                        onset_min_years = 3),
                          breaks = foo_classes,
                          ncores = ncores)@data
  foo_metrics_inc <- foo_metrics[foo_metrics$baseline.c == "included" &
                                   foo_metrics$activeness != "excluded", ]
  frontier_ids <- foo_metrics_inc$id_cell
  if(length(frontier_ids) == 0){
    warning("No frontiers were found in the study area that meet the defined criteria for frontier activeness (arguments 'min_rate' and 'window') and baseline forest (argument 'min_cover'). See ?fmetrics")
  }

  # Retain only those cells considered frontiers
  out@data <- out@data[out@data$id_cell %in% frontier_ids, ]

  # Retain excluded cells (for plotting purposes)
  out@excluded_cells <- unique(foo_metrics[!foo_metrics$id_cell %in% frontier_ids, 1:3])

  terra::terraOptions(progress = terra_progress_bar, print = FALSE)

  return(out)
}
