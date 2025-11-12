dt_append <- function(out, foo, ud = FALSE){
  if(!ud){
    if(nrow(out@data) == 0){
      out@data <- foo@data
      out@metrics <- foo@metrics
    } else {
      out@data <- merge(out@data, foo@data, by = "id_cell", all = T)
      out@metrics <- append(out@metrics, foo@metrics)
    }
  } else {
    if(nrow(out@data) == 0){
      out@data <- foo@data
      out@ud_metrics <- foo@ud_metrics
    } else {
      out@data <- merge(out@data, foo@data, by = "id_cell", all = T)
      out@ud_metrics <- append(out@ud_metrics, foo@ud_metrics)
    }
  }

  out
}

getBreaks <- function(col, vars){
  if(is.character(vars[[1]]) && vars[[1]] == "jenks"){
    brks <- BAMMtools::getJenksBreaks(col, length(vars[[2]]) + 1)
    brks[1] <- -Inf
    brks[length(brks)] <- Inf
  }
  if(is.character(vars[[1]]) && vars[[1]] == "equal"){
    brks <- length(vars[[2]])
  }
  if(is.character(vars[[1]]) && vars[[1]] == "quantile"){
    brks <- quantile(col, probs = seq(0, 1, 1/length(vars[[2]])))
    brks[1] <- -Inf
    brks[length(brks)] <- Inf
  }
  if(is.numeric(vars[[1]])){
    brks <- vars[[1]]
  }
  brks
}

get_archetypes <- function(out){
  fm_codes <- data.frame(metric = c("baseline", "baseline_frag", "loss", "speed", "loss_frag", "activeness", "left", "onset"),
                         cname = c("baseline.c", "baseline_frag.c", "loss.c", "speed.c", "loss_frag.c", "activeness", "left.c", "onset"))

  fm_classes <- fm_codes$cname[fm_codes$metric %in% out@metrics]

  if(length(fm_classes) > 0){
    out@data$arch_code <- do.call(paste, c(out@data[, ..fm_classes], sep = "_"))
    archs <- as.data.frame(table(out@data$arch_code))

    archs$perc <- round(100* archs$Freq / sum(archs$Freq), digits = 1)

    archs <- archs[order(archs$perc, decreasing = T), ]

    df_archs <- data.frame(matrix(ncol = length(out@metrics)+1, nrow = nrow(archs)))
    colnames(df_archs) <- c("arch_code", out@metrics)
    archs_foo <- strsplit(as.character(archs$Var1), "_")
    for(i in 1:nrow(df_archs)){
      df_archs[i, 1] <- as.character(archs$Var1[i])
      df_archs[i, 2:ncol(df_archs)] <- archs_foo[[i]]
    }
    df_archs$archetype <- as.character(1:nrow(df_archs))
    # calculates areas and percentages of areas
    df_archs$area <- df_archs$percentage <- NA
    tot_area <- sum(out@data$cell_area)
    for(i in 1:nrow(df_archs)){
      df_archs$area[i] <- round(sum(out@data[out@data$arch_code == df_archs$arch_code[i]]$cell_area), digits = 3)
      df_archs$percentage[i] <- round(100* df_archs$area[i] / tot_area, digits = 2)
    }

    out@data <- merge(out@data, df_archs[, c("arch_code", "archetype")], by = "arch_code")
    out@data[, arch_code := NULL]

    out@archetypes <- df_archs[, -1]
  }
  return(out)
}

#' Calculates frontier metrics
#'
#' Given a structured dataset of class 'init_FrontierMetric', generated with [init_fmetrics()],
#' calculates different deforestation frontier metrics.
#'
#' @param x An object of class 'init_FrontierMetric' generated with [init_fmetrics()].
#' @param metrics Frontier metrics to be calculated. Options are: "baseline",
#' "baseline_frag", "activeness", "speed", "loss", "left", "loss_frag" and "onset";
#' "all" (default) to calculate all available frontier metrics. User-defined metrics
#' can also be calculated. See Details.
#' @param params List of parameters related to the levels of activeness of a
#' frontier and the minimum years to consider frontier onset. See Details.
#' @param breaks An object of class 'FrontierMetric_breaks' generated with [breaks_rules()], containing the rules
#' to define discrete classes for individual frontier metrics. See Details.
#' @param dir A path to a directory to export raster layers of frontier metrics. If `NULL`,
#' (default) raster layers will not be exported, but stored in as an R object.
#' @param gdal GDAL driver specific datasource creation options. See ?terra::writeRaster for details.
#' @param overwrite Logical. If `TRUE` (default), raster layers files will be overwritten.
#' @param ncores Numbers of cores to parallelize processes. Default is 1. See Details.
#' @param silent Logical. If `TRUE`, suppresses messages. Default is `FALSE`.
#'
#' @details
#' The `metrics` argument specifies which frontier metrics will be calculated. These metrics are computed for each cell and are defined as follows:
#'
#' * baseline: The percentage of forest cover in each cell in the first year of the time-frame.
#' * baseline_frag: The value of edge density (m/ha) in the first year of the time-frame.
#' * loss: The percentage of total forest loss over the studied time-frame, relative to the baseline forest cover.
#' * loss_frag: The maximum value of edge density (m/ha) per year within the studied time-frame.
#' * speed: The maximum rate of forest loss (km²/year) during the studied time-frame.
#' * activeness: The level of activeness, as discrete categories, accoding to wheb the frontier was active
#' during the period of analysis.
#' * left: The percentage of forest cover left after at the end of the studied time-series.
#' * onset: The year of onset of the deforestation frontier, calculated as the
#' first year with at least 3 years (by default) of consecutive forest loss.
#'
#' Key parameters of "activeness" and "onset" can be changed within the argument `params`.
#' By default, `params = list(activeness_levels = NULL, onset_min_years = 3)`.  The level
#' of frontier activeness will depend on the time-windows where the frontier
#' was active along the time-frame. Time-windows can be visualized, before calculating frontiers,
#' by inspecting the slot `@temporal_windows` of an object of class 'init_FrontierMetric' generated
#' with `init_fmetrics()` and provided in argument `x` of `fmetrics()`. For instance,
#' if we consider a time-frame of 2000-2024 and a temporal window of 5 years, the
#' temporal windows for this time-frame will be:
#'
#' \tabular{rrr}{
#' window \tab first_year \tab last_year \cr
#' 1  \tab 2001 \tab 2005 \cr
#' 2  \tab 2002 \tab 2006 \cr
#' 3  \tab 2003 \tab 2007 \cr
#' 4  \tab 2004 \tab 2008 \cr
#' 5  \tab 2005 \tab 2009 \cr
#' 6  \tab 2006 \tab 2010 \cr
#' 7  \tab 2007 \tab 2011 \cr
#' 8  \tab 2008 \tab 2012 \cr
#' 9  \tab 2009 \tab 2013 \cr
#' 10 \tab 2010 \tab 2014 \cr
#' 11 \tab 2011 \tab 2015 \cr
#' 12 \tab 2012 \tab 2016 \cr
#' 13 \tab 2013 \tab 2017 \cr
#' 14 \tab 2014 \tab 2018 \cr
#' 15 \tab 2015 \tab 2019 \cr
#' 16 \tab 2016 \tab 2020 \cr
#' 17 \tab 2017 \tab 2021 \cr
#' 18 \tab 2018 \tab 2022 \cr
#' 19 \tab 2019 \tab 2023 \cr
#' 20 \tab 2020 \tab 2024 \cr
#' }
#'
#' If `activeness_levels = NULL`, frontier activeness will be classified in "emerging" (active
#' during temporal windows X to X), "active" (active during temporal windows X to X) or
#' "old" (active during temporal windows X to X). These categories can be changed. For instance,
#' if `activeness_levels = list(emerging = X, old = X, very_old = X)`, three levels of
#' activeness will be considered, according to the provided temporal windows for each one.
#' All temporal windows must be considered in any of the defined activeness levels.
#'
#' If `onset_min_years = 3` (default), onset will refer to the year of onset of the deforestation frontier, calculated as the
#' first year with at least 3 years of consecutive forest loss. The user can change this
#' parameter to a smaller or bigger minimum number of years.
#'
#' For advanced R users, user-defined metrics can also be calculated. The user must
#' create an R function (named "ud_" + "metric_name"; e.g. "ud_metric1") that would receive a unique argument `x`, an object of class
#' `init_FrontierMetric` generated with `init_fmetrics()`, work with the structured
#' dataset within this object (in slot `@data`) as desired, and return a data.frame
#' with the values of the metric for each individual cell. See vignettes for details
#' and examples.
#'
#' In argument `breaks`, the user can set the rules to generate discrete
#' classes of those continuous metrics, by providing an object of
#' class 'FrontierMetric_breaks' generated with [breaks_rules()]. Run `breaks_rules()` to
#' explore these rules. By default, continuous metrics
#' are divided in discrete classes by the "Jenks natural
#' breaks classification", using the function `BAMMtools::getJenksBreaks`.
#' See ?breaks_rules to explore how to re-define categories.
#'
#' Based on the calculated frontier metrics, the function also calculates
#' frontier archetypes. These can be defined as the combination
#' of different frontier metric classes that might represent a higher order of
#' frontier classification. Archetypes can be accessed through `@archetypes` in
#' the outputted object.
#'
#' If a path is provided in `dir` (default is `NULL`), raster layers of frontier metrics will be
#' exported as .tif files. GDAL options can be defined in argument `gdal`. See ?terra::writeRaster for details.
#'
#' Parallelization is recommended for very large datasets (ncores > 1).
#' The calculation of the fragmentation metrics is the most computationally intensive.
#' In general, it is advisable to first run the function with `ncores = 1` and see if it is feasible.
#' If needed, increase the number of cores. The calculation of this particular metric with
#' parallelization requires the `snowfall` package.
#'
#' @return An object of class 'FrontierMetric', containing a dataset with the calculated frontier metrics
#' for each individual cell. This object can be passed to [fmetrics_summary()], [fmetrics_plot()], and [fmetrics_rast()].
#'
#' @seealso [init_fmetrics()], [fmetrics_summary()], [fmetrics_plot()], and [fmetrics_rast()]
#'
#' @references
#' Buchadas, A., Baumann, M., Meyfroidt, P., & Kuemmerle, T. (2022). Uncovering major types of
#' deforestation frontiers across the world’s tropical dry woodlands. Nature Sustainability, 5(7), 619-627.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Downloads example of object of class 'init_FrontierMetric' generated with init_fmetrics()
#' download.file(frontiermetrics_data[4], "copo_dataset.RDS")
#'
#' # Loads object to R environment
#' copo_dataset <- readRDS("copo_dataset.RDS")
#'
#' # Calculates all frontier metrics
#' copo_metrics <- fmetrics(copo_dataset, metrics = "all", ncores = 2)
#'
#' # Calculates development frontiers
#' copo_metrics2 <- fmetrics(copo_dataset, metrics = "development", ncores = 2)
#' copo_metrics2
#'
#' # Calculates a single frontier
#' copo_metrics3 <- fmetrics(copo_dataset, metrics = "speed", ncores = 2)
#' copo_metrics3
#' }
fmetrics <- function(x,
                     metrics = "all",
                     params = list(activeness_levels = NULL,
                                   onset_min_years = 3),
                     breaks = breaks_rules(),
                     dir = NULL,
                     gdal = NULL,
                     overwrite = TRUE,
                     ncores = 1,
                     silent = FALSE){

  # Argument's checking
  environment(check_fmetrics) <- environment()
  chk <- check_fmetrics()
  if (length(chk[[1]]) > 0)
    for (w in 1:length(chk[[1]])) {
      warning(strwrap(chk[[1]], prefix = "\n", initial = ""), call. = FALSE)
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

  metrics <- unique(metrics)

  if(any(grepl("ud_", metrics))){
    ud_metrics <- metrics[grepl("ud_", metrics)]
  } else {
    ud_metrics <- NULL
  }

  if("all" %in% metrics){
    metrics <- c("baseline", "loss", "speed", "loss_frag",
                 "baseline_frag", "activeness", "left", "onset")
    if(!is.null(ud_metrics)) metrics <- c(metrics, ud_metrics)
  }

  if("activeness" %in% metrics){
    if(is.null(params$activeness_levels)){
      params$activeness_levels <- list(emerging = x@temporal_windows$window[nrow(x@temporal_windows)],
                                active = (nrow(x@temporal_windows)-x@window+1):(nrow(x@temporal_windows)-1),
                                old = 1:(nrow(x@temporal_windows)-x@window))
    }
    # aca chequear que cuadren years de classes con time frame
    # me refiero a lineas 53 y 54 de fm_activeness
  }

  fmetrics_list <- c("baseline", "baseline_frag", "loss", "loss_frag",
                     "speed", "activeness", "left", "onset")
  metrics <- fmetrics_list[fmetrics_list %in% metrics]

  out <- new("FrontierMetric",
             metrics = "",
             time_frame = x@time_frame,
             data = data.table(),
             extent = x@extent,
             excluded_cells = x@excluded_cells)

  # Order of metric calculations
  fm_order <- c("baseline", "baseline_frag", "activeness", "speed",
                "loss", "left", "loss_frag", "onset")
  metrics <- fm_order[fm_order %in% metrics]

  if(!silent) message("Calculating metrics")

  if("baseline" %in% metrics){
    if(!silent) message("   baseline forest")
    foo <- calc_fm_baseline(x, breaks@baseline)
    out <- dt_append(out, foo)
  }

  if("baseline_frag" %in% metrics){
    if(!silent) message("   baseline forest fragmentation")
    foo <- calc_fm_baseline_frag(x, breaks@baseline_frag, ncores)
    out <- dt_append(out, foo)
  }

  if("activeness" %in% metrics){
    if(!silent) message("   activeness")
    foo <- calc_fm_activeness(x, params$activeness_levels)
    out <- dt_append(out, foo)
  }

  if("speed" %in% metrics){
    if(!silent) message("   speed")
    foo <- calc_fm_speed(x, breaks@speed)
    out <- dt_append(out, foo)
  }

  if("loss" %in% metrics){
    if(!silent) message("   forest loss")
    foo <- calc_fm_loss(x, breaks@loss)
    out <- dt_append(out, foo)
  }

  if("left" %in% metrics){
    if(!silent) message("   forest left")
    foo <- calc_fm_left(x, breaks@left)
    out <- dt_append(out, foo)
  }

  if("loss_frag" %in% metrics){
    if(!silent) message("   forest loss fragmentation")
    foo <- calc_fm_loss_frag(x, breaks@loss_frag, ncores)
    out <- dt_append(out, foo)
  }

  if("onset" %in% metrics){
    if(!silent) message("   onset")
    foo <- calc_fm_onset(x, params$onset_min_years, ncores)
    out <- dt_append(out, foo)
  }

  if(!is.null(ud_metrics)){
    if(!silent) message("Calculating user-defined metrics")
    for(ud_f in ud_metrics){
      fm_ds <- do.call(ud_f, list(x = x))
      foo <- ud_metric(x, fm_ds, name = ud_f)
      out <- dt_append(out, foo, ud = TRUE)
    }
  }

  # Add coords and cell area
  out@data <- merge(out@data, x@coords, by = "id_cell")
  out@data <- cols_reorder(out@data,
                           c("id_cell", "x_cell", "y_cell", "cell_area"),
                           1:4)

  fm_order <- c("baseline", "baseline_frag", "loss", "loss_frag", "speed",
                "activeness", "left", "onset")
  out@metrics <- fm_order[fm_order %in% out@metrics]

  # Get archetypes
  out <- get_archetypes(out)

  # Reorder rows by id_cell
  out@data <- out@data[order(out@data$id_cell, decreasing = F), ]

  # Export rasters
  if(!is.null(dir)){
    fmetrics_rast(x = out, metrics = metrics, dir = dir, gdal = gdal, overwrite = overwrite)
  }

  return(out)
}
