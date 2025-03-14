dt_append <- function(out, foo){
  if(nrow(out@data) == 0){
    out@data <- foo@data
  } else {
    out@data <- merge(out@data, foo@data, by = "id_cell", all = T)
  }
  out
}

#' Calculates frontier metrics
#'
#' Given a structured dataset of class 'init_FrontierMetric', generated with [init_fmetrics()],
#' calculates frontier metrics primarily based on Buchadas et al. (2022).
#'
#' @param x An object of class 'init_FrontierMetric' generated with [init_fmetrics()].
#' @param metrics Frontier metrics to be calculated. Options are: "baseline",
#' "loss", "fragmentation", "speed", "activeness" and/or "left" for individual
#' frontier metrics; "severity", "spatio_temporal" and/or "development" for
#' frontier typologies; "all" to calculate all frontier metrics. Default is "all".
#' See Details.
#' @param classes An object of class 'FrontierMetric_classes' containing the rules
#' to define discrete classes for individual frontier metrics. See Details.
#' @param ncores Numbers of cores to parallelize processes. Default is 1. See Details.
#' @param silent Logical. If TRUE, suppresses messages. Default is FALSE.
#'
#' @details
#' The `metrics` argument specifies which frontier metrics will be calculated. These metrics are computed for each cell and are defined as follows:
#'
#' * baseline: The percentage of woodland cover in each cell in the first year of the time-frame.
#' * loss: The percentage of total woodland loss over the studied time-frame, relative to the baseline woodland cover.
#' * fragmentation: The maximum value of edge density (m/ha) per year within the studied time-frame.
#' * speed: The maximum rate of woodland loss (km²/year) during the studied time-frame.
#' * activeness: Categorized as "active", "old", or "emerging", depending on when the frontier was active
#' during the period of analysis.
#' * left: The percentage of woodland left after the period of woodland loss analysed.
#'
#' For detailed explanations of how frontier metrics are calculated, refer to Buchadas et al. (2022).
#' To improve flexibility, frontier metrics "baseline", "loss", and "left" are expressed as a percentages, rather than in km².
#'
#' The `metrics` argument also allows specifying groups of metrics or "frontier typologies"
#' (Buchadas et al. 2022). Typologies include:
#' "severity" (comprising the "baseline" and "loss" metrics),
#' "spatio_temporal" (comprising the "speed" and "fragmentation" metrics), and
#' "development" (comprising the "activeness" and "left" metrics).
#'
#' The rules to define discrete categories for frontier metrics can
#' be defined in argument `classes`. This accepts an object of class 'FrontierMetric_classes',
#' generated with [init_classes()]. Run `init_classes()` to explore these rules and see ?init_classes to
#' explore how to re-define these categories.
#'
#' By default, in the scenario of default definition of frontiers, rules to
#' categorize frontier metrics are the following:
#'
#' ````
#' Baseline woodland [%]:
#'   Low (5,10]
#'   Medium (10,55]
#'   High (55,Inf]
#'
#' Woodland loss [%]:
#'   Low (-Inf,20]
#'   Medium (20,50]
#'   High (50,Inf]
#'
#' Fragmentation [m/ha]:
#'   Low (-Inf,10]
#'   Medium (10,20]
#'   High (20,Inf]
#'
#' Speed [km²/year]:
#'   Slow (-Inf,0.05]
#'   Medium (0.05,0.25]
#'   Fast (0.25,Inf]
#'
#' Activeness:
#'   Old       ... ,[2012,2016], [2013,2017], [2014,2018]
#'   Active    [2015,2019], [2016,2020] ... [2018,2022]
#'   Emerging  [2019,2023]
#'
#' Woodland left [%]:
#'   Low (-Inf,33]
#'   Medium (33,66]
#'   High (66,Inf]
#' ````
#' Parallelization is recommended for very large datasets (ncores > 1).
#' The calculation of the fragmentation metric is the most computationally intensive.
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
                     classes = init_classes(),
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
  if("all" %in% metrics){
    metrics <- c("baseline", "loss", "speed", "fragmentation",
                 "activeness", "left")
  }

  fmetrics_list <- data.frame(metric = c("baseline", "loss", "speed",
                                         "fragmentation", "activeness", "left"),
                              group = c(rep("severity", 2),
                                        rep("spatio_temporal", 2),
                                        rep("development", 2)))
  metrics <- fmetrics_list[fmetrics_list$metric %in% metrics |
                             fmetrics_list$group %in% metrics, "metric"]

  out <- new("FrontierMetric",
             metrics = "",
             year_range = x@year_range,
             data = data.table(),
             extent = x@extent,
             excluded_cells = x@excluded_cells)

  fm_order <- c("baseline", "activeness", "speed", "loss", "left", "fragmentation")
  metrics <- fm_order[fm_order %in% metrics]

  if("baseline" %in% metrics){
    if(!silent) message("Calculating metrics: baseline")
    fm_ds <- fm_data_prep(x, "baseline")
    foo <- calc_fm_baseline(x, fm_ds, classes@baseline)
    out@metrics <- append(out@metrics, "baseline")
    out <- dt_append(out, foo)
  }

  if("activeness" %in% metrics){
    if(!silent) message("Calculating metrics: activeness")
    if(any(as.numeric(classes@activeness) > x@year_range[2])){
      stop("The default value for the classes of activeness exceeds the last year of the dataset. Please redefine the classes. See ?fmetrics and ?init_classes")
    }
    fm_ds1 <- fm_data_prep(x, "activeness")
    fm_ds1_d <- fm_ds1[,  colnames(fm_ds1)[grepl("d", colnames(fm_ds1))], with = F]
    foo <- calc_fm_activeness(x, fm_ds1_d, classes@activeness, x@window)
    out@metrics <- append(out@metrics, "activeness")
    out <- dt_append(out, foo)
  }

  if("speed" %in% metrics){
    if(!silent) message("Calculating metrics: speed")
    if(exists("fm_ds1", inherits = F)){
      fm_ds2 <- fm_ds1
    } else {
      fm_ds2 <- fm_data_prep(x, "speed")
    }
    fm_ds2_d <- as.data.table(fm_ds2)
    fm_ds2_d <- fm_ds2_d[, -x@initial_fc_col, with = F]
    foo <- calc_fm_speed(x, fm_ds2_d, classes@speed)
    out@metrics <- append(out@metrics, "speed")
    out <- dt_append(out, foo)
  }

  if("loss" %in% metrics){
    if(!silent) message("Calculating metrics: woodland loss")
    if(exists("fm_ds2", inherits = F)){
      fm_ds3 <- fm_ds2
      fm_ds3$total_FL <- rowSums(fm_ds3[, x@fl_cols, with = F])
    } else {
      fm_ds3 <- fm_data_prep(x, "loss")
    }
    fm_ds3_d <- fm_ds3[, c("id_cell", x@initial_fc_col, "total_FL"), with = F]
    foo <- calc_fm_loss(x, fm_ds3_d, classes@loss)
    out@metrics <- append(out@metrics, "loss")
    out <- dt_append(out, foo)
  }

  if("left" %in% metrics){
    if(!silent) message("Calculating metrics: woodland left")
    if(exists("fm_ds3", inherits = F)){
      fm_ds4 <- fm_ds3
    } else {
      fm_ds4 <- fm_data_prep(x, "left")
    }
    fm_ds4_d <- fm_ds4[, c("id_cell", x@initial_fc_col, "total_FL"), with = F]
    foo <- calc_fm_left(x, fm_ds4_d, classes@left)
    out@metrics <- append(out@metrics, "left")
    out <- dt_append(out, foo)
  }

  if("fragmentation" %in% metrics){
    if(!silent) message("Calculating metrics: fragmentation")
    foo <- calc_fm_fragmentation(x, ncores, classes@fragmentation)
    out@metrics <- append(out@metrics, "fragmentation")
    out <- dt_append(out, foo)
  }

  # Add coords and cell area
  out@data <- merge(out@data, x@coords, by = "id_cell")
  out@data <- cols_reorder(out@data,
                           c("id_cell", "x_cell", "y_cell", "cell_area"),
                           1:4)

  out@metrics <- out@metrics[-1]
  fm_order <- c("baseline", "loss", "speed", "fragmentation", "activeness", "left")
  out@metrics <- fm_order[fm_order %in% out@metrics]

  return(out)
}



