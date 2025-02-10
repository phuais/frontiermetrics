ff_plot_raster <- function(df, df_excluded, fill, type, title, aspect, extent, palette, direction, background){

  extent <- data.frame(xmin = extent[1],
                       xmax = extent[2],
                       ymin = extent[3],
                       ymax = extent[4])

  if(type == "values"){
    p <- ggplot2::ggplot() +
      ggplot2::geom_rect(data = extent,
                         ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                         fill = background[1], color = background[1]) +
      ggplot2::geom_tile(data = df_excluded, ggplot2::aes(x = x_cell, y = y_cell), na.rm = T, fill = background[2]) +
      ggplot2::geom_tile(data = df, ggplot2::aes(x = x_cell, y = y_cell, fill = get(fill)), na.rm = T) +
      ggplot2::ggtitle(title) +
      viridis::scale_fill_viridis(name = NULL, option = palette[1], direction = direction[1]) +
      # ggplot2::scale_fill_distiller(palette = "YlGnBu", name = NULL, na.value = "white") +
      ggplot2::theme_void() +
      ggplot2::theme(aspect.ratio = aspect)

  }
  if(type == "classes"){
    p <- ggplot2::ggplot() +
      ggplot2::geom_rect(data = extent,
                         ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                         fill = background[1], color = background[1]) +
      ggplot2::geom_tile(data = df_excluded, ggplot2::aes(x = x_cell, y = y_cell), na.rm = T, fill = background[2]) +
      ggplot2::geom_tile(data = df, ggplot2::aes(x = x_cell, y = y_cell, fill = get(fill)), na.rm = T) +
      ggplot2::ggtitle(title) +
      viridis::scale_fill_viridis(name = NULL, discrete = T,  option = palette[2], direction = direction[2]) +
      #ggplot2::scale_fill_brewer(palette = "YlGnBu", direction = 1, name = NULL, na.value = "white") +
      ggplot2::theme_void() +
      ggplot2::theme(aspect.ratio = aspect)
  }
  p
}

#' Plots frontier metrics
#'
#' Generates plots of frontier metrics based on an object of class 'FrontierMetric'
#' generated with [fmetrics()]
#'
#' @param x Object of class 'FrontierMetric' generated with [fmetrics()]
#' @param metrics Frontier metrics to be plotted. Options are: "baseline", "loss",
#' "fragmentation", "speed", "activeness" and/or "left" for individual frontier
#' metrics; "severity", "spatio_temporal" and/or "development" for frontier typologies;
#' "all" to calculate all frontier metrics available within `x`. Default is "all".
#' @param type One of the following: "both", to plot absolute values and discrete
#' classes of frontier metrics; "values", to plot only absolute values; or "classes"
#' to plot only discrete classes. Default is "both".
#' @param ncol Number of columns to arrange plots. Default is 4.
#' @param palette A vector of one or two strings with the names of the palettes to be used
#' in the plots. The first element is for the absolute values of frontier metrics and the second for the
#' discrete classes. Default is c("viridis", "viridis"). If only one palette is provided,
#' the same will be used for both plots. See Details.
#' @param direction A vector of two integers indicating the direction of the palette
#' for the absolute values of frontier metrics and for discrete classes, respectively.
#' Integers must be 1 or -1. If only one number is provided, the same direction will
#' be used for both palettes. Default is c(1, 1). See Details.
#' @param background A vector of two strings with the names of the colors to be used
#' for the background of the plot. The first depicts the color for the region outside
#' the range of the study area, while the second depicts the color for those cells
#' within the study area that did not qualify as frontiers. Default is c("gray90", "gray64").
#' See Details.
#'
#' @export
#'
#' @details
#' Available palettes are those available for the function `scale_fill_viridis()`:
#' "magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", or "turbo".
#' The `direction` argument is used to reverse the palette if necessary. To do this
#' the user must specify -1. The user can play with the background colors for
#' better visualization.
#'
#' @examples
#' \dontrun{
#' fmetrics_plot(copo_metrics)
#' fmetrics_plot(copo_metrics, metrics = "speed", type = "values", palette = "magma")
#' }
fmetrics_plot <- function(x,
                    metrics = "all",
                    type = "both",
                    ncol = 4,
                    palette = c("viridis", "viridis"),
                    direction = c(1, 1),
                    background = c("gray90", "gray64")){

  # Argument's checking
  environment(check_fmetrics_plot) <- environment()
  chk <- check_fmetrics_plot()
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

  if("all" %in% metrics){
    metrics <- x@metrics
  } else {
    metrics <- unique(metrics)
    fmetrics_list <- data.frame(metric = c("baseline", "loss", "speed",
                                           "fragmentation", "activeness", "left"),
                                group = c(rep("severity", 2),
                                          rep("spatio_temporal", 2),
                                          rep("development", 2)))
    metrics <- fmetrics_list[fmetrics_list$metric %in% metrics |
                               fmetrics_list$group %in% metrics, "metric"]
  }

  notfound <- character()
  for(i in 1:length(metrics)){
    if(!metrics[i] %in% x@metrics)
      notfound <- append(notfound, metrics[i])
  }
  if(length(notfound) > 0){
    stop(" The following metrics could not be found as a calculated metric in 'x': ", paste0(notfound, sep = " "))
  }

  aspect <- (x@extent[4] - x@extent[3])/(x@extent[2] - x@extent[1])

  # Absolute palette adjustment
  direction[1] <- -direction[1]

  plots <- list()
  if("baseline" %in% metrics){
    if(type %in% c("both", "values"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "Wct0", "values", "Baseline woodland [%]",
                                                 aspect, x@extent,
                                                 palette, direction, background)
    if(type %in% c("both", "classes"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "Wct0_class", "classes", "Baseline woodland (classes)",
                                                 aspect, x@extent,
                                                 palette, direction, background)
  }
  if("loss" %in% metrics){
    if(type %in% c("both", "values"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "PTWl", "values", "Woodland loss [%]",
                                                 aspect, x@extent,
                                                 palette, direction, background)
    if(type %in% c("both", "classes"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "PTWl_class", "classes", "Woodland loss (classes)",
                                                 aspect, x@extent,
                                                 palette, direction, background)
  }
  if("speed" %in% metrics){
    if(type %in% c("both", "values"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "Sp", "values", "Speed [Km\U00B2/year]",
                                                 aspect, x@extent,
                                                 palette, direction, background)
    if(type %in% c("both", "classes"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "Sp_class", "classes", "Speed (classes)",
                                                 aspect, x@extent,
                                                 palette, direction, background)
  }
  if("fragmentation" %in% metrics){
    if(type %in% c("both", "values"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "ED", "values", "Fragmentation [m/ha]",
                                                 aspect, x@extent,
                                                 palette, direction, background)
    if(type %in% c("both", "classes"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "ED_class", "classes", "Fragmentation (classes)",
                                                 aspect, x@extent,
                                                 palette, direction, background)
  }
  if("activeness" %in% metrics){
    plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                               "Activeness", "classes", "Activeness (classes)",
                                               aspect, x@extent,
                                               palette, direction, background)
  }
  if("left" %in% metrics){
    if(type %in% c("both", "values"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "Wleft", "values", "Woodland left [%]",
                                                 aspect, x@extent,
                                                 palette, direction, background)
    if(type %in% c("both", "classes"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "Wleft_class", "classes", "Woodland left (classes)",
                                                 aspect, x@extent,
                                                 palette, direction, background)
  }

  if(ncol > length(plots)) ncol <- length(plots)
  egg::ggarrange(plots = plots, ncol = ncol)
}

