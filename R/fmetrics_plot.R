ff_plot_raster <- function(df, df_excluded, fill, what, title, aspect, extent,
                           palette, direction, background, arch_levels = NULL,
                           arch_colors = NULL){

  extent <- data.frame(xmin = extent[1],
                       xmax = extent[2],
                       ymin = extent[3],
                       ymax = extent[4])

  if(what == "values"){
    p <- ggplot2::ggplot() +
      ggplot2::geom_rect(data = extent,
                         ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                         fill = background[1], color = background[1]) +
      ggplot2::geom_tile(data = df_excluded, ggplot2::aes(x = x_cell, y = y_cell), na.rm = T, fill = background[2]) +
      ggplot2::geom_tile(data = df, ggplot2::aes(x = x_cell, y = y_cell, fill = get(fill)), na.rm = T) +
      ggplot2::ggtitle(title) +
      viridis::scale_fill_viridis(name = NULL, option = palette, direction = direction) +
      # ggplot2::scale_fill_distiller(palette = "YlGnBu", name = NULL, na.value = "white") +
      ggplot2::scale_x_continuous(name = NULL, labels = NULL) +
      ggplot2::scale_y_continuous(name = NULL, labels = NULL) +
      ggplot2::theme_light() +
      ggplot2::theme(aspect.ratio = aspect, panel.grid = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(), panel.border = ggplot2::element_blank())
  }
  if(what == "classes"){
    p <- ggplot2::ggplot() +
      ggplot2::geom_rect(data = extent,
                         ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                         fill = background[1], color = background[1]) +
      ggplot2::geom_tile(data = df_excluded, ggplot2::aes(x = x_cell, y = y_cell), na.rm = T, fill = background[2]) +
      ggplot2::geom_tile(data = df, ggplot2::aes(x = x_cell, y = y_cell, fill = get(fill)), na.rm = T) +
      ggplot2::ggtitle(title) +
      viridis::scale_fill_viridis(name = NULL, discrete = T,  option = palette, direction = -direction) +
      #ggplot2::scale_fill_brewer(palette = "YlGnBu", direction = 1, name = NULL, na.value = "white") +
      ggplot2::scale_x_continuous(name = NULL, labels = NULL) +
      ggplot2::scale_y_continuous(name = NULL, labels = NULL) +
      ggplot2::theme_light() +
      ggplot2::theme(aspect.ratio = aspect, panel.grid = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(), panel.border = ggplot2::element_blank())
  }
  if(what == "archetypes"){
    if(is.null(arch_colors[[1]])){
      vir_function <- get(palette, envir = asNamespace("viridis"))
      vir_colors <- do.call(vir_function, list(n = 6,
                                               direction = -1))

      vir_colors <- do.call(vir_function, list(n = length(arch_levels)-1,
                                               direction = direction))
      names(vir_colors) <- arch_levels[arch_levels != "Other frontiers"]
      custom_colors <- c(vir_colors, "Other frontiers" = arch_colors[[2]])
    } else {
      if(length(arch_colors[[1]]) != length(arch_levels)-1){
        stop("The number of colors in the first element of 'arch_colors' must be equal to the number of required archetypes.")
      }
      vir_colors <- arch_colors[[1]]
      names(vir_colors) <- arch_levels[arch_levels != "Other frontiers"]
      custom_colors <- c(vir_colors, "Other frontiers" = arch_colors[[2]])
    }

    p <- ggplot2::ggplot() +
      ggplot2::geom_rect(data = extent,
                         ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                         fill = background[1], color = background[1]) +
      ggplot2::geom_tile(data = df_excluded, ggplot2::aes(x = x_cell, y = y_cell), na.rm = T, fill = background[2]) +
      ggplot2::geom_tile(data = df, ggplot2::aes(x = x_cell, y = y_cell, fill = get(fill)), na.rm = T) +
      ggplot2::ggtitle(title) +
      ggplot2::scale_fill_manual(name = NULL, values = custom_colors, labels = names(arch_levels)) +
      #viridis::scale_fill_viridis(name = NULL, discrete = T,  option = "turbo", direction = direction[2]) +
      #ggplot2::scale_fill_brewer(palette = "YlGnBu", direction = 1, name = NULL, na.value = "white") +
      ggplot2::scale_x_continuous(name = NULL, labels = NULL) +
      ggplot2::scale_y_continuous(name = NULL, labels = NULL) +
      ggplot2::theme_light() +
      ggplot2::theme(aspect.ratio = aspect, panel.grid = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(), panel.border = ggplot2::element_blank())
  }
  p
}

#' Plots frontier metrics
#'
#' Generates maps of frontier metrics based on an object of class 'FrontierMetric'
#' generated with [fmetrics()]
#'
#' @param x Object of class 'FrontierMetric' generated with [fmetrics()]
#' @param metrics Frontier metrics to be plotted. Options include those calculated
#' frontier metrics in `x` (including user-defined metrics) or "all" to calculate all frontier
#' metrics available within `x`. Default is "all".
#' @param what One of the following: "both", to plot continuous values and discrete
#' classes of frontier metrics; "values", to plot only continuous values; "classes"
#' to plot only discrete classes; or "archetypes", to plot frontier archetypes.
#' Default is "both". See Details.
#' @param ncol Number of columns to arrange plots. The number of rows is automatically
#' calculated based on this number. Default is 4.
#' @param palette A string with the name of the palette to be used
#' for the metric values. Default is `"viridis"`. See Details.
#' @param direction Either 1 (default) or -1, indicating the direction of the palette. See Details.
#' @param background A vector of two strings with the names of the colors to be used
#' for the background of the plot. The first depicts the color for the region outside
#' the range of the study area, while the second depicts the color for those cells
#' within the study area that did not qualify as frontiers. Default is `c("gray90", "gray64")`.
#' See Details.
#' @param archetypes A list of archetypes or group of archetypes to be plotted when `what = "archetypes"`.
#' Each element of the list must be a vector with the names of the archetypes (as
#' in `x@archetypes`) to be plotted. If `NULL` (default) only those archetypes with a minimum area
#' coverage of 5% will be plotted. See Details.
#' @param arch_colors A list of two elements. The first element is a vector with the colors
#' to be used for the archetypes. The second element is a string with the color to be used
#' for the unnamed archetypes ("Other frontiers").
#' @param silent Logical. If `TRUE`, suppresses messages. Default is `FALSE`.
#'
#' @export
#'
#' @details
#' Plots can depict the continuous values of metrics (`what = "values"`), the discrete
#' classes of metrics (`what = "classes"`), or both (`what = "both"`).
#'
#' Archetypes can also be plotted. These can be defined as the combination
#' of different frontier metric classes that might represent a higher order of
#' frontier classification. Archetypes are automatically calculated when `x` is generated with
#' `fmetrics()`, and can be seen by running `x@archetypes`. To plot archetypes, the
#' user must indicate `what = "archetypes"`, and a list of archetypes in argument
#' `archetypes`. Each element of the list can depict a unique archetype, but
#' also encompass a combination of them. For instance, `archetypes = list(1:3, 4, 5:6)`,
#' would plot with one color archetypes 1, 2, and 3, a second color will be used for archetype 4, and a
#' third color will be used for archetypes 5 and 6. Those cells not classified with the provided
#' archetypes will be classified as "Other frontiers".
#'
#' If `archetypes = NULL`, by default the function will plot those archetypes with a
#' minimum area coverage of 5% in the study area. Note that if `what = "archetypes"`,
#' argument `metrics` will be ignored.
#'
#' Colors for archetypes can be defined in the `arch_colors` argument. The first element
#' of the list must be a vector with the colors to be used for the archetypes. If
#' `NULL`, colors will be based on the selected palette in `palette`. The
#' second element must be a string with the color to be used for those cells not
#' falling with any of the provided archetypes ("Other frontiers").
#'
#' Available palettes are those available for the function `scale_fill_viridis()`:
#' "magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", or "turbo".
#' The `direction` argument is used to reverse the palette if necessary. The user can
#' play with different background colors for better visualization.
#'
#' @examples
#' \dontrun{
#' # Plots all calculated metrics
#' fmetrics_plot(copo_metrics)
#' }
#'
#' # Plots a single metric
#' fmetrics_plot(copo_metrics, metrics = "speed", what = "values", palette = "magma")
#'
#' # Plots archetypes: 1:5, 6:10 and 11:15. Each group with a different color
#' fmetrics_plot(copo_metrics, what = "archetypes",
#'               archetypes = list(1:5, 6:10, 11:15),
#'               arch_colors = list(c("firebrick", "blue", "yellow"), "gray42"))
fmetrics_plot <- function(x,
                          metrics = "all",
                          what = "both",
                          ncol = 4,
                          palette = "viridis",
                          direction = 1,
                          background = c("white", "gray64"),
                          archetypes = NULL,
                          arch_colors = list(NULL, "gray42"),
                          silent = FALSE){

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
    metrics <- c(x@metrics, x@ud_metrics)
  } else {
    metrics <- unique(metrics)
    # fmetrics_list <- c("baseline", "baseline_frag", "loss", "loss_frag", "speed",
    #                    "activeness", "left", "onset")
    # metrics <- fmetrics_list[fmetrics_list %in% metrics]
  }

  notfound <- character()
  for(i in 1:length(metrics)){
    if(grepl("ud_", metrics[i])){
      if(!metrics[i] %in% x@ud_metrics)
        notfound <- append(notfound, metrics[i])
    } else {
      if(!metrics[i] %in% x@metrics)
        notfound <- append(notfound, metrics[i])
    }
  }
  if(length(notfound) > 0){
    stop(" The following metrics could not be found as a calculated metric in 'x': ", paste0(notfound, sep = " "))
  }

  aspect <- (x@extent[4] - x@extent[3])/(x@extent[2] - x@extent[1])

  # Plotting archetypes
  if(what == "archetypes"){
    if(is.null(archetypes)){
      archetypes <- x@archetypes[x@archetypes$percentage > 5, "archetype"]
      if(length(archetypes) == 0) stop("There are no archetypes with a minimum area coverage of 5%. Please select desired archetypes manually.")
      if(!silent) message("Archetypes with a minimum area coverage of 5% were used.")
    } else {
      for(i in 1:length(archetypes)){
        if(any(!archetypes[[i]] %in% x@archetypes$archetype)){
          stop("The following archetypes were not found in the dataset: ",
               paste0(archetypes[!archetypes[[i]] %in% x@archetypes$archetype], sep = " "))
        }
      }
    }

    for(i in 1:length(archetypes)){
      x@data$Archetype_label[x@data$archetype %in% archetypes[[i]]] <- archetypes[i]
    }
    x@data$Archetype_label[!x@data$archetype %in% unlist(archetypes)] <- "Other frontiers"
    arch_levels <- c(archetypes, "Other frontiers")
    names(arch_levels) <- c(archetypes, "Other frontiers")
    x@data$Archetype_label <- factor(x@data$Archetype_label, levels = arch_levels)

    plot <- ff_plot_raster(x@data, x@excluded_cells, "Archetype_label", "archetypes", "Archetypes",
                   aspect, x@extent, palette, direction, background, arch_levels, arch_colors)
    return(plot)
  }

  plots <- list()
  if("baseline" %in% metrics){
    if(what %in% c("both", "values"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "baseline", "values", "Baseline forest [%]",
                                                 aspect, x@extent,
                                                 palette, direction, background)
    if(what %in% c("both", "classes"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "baseline.c", "classes", "Baseline forest (classes)",
                                                 aspect, x@extent,
                                                 palette, direction, background)
  }
  if("baseline_frag" %in% metrics){
    if(what %in% c("both", "values"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "baseline_frag", "values", "Baseline forest fragmentation [m/ha]",
                                                 aspect, x@extent,
                                                 palette, direction, background)
    if(what %in% c("both", "classes"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "baseline_frag.c", "classes", "Baseline forest fragmentation (classes)",
                                                 aspect, x@extent,
                                                 palette, direction, background)
  }
  if("loss" %in% metrics){
    if(what %in% c("both", "values"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "loss", "values", "Forest loss [%]",
                                                 aspect, x@extent,
                                                 palette, direction, background)
    if(what %in% c("both", "classes"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "loss.c", "classes", "Forest loss (classes)",
                                                 aspect, x@extent,
                                                 palette, direction, background)
  }
  if("loss_frag" %in% metrics){
    if(what %in% c("both", "values"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "loss_frag", "values", "Forest loss fragmentation [m/ha]",
                                                 aspect, x@extent,
                                                 palette, direction, background)
    if(what %in% c("both", "classes"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "loss_frag.c", "classes", "Forest loss fragmentation (classes)",
                                                 aspect, x@extent,
                                                 palette, direction, background)
  }
  if("speed" %in% metrics){
    if(what %in% c("both", "values"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "speed", "values", "Speed [Km\U00B2/year]",
                                                 aspect, x@extent,
                                                 palette, direction, background)
    if(what %in% c("both", "classes"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "speed.c", "classes", "Speed (classes)",
                                                 aspect, x@extent,
                                                 palette, direction, background)
  }
  if("activeness" %in% metrics){
    plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                               "activeness", "classes", "Activeness (classes)",
                                               aspect, x@extent,
                                               palette, direction, background)
  }
  if("left" %in% metrics){
    if(what %in% c("both", "values"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "left", "values", "Forest left [%]",
                                                 aspect, x@extent,
                                                 palette, direction, background)
    if(what %in% c("both", "classes"))
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 "left.c", "classes", "Forest left (classes)",
                                                 aspect, x@extent,
                                                 palette, direction, background)
  }
  if("onset" %in% metrics){
    plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                               "onset", "values", "Onset [year]",
                                               aspect, x@extent,
                                               palette, direction, background)
  }
  # Plotting user-defined metrics
  if(any(grepl("ud_", metrics))){
    ud_metrics <- metrics[grepl("ud_", metrics)]
    for(i in 1:length(ud_metrics)){
      plots[[length(plots)+1]] <- ff_plot_raster(x@data, x@excluded_cells,
                                                 ud_metrics[i], "values", ud_metrics[i],
                                                 aspect, x@extent,
                                                 palette, direction, background)
    }
  }

  if(ncol > length(plots)) ncol <- length(plots)
  suppressMessages(egg::ggarrange(plots = plots, ncol = ncol))
}

