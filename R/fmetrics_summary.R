#' Calculates summary statistics of frontier metrics
#'
#' Calculates statistics of continuous values of frontier metrics, plots
#' histograms, and calculates the total area of each discrete class of frontier metrics.
#'
#' @param x Object of class 'FrontierMetric' generated with [fmetrics()].
#' @param metrics Frontier metrics to consider. Options include one or more metrics contained in `x`,
#' or "all" to calculate all frontier metrics available within `x`. Default is "all".
#' @param show_plots Logical. If `TRUE` (default), histograms will be plotted when
#' running the function.
#'
#' @details
#' Access to statistics once generated the object can be done with `@summary_stats` and `@classes_areas`.
#' To access and plot the histograms, use `@hists`.
#'
#' User-defined metrics will not be considered.
#'
#' @return A list with three elements:
#' (1) a data frame with basic summary statistics;
#' (2) a data frame with the total area of each class within each frontier metric; and
#' (3) a ggplot object with histograms of frontier metrics.
#'
#' @export
#'
#' @examples
#' summ <- fmetrics_summary(copo_metrics)
#' summ
#'
#' # Access to summary statistics
#' summ@summary_stats
#'
#' # Access to classes areas
#' summ@classes_areas
#'
#' # Plotting of histograms
#' summ@hists
fmetrics_summary <- function(x, metrics = "all", show_plots = TRUE){

  # Argument's checking
  environment(check_fmetrics_summary) <- environment()
  chk <- check_fmetrics_summary()
  if(length(chk[[1]]) > 0){
    for(w in 1:length(chk[[1]])){
      warning(strwrap(chk[[1]], prefix = "\n", initial = ""), call. = FALSE)
    }
  }
  if(length(chk[[2]]) > 0){
    errors <- chk[[2]]
    stop(strwrap(errors, prefix = "\n", initial = "\n"))
  } else {
    if(length(chk) > 2){
      objs <- names(chk)
      for(i in 3:length(chk)){
        assign(objs[i], chk[[i]])
      }
    }
  }

  if("all" %in% metrics){
    metrics <- x@metrics
  } else {
    metrics <- unique(metrics)
    fmetrics_list <- c("baseline", "baseline_frag", "loss", "loss_frag",
                       "speed", "activeness", "left", "onset")
    metrics <- fmetrics_list[fmetrics_list %in% metrics]
  }

  df <- data.frame(baseline = rep(NA, 10),
                   baseline_frag = NA,
                   loss = NA,
                   loss_frag = NA,
                   speed = NA,
                   left = NA,
                   onset = NA)
  df <- df[, metrics[metrics != "activeness"], drop = F]

  get_mode <- function(y) {
    ux <- na.exclude(unique(y))
    ux[which.max(tabulate(match(y, ux)))]
  }

  rownames(df) <- c("min", "max", "mean", "mode", "sd",
                    "q0.05", "q0.25", "q50", "q75", "q95")

  get_meds <- function(x, df, metric){
    vec <- x@data[, ..metric][[1]]
    if(metric == "onset"){
      df[, metric][1] <- min(vec, na.rm = T)
      df[, metric][2] <- max(vec, na.rm = T)
      df[, metric][4] <- get_mode(vec)
    } else {
      df[, metric][1] <- min(vec, na.rm = T)
      df[, metric][2] <- max(vec, na.rm = T)
      df[, metric][3] <- mean(vec, na.rm = T)
      df[, metric][5] <- sd(vec, na.rm = T)
      df[, metric][6] <- quantile(vec, 0.05, na.rm = T)
      df[, metric][7] <- quantile(vec, 0.25, na.rm = T)
      df[, metric][8] <- quantile(vec, 0.50, na.rm = T)
      df[, metric][9] <- quantile(vec, 0.75, na.rm = T)
      df[, metric][10] <- quantile(vec, 0.95, na.rm = T)
    }
    df
  }

  if(length(metrics) == 1 & metrics[1] == "activeness"){
    df <- data.frame()
  } else {
    for(i in 1:ncol(df)){
      df <- get_meds(x, df, metrics[metrics != "activeness"][i])
    }
  }

  # Classes areas
  fms <- data.frame(metric = c("baseline", "baseline_frag", "loss", "loss_frag", "speed", "activeness", "left", "onset"),
                    metric_abb = c("baseline.c", "baseline_frag.c", "loss.c", "loss_frag.c", "speed.c", "activeness", "left.c", "onset"))
  fms <- fms[fms$metric %in% metrics, ]

  ddt <- data.frame()
  for(i in 1:nrow(fms)){
    dd <- aggregate(cell_area ~ x@data[[fms$metric_abb[i]]], x@data, sum)
    colnames(dd)[1] <- "class"
    dd$metric <- fms$metric[i]
    dd$class <- factor(dd$class)
    ddt <- rbind(ddt, dd)
  }
  ddt <- ddt[, c("metric", "class", "cell_area")]
  colnames(ddt) <- c("metric", "class", "total.area")

  # Histograms
  if(length(metrics) == 1 & metrics[1] == "activeness"){
    plot <- NULL
  } else {
    cc_mets <- c("baseline", "baseline_frag", "speed", "loss", "loss_frag", "left", "onset")
    cc_mets <- cc_mets[which(cc_mets %in% metrics)]

    dd <- data.table::melt(x@data[, c("id_cell", cc_mets), with = F],
                           idvar = "id_cell", measure.vars = 2:(length(cc_mets)+1))
    dd_labels <- data.frame(variable = c("baseline", "baseline_frag", "loss", "loss_frag", "speed", "left", "onset"),
                            time =  c("baseline", "baseline_frag", "loss", "loss_frag", "speed", "left", "onset"),
                            label = c("Baseline forest [%]", "Baseline fragmentation [m/ha]", "Forest loss [%]",
                                      "Forest loss fragmentation [m/ha]", "Speed [km\u00b2/year]",
                                      "Forest left [%]", "Onset [year]"))
    dd <- merge(dd, dd_labels[, c("variable", "label")])

    # Mean and median lines
    df2 <- df
    df2$stats <- rownames(df2)
    summ_long <- reshape(df2, direction = "long", idvar = "stats",
                         varying = list(1:length(metrics[metrics != "activeness"])),
                         times = colnames(df2)[1:length(metrics[metrics != "activeness"])], v.names = "value")


    summ_long <- merge(summ_long, dd_labels[, c("time", "label")])

    dd$label <- factor(dd$la)
    vlines <- summ_long[summ_long$stats %in% c("mean", "q50") & is.finite(summ_long$value),  ]

    if(nrow(vlines) > 0){
      vlines$stats <- factor(
        vlines$stats,
        levels = c("mean", "q50"),
        labels = c("Mean", "Median")
      )
    }

    plot <- ggplot2::ggplot(dd, ggplot2::aes(x = value)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(count / sum(count) * 100)),
                              color = "white", width = 0.2, bins = 30, na.rm = TRUE) +
      ggplot2::facet_wrap(~label, scales = "free") +
      ggplot2::scale_x_continuous(name = "") +
      ggplot2::scale_y_continuous(name = "Frequency [%]") +
      ggplot2::theme_light() +
      ggplot2::theme(aspect.ratio = 1, strip.text = ggplot2::element_text(color = "black"),
        legend.position = "bottom")

    if(nrow(vlines) > 0){
      plot <- plot +
        ggplot2::geom_vline(data = vlines, ggplot2::aes(xintercept = value, color = stats)) +
        ggplot2::scale_color_manual(name = NULL, values = c("Mean" = "red", "Median" = "green4"))
    }
  }

  out <- new("FrontierMetric_summary",
             summary_stats = df,
             classes_areas = ddt,
             hists = plot)

  if(show_plots){
    print(out@hists)
    return(out)
  } else {
    return(out)
  }
}
