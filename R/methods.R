#' @include classes.R
#NULL

if(!isGeneric("show"))
  methods::setGeneric("show", function(object) standardGeneric("show"))

#' Show 'init_FrontierMetric' object
#'
#' @param object Prints relevant information about a 'FrontierMetric' object.
#'
#' @export
methods::setMethod(f = "show", signature = "init_FrontierMetric",
                   definition =
                     function(object){
                       cat("Class               : init_FrontierMetric\n")
                       cat("Time-frame          : ", object@time_frame[1], " - ", object@time_frame[2], "\n", sep = "")
                       if(round(object@grain[[1]][1], digits = 10) == round(object@grain[[1]][2], digits = 10)){
                         met <- round(object@grain[[1]][1]*111120.0000012, digits = 1)
                         cat("Fine grain (\u00b0|m)    : ", format(round(object@grain[[1]][1], digits = 4), nsmall = 4), " | ~", met, "\n", sep = "")
                         met <- round(object@grain[[2]][1]*111120.0000012, digits = 1)
                         cat("Coarse grain (\u00b0|m)  : ", format(round(object@grain[[2]][2], digits = 4), nsmall = 4) , " | ~", met, "\n", sep = "")
                       } else {
                         met1 <- round(object@grain[[1]][1]*111120.0000012, digits = 1)
                         met2 <- round(object@grain[[1]][2]*111120.0000012, digits = 1)
                         cat("Fine grain (\u00b0|m)    : ", paste0(format(round(object@grain[[1]],
                                                                  digits = 4),
                                                            nsmall = 4), sep = " "),
                             "| ~", met1, " ", met2, "\n", sep = "")
                         met1 <- round(object@grain[[2]][1]*111120.0000012, digits = 1)
                         met2 <- round(object@grain[[2]][2]*111120.0000012, digits = 1)
                         cat("Coarse grain (\u00b0|m)  : ", paste0(format(round(object@grain[[2]],
                                                                  digits = 4),
                                                            nsmall = 4), sep = " "), "| ~", met1, " ", met2, "\n", sep = "")
                       }
                       cat("Frontier definition :\n")
                       cat("  Min. cover        : ", object@min_cover, "%", "\n", sep = "")
                       cat("  Min. loss rate    : ", object@min_rate, "%", "\n", sep = "")
                       cat("  Window size       : ", object@window, " years", "\n", sep = "")
                     }
)

#' Show 'FrontierMetric' object
#'
#' @param object Prints relevant information about a 'FrontierMetric' object.
#'
#' @export
methods::setMethod(f = "show", signature = "FrontierMetric",
                   definition =
                     function(object){
                       cat("Class               : FrontierMetric\n")
                       if(length(object@metrics) > 0){
                         cat("Metrics             :", object@metrics, "\n")
                       } else {
                         cat("Metrics             : -\n")
                       }
                       if(length(object@ud_metrics) > 0){
                         cat("User-defined metrics:", object@ud_metrics, "\n")
                       } else {
                         cat("User-defined metrics: -\n")
                       }
                       cat("Time-frame          : ", object@time_frame[1], " - ", object@time_frame[2], "\n", sep = "")
                     }
)

ff_show_classes <- function(x){
  if(is.numeric(x[[1]])){
    foo <- x[[1]][!x[[1]] %in% c(-Inf, Inf)]
    intervals <- levels(cut(min(foo):max(foo), x[[1]]))
    intervals <- gsub(" ", "", intervals)
    classes <- paste(paste0(paste0("    ", x[[2]], sep = " "), intervals), collapse = "\n")
    classes <- paste0("\n", classes)
  } else {
    classes <- x[[1]]
  }
  classes
}

#' Show 'FrontierMetric_breaks' object
#'
#' @param object Prints relevant information about a 'FrontierMetric_breaks' object.
#'
#' @export
methods::setMethod(f = "show", signature = "FrontierMetric_breaks",
                   definition =
                     function(object){
                       cat("Rules to define frontier metrics' classes\n\n")
                       cat("- Baseline forest [%]: ")
                       cat(ff_show_classes(object@baseline), "\n")
                       cat("- Baseline fragmentation [m/ha]: ")
                       cat(ff_show_classes(object@baseline_frag), "\n")
                       cat("- Forest loss [%]: ")
                       cat(ff_show_classes(object@loss), "\n")
                       cat("- Forest loss fragmentation [m/ha]: ")
                       cat(ff_show_classes(object@loss_frag), "\n")
                       cat("- Speed [km\U00B2/year]: ")
                       cat(ff_show_classes(object@speed), "\n")
                       # Activeness
                       # k <- object@activeness
                       # window <- k$window
                       # cat("- Activeness: \n", sep = "")
                       # for(i in 1:length(object@activeness)){
                       #   cat(paste0("    ", names(object@activeness)[i], ": ", object@activeness[i], "\n"))
                       # }
                       # cat("  old       ",
                       #     paste0("... ,[",(k[[1]]-window-1), ",", (k[[1]]-2),"], [",
                       #            (k[[1]]-window), ",", (k[[1]]-1), "], [",
                       #            (k[[1]]-(window-1)), ",", (k[[1]]), "]\n"),
                       #     "  active    ",
                       #     paste0("[", (k[[1]]-3), ",", (k[[1]]+(window-4)), "], [",
                       #            (k[[1]]-2), ",", (k[[1]]+(window-3)), "] ... [",
                       #            (k[[2]]-1), ",", (k[[2]])+(window-2), "]\n"),
                       #     "  emerging  ",
                       #     paste0("[", k[[2]], ",", k[[2]]+(window-1), "]\n\n"), sep = "")
                       cat("- Forest left [%]: ")
                       cat(ff_show_classes(object@left))
                       # cat("- Onset [year]:\n")
                       # cat(paste0("    min. number of years = ", object@onset))
                     }
)

print_fdf <- function(x, chars){
  for (i in 1:nrow(x)) {
    cat(sprintf(paste0("    %-", chars+1, "s %", chars+1, "s\n"),
                x$class[i], x$total.area[i]))
  }
}

#' Show 'FrontierMetric_summary' object
#'
#' @param object Prints relevant information about a 'FrontierMetric_summary' object.
#'
#' @export
methods::setMethod(f = "show", signature = "FrontierMetric_summary",
                   definition =
                     function(object){
                       dd_labels <- data.frame(metric = c("baseline", "baseline_frag", "loss", "loss_frag", "speed",
                                                          "left", "activeness", "onset"),
                                               label = c("Baseline forest [%]", "Baseline fragmentation [m/ha]", "Forest loss [%]",
                                                         "Forest loss fragmentation [m/ha]", "Speed [km\u00b2/year]",
                                                         "Forest left [%]", "Activeness", "Onset [year]"),
                                               label2 = c("Baseline forest", "Baseline fragmentation", "Forest loss",
                                                          "Forest loss fragmentation", "Speed",
                                                          "Forest left", "Activeness", "Onset"))
                       cat("- Summary statistics (absolute values)\n\n")
                       formatted_df <- round(object@summary_stats, digits = 2)
                       dd_labels2 <- dd_labels[sapply(colnames(formatted_df),
                                                      function(x) which(x == dd_labels$metric)), ]
                       colnames(formatted_df) <- dd_labels2$label
                       chars <- max(sapply(colnames(formatted_df), nchar))
                       for(i in 1:ncol(formatted_df)){
                         formatted_df[[i]] <- format(formatted_df[[i]],
                                                     width = chars,
                                                     justify = "right")
                       }
                       print(formatted_df)
                       cat("\n- Total area (km\u00b2) per metric class\n\n")
                       metrics <- unique(object@classes_areas$metric)
                       chars <- max(sapply(as.character(object@classes_areas$class), nchar))
                       for(i in 1:length(metrics)){
                         cat(paste0(dd_labels[dd_labels$metric == metrics[i], ]$label2), "\n")
                         object@classes_areas$total.area <- round(object@classes_areas$total.area, digits = 2)
                         foo <- object@classes_areas[object@classes_areas$metric == metrics[i], 2:3]
                         print_fdf(foo, chars)
                       }
                       print(object@hists)
                     }
)

# Show 'FrontierMetric_raster' object
#
# @param object Prints relevant information about a 'FrontierMetric_raster' object.
#
# @export
# methods::setMethod(f = "show", signature = "FrontierMetric_raster",
#                    definition =
#                      function(object){
#                        cat("Class      : FrontierMetric_raster\n")
#                        cat("Metrics    :", paste(names(object@raster_layers)[!grepl("\\.c",
#                                                                                     names(object@raster_layers))],
#                                                   collapse = " "), "\n")
#                        cat("Time-frame : ", object@time_frame[1], " - ", object@time_frame[2], "\n", sep = "")
#                      }
# )
