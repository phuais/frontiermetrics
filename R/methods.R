#' @include classes.R
#NULL

if(!isGeneric("show"))
  methods::setGeneric("show", function(object) standardGeneric("show"))

#' Shows 'init_FrontierMetric' object
#'
#' @param object Prints relevant information about a 'FrontierMetric' object.
#'
#' @return No return value.
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

#' Shows 'FrontierMetric' object
#'
#' @param object Prints relevant information about a 'FrontierMetric' object.
#'
#' @return No return value.
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

#' Shows 'FrontierMetric_breaks' object
#'
#' @param object Prints relevant information about a 'FrontierMetric_breaks' object.
#'
#' @return No return value.
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
                       cat("- Forest left [%]: ")
                       cat(ff_show_classes(object@left))
                     }
)

print_fdf <- function(x, chars){
  for (i in 1:nrow(x)) {
    cat(sprintf(paste0("    %-", chars+1, "s %", chars+1, "s\n"),
                x$class[i], x$total.area[i]))
  }
}

#' Shows 'FrontierMetric_summary' object
#'
#' @param object Prints relevant information about a 'FrontierMetric_summary' object.
#'
#' @return No return value.
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

