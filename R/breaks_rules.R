#' Sets the rules to categorize frontier metrics
#'
#' Defines rules to generate discrete categories for continuous frontier metrics.
#'
#' @param baseline,baseline_frag,loss,loss_frag,speed,left A list defining the
#' rules to define discrete classes for each frontier metric. See Details.
#'
#' @details
#' For each metric, the user must provide a list. The first element of the list
#' defines the algorithm to be used for the categorization. The second element of  the list must be a character vector with the names
#' for each category that will be generated.
#'
#' The following algoritms are available (first element of the list):
#'
#' (1) "jenks" (default): Jenks natural breaks classification (uses BAMMtools::getJenksBreaks).
#' The number (n) of intervals is given by the number of categories provided
#' in the second element of the list.
#'
#' (2) "equal": splits the numeric range [min(y), max(y)] of metric values (`y`) into n intervals of equal
#' width. The number (n) of intervals is given by the number of categories provided
#' in the second element of the list.
#'
#' (3) "quantile": splits observations by quantiles so that each bin contains (roughly)
#' the same number of observations (frontier cells). Quantiles are calculated given
#' the number of categories provided in the second element of the list. For instance,
#' if 4 categories are provided, the following intervals be used as cuts: 0-0.25, 0.25-0.5, 0.5-0.75, and 0.75-1.
#'
#' (4) Alternatively, a numeric vector with two or more unique cut points can be
#' provided. For example: `c(5, 10, 55, Inf)`, and the categories (second element
#' of the list) could be `c("Low", "Medium", "High")`, where "Low": 5-10, "Medium": 10-55, and
#' "High": 55-Inf.
#'
#' To define new rules, the user must provide a list for each frontier metric. For all metrics except "activeness"
#' the rules should follow the format used in [cut()]. Specifically, the first element
#' of the list should be a numeric vector of two or more unique cut points (breaks), and the second
#' element should be a character vector of labels corresponding to each resulting category. For
#' instance, classes for frontier metric "baseline" could be ruled as follows:
#'
#' When assigned to an object, the new definition of classes can be passed to [fmetrics()],
#' inside the argument `breaks`, to generate frontier metrics's classes with a new definition.
#'
#' @return An object of class 'FronterMetric_classes' to be used in argument `breaks` within [fmetrics()].
#' @export
#'
#' @examples
#' \dontrun{
#' # Explore default parameters
#' init_classes()
#'
#' # Define new parameters for baseline and activeness
#' # Baseline
#' #   Low: between 5 and 20
#' #   Medium: between 20 and 70
#' #   High: Higher than 70
#' # Activeness
#' #   Old frontiers: last year active in 2019
#' #   Emerging frontiers: first year active in 2020
#' class_par <- init_classes(baseline = list(c(5, 20, 70, Inf),
#'                                           c("Low", "Medium", "High")),
#'                           activeness = list(old = 2019, emerging = 2020, window = 5))
#'
#' # Shows new defined classes
#' class_par
#'
#' # This object can be passes to [fmetrics()] inside the argument
#' # `classes`, to generate frontier metrics's classes with this new definition
#' }
breaks_rules <- function(baseline = list("jenks", c("Low", "Medium", "High")),
                         baseline_frag = list("jenks", c("Low", "Medium", "High")),
                         loss = list("jenks", c("Low", "Medium", "High")),
                         loss_frag = list("jenks", c("Low", "Medium", "High")),
                         speed = list("jenks", c("Slow", "Medium", "Fast")),
                         left = list("jenks", c("Low", "Medium", "High"))) {


  # Argument's checking
  environment(check_breaks_rules) <- environment()
  chk <- check_breaks_rules()
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

  out <- new("FrontierMetric_breaks")
  out@baseline <- baseline
  out@baseline_frag <- baseline_frag
  out@loss <- loss
  out@loss_frag <- loss_frag
  out@speed <- speed
  out@left <- left

  return(out)
}

# fm_updateclass <- function(x, class){
#   if("baseline" %in% x@metrics){
#     x@data$Wct0_class <- cut(x@data$Wct0, class@baseline[[1]],
#                              labels = class@baseline[[2]])
#   }
#   if("loss" %in% x@metrics){
#     x@data$PTWl_class <- cut(x@data$PTWl, class@loss[[1]],
#                              labels = class@loss[[2]])
#   }
#   if("speed" %in% x@metrics){
#     x@data$Sp_class <- cut(x@data$Sp, class@speed[[1]],
#                            labels = class@speed[[2]])
#   }
#   if("fragmentation" %in% x@metrics){
#     x@data$ED_class <- cut(x@data$Ed, class@fragmentation[[1]],
#                            labels = class@fragmentation[[2]])
#   }
#   if("left" %in% x@metrics){
#     x@data$Wleft_class <- cut(x@data$Wleft, class@left[[1]],
#                               labels = class@left[[2]])
#   }
#
#   return(x)
# }
