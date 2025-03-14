#' Sets the rules to categorize frontier metrics
#'
#' Defines rules to generate discrete categories for individual frontier metrics.
#'
#' @param baseline,loss,fragmentation,speed,activeness,left A list defining the
#' rules to define discrete classes for each frontier metric. See Details.
#'
#' @details
#' By default, in the scenario of default definition of frontiers, rules to
#' categorize frontier metrics are the following:
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
#'
#' To define new rules, provide a list for each frontier metric. For all metrics except "activeness"
#' the rules should follow the format used in [cut()]. Specifically, the first element
#' of the list should be a numeric vector of two or more unique cut points (breaks), and the second
#' element should be a character vector of labels corresponding to each resulting category. For
#' instance, classes for frontier metric "baseline" could be ruled as follows:
#'
#' ````
#' baseline = list(c(5, 10, 55, Inf),
#'                 c("Low", "Medium", "High"))
#' ````
#'
#' Note that, for baseline, by default the lowest value is 5%, which is the lowest limit defined by
#' default to define a frontier when running [init_fmetrics()]. This lowest limit should be changed if
#' by a frontier was defined with a different threshold in the percentage of initial woodland cover
#' when running [init_fmetrics()].
#'
#' The "activeness" metric, which is discrete by definition, requires a list of three elements:
#' (1) the last year of a given-consecutive year period a frontier was active to be labeled "Old";
#' (2) the first year of a given-consecutive year period a frontier became active to be labeled "Emerging".
#' (3) An whole number representing the number of years considered in the activeness calculation. This is for
#' informational purposes only, as the actual window is set when running [init_fmetrics()].
#'
#' A frontier will be classified as simply "Active" if it was active during any of all given-consecutive
#' years between the "Old" and "Emerging" periods.
#'
#' For instance, by default:
#'
#' ````
#' activeness = list(old = 2018, emerging = 2019, window = 5)
#' ````
#'
#' When assigned to an object, the new definition of classes can be passed to [fmetrics()],
#' inside the argument `classes`, to generate frontier metrics's classes with a new definition.
#'
#' For a clearer understanding of rule definition, see the example below.
#'
#' @return An object of class 'FronterMetric_classes' to be used with [fmetrics()].
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
#' #   Old frontiers: last year active in 2015
#' #   Emerging frontiers: first year active in 2018
#' class_par <- init_classes(baseline = list(c(5, 20, 70, Inf),
#'                                           c("Low", "Medium", "High")),
#'                           activeness = list(old = 2015, emerging = 2018, window = 5))
#'
#' # Shows new defined classes
#' class_par
#'
#' # This object can be passes to [fmetrics()] inside the argument
#' # `classes`, to generate frontier metrics's classes with this new definition
#' }
init_classes <- function(baseline = list(c(5, 10, 55, Inf),
                                        c("Low", "Medium", "High")),
                        loss = list(c(-Inf, 20, 50, Inf),
                                    c("Low", "Medium", "High")),
                        fragmentation = list(c(-Inf, 10, 20, Inf),
                                             c("Low", "Medium", "High")),
                        speed = list(c(-Inf, 0.05, 0.25, Inf),
                                     c("Slow", "Medium", "Fast")),
                        activeness = list(old = 2018, emerging = 2019, window = 5),
                        left = list(c(-Inf, 33, 66, Inf),
                                    c("Low", "Medium", "High"))){

  # Argument's checking
  environment(check_init_classes) <- environment()
  chk <- check_init_classes()
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

  out <- new("FrontierMetric_classes")
  out@baseline <- baseline
  out@loss <- loss
  out@fragmentation <- fragmentation
  out@speed <- speed
  out@activeness <- activeness
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
