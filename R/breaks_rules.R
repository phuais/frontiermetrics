#' Defines the rules to categorize frontier metrics
#'
#' For those continuous frontier metrics, defines rules to generate discrete classes.
#'
#' @param baseline,baseline_frag,loss,loss_frag,speed,left A list defining the
#' rules for the discrete classes of continuous frontier metrics. See Details.
#'
#' @details
#' For each continuous metric, the user must provide a list. The first element of the list
#' defines the algorithm to be used for the categorization. The second element of  the list must be a character vector with the names
#' for each class that will be generated.
#'
#' The following algorithms are available (first element of the list):
#'
#' (1) "jenks" (default): Jenks natural breaks classification (uses BAMMtools::getJenksBreaks).
#' The number (n) of intervals is given by the number of categories provided
#' in the second element of the list.
#'
#' (2) "equal": splits the numeric range between minimum and maximum metric values into n intervals of equal
#' width. The number (n) of intervals is given by the number of categories provided
#' in the second element of the list.
#'
#' (3) "quantile": splits observations by quantiles so that each bin contains (roughly)
#' the same number of observations (frontier cells). Quantiles are calculated given
#' the number of categories provided in the second element of the list. For instance,
#' if 4 categories are provided, the following intervals will be used: 0-0.25, 0.25-0.5, 0.5-0.75, and 0.75-1.
#'
#' (4) Alternatively, a numeric vector with two or more unique cut points can be
#' provided. For example: `c(5, 10, 55, Inf)`, and the categories (second element
#' of the list) `c("Low", "Medium", "High")`, where "Low": 5-10, "Medium": 10-55, and
#' "High": 55-Inf.
#'
#' When assigned to an object, the new definition of classes can be passed to [fmetrics()],
#' inside the argument `breaks`, to generate frontier metrics's classes with a new definition.
#'
#' @return An object of class 'FronterMetric_classes' to be used in argument `breaks` in [fmetrics()].
#' @export
#'
#' @examples
#' # Explore default rules
#' breaks_rules()
#'
#' # Define new parameters for baseline
#' #   Low: between 5 and 20
#' #   Medium: between 20 and 70
#' #   High: Higher than 70
#' breaks_1 <- breaks_rules(baseline = list(c(5, 20, 70, Inf),
#'                                          c("Low", "Medium", "High")))
#' # The rules for the rest of the metrics will be those by default
#'
#' # Shows new defined rules
#' breaks_1
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
