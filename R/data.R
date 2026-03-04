#' 'FrontierMetric' object
#'
#' An object of class 'FrontierMetric' generated with [fmetrics()] for its use in examples.
#' It contains the values of all available frontier metrics for the surroundings of the
#' Copo National Park (Argentina).
#'
#' The main internal object is a data frame (accesible through `copo_metrics@data`) containing all
#' metric values for this study area.
#'
#' @seealso See the examples sections of [fmetrics()], [fmetrics_plot()], [fmetrics_summary()] and [fmetrics_rast()]
#' for more context.
"copo_metrics"

#' Data frame containing the links to each tile of tree canopy cover for year 2000 from Global Forest Watch datasets (Hansen et al. 2013).
#' It is intended to be used for internal functions of the package.
#'
#' @references
#' Hansen, M. C., Potapov, P. V., Moore, R., Hancher, M., Turubanova, S. A., Tyukavina, A.,
# ... & Townshend, J. R. (2013). High-resolution global maps of 21st-century forest cover change. science, 342(6160), 850-853.
"GFL_2024_v1.12_treecover2000"

#' Data frame containing the links to each tile of year of forest loss until 2024 from Global Forest Watch datasets (Hansen et al. 2013).
#' It is intended to be used for internal functions of the package.
#'
#' @references
#' Hansen, M. C., Potapov, P. V., Moore, R., Hancher, M., Turubanova, S. A., Tyukavina, A.,
#' ... & Townshend, J. R. (2013). High-resolution global maps of 21st-century forest cover change. science, 342(6160), 850-853.
"GFL_2024_v1.12_loss"

#' URLs to download data to run examples.
"frontiermetrics_data"
