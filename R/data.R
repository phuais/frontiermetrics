#' 'FrontierMetric' object
#'
#' An object of class 'FrontierMetric' generated with [fmetrics()] for use in package examples.
#' It cointains the values of all available frontier metrics for the surroundings of the
#' Copo National Park (Argentina).
#'
#' The main internal object is a data.frame (accesible through `ed_metrics@data`) with information
#' about the values of two landscape metrics: "pland" (percentage of landscape) and "ed" (edge density).
#'
#' The object was created from the MultiLand object named "ernesdesign", which
#' received two raster layers from a small portion of the ecoregion "El Chaco" as main inputs. The main rasterlayer
#' was provided by the project "MapBiomas Chaco" for the year 2000.
#' The extra rasterlayer contained the NDVI values of cells within the same extent of the main rasterlayer, and was provided by Landsat.
#'
#' @seealso See the examples sections of [fmetrics()], [fmetrics_plot()], [fmetrics_summary()] and [fmetrics_rast()]
#' for more context.
"copo_metrics"

#' Data frame containing the links to each tile of tree cover from Global Forest Watch (Hansen et al. 2013).
#' It is intended to be used for internal functions of the package.
#'
#' @references
#' Hansen, M. C., Potapov, P. V., Moore, R., Hancher, M., Turubanova, S. A., Tyukavina, A.,
# ... & Townshend, J. R. (2013). High-resolution global maps of 21st-century forest cover change. science, 342(6160), 850-853.
"GFL_2024_v1.12_treecover2000"

#' Data frame containing the links to each tile of forest loss from Global Forest Watch (Hansen et al. 2013).
#' It is intended to be used for internal functions of the package.
#'
#' @references
#' Hansen, M. C., Potapov, P. V., Moore, R., Hancher, M., Turubanova, S. A., Tyukavina, A.,
#' ... & Townshend, J. R. (2013). High-resolution global maps of 21st-century forest cover change. science, 342(6160), 850-853.
"GFL_2024_v1.12_loss"

#' URLs to download data for example use.
"frontiermetrics_data"
