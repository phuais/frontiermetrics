#' @import methods
## @importFrom methods is show setClassUnion
#' @importFrom data.table data.table as.data.table
#' @importClassesFrom terra SpatRaster
#' @importClassesFrom data.table data.table

setClassUnion("null_rast", c("NULL", "SpatRaster"))
setClassUnion("null_ds", c("NULL", "data.table"))
setClassUnion("null_num", c("NULL", "numeric"))

#' Class 'init_FrontierMetric'
#'
#' Objects of class 'init_FrontierMetric' are generated with [init_fmetrics()], and hold
#' the necessary objects to calculate frontier metrics with [fmetrics()]. The main
#' object is under `@data`, which is a data frame containing -among other relevant data- the amount of forest
#' cover in the first year of the time series, and the amount of forest loss
#' in each year of the time series, for each individual cell of the study area.
#'
#' @slot data Main dataset containing data about forest cover and forest loss.
#' @slot coords Central coordinates of bigger cells.
#' @slot time_frame Year range of the studied time series.
#' @slot initial_fc_col Name of the column containing the data about initial forest cover.
#' @slot fl_cols Name of the columns containing the data about annual forest loss.
#' @slot extent Extent of the study area.
#' @slot grain Smallest and biggest studied grain (resolution).
#' @slot aggregation Pre-defined aggregation during running of [init_fmetrics()].
#' @slot is_series Logical. Whether the dataset was generated with direct databases from Global Forest Watch (FALSE) or from a cover time-series (TRUE).
#' @slot min_treecover Pre-defined minimum tree cover during running of [init_fmetrics()].
#' @slot min_cover Pre-defined minimum forest cover during running of [init_fmetrics()].
#' @slot min_rate Pre-defined minimum forest loss rate during running of [init_fmetrics()].
#' @slot window Pre-defined window of years to calculate `min_rate` during running of [init_fmetrics()].
#' @slot temporal_windows Pre-defined time windows, relevant for activeness metric.
#' @slot excluded_cells Cells excluded for future analysis (not frontiers)
#'
#' @export
setClass("init_FrontierMetric",

         slots = c(
           # dataset
           data = "data.table",

           # coordinates
           coords = "data.table",

           # initial year - last year
           time_frame = "numeric",

           # Name of the column with the forest cover for the initial year
           initial_fc_col = "character",

           # Vector of strings with the names of the columns for forest loss,
           # for each year, in chronological order
           fl_cols = "character",

           # extent of analysis
           extent = "numeric",

           # grain
           grain = "list",

           # first and second aggregation factor
           aggregation = "numeric",

           # was created from a cover time-series?
           is_series = "logical",

           # tree cover threshold
           min_treecover = "null_num",

           # forest cover threshold
           min_cover = "numeric",

           # deforestation rate threshold
           min_rate = "numeric",

           # size, in years, of temporal window
           window = "numeric",

           # temporal windows
           temporal_windows = "data.frame",

           # data with the ids and coordinates of excluded cells
           excluded_cells = "data.table"
         )
)

#' Class 'FrontierMetric'
#'
#' Object of class 'FrontierMetric' are generated with [fmetrics()], and hold
#' the values of the calculated frontier metrics for each cell of the study area.
#' Objects of this class can be passed to [fmetrics_plot()] and [fmetrics_rast()].
#'
#' @slot metrics Calculated frontier metrics.
#' @slot time_frame Year range of the studied time series.
#' @slot data Main data frame containing the values of the calculated frontier
#' metrics for each individual cell of the study area.
#' @slot extent Extent of the study area.
#' @slot grain Smallest and biggest studied grain (resolution).
#' @slot aggregation Pre-defined aggregation during running of [init_fmetrics()].
#' @slot min_treecover Pre-defined minimum tree cover during running of [init_fmetrics()].
#' @slot min_cover Pre-defined minimum forest cover during running of [init_fmetrics()].
#' @slot min_rate Pre-defined minimum forest loss rate during running of [init_fmetrics()].
#' @slot window Pre-defined window of years to calculate `min_rate` during running of [init_fmetrics()].
#' @slot excluded_cells Cells excluded for future analysis (not frontiers)
#'
#' @export
setClass("FrontierMetric",
         slots = c(
           # Contained frontier metrics
           metrics = "character",

           # Contained used defined metrics
           ud_metrics = "character",

           # year range
           time_frame = "numeric",

           # frontier metrics dataset
           data = "data.table",

           # extent of analysis
           extent = "numeric",

           # grain
           grain = "list",

           # first and second aggregation factor
           aggregation = "numeric",

           # tree cover threshold
           min_treecover = "null_num",

           # forest cover threshold
           min_cover = "numeric",

           # deforestation rate threshold
           min_rate = "numeric",

           # size, in years, of temporal window
           window = "numeric",

           # frontier archetypes
           archetypes = "data.frame",

           # data with the ids and coordinates of excluded cells
           excluded_cells = "data.table"
         )
)

#' Class 'FrontierMetric_breaks'
#'
#' Objects of class 'FrontierMetric_breaks' are generated with [breaks_rules()].
#' It holds the definition of rules to categorize the values of frontier metrics
#' into discrete classes.
#'
#' @slot baseline List of rules for frontier metric "baseline forest".
#' @slot baseline_frag List of rules for frontier metric "baseline forest fragmentation".
#' @slot loss List of rules for frontier metric "forest loss".
#' @slot loss_frag List of rules for frontier metric "forest loss fragmentation".
#' @slot speed List of rules for frontier metric "speed".
#' @slot left List of rules for frontier metric "forest left".
#'
#' @export
setClass("FrontierMetric_breaks",
         slots = c(
           baseline = "list",
           baseline_frag = "list",
           loss = "list",
           loss_frag = "list",
           speed = "list",
           left = "list"))

#' Class 'FrontierMetric_summary'
#'
#' Objects of class 'FrontierMetric_summary' are generated with [fmetrics_summary()],
#' and holds summary statistics of frontier metrics, as well as the area occupied by
#' each frontier metric class.
#'
#' @slot summary_stats Data frame with summary statistics.
#' @slot classes_areas Data frame with the areas of frontier metric's classes.
#' @slot hists 'ggplot' objects of frontier metric's histograms.
#'
#' @export
setClass("FrontierMetric_summary",
         slots = c(
           summary_stats = "data.frame",
           classes_areas = "data.frame",
           hists = "ANY")
         )

# Class 'FrontierMetric_summary'
#
# Objects of class 'FrontierMetric_summary' are generated with [fmetrics_summary()],
# and holds summary statistics of frontier metrics, as well as the area occupied by
# each frontier metric class.
#
# @slot summary_stats Data frame with summary statistics.
# @slot classes_areas Data frame with the areas of frontier metric's classes.
# @slot hists 'ggplot' objects of frontier metric's histograms.
#
# setClass("FrontierMetric_raster",
#          slots = c(
#            # Contained frontier metrics
#            metrics = "character",
#
#            # Contained used defined metrics
#            ud_metrics = "character",
#
#            # year range
#            time_frame = "numeric",
#
#            # frontier metrics dataset
#            data = "data.table",
#
#            # extent of analysis
#            extent = "numeric",
#
#            # grain
#            grain = "list",
#
#            # first and second aggregation factor
#            aggregation = "numeric",
#
#            # tree cover threshold
#            min_treecover = "null_num",
#
#            # forest cover threshold
#            min_cover = "numeric",
#
#            # deforestation rate threshold
#            min_rate = "numeric",
#
#            # size, in years, of temporal window
#            window = "numeric",
#
#            # frontier archetypes
#            archetypes = "data.frame",
#
#            # data with the ids and coordinates of excluded cells
#            excluded_cells = "data.table",
#
#            # raster layers
#            raster_layers = "SpatRaster",
#            info = "list",
#            time_frame = "numeric")
# )
