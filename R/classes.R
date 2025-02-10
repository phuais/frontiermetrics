#' @importFrom methods is show
#' @importFrom data.table data.table as.data.table
#' @importClassesFrom terra SpatRaster
#' @importClassesFrom data.table data.table

setClassUnion("null_rast", c("NULL", "SpatRaster"))
setClassUnion("null_ds", c("NULL", "data.table"))

#' Class 'GFW_dataset'
#'
#' Objects of class 'GFW_dataset' are generated with [init_fmetrics()], and hold
#' the necessary objects to calculate frontier metrics with [fmetrics()]. The main
#' object is under `@data`, which is a data frame containing -among other relevant data- the amount of woodland
#' cover in the first year of the time series, and the amount of woodland loss
#' in each year of the time series, for each individual cell of the study area.
#'
#' @slot data Main dataset containing data about woodland cover and woodland loss.
#' @slot coords Central coordinates of bigger cells.
#' @slot year_range Year range of the studied time series.
#' @slot initial_fc_col Name of the column containing the data about initial woodland cover.
#' @slot fl_cols Name of the columns containing the data about annual woodland loss.
#' @slot extent Extent of the study area.
#' @slot grain Smallest and biggest studied grain (resolution).
#' @slot aggregation Pre-defined aggregation during running of [init_fmetrics()].
#' @slot min_treecover Pre-defined minimum tree cover during running of [init_fmetrics()].
#' @slot min_cover Pre-defined minimum woodland cover during running of [init_fmetrics()].
#' @slot min_rate Pre-defined minimum woodland loss rate during running of [init_fmetrics()].
#' @slot window Pre-defined window of years to calculate `min_rate` during running of [init_fmetrics()].
#' @slot excluded_cells Cells excluded for future analysis (not frontiers)
#'
#' @export
setClass("GFW_dataset",

         slots = c(
           # dataset
           data = "data.table",

           # coordinates
           coords = "data.table",

           # initial year - last year
           year_range = "numeric",

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

           # tree cover threshold
           min_treecover = "numeric",

           # woodland cover threshold
           min_cover = "numeric",

           # deforestation rate threshold
           min_rate = "numeric",

           # size, in years, of temporal window
           window = "numeric",

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
#' @slot year_range Year range of the studied time series.
#' @slot data Main data frame containing the values of the calculated frontier
#' metrics for each individual cell of the study area.
#' @slot extent Extent of the study area.
#' @slot grain Smallest and biggest studied grain (resolution).
#' @slot aggregation Pre-defined aggregation during running of [init_fmetrics()].
#' @slot min_treecover Pre-defined minimum tree cover during running of [init_fmetrics()].
#' @slot min_cover Pre-defined minimum woodland cover during running of [init_fmetrics()].
#' @slot min_rate Pre-defined minimum woodland loss rate during running of [init_fmetrics()].
#' @slot window Pre-defined window of years to calculate `min_rate` during running of [init_fmetrics()].
#' @slot excluded_cells Cells excluded for future analysis (not frontiers)
#'
#' @export
setClass("FrontierMetric",
         slots = c(
           # Contained frontier metrics
           metrics = "character",

           # year range
           year_range = "numeric",

           # frontier metrics dataset
           data = "data.table",

           # extent of analysis
           extent = "numeric",

           # grain
           grain = "list",

           # first and second aggregation factor
           aggregation = "numeric",

           # tree cover threshold
           min_treecover = "numeric",

           # woodland cover threshold
           min_cover = "numeric",

           # deforestation rate threshold
           min_rate = "numeric",

           # size, in years, of temporal window
           window = "numeric",

           # data with the ids and coordinates of excluded cells
           excluded_cells = "data.table"
         )
)

#' Class 'FrontierMetric_classes'
#'
#' Objects of class 'FrontierMetric_classes' are generated with [init_classes()].
#' It holds the definition of rules to categorize the values of frontier metrics
#' into discrete classes.
#'
#' @slot min_cover Pre-defined minimum woodland cover during running of [init_fmetrics()].
#' @slot min_rate Pre-defined minimum woodland loss rate during running of [init_fmetrics()].
#' @slot window Pre-defined window of years to calculate `min_rate` during running of [init_fmetrics()].
#' @slot baseline List of rules for frontier metric " baseline woodland".
#' @slot loss List of rules for frontier metric "woodland loss".
#' @slot fragmentation List of rules for frontier metric "fragmentation".
#' @slot speed List of rules for frontier metric "speed".
#' @slot activeness List of rules for frontier metric "activeness".
#' @slot left List of rules for frontier metric "woodland left".
#'
#' @export
setClass("FrontierMetric_classes",
         slots = c(
           min_cover = "numeric",
           min_rate = "numeric",
           window = "numeric",
           baseline = "list",
           loss = "list",
           fragmentation = "list",
           speed = "list",
           activeness = "list",
           left = "list"))

setOldClass(c("gg", "ggplot"))

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
setClass("FrontierMetric_summary", contains = "ggplot",
         slots = c(
           summary_stats = "data.frame",
           classes_areas = "data.frame",
           hists = "ggplot")
         )
