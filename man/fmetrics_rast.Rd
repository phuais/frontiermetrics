% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fmetrics_rast.R
\name{fmetrics_rast}
\alias{fmetrics_rast}
\title{Exports raster layers of frontier metrics}
\usage{
fmetrics_rast(x, metrics = "all", dir = NULL, gdal = NULL, overwrite = TRUE)
}
\arguments{
\item{x}{An object of class 'FrontierMetric' generated with \code{\link[=fmetrics]{fmetrics()}}}

\item{metrics}{Frontier metrics to be exported as raster layers. Options are: "baseline", "loss",
"fragmentation", "speed", "activeness" and/or "left" for individual frontier
metrics; "severity", "spatio_temporal" and/or "development" for frontier typologies;
"all" to calculate all frontier metrics available within \code{x}. Default is "all".}

\item{dir}{Path to the directory where to export raster layers.}

\item{gdal}{GDAL driver specific datasource creation options. See the GDAL documentation.}

\item{overwrite}{Logical. If \code{TRUE}, files will be overwritten.}
}
\value{
Invisible, it exports raster layers as .tif files.
}
\description{
Based on an object of class 'FrontierMetric', exports raster layers of
frontier metrics. Exported raster layers can depict the absolute values, the
defined classes, or both. Raster layers are exported as .tif files, with
the following names: "baseline_woodland.tif", "woodland_loss.tif,
"speed.tif", "fragmentation.tif", "activeness.tif", "woodland_left.tif".
}
\examples{
\dontrun{
fmetrics_rast(copo_metrics)
}
}
