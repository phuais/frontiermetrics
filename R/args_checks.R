check_get_gfw <- function(){
  messages <- NULL
  what     <- NULL

  # polygon
  if(!class(polygon) %in% c("character", "SpatVector", "SpatialPolygonsDataFrame", "SpatialPolygons")){
    messages <- append(messages,
                       "- argument 'polygon' must be a path (string) to a vector file or an object of class
                       'SpatVector', 'SpatialPolygonsDataFrame' or 'SpatialPolygons'.")
    what     <- append(what, 2)
  } else {
    if(!class(polygon) %in% c("SpatVector", "SpatialPolygonsDataFrame", "SpatialPolygons")){
      if(!file.exists(polygon)){
        messages <- append(messages, paste("- could not find polygon file: ", polygon, "."))
        what     <- append(what, 2)
      }
    }
  }

  # mask
  if(!is.logical(mask)){
    messages <- append(messages,
                       "- argument 'mask' must be logical.")
    what     <- append(what, 2)
  }

  # dir
  if(!is(dir, "character")){
    messages <- append(messages,
                       "- argument 'dir' must be a path to a directory.")
    what     <- append(what, 2)
  } else {
    if(dir != ""){
      if(!dir.exists(dir)){
        messages <- append(messages, paste("- the provided directory does not exist: ", dir, "."))
        what     <- append(what, 2)
      }
    }
  }

  # timeout
  if(!is(timeout, "numeric") || !timeout > 0){
    messages <- append(messages, paste("- argument 'timeout' must be a positive number (seconds). Default 600 was taken."))
    what     <- append(what, 1)
    timeout  <- 600
  }

  # overwrite
  if(!is.logical(overwrite)){
    messages <- append(messages, paste("- argument 'overwrite' must be logical."))
    what     <- append(what, 2)
  }

  warnings <- messages[which(what == 1)]
  errors   <- messages[which(what == 2)]

  out <- list(warnings = warnings,
              errors = errors,
              timeout = timeout)
  return(out)
}

check_to_gfw <- function(){
  messages <- NULL
  what     <- NULL

  # cover_series
  if(!class(cover_series) %in% c("SpatRaster", "character", "NULL")){
    messages <- append(messages, paste0("- argument 'cover_series' must be an object of class 'SpatRaster' with the time
                                         series of woodland cover for a given time-frame. See ?to_gfw"))
    what     <- append(what, 2)
  }
  if(is.character(cover_series)){
    if(!file.exists(cover_series)){
      messages <- append(messages, paste0("- could not find the provided file (argument 'cover_series')."))
      what     <- append(what, 2)
    }
  }

  # overwrite
  if(!is.logical(overwrite)){
    messages <- append(messages, paste("- argument 'overwrite' must be logical."))
    what     <- append(what, 2)
  }

  warnings <- messages[which(what == 1)]
  errors   <- messages[which(what == 2)]

  out <- list(warnings = warnings,
              errors = errors)
  return(out)
}

check_init_fmetrics <- function(){
  messages <- NULL
  what     <- NULL

  if(!is.logical(is_series)){
    messages <- append(messages, paste0("- argument 'is_series' must be logical. See ?init_fmetrics"))
    what     <- append(what, 2)
  } else {
    if(!is_series){
      if(!is.list(raster)){
        messages <- append(messages, paste0("- if 'is_series = FALSE, argument 'raster' must be a list with two SpatRaster or two
                                        path to two raster layers, representing tree cover and woodland loss (in this order) from GFW databases."))
        what     <- append(what, 2)
      } else {
        if(length(raster) != 2){
          messages <- append(messages, paste0("- if 'is_series = FALSE', argument 'raster' must be a list 'raster' of length 2. See ?init_fmetrics"))
          what     <- append(what, 2)
        } else {
          # tree cover
          if(!class(raster[[1]]) %in% c("SpatRaster", "character")){
            messages <- append(messages, paste0("- the first element of the list provided in 'raster' must be a raster layer or a
                                          path to a raster layer representing tree cover from the GFW database. See ?init_fmetrics"))
            what     <- append(what, 2)
          } else {
            if(is.character(raster[[1]])){
              if(!file.exists(raster[[1]])){
                messages <- append(messages, paste0("- could not find raster layer for tree cover (first element of the list provided in 'raster')."))
                what     <- append(what, 2)
              }
            }
          }
          # woodland loss
          if(!class(raster[[2]]) %in% c("SpatRaster", "character")){
            messages <- append(messages, paste0("- the second element of the list provided in 'raster' must be a raster layer or a
                                          path to a raster layer representing woodland loss from the GFW database. See ?init_fmetrics"))
            what     <- append(what, 2)
          } else {
            if(is.character(raster[[2]])){
              if(!file.exists(raster[[2]])){
                messages <- append(messages, paste0("- could not find raster layer for woodland loss (second element of the list provided in 'raster')."))
                what     <- append(what, 2)
              }
            }
          }
        }
      }
    } else {
      # cover series
      if(!class(raster) %in% c("SpatRaster", "character")){
        messages <- append(messages, paste0("- if 'is_series = TRUE', argument 'raster' must be a SpatRaster or a path to a raster layer
      representing cover time-series. See ?init_fmetrics"))
        what     <- append(what, 2)
      } else {
        if(is.character(raster)){
          if(!file.exists(raster)){
            messages <- append(messages, paste0("- could not find raster layer for the cover time-series."))
            what     <- append(what, 2)
          }
        }
      }
    }
  }

  # aggregation
  if(!is.numeric(aggregation) || length(aggregation) != 2){
    messages <- append(messages, paste("- argument 'aggregation' must be a numeric vector of length 2. See ?init_fmetrics"))
    what     <- append(what, 2)
  }

  # min_treecover
  if(!is.numeric(min_treecover) || !(min_treecover > 0 & min_treecover <= 100)){
    messages <- append(messages, paste("- argument 'min_treecover' must be a number depicting a percentage of minimum tree cover. See ?init_fmetrics"))
    what     <- append(what, 2)
  }

  # min_cover
  if(!is.numeric(min_cover) || !(min_cover > 0 & min_cover <= 100)){
    messages <- append(messages, paste("- argument 'min_cover' must be a number depicting a percentage of minimum woodland cover to qualify for a frontier. See ?init_fmetrics"))
    what     <- append(what, 2)
  }

  # min_rate
  if(!is.numeric(min_rate) || !(min_rate > 0 & min_rate <= 100)){
    messages <- append(messages, paste("- argument 'min_rate' must be a number depicting a percentage of minimum woodland loss rate to qualify for a frontier. See ?init_fmetrics"))
    what     <- append(what, 2)
  }

  # window
  if(!is.numeric(window) || ((window %% 1) != 0) || (window < 1)){
    messages <- append(messages, paste("- argument 'window' must be a whole number depicting the size of the temporal window to be considered for the calculation of the average of annual woodland loss rate. See ?init_fmetrics"))
    what     <- append(what, 2)
  }

  # year_range
  if(!is.numeric(year_range) || length(year_range) != 2){
    messages <- append(messages, paste("- argument 'year_range' must be a numeric vector of length 2 depicting the first and last year of the studied time-frame.
                                       If using GFW databases, the first year must be at least 2000, and the last year not be higher than the maximum year must match the maximum year available in the GFW database. See ?init_fmetrics"))
    what     <- append(what, 2)
  }

  # ncores
  if(!is.numeric(ncores)){
    messages <- append(messages, paste("- argument 'ncores' must be a number that specifies the number of cores to use to parallelize processes."))
    what     <- append(what, 2)
  }

  warnings <- messages[which(what == 1)]
  errors   <- messages[which(what == 2)]

  out <- list(warnings = warnings,
              errors = errors)
  return(out)
}

check_fmetrics <- function(){
  messages <- NULL
  what     <- NULL

  # x
  if(!is(x, "init_FrontierMetric")){
    messages <- append(messages, paste("- argument 'x' must be an object of class 'init_FrontierMetric' generated with init_fmetrics()."))
    what     <- append(what, 2)
  }

  # metrics
  if(!all(metrics %in% c("all", "baseline", "loss", "speed", "fragmentation",
                         "activeness", "left", "severity", "spatio_temporal",
                         "development"))){
    messages <- append(messages, paste("- in argument 'metrics', unknown frontier metrics were required. See ?fmetrics"))
    what     <- append(what, 2)
  }

  # classes
  if(!is(classes, "FrontierMetric_classes")){
    messages <- append(messages, paste("- argument 'classes' must be an object of class 'FrontierMetric_classes' generated with init_classes()."))
    what     <- append(what, 2)
  }

  # ncores
  if(!is.numeric(ncores)){
    messages <- append(messages, paste("- argument 'ncores' must be a number that specifies the number of cores to use to parallelize processes."))
    what     <- append(what, 2)
  }

  # silent
  if(!is.logical(silent)){
    messages <- append(messages, paste("- argument 'silent' must be logical. Default was taken (FALSE)"))
    what     <- append(what, 1)
    silent   <- FALSE
  }

  warnings <- messages[which(what == 1)]
  errors   <- messages[which(what == 2)]

  out <- list(warnings = warnings,
              errors = errors)
  return(out)
}

check_init_classes <- function(){
  messages <- NULL
  what     <- NULL

  if(!is(baseline, "list") || length(baseline) != 2){
    messages <- append(messages, paste("- argument 'baseline' must be a list of two elements. See ?init_classes"))
    what     <- append(what, 2)
  }

  if(!is(loss, "list") || length(loss) != 2){
    messages <- append(messages, paste("- argument 'loss' must be a list of two elements. See ?init_classes"))
    what     <- append(what, 2)
  }

  if(!is(fragmentation, "list") || length(fragmentation) != 2){
    messages <- append(messages, paste("- argument 'fragmentation' must be a list of two elements. See ?init_classes"))
    what     <- append(what, 2)
  }

  if(!is(speed, "list") || length(speed) != 2){
    messages <- append(messages, paste("- argument 'speed' must be a list of two elements. See ?init_classes"))
    what     <- append(what, 2)
  }

  if(!is(activeness, "list") || length(activeness) != 3){
    messages <- append(messages, paste("- argument 'activeness' must be a list of three elements. See ?init_classes"))
    what     <- append(what, 2)
  }

  if(!is(left, "list") || length(left) != 2){
    messages <- append(messages, paste("- argument 'left' must be a list of two elements. See ?init_classes"))
    what     <- append(what, 2)
  }

  warnings <- messages[which(what == 1)]
  errors   <- messages[which(what == 2)]

  out <- list(warnings = warnings,
              errors = errors)
  return(out)
}

check_fmetrics_plot <- function(){
  messages <- NULL
  what     <- NULL

  # x
  if(!is(x, "FrontierMetric")){
    messages <- append(messages, paste("- 'x' must be an object of class 'FrontierMetric', generated with fmetrics()"))
    what     <- append(what, 2)
  }

  # metrics
  if(!all(metrics %in% c("all", "baseline", "loss", "speed", "fragmentation",
                         "activeness", "left", "severity", "spatio_temporal",
                         "development"))){
    messages <- append(messages, paste("- in argument 'metrics', unknown frontier metrics were required."))
    what     <- append(what, 2)
  }

  # type
  if(!is.character(type) || !type[1] %in% c("both", "values", "classes")){
    messages <- append(messages, paste("- 'type' must be one of the following: \"both\", \"values\" or \"classes\". See ?fmetrics_plot"))
    what     <- append(what, 2)
  }

  # ncol
  if(!is.numeric(ncol) || ncol <= 0){
    messages <- append(messages, paste("- 'ncol' must be a positive number."))
    what     <- append(what, 2)
  }

  # palette
  if(!is.character(palette) || !length(palette) %in% 1:2){
    messages <- append(messages, paste("- 'palette' must be a character vector of length 1 or 2. Default was taken. See ?fmetrics_plot"))
    what     <- append(what, 1)
    palette <- c("viridis", "viridis")
  } else {
    if(length(palette) == 1) palette <- c(palette, palette)
  }

  # direction
  if(!is.numeric(direction) || !length(direction) %in% 1:2 || !all(direction %in% c(1, -1))){
    messages <- append(messages, paste("- 'direction' must be a numeric vector of length 1 or 2, with values 1 or -1. Default was taken. See ?fmetrics_plot"))
    what     <- append(what, 1)
    direction <- c(-1, -1)
  } else {
    if(length(direction) == 1) direction <- c(direction, direction)
  }

  # background
  if(!is.character(background) || length(background) != 2){
    messages <- append(messages, paste("- 'background' must be a character vector of length 2. Default was taken. See ?fmetrics_plot"))
    what     <- append(what, 1)
    background <- c("gray90", "gray64")
  }

  warnings <- messages[which(what == 1)]
  errors   <- messages[which(what == 2)]

  out <- list(warnings = warnings,
              errors = errors,
              palette = palette,
              direction = direction)
  return(out)
}

check_fmetrics_summary <- function(){
  messages <- NULL
  what     <- NULL

  # x
  if(!is(x, "FrontierMetric")){
    messages <- append(messages, paste("- 'x' must be an object of class 'FrontierMetric', generated with fmetrics()"))
    what     <- append(what, 2)
  }

  # metrics
  if(!all(metrics %in% c("all", "baseline", "loss", "speed", "fragmentation",
                         "activeness", "left", "severity", "spatio_temporal",
                         "development"))){
    messages <- append(messages, paste("- in argument 'metrics', unknown frontier metrics were required."))
    what     <- append(what, 2)
  }

  warnings <- messages[which(what == 1)]
  errors   <- messages[which(what == 2)]

  out <- list(warnings = warnings,
              errors = errors)
  return(out)
}
