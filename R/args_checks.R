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

check_init_fmetrics <- function(){
  messages <- NULL
  what     <- NULL

  # gfw_cover
  if(!class(gfw_cover) %in% c("RasterLayer", "SpatRaster", "character")){
    messages <- append(messages, paste0("- argument 'gfw_cover' must be a raster layer or a
                                          path to a raster layer depicting tree cover from the GFW database. See ?init_fmetrics"))
    what     <- append(what, 2)
  }
  if(is.character(gfw_cover)){
    if(!file.exists(gfw_cover)){
      messages <- append(messages, paste0("- could not find raster layer for tree cover (argument 'gfw_cover')."))
      what     <- append(what, 2)
    }
  }

  # gfw_loss
  if(!class(gfw_loss) %in% c("RasterLayer", "SpatRaster", "character")){
    messages <- append(messages, paste0("- argument 'gfw_loss' must be a raster layer or a
                                          path to a raster layer depicting tree cover from the GFW database. See ?init_fmetrics"))
    what     <- append(what, 2)
  }
  if(is.character(gfw_loss)){
    if(!file.exists(gfw_loss)){
      messages <- append(messages, paste0("- could not find raster layer for tree cover (argument 'gfw_loss')."))
      what     <- append(what, 2)
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
  if(!is(x, "GFW_dataset")){
    messages <- append(messages, paste("- argument 'x' must be an object of class 'GFW_dataset' generated with init_fmetrics()."))
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

  if(!is(activeness, "list") || length(activeness) != 2){
    messages <- append(messages, paste("- argument 'activeness' must be a list of two elements. See ?init_classes"))
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
