check_get_gfw <- function(){
  messages <- NULL
  what     <- NULL

  # study_area
  if(!class(study_area) %in% c("character", "SpatExtent", "SpatVector", "SpatialPolygonsDataFrame", "SpatialPolygons")){
    messages <- append(messages,
                       "- argument 'study_area' must be an object of class 'SpatExtent', a path (string) to a vector file or an object of class
                       'SpatVector', 'SpatialPolygonsDataFrame' or 'SpatialPolygons'.")
    what     <- append(what, 2)
  } else {
    if(!class(study_area) %in% c("SpatExtent", "SpatVector", "SpatialPolygonsDataFrame", "SpatialPolygons")){
      if(!file.exists(study_area)){
        messages <- append(messages, paste("- could not find study_area file: ", study_area, "."))
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

  if(!is.logical(is_series)){
    messages <- append(messages, paste0("- argument 'is_series' must be logical. See ?init_fmetrics"))
    what     <- append(what, 2)
  } else {
    if(!is_series){
      if(!is.list(raster)){
        messages <- append(messages, paste0("- if 'is_series = FALSE, argument 'raster' must be a list with two SpatRaster or two
                                        path to two raster layers, representing tree cover and forest loss (in this order) from GFW databases."))
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
          # forest loss
          if(!class(raster[[2]]) %in% c("SpatRaster", "character")){
            messages <- append(messages, paste0("- the second element of the list provided in 'raster' must be a raster layer or a
                                          path to a raster layer representing forest loss from the GFW database. See ?init_fmetrics"))
            what     <- append(what, 2)
          } else {
            if(is.character(raster[[2]])){
              if(!file.exists(raster[[2]])){
                messages <- append(messages, paste0("- could not find raster layer for forest loss (second element of the list provided in 'raster')."))
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
    messages <- append(messages, paste("- argument 'min_cover' must be a number depicting a percentage of minimum forest cover to qualify for a frontier. See ?init_fmetrics"))
    what     <- append(what, 2)
  }

  # min_rate
  if(!is.numeric(min_rate) || !(min_rate > 0 & min_rate <= 100)){
    messages <- append(messages, paste("- argument 'min_rate' must be a number depicting a percentage of minimum forest loss rate to qualify for a frontier. See ?init_fmetrics"))
    what     <- append(what, 2)
  }

  # window
  if(!is.numeric(window) || ((window %% 1) != 0) || (window < 1)){
    messages <- append(messages, paste("- argument 'window' must be a whole number depicting the size of the temporal window to be considered for the calculation of the average of annual forest loss rate. See ?init_fmetrics"))
    what     <- append(what, 2)
  }

  # time_frame
  if(!is.numeric(time_frame) || length(time_frame) != 2){
    messages <- append(messages, paste("- argument 'time_frame' must be a numeric vector of length 2 depicting the first and last year of the studied time-frame.
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
  metrics <- metrics[!grepl("ud_", metrics)]
  if(!all(metrics[!grepl("ud_", metrics)] %in% c("all", "baseline", "loss", "speed", "loss_frag",
                         "baseline_frag", "activeness", "left", "onset"))){
    messages <- append(messages, paste("- in argument 'metrics', unknown frontier metrics were required. See ?fmetrics"))
    what     <- append(what, 2)
  }
  # user-defined metrics
  if(!all(grepl("ud_", metrics[!metrics %in% c("all", "baseline", "loss", "speed", "loss_frag",
                         "baseline_frag", "activeness", "left", "onset")]))){
    messages <- append(messages, paste("- in argument 'metrics', if a user-defined metric is provided, its function name must start with 'ud_'. See ?fmetrics"))
    what     <- append(what, 2)
  }

  # breaks
  if(!is(breaks, "FrontierMetric_breaks")){
    messages <- append(messages, paste("- argument 'breaks' must be an object of class 'FrontierMetric_breaks' generated with breaks_rules()."))
    what     <- append(what, 2)
  }

  # export_archetypes
  if(!is.logical(export_archetypes)){
    messages <- append(messages, paste("- argument 'export_archetypes' must be logical. Default was taken (FALSE)"))
    what     <- append(what, 1)
    export_archetypes <- FALSE
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

check_breaks_rules <- function(){
  messages <- NULL
  what     <- NULL

  if(!is(baseline, "list") || length(baseline) != 2){
    messages <- append(messages, paste("- argument 'baseline' must be a list of two elements. See ?breaks_rules"))
    what     <- append(what, 2)
  }

  if(!is(baseline_frag, "list") || length(baseline_frag) != 2){
    messages <- append(messages, paste("- argument 'baseline_frag' must be a list of two elements. See ?breaks_rules"))
    what     <- append(what, 2)
  }

  if(!is(loss, "list") || length(loss) != 2){
    messages <- append(messages, paste("- argument 'loss' must be a list of two elements. See ?breaks_rules"))
    what     <- append(what, 2)
  }

  if(!is(loss_frag, "list") || length(loss_frag) != 2){
    messages <- append(messages, paste("- argument 'loss_frag' must be a list of two elements. See ?breaks_rules"))
    what     <- append(what, 2)
  }

  if(!is(speed, "list") || length(speed) != 2){
    messages <- append(messages, paste("- argument 'speed' must be a list of two elements. See ?breaks_rules"))
    what     <- append(what, 2)
  }

  if(!is(left, "list") || length(left) != 2){
    messages <- append(messages, paste("- argument 'left' must be a list of two elements. See ?breaks_rules"))
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
  whata     <- NULL

  # x
  if(!is(x, "FrontierMetric")){
    messages <- append(messages, paste("- 'x' must be an object of class 'FrontierMetric', generated with fmetrics()"))
    whata     <- append(whata, 2)
  }

  # metrics
  # if(!all(metrics %in% c("all", "baseline", "baseline_frag", "loss", "loss_frag", "speed",
  #                        "activeness", "left", "onset"))){
  #   messages <- append(messages, paste("- in argument 'metrics', unknown frontier metrics were required."))
  #   whata     <- append(whata, 2)
  # }

  # what
  if(!is.character(what) || !what[1] %in% c("both", "values", "classes", "archetypes")){
    messages <- append(messages, paste("- 'what' must be one of the following: \"both\", \"values\", \"classes\" or \"archetypes\". See ?fmetrics_plot"))
    whata     <- append(whata, 2)
  }

  # ncol
  if(!is.numeric(ncol) || ncol <= 0){
    messages <- append(messages, paste("- 'ncol' must be a positive number."))
    whata     <- append(whata, 2)
  }

  # palette
  if(!is.character(palette)){
    messages <- append(messages, paste("- 'palette' must be the name of a palette. Default was taken. See ?fmetrics_plot"))
    whata     <- append(whata, 1)
    palette <- "viridis"
  } else {
    if(!palette %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")){
      messages <- append(messages, paste("- 'palette' must be one of following: \"magma\", \"inferno\", \"plasma\",
                                       \"viridis\", \"cividis\", \"rocket\", \"mako\", or \"turbo\".
                                       Default was taken. See ?fmetrics_plot"))
      whata     <- append(whata, 1)
      palette <- "viridis"
    }
  }

  # direction
  if(!is.numeric(direction) || !direction %in% c(1, -1)){
    messages <- append(messages, paste("- 'direction' must be 1 or -1. Default was taken. See ?fmetrics_plot"))
    whata     <- append(whata, 1)
    direction <- 1
  }

  # background
  if(!is.character(background) || length(background) != 2){
    messages <- append(messages, paste("- 'background' must be a character vector of length 2. Default was taken. See ?fmetrics_plot"))
    whata     <- append(whata, 1)
    background <- c("gray90", "gray64")
  }

  # archetypes
  if(!is.null(archetypes)){
    if(!is.vector(archetypes)){
      messages <- append(messages, paste("- if not NULL, 'archetypes' must a numeric vector. See ?fmetrics_plot"))
      whata     <- append(whata, 2)
    }
  }

  # arch_colors
  if(!is.list(arch_colors)){
    messages <- append(messages, paste("- 'arch_colors' must a list of two elements. See ?fmetrics_plot"))
    whata     <- append(whata, 2)
  }

  warnings <- messages[which(whata == 1)]
  errors   <- messages[which(whata == 2)]

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
  if(!all(metrics %in% c("all", "baseline", "baseline_frag", "loss", "loss_frag",
                         "speed", "activeness", "left", "onset"))){
    messages <- append(messages, paste("- in argument 'metrics', unknown frontier metrics were required."))
    what     <- append(what, 2)
  }

  warnings <- messages[which(what == 1)]
  errors   <- messages[which(what == 2)]

  out <- list(warnings = warnings,
              errors = errors)
  return(out)
}
