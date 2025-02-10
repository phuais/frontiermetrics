.datatable.aware = TRUE

# falta poder definir el crs adecuado a partir del poligono, o lo pone el usuario
fm_data_prep <- function(x, fm){

  if("baseline" %in% fm){
    if(length(fm[-which(fm == "baseline")]) == 0){
      cols <- c("id_cell",
                x@initial_fc_col)
      return(x@data[, cols, with = F])
    } else { fm <- fm[-which(fm == "baseline")] }
  }

  # Summarize to the most coarse resolution
  # data_maxgrain <- x@data %>%
  #   dplyr::group_by_at("id_cell") %>%
  #   dplyr::summarise_at(c(x@initial_fc_col, x@fl_cols), sum, na.rm = TRUE)
  data_maxgrain <- x@data[, lapply(.SD, sum, na.rm = TRUE),
                          by = id_cell,
                          .SDcols = c(x@initial_fc_col, x@fl_cols)]

  # Woodland cover over the years and % of woodland loss per year
  prev_col <- x@initial_fc_col
  for(i in 1:length(x@fl_cols)){
    col_name <- paste0("FC_", substring(x@fl_cols[i], first = 4, last = nchar(x@fl_cols[i])))
    data_maxgrain[[col_name]] <- data_maxgrain[[prev_col]] - data_maxgrain[[x@fl_cols[i]]]
    data_maxgrain[[paste0(col_name, "d")]] <- data_maxgrain[[x@fl_cols[i]]] / data_maxgrain[[prev_col]]
    prev_col <- col_name
  }

  #data_maxgrain <- data_maxgrain %>% tidyr::drop_na(x@initial_fc_col)
  data_maxgrain <- data_maxgrain[!is.na(data_maxgrain[[x@initial_fc_col]]), ]

  if("activeness" %in% fm){
    if(length(fm[-which(fm == "activeness")]) == 0){
      return(data_maxgrain)
    } else { fm <- fm[-which(fm == "activeness")] }
  }

  #data_maxgrain <- tidyr::drop_na(data_maxgrain, x@initial_fc_col)
  data_maxgrain_cts <- data_maxgrain[, c(1:2, grep("FL_", colnames(data_maxgrain))), with = F]

  if("speed" %in% fm){
    if(length(fm[-which(fm == "speed")]) == 0){
      return(data_maxgrain_cts)
    } else { fm <- fm[-which(fm == "speed")] }
  }

  data_maxgrain_cts$total_FL <- rowSums(data_maxgrain_cts[, 3:ncol(data_maxgrain_cts)])

  if(fm %in% c("loss", "left")){
    return(data_maxgrain_cts)
  }
}
