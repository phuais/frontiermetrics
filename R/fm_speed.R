calc_fm_speed <- function(x, classes_sub){

  fm_ds <- x@data[, lapply(.SD, sum, na.rm = TRUE),
                  by = id_cell,
                  .SDcols = c(x@initial_fc_col, x@fl_cols)]

  # Forest cover over the years and % of forest loss per year
  # prev_col <- x@initial_fc_col
  # for(i in 1:length(x@fl_cols)){
  #   col_name <- paste0("FC_", substring(x@fl_cols[i], first = 4, last = nchar(x@fl_cols[i])))
  #   fm_ds[[col_name]] <- fm_ds[[prev_col]] - fm_ds[[x@fl_cols[i]]]
  #   fm_ds[[paste0(col_name, "d")]] <- fm_ds[[x@fl_cols[i]]] / fm_ds[[prev_col]]
  #   prev_col <- col_name
  # }

  fm_ds <- fm_ds[!is.na(fm_ds[[x@initial_fc_col]]), ]
  fm_ds <- fm_ds[, c(1:2, grep("FL_", colnames(fm_ds))), with = F]
  fm_ds <- as.data.table(fm_ds)
  fm_ds <- fm_ds[, -x@initial_fc_col, with = F]

  # Prepare dataset to long format
  years_labs <- colnames(fm_ds)[grep("FL_", colnames(fm_ds))]
  yeartexttonum <- data.frame(year_l = years_labs,
                              year_id = 1:length(years_labs),
                              year_n = as.numeric(gsub("FL_", "", years_labs)))

  long_data_maxgrain_FL <- reshape(fm_ds, direction = "long",
                                   varying = list(2:ncol(fm_ds)),
                                   idvar = "id_cell", v.names = "FL")
  colnames(long_data_maxgrain_FL)[2] <- "year_id"
  long_data_maxgrain_FL <- merge(long_data_maxgrain_FL, yeartexttonum[, 2:3])
  long_data_maxgrain_FL <- long_data_maxgrain_FL[, c("id_cell", "year_n", "FL"), with = F]

  long_data_maxgrain_FL$zerocheck <- sum(long_data_maxgrain_FL$FL, na.rm = T)
  long_data_maxgrain_FL <- long_data_maxgrain_FL[long_data_maxgrain_FL$zerocheck != 0, ]

  # Loess fitting
  ts_cell <- long_data_maxgrain_FL[, list(data = list(.SD)), by = id_cell]
  ts_cell$m <- lapply(ts_cell$data, FUN = loess, formula = FL ~ year_n, span = .3)
  ts_cell$fitted <- lapply(ts_cell$m, function(x) x$fitted)
  ts_cell$diff <- lapply(ts_cell$fitted, function(x) c(NA, diff(x)))

  # Unnesting
  ts_cell <- ts_cell[, .(diff = unlist(diff, recursive = FALSE)), by = id_cell]

  # Find maximum slope (derivative)
  ts_cell <- ts_cell[, .SD[which.max(diff)], by = .(id_cell)]

  colnames(ts_cell)[2] <- "speed"
  # Categorize cells
  brks <- getBreaks(ts_cell$speed, classes_sub)
  ts_cell$speed.c <- cut(ts_cell$speed, brks, labels = classes_sub[[2]])
  ts_cell$speed.c <- factor(ts_cell$speed.c, levels = rev(classes_sub[[2]]))

  out <- new("FrontierMetric",
             metrics = "speed",
             ud_metrics = "",
             time_frame = x@time_frame,
             data = as.data.table(ts_cell),
             extent = x@extent,
             grain = x@grain,
             aggregation = x@aggregation,
             min_treecover = x@min_treecover,
             min_cover = x@min_cover,
             min_rate = x@min_rate,
             window = x@window,
             archetypes = data.frame(),
             excluded_cells = x@excluded_cells)

  return(out)
}
