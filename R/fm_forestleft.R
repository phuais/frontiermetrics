calc_fm_left <- function(x, classes_sub){

  fm_ds <- x@data[, lapply(.SD, sum, na.rm = TRUE),
                  by = id_cell,
                  .SDcols = c(x@initial_fc_col, x@fl_cols)]

  # Forest cover over the years and % of forest loss per year
  prev_col <- x@initial_fc_col
  for(i in 1:length(x@fl_cols)){
    col_name <- paste0("FC_", substring(x@fl_cols[i], first = 4, last = nchar(x@fl_cols[i])))
    fm_ds[[col_name]] <- fm_ds[[prev_col]] - fm_ds[[x@fl_cols[i]]]
    fm_ds[[paste0(col_name, "d")]] <- fm_ds[[x@fl_cols[i]]] / fm_ds[[prev_col]]
    prev_col <- col_name
  }

  fm_ds <- fm_ds[!is.na(fm_ds[[x@initial_fc_col]]), ]
  fm_ds <- fm_ds[ , c(1:2, grep("FL_", colnames(fm_ds))), with = F]
  fm_ds$total_FL <- rowSums(fm_ds[, 3:ncol(fm_ds)])
  fm_ds <- fm_ds[, c("id_cell", x@initial_fc_col, "total_FL"), with = F]
  fm_ds <- merge(fm_ds, x@coords[, c("id_cell", "cell_area")], by = "id_cell")
  fm_ds$left <- 100*(fm_ds[, x@initial_fc_col, with = F] - fm_ds$total_FL)/fm_ds$cell_area
  fm_ds <- fm_ds[, c("id_cell", "left")]
  fm_ds <- as.data.table(fm_ds)

  # Restrict to logic limits
  fm_ds$left[fm_ds$left < 0] <- 0
  fm_ds$left[fm_ds$left > 100] <- 100

  # Classify cells
  brks <- getBreaks(fm_ds$left, classes_sub)
  fm_ds$left.c <- cut(fm_ds$left, brks, labels = classes_sub[[2]])
  fm_ds$left.c <- factor(fm_ds$left.c, levels = rev(classes_sub[[2]]))

  out <- new("FrontierMetric",
             metrics = "left",
             ud_metrics = "",
             time_frame = x@time_frame,
             data = as.data.table(fm_ds),
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
