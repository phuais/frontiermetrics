calc_fm_baseline <- function(x, classes_sub){

  cols <- c("id_cell", x@initial_fc_col)
  fm_ds <- x@data[, cols, with = F]

  fm_ds <- fm_ds[, sum(.SD), .SD = x@initial_fc_col, by = id_cell]
  colnames(fm_ds)[2] <- x@initial_fc_col

  fm_ds <- merge(fm_ds, x@coords[, c("id_cell", "cell_area")], by = "id_cell")
  fm_ds$baseline <- 100 * fm_ds[[x@initial_fc_col]] / fm_ds$cell_area

  # Restrict to logic limits
  fm_ds$baseline[fm_ds$baseline < 0] <- 0
  fm_ds$baseline[fm_ds$baseline > 100] <- 100

  # Categorize cells
  brks <- getBreaks(fm_ds$baseline, classes_sub)
  fm_ds$baseline.c <- cut(fm_ds$baseline, breaks = brks, labels = classes_sub[[2]])
  fm_ds$baseline.c <- factor(fm_ds$baseline.c, levels = rev(classes_sub[[2]]))

  # Filter colums
  fm_ds <- fm_ds[, c("id_cell", x@initial_fc_col, "baseline", "baseline.c"), with = F]

  out <- new("FrontierMetric",
             metrics = "baseline",
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

