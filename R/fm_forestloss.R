calc_fm_loss <- function(x, classes_sub){

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
  fm_ds <- fm_ds[, c(1:2, grep("FL_", colnames(fm_ds))), with = F]
  fm_ds$total_FL <- rowSums(fm_ds[, 3:ncol(fm_ds)])
  fm_ds <- fm_ds[, c("id_cell", x@initial_fc_col, "total_FL"), with = F]

  # Here, if initial FC = 0, it returns NA
  fm_ds$loss <- 100 * fm_ds$total_FL / fm_ds[[x@initial_fc_col]]
  fm_ds <- fm_ds[, c(1, ncol(fm_ds)), with = F]

  # Restrict to logic limits
  fm_ds$loss[fm_ds$loss < 0] <- 0
  fm_ds$loss[fm_ds$loss > 100] <- 100

  # Classify cells
  brks <- getBreaks(fm_ds$loss, classes_sub)
  fm_ds$loss.c <- cut(fm_ds$loss, brks, labels = classes_sub[[2]])
  fm_ds$loss.c <- factor(fm_ds$loss.c, levels = rev(classes_sub[[2]]))

  out <- new("FrontierMetric",
             metrics = "loss",
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
