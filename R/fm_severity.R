calc_fm_baseline <- function(x, fm_ds, classes_sub){

  fm_ds <- fm_ds[, sum(.SD), .SD = x@initial_fc_col, by = id_cell]
  colnames(fm_ds)[2] <- x@initial_fc_col

  fm_ds <- merge(fm_ds, x@coords[, c("id_cell", "cell_area")], by = "id_cell")
  fm_ds$Wct0 <- 100 * fm_ds$FC_2000 / fm_ds$cell_area

  # Restrict to logic limits
  fm_ds$Wct0[fm_ds$Wct0 < 0] <- 0
  fm_ds$Wct0[fm_ds$Wct0 > 100] <- 100

  # Categorize cells
  fm_ds$Wct0_class <- cut(fm_ds$Wct0, classes_sub[[1]], labels = classes_sub[[2]])
  fm_ds$Wct0_class <- factor(fm_ds$Wct0_class, levels = rev(classes_sub[[2]]))

  # Filter colums
  fm_ds <- fm_ds[, c("id_cell", x@initial_fc_col, "Wct0", "Wct0_class"), with = F]

  out <- new("FrontierMetric",
             metrics = "baseline",
             year_range = x@year_range,
             data = as.data.table(fm_ds),
             extent = x@extent,
             grain = x@grain,
             aggregation = x@aggregation,
             min_treecover = x@min_treecover,
             min_cover = x@min_cover,
             min_rate = x@min_rate,
             window = x@window,
             excluded_cells = x@excluded_cells)

  return(out)
}

calc_fm_loss <- function(x, fm_ds, classes_sub){

  # Here, if initial FC = 0, it returns NA
  fm_ds$PTWl <- 100 * fm_ds$total_FL / fm_ds[[x@initial_fc_col]]
  fm_ds <- fm_ds[, c(1, ncol(fm_ds)), with = F]

  # Restrict to logic limits
  fm_ds$PTWl[fm_ds$PTWl < 0] <- 0
  fm_ds$PTWl[fm_ds$PTWl > 100] <- 100

  # Classify cells
  fm_ds$PTWl_class <- cut(fm_ds$PTWl, classes_sub[[1]], labels = classes_sub[[2]])
  fm_ds$PTWl_class <- factor(fm_ds$PTWl_class, levels = rev(classes_sub[[2]]))

  out <- new("FrontierMetric",
             metrics = "loss",
             year_range = x@year_range,
             data = as.data.table(fm_ds),
             extent = x@extent,
             grain = x@grain,
             aggregation = x@aggregation,
             min_treecover = x@min_treecover,
             min_cover = x@min_cover,
             min_rate = x@min_rate,
             window = x@window,
             excluded_cells = x@excluded_cells)

  return(out)
}

