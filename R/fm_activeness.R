calc_fm_activeness <- function(x, classes_sub){

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
  fm_ds <- fm_ds[, colnames(fm_ds)[grepl("d", colnames(fm_ds))], with = F]

  for(i in 1:nrow(x@temporal_windows)){
    fm_ds[[paste0("T", i)]] <- 100*rowMeans(fm_ds[, (i+1):(i+x@window)], na.rm = T)
  }

  fm_ds <- fm_ds[, c("id_cell", paste0("T", x@temporal_windows$window)), with = F]

  ff_act <- function(i){
    roww <- fm_ds[i, 2:ncol(fm_ds)]
    if(!all(is.na(roww))){
      if(any(na.omit(roww >= x@min_rate))){
        for(i in 1:length(classes_sub)){
          foo <- na.omit(as.numeric(roww[, classes_sub[[i]], with = F]))
          if(length(foo) > 0 && any(foo >= x@min_rate)){
            act <- names(classes_sub)[i]
            break
          }
        }
      } else {
        act <- "excluded"
      }
    } else {
      act <- "excluded"
    }

    act
  }

  act_outs <- unlist(sapply(1:nrow(fm_ds), ff_act))
  fm_ds$activeness <- act_outs
  fm_ds$activeness <- factor(fm_ds$activeness, levels = c(names(classes_sub), "excluded"))
  fm_ds <- fm_ds[, c("id_cell", "activeness")]

  out <- new("FrontierMetric",
             metrics = "activeness",
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
