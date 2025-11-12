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

  # T_ranges <- data.frame(col_name = paste0("T", 1:(x@time_frame[2]-x@time_frame[1]-(x@window-1))),
  #                        first_year = (x@time_frame[1]+1):(x@time_frame[2]-(x@window-1)),
  #                        last_year = (x@time_frame[1]+x@window):x@time_frame[2])

  for(i in 1:nrow(x@temporal_windows)){
    fm_ds[[paste0("T", i)]] <- 100*rowMeans(fm_ds[, (i+1):(i+x@window)], na.rm = T)
  }

  fm_ds <- fm_ds[, c("id_cell", paste0("T", x@temporal_windows$window)), with = F]

  # years <- as.numeric(classes_sub)

  #ff_act <- function(i, foo1, foo2){
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
#         foo <- na.omit(as.numeric(roww[, foo1, with = F]))
#         if(length(foo) > 0 && any(foo >= x@min_rate)){
#           act <- "Emerging"
#         } else {
#           foo <- na.omit(as.numeric(roww[, foo2, with = F]))
#           if(length(foo) == 0 || all(foo < x@min_rate)){
#             act <- "Old"
#           } else {
#             act <- "Active"
#           }
#         }
      } else {
        act <- "excluded"
      }
    } else {
      act <- "excluded"
    }

    #list(act, which_active)
    act
  }

  # foo1 <- which(T_ranges$first_year >= years[2])
  # foo2 <- which(T_ranges$last_year > years[1])[1]:(foo1[1]-1)
  #act_outs <- unlist(sapply(1:nrow(fm_ds), ff_act, foo1, foo2))
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
