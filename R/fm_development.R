calc_fm_activeness <- function(x, fm_ds, classes_sub, window){

  T_ranges <- data.frame(col_name = paste0("T", 1:(x@year_range[2]-x@year_range[1]-(window-1))),
                         first_year = (x@year_range[1]+1):(x@year_range[2]-(window-1)),
                         last_year = (x@year_range[1]+window):x@year_range[2])

  for(i in 1:nrow(T_ranges)){
    fm_ds[[paste0("T", i)]] <- 100*rowMeans(fm_ds[, (i+1):(i+window)], na.rm = T)
    # fm_ds[[paste0("T", i)]] <- sapply(1:nrow(fm_ds),
    #                                   function(row_idx) all(100*fm_ds[row_idx, (i+1):(i+5)] >= x@min_rate))
  }

  fm_ds <- fm_ds[, c("id_cell", T_ranges$col_name), with = F]

  years <- as.numeric(classes_sub)

  ff_act <- function(i, foo1, foo2){
    roww <- fm_ds[i, 2:ncol(fm_ds)]
    #which_active <- NA
    if(!all(is.na(roww))){
      #if(any(roww == T)){
      if(any(roww >= x@min_rate)){
        #if(roww[, which(T_ranges$first_year >= years[2])] == T){
        foo <- na.omit(as.numeric(roww[, foo1, with = F]))
        if(length(foo) > 0 && any(foo >= x@min_rate)){
        #if(!is.na(foo) & foo >= x@min_rate){
          act <- "Emerging"
        } else {
           # if(any(roww[, which(T_ranges$last_year > years[1] &
           #                      T_ranges$last_year <= years[1] + 4)] == T)){
          foo <- na.omit(as.numeric(roww[, foo2, with = F]))
          if(length(foo) == 0 || all(foo < x@min_rate)){
          #if(!any(foo >= x@min_rate) | is.na(any(foo >= x@min_rate))){
            act <- "Old"
          } else {
            act <- "Active"
          }
        }
        #which_active <- paste(which(roww >= x@min_rate), collapse = ".")
      } else {
        act <- "Excluded"
      }
    } else {
      act <- "Excluded"
    }

    #list(act, which_active)
    act
  }

  foo1 <- which(T_ranges$first_year >= years[2])
  foo2 <- which(T_ranges$last_year > years[1])[1]:(foo1[1]-1)
  act_outs <- unlist(sapply(1:nrow(fm_ds), ff_act, foo1, foo2))
  #fm_ds$Activeness <- act_outs[seq(1, length(act_outs), 2)]
  fm_ds$Activeness <- act_outs
  fm_ds$Activeness <- factor(fm_ds$Activeness, levels = c("Emerging", "Active", "Old", "Excluded"))
  #fm_ds$which_active <- act_outs[seq(2, length(act_outs), 2)]
  #fm_ds <- fm_ds[, c("id_cell", "Activeness", "which_active")]
  fm_ds <- fm_ds[, c("id_cell", "Activeness")]

  out <- new("FrontierMetric",
             metrics = "activeness",
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

calc_fm_left <- function(x, fm_ds, classes_sub){

  fm_ds <- merge(fm_ds, x@coords[, c("id_cell", "cell_area")], by = "id_cell")
  fm_ds$Wleft <- 100*(fm_ds[, x@initial_fc_col, with = F] - fm_ds$total_FL)/fm_ds$cell_area
  fm_ds <- fm_ds[, c("id_cell", "Wleft")]
  fm_ds <- as.data.table(fm_ds)

  # Restrict to logic limits
  fm_ds$Wleft[fm_ds$Wleft < 0] <- 0
  fm_ds$Wleft[fm_ds$Wleft > 100] <- 100

  # Classify cells
  fm_ds$Wleft_class <- cut(fm_ds$Wleft, classes_sub[[1]], labels = classes_sub[[2]])
  fm_ds$Wleft_class <- factor(fm_ds$Wleft_class, levels = rev(classes_sub[[2]]))

  out <- new("FrontierMetric",
             metrics = "left",
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

