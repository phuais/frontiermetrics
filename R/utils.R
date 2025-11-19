crs_transform <- function(x){
  if(!terra::same.crs(x, "epsg:4326")){
    x <- terra::project(x, "epsg:4326")
  }
  x
}

cols_reorder <- function(x, cols_obj, positions){
  cols <- colnames(x)
  cols_rest <- cols[which(!cols %in% cols_obj)]
  new_cols <- vector("character", length(cols))
  new_cols[positions] <- cols_obj
  new_cols[which(!1:length(cols) %in% positions)] <- cols_rest

  if("data.table" %in% class(x)){
    x <- x[, ..new_cols]
  } else {
    x <- x[, new_cols]
  }

  return(x)
}

correct_path <- function(dir){
  repeat{
    c <- substr(dir, 1, 1)
    if(c == "/"){
      dir <- substr(dir, 2, nchar(dir))
    } else {
      break
    }
  }
  dir
}



