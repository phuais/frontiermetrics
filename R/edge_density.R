edge_density <- function(x, count_boundary = F, directions = 8){

  rcpp_get_coocurrence_matrix_single <- utils::getFromNamespace("rcpp_get_coocurrence_matrix_single", "landscapemetrics")

  resolution <- x[[1]]
  landscape <- x[[2]]

  # all values NA
  if(all(is.na(landscape))) return(NA)

  extras <- landscapemetrics::prepare_extras("lsm_c_te", landscape_mat = landscape,
                             directions = directions, resolution = resolution)

  # get class id
  classes <- extras$classes
  class_patches <- extras$class_patches
  resolution_x <- resolution[[1]]
  resolution_y <- resolution[[2]]

  if (length(classes) == 1 && !count_boundary) {
    return(0)
  } else {
    # resolution not identical in x and y direction
    if (resolution_x != resolution_y) {
      top_bottom_matrix <- matrix(c(NA, NA, NA,
                                    1,  0, 1,
                                    NA, NA, NA), 3, 3, byrow = TRUE)

      left_right_matrix <- matrix(c(NA, 1, NA,
                                    NA, 0, NA,
                                    NA, 1, NA), 3, 3, byrow = TRUE)
    }

    patches_class <- classes[1]

    # get connected patches
    landscape_labeled <- class_patches[[as.character(patches_class)]]

    # set all non-class patches, but not NAs, to -999
    edge_cells <- which(!is.na(landscape) & landscape != patches_class)

    landscape_labeled[edge_cells] <- -999

    # add one row/column to count landscape boundary
    if(count_boundary){
      landscape_labeled <- pad_raster_internal(landscape = landscape_labeled,
                                               pad_raster_value = -999,
                                               pad_raster_cells = 1,
                                               global = FALSE)

      # set NA to -999
      landscape_labeled[is.na(landscape_labeled)] <- -999
    }

    # resolution identical in x and y direction
    if(resolution_x == resolution_y){

      # get adjacencies
      neighbor_matrix <- rcpp_get_coocurrence_matrix_single(landscape_labeled,
                                                                               directions = as.matrix(4),
                                                                               single_class = -999)

      # sum of all adjacencies between patch id and non-class patches (-999) converted to edge length
      edge_ik <- (sum(neighbor_matrix[2:nrow(neighbor_matrix), 1])) * resolution_x
    } else {

      # get adjacencies
      left_right_neighbours <- rcpp_get_coocurrence_matrix_single(landscape_labeled,
                                                                                     directions = as.matrix(left_right_matrix),
                                                                                     single_class = -999)

      # sum of all adjacencies between patch id and non-class patches (-999) converted to edge length
      edge_ik_left_right <- sum(left_right_neighbours[2:nrow(left_right_neighbours), 1]) * resolution_x

      # get adjacencies
      top_bottom_neighbours <- rcpp_get_coocurrence_matrix_single(landscape_labeled,
                                                                                     directions = as.matrix(top_bottom_matrix),
                                                                                     single_class = -999)

      # sum of all adjacencies between patch id and non-class patches (-999) converted to edge length
      edge_ik_top_bottom <- sum(top_bottom_neighbours[2:nrow(top_bottom_neighbours), 1]) * resolution_y

      # add edge length in x- and y-direction
      edge_ik <- edge_ik_left_right + edge_ik_top_bottom
    }

    # Calculate edge density (m / he)
    ed <- 10000*edge_ik/(resolution_x*resolution_y*length(landscape))

    return(ed)
  }
}
