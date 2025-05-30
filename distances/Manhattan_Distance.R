calc_manhattan_dist <- function(data_mat, row_dist, col_dist) {
  dist <- 0
  for (index in 1:ncol(data_mat)) {
    dist <- dist + abs(data_mat[row_dist, index] - data_mat[col_dist, index])
  }
  return(dist)
}

calc_euclidian_dist <- function(data_mat, row_dist, col_dist) {
  dist <- 0
  for (index in 1:ncol(data_mat)) {
    dist <- dist + (abs(data_mat[row_dist, index] - data_mat[col_dist, index]))^2
  }
  return(sqrt(dist))
}



calc_dist_mat <- function(data_mat) {
  dist_mat <- matrix(0, nrow=nrow(data_mat), ncol=nrow(data_mat))
  for (row_dist in 1:nrow(data_mat)) {
    for (col_dist in 1:nrow(data_mat)) {
      # dist <- calc_manhattan_dist(data_mat, row_dist, col_dist)
      dist <- calc_euclidian_dist(data_mat, row_dist, col_dist)
      dist_mat[row_dist, col_dist] <- dist
    }
  }
  printf("tst")
  return(dist_mat)
}



setup_calc_dist_mat()
# remove packages for having a basic package set
  remove_all_packages <- function() {
    pkgs <- names(sessionInfo()$otherPkgs)
    while (length(pkgs) > 0) {
      lapply(pkgs, function(pkg) detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE))
      pkgs <- names(sessionInfo()$otherPkgs)
    }
  }
  remove_all_packages()
# check completeness of packages
  source("Distance_Matrix.R")
  caöc