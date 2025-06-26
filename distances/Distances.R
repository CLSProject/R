#' @title        dist: Distance Matrix Computation
#' @description  This function computes and returns the distance matrix computed by using the specified distance measure to compute the distances between the rows of a data matrix.
#' @usage        dist(data, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
#'               print(obj)     # S3 method for dist
#'               as.matrix(obj) # S3 method for dist
#' @param data   numeric matrix, data frame or "dist" object
#' @param method the distance measure to be used. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#' @param diag   logical value indicating whether the diagonal of the distance matrix should be printed by print.dist
#' @param upper  logical value indicating whether the upper triangle of the distance matrix should be printed by print.dist
#' @param p      the power of the Minkowski distance.
#' @return       object of class "dist"
#'               The lower triangle of the distance matrix stored by columns in a vector, say do.
#'               If n is the number of observations, i.e., n <- attr(do, "Size"), then for the dissimilarity between (row) i and j is do[n*(i-1) - i*(i-1)/2 + j-i].
#'               The length of the vector is, i.e., of order
#'               The object has the following attributes (besides "class" equal to "dist"):
#'               Size:        integer, the number of observations in the dataset
#'               Labels:      optionally, contains the labels, if any, of the observations of the dataset
#'               Diag, Upper: logicals corresponding to the arguments diag and upper above, specifying how the object should be printed
#'               call:        optionally, the call used to create the object.
#'               method:      optionally, the distance method used; resulting from dist()
#' @details



# construct dist
  dist <- function(data, method = "euclidean", diag = FALSE, upper = FALSE, p = 2) {

    calculate_manhattan <- function(row_lower_number, row_higher_number) {
      distance <- 0
      for (col_number in 1:ncol(data)) {
        distance <- distance + abs(data[row_lower_number, col_number] - data[row_higher_number, col_number])
      }
      return (distance)
    }

    calculate_euclidean <- function(row_lower_number, row_higher_number) {
      distance <- 0
      for (col_number in 1:ncol(data)) {
        distance <- distance + (data[row_lower_number, col_number] - data[row_higher_number, col_number])^2
      }
      return (sqrt(distance))
    }

    calculate_maximum <- function(row_lower_number, row_higher_number) {
      distance <- 0
      for (col_number in 1:ncol(data)) {
        distance <- max(distance, abs(data[row_lower_number, col_number] - data[row_higher_number, col_number]))
      }
      return (distance)
    }

    calculate_binary <- function(row_lower_number, row_higher_number) {
      only_one_on     <- 0
      at_least_one_on <- 0
      for (col_number in 1:ncol(data)) {
        if (xor((data[row_lower_number, col_number] != 0),   (data[row_higher_number, col_number] != 0))) only_one_on     <- only_one_on     + 1
        if (    (data[row_lower_number, col_number] != 0) || (data[row_higher_number, col_number] != 0) ) at_least_one_on <- at_least_one_on + 1
      }
      return (only_one_on / at_least_one_on)
    }

    calculate_canberra <- function(row_lower_number, row_higher_number) {
      distance <- 0
      for (col_number in 1:ncol(data)) {
        denominator <- abs(data[row_lower_number, col_number]) + abs(data[row_higher_number, col_number])
        if (denominator != 0) {
          summand <- ( (abs(data[row_lower_number, col_number]  -     data[row_higher_number, col_number]))
                     / denominator
                     )
          distance <- distance + summand
        }
      }
      return (distance)
    }

    calculate_minkowski <- function(row_lower_number, row_higher_number) {
      distance <- 0
      for (col_number in 1:ncol(data)) {
        distance <- distance + (abs(data[row_lower_number, col_number] - data[row_higher_number, col_number]))^p
      }
      return (distance^(1/p))
    }

    calculate_distances <- function(data, method) {
      dist <- c()
      for (row_lower_number in  1:(nrow(data) - 1)) {
        for (row_higher_number in (row_lower_number + 1): nrow(data)) {
          distance <- get(paste("calculate", method, sep = "_"))(row_lower_number, row_higher_number)
          dist     <- c(dist, distance)
        }
      }
      return (dist)
    }

    calculate_vectorized_manhattan <- function(lower_row, higher_row) {
      return (sum(
                  abs(lower_row - higher_row)
                 )
             )
    }

    calculate_vectorized_euclidean <- function(lower_row, higher_row) {
      return (sqrt(
                   sum((lower_row - higher_row)^2
                  )
              )
             )
    }

    calculate_vectorized_maximum <- function(lower_row, higher_row) {
      return (max(abs(
                      lower_row - higher_row
                     )
                 )
             )
    }

    calculate_vectorized_binary <- function(lower_row, higher_row) {
      lower_row_unequal_zero  <- lower_row  != 0
      higher_row_unequal_zero <- higher_row != 0
      only_one_on             <- sum(xor(lower_row_unequal_zero,   higher_row_unequal_zero))
      at_least_one_on         <- sum(    lower_row_unequal_zero  | higher_row_unequal_zero )
      return (only_one_on / at_least_one_on)
    }

    calculate_vectorized_canberra <- function(lower_row, higher_row) {
      denominator <- abs(lower_row) + abs(higher_row)
      numerator   <- abs(lower_row  - higher_row)
      return (sum(ifelse(denominator == 0, NA, numerator / denominator)
                 ,na.rm = TRUE
                 )
             )
    }

    calculate_vectorized_minkowski <- function(lower_row, higher_row) {
      return (sum(
                  abs(
                      (lower_row - higher_row)^p
                     )
                 )^(1/p)
             )
    }

    calculate_vectorized_distances <- function(data, method) {
      dist <- c()
      for (row_lower_number in  1:(nrow(data) - 1)) {
        for (row_higher_number in (row_lower_number + 1): nrow(data)) {
          distance <- get(paste("calculate_vectorized", method, sep = "_"))(data[row_lower_number, ], data[row_higher_number, ])
          dist     <- c(dist, distance)
        }
      }
      return (dist)
    }

    # define object
      # convert dist class and data frame as data to matrix
        if (inherits(data, "dist"))   as.matrix(data)
        else if (is.data.frame(data)) as.matrix(data)
      # construct object
        obj <- calculate_vectorized_distances(data, method)
        # obj <- calculate_distances(data, method)

    # set class
      class(obj) <- "dist"

    # set attributes
      attr(obj, "Size")   <- nrow(data)
      attr(obj, "Labels") <- rownames(data)
      attr(obj, "Diag")   <- diag
      attr(obj, "Upper")  <- upper
      attr(obj, "method") <- method
      attr(obj, "p")      <- p
      attr(obj, "call")   <- match.call()

    return(obj)

  }



# convert dist to matrix
  as.matrix.dist <- function(obj) {
    mat <- matrix(0, nrow = attr(obj, "Size"), ncol = attr(obj, "Size")
                 ,dimnames = list(attr(obj, "Labels"), attr(obj, "Labels"))
                 )
    # fill lower triangular matrix
      mat[row(mat) > col(mat)] <- obj
    # fill upper triangular matrix
      mat <- mat + t(mat)
    return (mat)
  }



# print dist
  print.dist <- function(obj) {
    mat <- as.matrix(obj)
    if (!attr(obj, "Upper")) mat[row(mat) <  col(mat)] <- NA
    if (!attr(obj, "Diag"))  mat[row(mat) == col(mat)] <- NA
    print(mat)
  }
