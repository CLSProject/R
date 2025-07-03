#' @title
#' dist: Distance Matrix Computation
#'
#' @description
#' This function computes and returns the distance matrix computed by using the specified distance measure to compute the distances between the rows of a data matrix.
#'
#' @usage
#'               dist(data, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
#' print(obj)        # S3 method for dist
#' as.matrix(obj)    # S3 method for dist
#'
#' @param data   numeric matrix, data frame or "dist" object
#'
#' @param method the distance measure to be used.
#' #' This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#'
#' @param diag   logical value indicating whether the diagonal of the distance matrix should be printed by print.dist
#'
#' @param upper  logical value indicating whether the upper triangle of the distance matrix should be printed by print.dist
#'
#' @param p      the power of the Minkowski distance
#'
#' @return       object of class "dist"
#'
#' The lower triangle of the distance matrix stored by columns in a vector.
#' If n is the number of data rows, the length of the vector is n * (n - 1) / 2, i.e., of order n^2.
#'
#' The object has the following attributes (besides "class" equal to "dist"):
#'
#' Size:        integer, the number of observations in the dataset
#'
#' Labels:      optionally, contains the labels, if any, of the observations of the dataset
#'
#' Diag, Upper: logicals corresponding to the arguments diag and upper above, specifying how the object should be printed
#'
#' call:        optionally, the call used to create the object.
#'
#' method:      optionally, the distance method used; resulting from dist()


# construct dist
  dist <- function(data, method = "euclidean", diag = FALSE, upper = FALSE, p = 2) {

    calculate_manhattan <- function(lower_row, higher_row) {
      return (sum(
                  abs(lower_row - higher_row)
                 )
             )
    }

    calculate_euclidean <- function(lower_row, higher_row) {
      return (sqrt(
                   sum((lower_row - higher_row)^2
                  )
              )
             )
    }

    calculate_maximum <- function(lower_row, higher_row) {
      return (max(abs(
                      lower_row - higher_row
                     )
                 )
             )
    }

    calculate_binary <- function(lower_row, higher_row) {
      lower_row_unequal_zero  <- lower_row  != 0
      higher_row_unequal_zero <- higher_row != 0
      only_one_on             <- sum(xor(lower_row_unequal_zero,   higher_row_unequal_zero))
      at_least_one_on         <- sum(    lower_row_unequal_zero  | higher_row_unequal_zero )
      return (only_one_on / at_least_one_on)
    }

    calculate_canberra <- function(lower_row, higher_row) {
      denominator <- abs(lower_row) + abs(higher_row)
      numerator   <- abs(lower_row  - higher_row)
      return (sum(ifelse(denominator == 0, NA, numerator / denominator)
                 ,na.rm = TRUE
                 )
             )
    }

    calculate_minkowski <- function(lower_row, higher_row) {
      return (sum(
                  abs(
                      (lower_row - higher_row)^p
                     )
                 )^(1/p)
             )
    }

    calculate_distances <- function(data, method) {
      dist <- c()
      for (row_lower_number in  1:(nrow(data) - 1)) {
        for (row_higher_number in (row_lower_number + 1): nrow(data)) {
          distance <- get(paste("calculate", method, sep = "_"))(data[row_lower_number, ], data[row_higher_number, ])
          dist     <- c(dist, distance)
        }
      }
      return (dist)
    }

    # define object
      # convert dist class and data frame as data to matrix
        if (inherits(data, "dist"))   data <- as.matrix(data)
        else if (is.data.frame(data)) data <- as.matrix(data)
      # construct object
        obj <- calculate_distances(data, method)

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
