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

    # define object
      # convert dist class and data frame as data to matrix
        if (inherits(data, "dist"))   as.matrix(data)
        else if (is.data.frame(data)) as.matrix(data)
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
