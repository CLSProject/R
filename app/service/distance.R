dist <- function(
  data,
  method = "euclidean",
  diag = FALSE,
  upper = FALSE,
  p = 2
) {

  calculate_manhattan <- function(lower_row, higher_row) { # nolint: line_length_linter, object_usage_linter.
    sum(
      abs(lower_row - higher_row)
    )
  }

  calculate_euclidean <- function(lower_row, higher_row) { # nolint: line_length_linter, object_usage_linter.
    sqrt(
      sum((lower_row - higher_row)^2)
    )
  }

  calculate_maximum <- function(lower_row, higher_row) { # nolint: line_length_linter, object_usage_linter.
    max(abs(
      lower_row - higher_row
    ))
  }

  calculate_binary <- function(lower_row, higher_row) { # nolint: line_length_linter, object_usage_linter.
    lower_row_unequal_zero <- lower_row != 0
    higher_row_unequal_zero <- higher_row != 0
    only_one_on <- sum(xor(lower_row_unequal_zero, higher_row_unequal_zero))
    at_least_one_on <- sum(lower_row_unequal_zero | higher_row_unequal_zero)
    (only_one_on / at_least_one_on)
  }

  calculate_canberra <- function(lower_row, higher_row) { # nolint: line_length_linter, object_usage_linter.
    denominator <- abs(lower_row) + abs(higher_row)
    numerator <- abs(lower_row - higher_row)
    sum(ifelse(denominator == 0, NA, numerator / denominator),
      na.rm = TRUE
    )
  }

  calculate_minkowski <- function(lower_row, higher_row) { # nolint: line_length_linter, object_usage_linter.
    sum(
      abs(
        (lower_row - higher_row)^p
      )
    )^(1 / p)
  }

  calculate_distances <- function(data, method) {
    dist <- c()
    for (row_lower_number in 1:(nrow(data) - 1)) {
      for (row_higher_number in (row_lower_number + 1):nrow(data)) {
        distance <- get(paste("calculate", method, sep = "_"))(
          data[row_lower_number, ],
          data[row_higher_number, ]
        )
        dist <- c(dist, distance)
      }
    }
    dist
  }

  # define object
  # convert dist class and data frame as data to matrix
  if (inherits(data, "dist")) {
    data <- as.matrix(data)
  } else if (is.data.frame(data)) {
    data <- as.matrix(data)
  }

  # construct object
  obj <- calculate_distances(data, method)

  # set class
  class(obj) <- "dist"

  # set attributes
  attr(obj, "Size") <- nrow(data)
  attr(obj, "Labels") <- rownames(data)
  attr(obj, "Diag") <- diag
  attr(obj, "Upper") <- upper
  attr(obj, "method") <- method
  attr(obj, "p") <- p
  attr(obj, "call") <- match.call()

  return(obj)
}

# convert dist to matrix
as.matrix.dist <- function(obj) {
  mat <- matrix(
    0,
    nrow = attr(obj, "Size"),
    ncol = attr(obj, "Size"),
    dimnames = list(attr(obj, "Labels"), attr(obj, "Labels"))
  )
  # fill lower triangular matrix
  mat[row(mat) > col(mat)] <- obj
  # fill upper triangular matrix
  mat <- mat + t(mat)
  mat
}
