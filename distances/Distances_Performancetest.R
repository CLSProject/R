# prepare test
  source("Utilities.R")
  remove_packages()
  clear_envrionment()
  # don't install packages besides test suite in order not to influence script to be tested
  # define constants and functions, but be aware they might be overwritten by following loading of scripts
    load("TCGA_kidney_unnormalized.RData")
    DATA <- dataset$data[1:300, 1:100]








    METHOD <- "euclidean"
    DIAG   <- FALSE
    UPPER  <- TRUE
    P      <-           3
  # don't load utility scripts besides script to be tested in order not to influence script to be tested



# execute test

  # calculate distances by stats package
    cat("\f")
    cat("dist by stats package\n")
    cat("\n")
    if (!require(stats)) install.packages("stats")
    library(stats)
    # create and calculate dist
      stats_time <- system.time(stats_dist <- dist(DATA, method = METHOD, diag = DIAG, upper = UPPER, p = P))
    # call methods
      # print_object(format(stats_dist))
      # cat("\n")
      # print(as.matrix(stats_dist))
      # cat("\n")
      # print_object(as.dist(stats_dist))
      # cat("\n")
      # print(stats_dist)
      # cat("\n")
      # print_object(labels(stats_dist))
      # cat("\n")
  cat("Execution Times by stats Package:\n", stats_time)
  cat("\n")

  # calculate distances by distances script
    cat("\n\n\n")
    cat("dist by distances script\n")
    cat("\n")
    source("Distances.R")
    # create and calculate dist
      distances_time <- system.time(distances <- dist(DATA, method = METHOD, diag = DIAG, upper = UPPER, p = P))
    # call methods
      # print_object(format(distances))
      # cat("\n")
      # print(as.matrix(distances))
      # cat("\n")
      # print_object(as.dist(distances))
      # cat("\n")
      # print(distances)
      # cat("\n")
      # print_object(labels(distances))
      # cat("\n")
    cat("Execution Times by Distances Script:\n", distances_time)
    cat("\n")

  # compare results
    cat("\n\n\n")
    if (all.equal(as.vector(stats_dist), as.vector(distances))) {
      print("Tolerance Test PASSED")
    } else {
      print("Tolerance Test  FAILED")
      stop()
    }
    cat("\n")
    if (identical(as.vector(stats_dist), as.vector(distances))) {
      print("Precision Test PASSED")
    } else {
      print("Precision Test FAILED")
      stop()
    }
