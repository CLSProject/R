# prepare test
  source("Utilities.R")
  remove_packages()
  clear_envrionment()
  # don't install packages besides test suite in order not to influence script to be tested
  # define constants and functions, but be aware they might be overwritten by following loading of scripts
    # DATA  <- matrix(c( 0,  0,  0,
    #                    0,  3,  0,
    #                    0,  0,  4
    #                  ),
    #                 ncol     = 3,
    #                 byrow    = TRUE,
    #                 dimnames = list(c("Zeile 1",  "Zeile 2",  "Zeile 3"),
    #                                 c("Spalte 1", "Spalte 2", "Spalte 3")
    #                                ),
    #                )
    DATA   <- matrix(c( 1,  0,  3,  4
                      , 5,  6,  0,  8
                      , 9,  0, 11,  0
                      )
                     ,ncol     = 4
                     ,byrow    = TRUE
                     ,dimnames = list(c("Zeile 1",  "Zeile 2",  "Zeile 3")
                                     ,c("Spalte 1", "Spalte 2", "Spalte 3", "Spalte 4")
                                     )
                     )
    METHOD <- "euclidian"
    DIAG   <- FALSE
    UPPER  <- TRUE
    P      <-           3
  # don't load utility scripts besides script to be tested in order not to influence script to be tested



# execute test

  # calculate distances by package stats
    cat("\n\n\n")
    cat("dist by package stats\n")
    cat("\n")
    if (!require(stats)) install.packages("stats")
    library(stats)
    # create and calculate dist
      stats_dist <- dist(DATA, method = METHOD, diag = DIAG, upper = UPPER, p = P)
    # call methods
      # print_object(format(stats_dist))
      print(as.matrix(stats_dist))
      # print_object(as.dist(stats_dist))
      print(stats_dist)
      # print_object(labels(stats_dist))
    # call generic functions
      cat("\n\n")

  # calculate distances by distance
    cat("\n\n\n")
    cat("dist by distances\n")
    cat("\n")
    try(detach("package:stats", unload = TRUE), silent = TRUE)
    source("Distances.R")
    # create and calculate dist
      distances <- dist(DATA, method = METHOD, diag = DIAG, upper = UPPER, p = P)
    # call methods
      # print_object(format(distances))
      print(as.matrix(distances))
      # print_object(as.dist(distances))
      print(distances)
      # print_object(labels(distances))
    # call generic functions
      cat("\n\n")

  # compare results
    print(as.vector(stats_dist))
    print(as.vector(distances))
    print(identical(as.vector(distances), as.vector(stats_dist)))
