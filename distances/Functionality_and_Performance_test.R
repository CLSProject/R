# prepare tests

  # close open devices
    while (dev.cur() > 1) dev.off()

  # clear envrionment
    rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  # define constants and functions, but be aware they might be overwritten by following loading of scripts
    # set working directory
      if (!require(this.path)) install.packages("this.path")
      library(this.path)
      setwd(dirname(this.path()))
    # define dummy data
      # DATA <- matrix(c( 1,  0,  3,  4
      #                 , 5,  6,  0, -8
      #                 , 9,  0, 11,  0
      #                 )
      #               ,ncol     = 4
      #               ,byrow    = TRUE
      #               ,dimnames = list(c("Zeile 1",  "Zeile 2",  "Zeile 3")
      #                               ,c("Spalte 1", "Spalte 2", "Spalte 3", "Spalte 4")
      #                               )
      #               )
      load("TCGA_kidney_unnormalized.RData")
      DATA <- dataset$data[1:300, 1:100]
      METHODS <- c("manhattan", "euclidean", "maximum", "binary","minkowski", "canberra") # miscalculation of canberra by stats::dist
      DIAG    <- FALSE
      UPPER   <- TRUE
      P       <- 3

  # remove packages
    pkgs <- names(sessionInfo()$otherPkgs)
    while (length(pkgs) > 0) {
      lapply(pkgs, function(pkg) detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE))
      pkgs <- names(sessionInfo()$otherPkgs)
    }
  # don't install packages besides test suite in order not to influence script to be tested

  # don't load utility scripts besides script to be tested in order not to influence script to be tested



# execute tests

  for (method in METHODS) {

    cat("\fFunctionality Test for Method", method, "\n")

    # calculate distances by stats package
      cat("\n\n\ndist by stats package\n\n")
      if (!require(stats)) install.packages("stats")
      library(stats)
      # create and calculate dist
        stats_time <- system.time(stats_dist <- dist(DATA, method = method, diag = DIAG, upper = UPPER, p = P))
      # call methods
        print((as.matrix(stats_dist))[1:min(nrow(as.matrix(stats_dist)), 4), 1:min(ncol(as.matrix(stats_dist)), 4)])
        cat("\n")
        # print(stats_dist)
        # cat("\n")
      cat("Execution Times by stats Package:\n", stats_time)

    # calculate distances by distances script with for loops in method functions
      cat("\n\n\n\ndist by distances script with for loops in method functions\n\n")
      source("Distances_For_Loops.R")
      # create and calculate dist
        distances_time <- system.time(distances_for_loops <- dist(DATA, method = method, diag = DIAG, upper = UPPER, p = P))
      # call methods
        print((as.matrix(distances_for_loops))[1:min(nrow(as.matrix(distances_for_loops)), 4), 1:min(ncol(as.matrix(distances_for_loops)), 4)])
        cat("\n")
        # print(distances_for_loops)
        # cat("\n")
      rm(dist)
      rm(as.matrix.dist)
      rm(print.dist)
      cat("Execution Times by Distances Script:\n", distances_time)
    # compare results
      cat("\n\n")
      if (all.equal(as.vector(stats_dist), as.vector(distances_for_loops))) {
        cat("Tolerance Test PASSED")
      } else {
        cat("Tolerance Test FAILED")
        stop()
      }
      cat("\n")
      if (identical(as.vector(stats_dist), as.vector(distances_for_loops))) {
        cat("Precision Test PASSED")
      } else {
        cat("Precision Test FAILED")
        print((as.vector(stats_dist) - as.vector(distances_for_loops))[1:16])
      }

    # calculate distances by distances script with vector operations in method functions
      cat("\n\n\n\ndist by distances script with vector operations in method functions\n\n")
      source("Distances.R")
      # create and calculate dist
        distances_time <- system.time(distances <- dist(DATA, method = method, diag = DIAG, upper = UPPER, p = P))
      # call methods
        print((as.matrix(distances))[1:min(nrow(as.matrix(distances)), 4), 1:min(ncol(as.matrix(distances)), 4)])
        cat("\n")
        # print(distances)
        # cat("\n")
      rm(dist)
      rm(as.matrix.dist)
      rm(print.dist)
      cat("Execution Times by Distances Script:\n", distances_time)
    # compare results
      cat("\n\n")
      print(all.equal(as.vector(stats_dist), as.vector(distances)))
      if (all.equal(as.vector(stats_dist), as.vector(distances))) {
        cat("Tolerance Test PASSED")
      } else {
        cat("Tolerance Test FAILED")
        stop()
      }
      cat("\n")
      if (identical(as.vector(stats_dist), as.vector(distances))) {
        cat("Precision Test PASSED")
      } else {
        cat("Precision Test FAILED\n")
        print((as.vector(stats_dist) - as.vector(distances))[1:16])
      }

    readline(prompt = "Drücke [Enter], um fortzufahren...")

  }

  cat("\n\n\nFunctionality Tests for All Methods PASSED\n")
