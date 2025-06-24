# prepare tests

  # close open devices
    while (dev.cur() > 1) dev.off()

  # clear envrionment
    rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  # define constants and functions, but be aware they might be overwritten by following loading of scripts
    # set working directory
      # if (!require(this.path)) install.packages("this.path")
      # library(this.path)
      # setwd(dirname(this.path()))
    # define dummy data
      load("TCGA_kidney_unnormalized.RData")
      DATA <- dataset$data[1:300, 1:100]
      METHOD  <- "euclidean"
      METHODS <- c("manhattan", "euclidean", "maximum", "binary"#, "canberra"
                  ,"minkowski")
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

  for (method in METHOD) {

    cat("\fFunction Test for Method", method, "\n")

    # calculate distances by stats package
      cat("\n\n\ndist by stats package\n\n")
      if (!require(stats)) install.packages("stats")
      library(stats)
      # create and calculate dist
        stats_time <- system.time(stats_dist <- dist(DATA, method = method, diag = DIAG, upper = UPPER, p = P))
      # # call methods
      #   print(as.matrix(stats_dist))
      #   cat("\n")
      #   print(stats_dist)
      #   cat("\n")
      cat("Execution Times by stats Package:\n\n", stats_time)

    # calculate distances by distances script
      cat("\n\n\ndist by distances script\n\n")
      source("Distances.R")
      # create and calculate dist
        distances_time <- system.time(distances <- dist(DATA, method = method, diag = DIAG, upper = UPPER, p = P))
      # # call methods
      #   print(as.matrix(distances))
      #   cat("\n")
      #   print(distances)
      #   cat("\n")
      rm(dist)
      rm(as.matrix.dist)
      rm(print.dist)
      cat("Execution Times by Distances Script:\n\n", distances_time)

    # compare results
      cat("\n\n\n")
      if (all.equal(as.vector(stats_dist), as.vector(distances))) {
        cat("Tolerance Test PASSED\n")
      } else {
        cat("Tolerance Test FAILED\n")
        stop()
      }
      cat("\n")
      if (identical(as.vector(stats_dist), as.vector(distances))) {
        cat("Precision Test PASSED\n")
      } else {
        cat("Precision Test FAILED\n")
        stop()
      }

  }

  cat("\n\n\nFunction Tests for All Methods PASSED\n")
