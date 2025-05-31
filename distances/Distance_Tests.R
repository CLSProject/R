# prepare test
  # remove packages for having a basic package set
    remove_all_packages <- function() {
      pkgs <- names(sessionInfo()$otherPkgs)
      while (length(pkgs) > 0) {
        lapply(pkgs, function(pkg) detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE))
        pkgs <- names(sessionInfo()$otherPkgs)
      }
    }
    remove_all_packages()
  # clear environment
    rm(list = ls())

# don't install packages besides test suite              in order not to influence script to be tested
# preset constants, but be aware they might be overwritten be script to be tested
  data_mat <- matrix(c( 1,  2,  0,  0,  0,  0,
                        0,  0,  3,  4,  0,  0,
                        0,  0,  0,  0,  5,  6),
                     ncol = 6,
                     byrow = TRUE
                    )
# don't load utility scripts besides script to be tested in order not to influence script to be tested
  source("Distances.R")
  source("Utilities.R")



# execute tests

  # distance matrix by stats::dist
    cat("\fdistance matrix by stats::dist\n")
    # dist_mat <- stats::dist(data_mat, method = "euclidian") #, diag = TRUE, upper = TRUE)
    source("stats_dist.R")
    dist_mat <- dist(data_mat, method = "euclidian") #, diag = TRUE, upper = TRUE)
    cat("\n")
    print(dist_mat)
    myprint.dist(dist_mat)
    print(as.matrix(dist_mat))
    print_everything(dist_mat)

  readline(prompt = "Drücke [Enter], um fortzufahren...")

  # S3Class
    cat("S3Class\n")
    source("Distances.R")
    cat("\n")
    print_everything(movieList)

  # # calculate distance matrix by calc_dist_mat
  #   dist_mat <- calc_dist_mat(data_mat)
  #   print_everything(dist_mat)
