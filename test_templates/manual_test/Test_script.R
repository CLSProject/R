# save open files
  # if (!require(rstudioapi))install.packages("rstudioapi")
  # library(rstudioapi)
  # documentSaveAll()
# close open devices
  # while (dev.cur() > 1) dev.off()



# prepare script test

  # remove packages for having a basic package set
    # remove_all_packages <- function() {
    #   pkgs <- names(sessionInfo()$otherPkgs)
    #   while (length(pkgs) > 0) {
    #     lapply(pkgs, function(pkg) detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE))
    #     pkgs <- names(sessionInfo()$otherPkgs)
    #   }
    # }
    # remove_all_packages()
  # clear environment
    # rm(list = ls())

  # install necessary packages if not installed yet and load them
    # if (!require(this.path))install.packages("this.path")
    # library(this.path)
  # load necessary scripts
    # setwd(dirname(this.path()))
    # source("Utilities.R")
    # setwd("e:/")

  # load script to be tested
    setwd("e:/CLS_Projekt/Test/")
    source("Script_to_be_tested.R")
    setwd("e:/")



# execute tests

  test_matrix <- matrix(1:(3 * 3), nrow = 3)
  print(test_matrix)
  function_to_be_tested(test_matrix)

  # # test script to be tested manually
  #   # execute positive test
  #     test_matrix <- matrix(1:(NUMBER*NUMBER), nrow = NUMBER)
  #     expected_dist_matrix <- c(3, 6, 3)
  #     if (sum(as.vector(function_to_be_tested(test_matrix)) - expected_dist_matrix) == 0) {
  #       print("Test passed")
  #     } else {
  #       print("Test failed")
  #       stop()
  #     }
  #   # execute positive test
  #     test_matrix <- matrix(1:(NUMBER*NUMBER), nrow = NUMBER)
  #     expected_dist_matrix <- c(3, 7, 3)
  #     if (sum(as.vector(function_to_be_tested(test_matrix)) - expected_dist_matrix) != 0) {
  #       print("Test passed")
  #     } else {
  #       print("Test failed")
  #     }

  # test script to be tested by testthat
    # tt(
    #   "Distance Calculation works as expected",
    #   {
    #     NUMBER <- 3
    #     expect_equal(as.vector(function_to_be_tested( matrix(1:(NUMBER*NUMBER), nrow = NUMBER))),
    #                  c(3, 6, 3)
    #                 )
    #   }
    # )
