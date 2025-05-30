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
  A <- 1
# don't load utility scripts besides script to be tested in order not to influence script to be tested
  source("To_Be_Tested.R")



# execute tests
  expected_result <- 6
  actual_result  <- function_to_be_tested(A, 2)
  if (actual_result == expected_result) {
    print("Test PASSED")
  } else {
    print("Test FAILED")
    stop()
  }
  print("Test finished")
