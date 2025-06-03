remove_packages <- function() {
  remove_all_packages <- function() {
    pkgs <- names(sessionInfo()$otherPkgs)
    while (length(pkgs) > 0) {
      lapply(pkgs, function(pkg) detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE))
      pkgs <- names(sessionInfo()$otherPkgs)
    }
  }
  remove_all_packages()
}

clear_envrionment <- function() {
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
}



# execute tests

  # prepare tests
    remove_packages()
    clear_envrionment()
    # don't install packages besides test suite in order not to influence script to be tested
    # define constants and functions, but be aware they might be overwritten by following loading of scripts
      # A <- 1
    # don't load utility scripts besides script to be tested in order not to influence script to be tested
      source("To_Be_Tested.R")

  # execute single test
    # execute positive test
      expected_result <- 6
      actual_result  <- function_to_be_tested(A, 2)
      if (actual_result == expected_result) {
        print("Test PASSED")
      } else {
        print("Test FAILED")
        stop()
      }
      print("Postive test finished")
    # execute negative test
      expected_result <- 6
      actual_result  <- function_to_be_tested(A, 4)
      if (actual_result != expected_result) {
        print("Test PASSED")
      } else {
        print("Test FAILED")
        stop()
      }
      print("Negative test finished")
