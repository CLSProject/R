# save open files
  if (!require(rstudioapi))install.packages("rstudioapi")
  library(rstudioapi)
  documentSaveAll()
# close open devices
  sink()
  while (dev.cur() > 1) dev.off()



# prepare script test
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







# load script to be tested
  # source("Aufgabe.1a.R")



# install necessary packages if not installed yet and load them
  # if (!require(rstudioapi))install.packages("rstudioapi")
  # library(rstudioapi)
# load necessary scripts
  # source("Utilities.R")
# preset constants
  # CONSTANT <- "Constant"

    

function_to_be_tested <- function() {
}







# execute script test
