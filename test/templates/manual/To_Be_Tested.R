# install necessary packages if not installed yet and load them
  # if (!require(this.path))install.packages("this.path")
  # library(this.path)
# preset constants, but be aware they might be overwritten by following loading of scripts
  # CONSTANT <- 3
# load necessary scripts
  # setwd(dirname(this.path()))
  # source("Utilities.R")



function_to_be_tested <- function(a, b) {
  return (a + b + CONSTANT)
}
