# install necessary packages if not installed yet and load them
  if (!require(this.path))install.packages("this.path")
  library(this.path)
# load necessary scripts
  setwd(dirname(this.path()))
  source("Utilities.R")
# preset constants
  CONSTANT <- 3



function_to_be_tested <- function (a, b) {
  return (a + b + CONSTANT)
}
