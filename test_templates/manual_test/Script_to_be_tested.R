# install necessary packages if not installed yet and load them
  if (!require(this.path))install.packages("this.path")
  library(this.path)
# load necessary scripts
  setwd(dirname(this.path()))
  source("Utilities.R")

# preset constants
  METHOD <- "manhattan"



# define function to be tested
  function_to_be_tested <- function(matrix) {
    dist_matrix <- dist(matrix, method = METHOD)
    print_anything(dist_matrix)
    return(dist_matrix)
  }







# test_matrix <- matrix(1:(9), nrow = 3)
# print(test_matrix)
# function_to_be_tested(test_matrix)
