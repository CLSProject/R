if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("GEOquery")
library(GEOquery)


getGEO <- function(GSE) {
  # Check if the GEO object already exists
  if (exists("geo_data")) {
    message("GEO object already exists. Loading from cache.")
    return(geo_data)
  }
  
  # Download the GEO data
  geo_data <- getGEO(GSE, GSEMatrix = TRUE)
  
  # Save the GEO object to the global environment
  assign("geo_data", geo_data, envir = .GlobalEnv)
  
  return(geo_data)
}
# Example usage

GSE <- "GSE12345"  # Replace with your GEO Series ID
geo_data <- getGEO(GSE)
# Check if the GEO object is loaded
if (exists("geo_data")) {
  message("GEO object loaded successfully.")
} else {
  message("Failed to load GEO object.")
}
# Check the structure of the GEO data
str(geo_data)
# Check the class of the GEO data
class(geo_data)
# Check the length of the GEO data
length(geo_data)
# Check the names of the GEO data
names(geo_data)
# Check the first few rows of the GEO data
head(geo_data[[1]])
# Check the dimensions of the GEO data
dim(geo_data[[1]])
# Check the column names of the GEO data
colnames(geo_data[[1]])
# Check the row names of the GEO data
rownames(geo_data[[1]])
# Check the first few rows of the expression data
head(exprs(geo_data[[1]]))
# Check the dimensions of the expression data
dim(exprs(geo_data[[1]]))

