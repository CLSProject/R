if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("GEOquery")
library(GEOquery)



getGEOdata <- function(GSE) {
  # Check if the GEO object already exists
  if (exists("geo_data")) {
    message("GEO object already exists. Loading from cache.")
    return(geo_data)
  }
  
  # Download the GEO data using the GEOquery::getGEO function
  geo_data <- GEOquery::getGEO(GSE, GSEMatrix = TRUE, AnnotGPL = TRUE)
  if (length(geo_data) == 0) {
    stop("Failed to download GEO data. Please check the GSE ID.")
  }
  # Save the GEO object to the global environment
  assign("geo_data", geo_data, envir = .GlobalEnv)
  
  return(geo_data)
}

# Example usage
GSE <- "GSE12345"  # Replace with your GEO Series ID
geo_data <- getGEOdata(GSE)
head(geo_data)  # Display the first few rows of the GEO data
# Extract the expression data
expr_data <- exprs(geo_data[[1]])

# Extract the sample information
sample_info <- pData(geo_data[[1]])

# Extract the feature information
feature_info <- fData(geo_data[[1]])
