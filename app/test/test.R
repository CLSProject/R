library(pheatmap)
library(gplots)
library(RColorBrewer)
library(dendextend)

source("../feature_selection/ReadInCSV.R")
source("../feature_selection/SelectFeaturesData.R")
source("../normalization/Normalization.R")
source("../clustern/clustering_base_algo.R")


test_file <- "../feature_selection/data/TCGA_kidney_unnormalized.csv"
disease <- "hsa05211" # Renal Cancer Kidney

cat("DEBUG: Datei wurde hochgeladen\n")

filtered_patient_data <- feature_selection(test_file, disease)
cat("DEBUG: Feature Selection abgeschlossen\n")

print("DEBUG: Filtered Patient Data:")
print(filtered_patient_data)

