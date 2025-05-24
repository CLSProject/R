source("feature_selection/ReadInCSV.R")
source("feature_selection/SelectFeaturesData.R")


###
# example CSV file
test_file_name <- paste(getwd(),
                        "/feature_selection/data",
                        "/Colon_vs_Pancreas_selected.csv",
                        sep = "", collapse = "")
# example gene ids (KEGG results)
test_gene_ids <- c(5406, 4602, 3630, 10223, 27035, 10000, 10023)
test_gene_names <- c(
  "pancreatic triacylglycerol lipase",
  "transcriptional activator Myb",
  "insulin",
  "cell surface A33 antigen",
  "NADPH oxidase 1",
  # not in patient data example
  "AKT serine/threonine kinase 3",
  "FRAT regulator of WNT signaling pathway 1"
)



###
# reading CSV file
test_reading_in_example <- function() {
  test_data_results <- get_processed_patient_data(test_file_name)

  print("[Dimension] Data matrix of example")
  print(dim(test_data_results[[1]]))
  print("[Dimension] Labels of example")
  print(dim(test_data_results[[2]]))
}


###
# actual feature selection
test_selection_example <- function() {
  test_data_results <- get_processed_patient_data(test_file_name)
  test_selection_results <- get_filtered_matrix_genes(test_data_results[[1]],
                                                      test_gene_ids,
                                                      test_gene_names)

  print("[Dimension] Labels of example")
  print(dim(test_data_results[[2]]))
  print("[Dimension] Data matrix of example")
  print(dim(test_data_results[[1]]))
  print("[Dimension] Data matrix of example")
  print(dim(test_selection_results[[1]]))
  print("[Dimension] Filtered genes of example")
  print(dim(test_selection_results[[2]]))
  print("[Dimension] Filtered gene names of example")
  print(dim(test_selection_results[[3]]))
}
