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
names(test_gene_ids) <- c(
  "pancreatic triacylglycerol lipase",
  "transcriptional activator Myb",
  "insulin",
  "cell surface A33 antigen",
  "NADPH oxidase 1",
  # not in patient data example
  "AKT serine/threonine kinase 3",
  "FRAT regulator of WNT signaling pathway 1"
)

# example gene ids with missing gen ids (KEGG results)
test_gene_ids_missing <- c(5406, 4602, 3630, 0, 10223, 27035, 10000, 0, 10023)
names(test_gene_ids_missing) <- c(
  "pancreatic triacylglycerol lipase",
  "transcriptional activator Myb",
  "insulin",
  "missing gen id 1", # missing
  "cell surface A33 antigen",
  "NADPH oxidase 1",
  # not in patient data example
  "AKT serine/threonine kinase 3",
  "missing gen id 2", # missing
  "FRAT regulator of WNT signaling pathway 1"
)

# limit of gen ids to cluster
test_limit <- 4




###
# reading CSV file
test_reading_in_example <- function() {
  test_data_results <- get_processed_patient_data(test_file_name)
  print("[Dimension] Data matrix of example")
  print(dim(test_data_results[[1]]))
  print("[Length] Labels of example")
  print(length(test_data_results[[2]]))
}


# testing
execute_test_reading_in <- readline("Test reading in CSV file? (y|n) ")
if (tolower(execute_test_reading_in) == "y") {
  test_reading_in_example()
}



###
# actual feature selection
test_selection <- function(file_name, gen_ids, limit = 0) {
  test_data_results <- get_processed_patient_data(file_name)
  test_selection_results <- get_filtered_matrix_genes(test_data_results[[1]],
                                                      gen_ids,
                                                      limit)
  print("[Length] Labels of example")
  print(length(test_data_results[[2]]))
  print("[Dimension] Data matrix of example")
  print(dim(test_data_results[[1]]))
  print("[Dimension] Data matrix of example")
  print(dim(test_selection_results[[1]]))
  print("[Length] Filtered genes of example")
  print(length(test_selection_results[[2]]))
}


# testing
execute_feature_selection <- readline("Test complete feature selection? (y|n) ")
if (tolower(execute_feature_selection) == "y") {
  print("basic feature selection")
  test_selection(test_file_name, test_gene_ids)

  print("actual feature selection with missing gen ids")
  test_selection(test_file_name, test_gene_ids_missing)

  print("actual feature selection with limit of gen ids")
  test_selection(test_file_name, test_gene_ids_missing, test_limit)
}
