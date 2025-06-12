source("feature_selection/ReadInCSV.R")
source("feature_selection/SelectFeaturesData.R")


###
# example CSV file
test_file_name <- paste(getwd(),
                        "/feature_selection/data",
                        "/Colon_vs_Pancreas_selected.csv",
                        sep = "", collapse = "")
# large CSV file
test_file_name_large <- paste(getwd(),
                              "/feature_selection/data",
                              "/TCGA_kidney_unnormalized.csv",
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
test_reading_in_example <- function(file_name, print_gid = FALSE) {
  test_data_results <- get_processed_patient_data(file_name)
  print("[Dimension] Data matrix of example")
  print(dim(test_data_results[[1]]))
  print("[Length] Labels of example")
  print(length(test_data_results[[2]]))
  print("[Length] Gen ids of example")
  print(length(test_data_results[[3]]))
  if (print_gid) {
    print(test_data_results[[3]])
  }
}


# testing
execute_test_reading_in <- readline("Test reading in CSV file? (y|n) ")
if (tolower(execute_test_reading_in) == "y") {
  cat("\nBasic reading in")
  test_reading_in_example(test_file_name, print_gid = TRUE)

  cat("\nReading in large CSV")
  test_reading_in_example(test_file_name_large)
}



###
# actual feature selection
test_selection <- function(file_name, query_gen_ids, limit = 0, print_gid = FALSE) { # nolint: line_length_linter.
  test_data_results <- get_processed_patient_data(file_name)
  test_selection_results <- get_filtered_matrix_genes(test_data_results[[1]],
                                                      test_data_results[[3]],
                                                      query_gen_ids,
                                                      limit)
  print("[Length] Labels of example")
  print(length(test_data_results[[2]]))
  print("[Dimension] Data matrix of example")
  print(dim(test_data_results[[1]]))
  print("[Dimension] Data matrix of example")
  print(dim(test_selection_results[[1]]))
  print("[Length] Filtered query genes of example")
  print(length(test_selection_results[[3]]))
  print("[Length] Gen ids of example")
  print(length(test_data_results[[3]]))
  print("[Length] Filtered gen ids of example")
  print(length(test_selection_results[[2]]))
  if (print_gid) {
    print(test_data_results[[3]])
  }
  print(test_selection_results[[2]])
}


# testing
execute_feature_selection <- readline("Test complete feature selection? (y|n) ")
if (tolower(execute_feature_selection) == "y") {
  cat("\nBasic feature selection")
  test_selection(test_file_name, test_gene_ids, print_gid = TRUE)

  cat("\nActual feature selection with missing gen ids")
  test_selection(test_file_name, test_gene_ids_missing, print_gid = TRUE)

  cat("\nActual feature selection with limit of gen ids")
  test_selection(test_file_name, test_gene_ids_missing, test_limit, print_gid = TRUE) # nolint: line_length_linter.
}
# with large input data
execute_feature_selection_large <- readline("Test feature selection on large CSV? (y|n) ") # nolint: line_length_linter.
if (tolower(execute_feature_selection_large) == "y") {
  cat("\nFeature selection on large data")
  test_selection(test_file_name_large, test_gene_ids)
}



###
# duplicated ids in patient data CSV
execute_show_duplicated_ids <- readline("Show problem with duplicated ids? (y|n) ") # nolint: line_length_linter.
if (tolower(execute_show_duplicated_ids) == "y") {
  dup_ids_data <- read.csv(test_file_name_large)
  occurr_dup_ids_all <- table(dup_ids_data[, 1])
  occurr_dup_ids <- occurr_dup_ids_all[occurr_dup_ids_all > 1]
  print(length(occurr_dup_ids))
  print(table(occurr_dup_ids))
}
