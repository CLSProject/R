source("feature_selection/ReadInCSV.R")
source("feature_selection/SelectFeaturesData.R")


# !!!
# check if this path works! depends on the working directory
# !!!
file_name_example <- "feature_selection/data/Colon_vs_Pancreas_selected.csv"

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



get_example_data <- function() {
  results_read_in <- get_processed_patient_data(file_name_example)
  results_selected <- get_filtered_matrix_genes(results_read_in[[1]],
                                                results_read_in[[3]],
                                                test_gene_ids)
  list(
    results_read_in[[2]], # labels
    results_selected[[1]], # matrix patient data after feature selection
    results_selected[[3]], # gen ids from matrix patient data (mapping to rows in matrix) # nolint: line_length_linter.
    results_selected[[2]] # query gen ids & names used for feature selection
  )
}
