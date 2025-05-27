library(dplyr)


###
# Input:
# gene_ids (ids of genes that are relevant to the previously choosen disease)
###
# example gene ids as nested vectors (name, id of one gene per inner vector)
proto_gene_ids <- c(c("AKT serine/threonine kinase 3", 10000),
                    c("FRAT regulator of WNT signaling pathway 1", 10023),
                    c("caspase 12 (gene/pseudogene)", 100506742),
                    c("NDUFC2-KCTD14 readthrough", 100532726),
                    c("peptidylprolyl isomerase F", 10105),
                    c("ADAM metallopeptidase domain 10", 102),
                    c("cyclin dependent kinase 5", 1020))
###
# example gene ids as named vector ([vector]names -> gene names, values -> ids)
proto_gene_ids_v2 <- c(10000, 10023, 100506742, 100532726, 10105, 102, 1020)
names(proto_gene_ids_v2) <- c(
  "AKT serine/threonine kinase 3",
  "FRAT regulator of WNT signaling pathway 1",
  "caspase 12 (gene/pseudogene)",
  "NDUFC2-KCTD14 readthrough",
  "peptidylprolyl isomerase F",
  "ADAM metallopeptidase domain 10",
  "cyclin dependent kinase 5"
)
###
# same as previous, just a few specifically choosen gene ids & names
proto_gene_ids_specif <- c(5406, 4602, 3630, 10223, 27035, 10000, 10023)
names(proto_gene_ids_specif) <- c(
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
# functions
ensure_gene_ids_are_strings <- function(genes) {
  # checking given gene ids to be strings (else problems with indexing later)
  if (! is.character(genes)) {
    # converting to strings
    temp_names <- names(genes)
    genes <- as.character(genes)
    names(genes) <- temp_names
  }
  return(genes)
}


find_all_occurring_gene_ids <- function(rownames_data, genes) {
  genes[genes %in% rownames_data]
}

filter_matrix_by_genes <- function(data, genes) {
  data[genes, ]
}



###
# Reading and creating workable data matrix + labels
source(paste(getwd(),
             "/feature_selection/",
             "ReadInCSV.R",
             sep = "", collapse = ""))
patient_data_results <- get_processed_patient_data()
patient_data_matrix <- patient_data_results[[1]]
patient_data_labels <- patient_data_results[[2]]
gene_ids <- proto_gene_ids_specif

gene_ids <- ensure_gene_ids_are_strings(gene_ids)

occuring_gene_ids <- find_all_occurring_gene_ids(rownames(patient_data_matrix), gene_ids) # nolint: line_length_linter.
patient_data_matrix_filtered <- filter_matrix_by_genes(patient_data_matrix, occuring_gene_ids) # nolint: line_length_linter.

# small snippet to better visualize the filtering
patient_data_matrix_snippet <- patient_data_matrix[, 1:2]
patient_data_matrix_filtered_snippet <- filter_matrix_by_genes(patient_data_matrix_snippet, occuring_gene_ids) # nolint: line_length_linter.


###
# function printing details to results (only for development)
print_results_details <- function() {
  # differences patient matrix
  print("Using whole example data set")
  print("[Dimension] Whole matrix before filtering")
  print(dim(patient_data_matrix))
  print("[Dimension] Resulting matrix after filtering")
  print(dim(patient_data_matrix_filtered))
  print("")
  # small snippet of patient matrix to better visualize the filtering
  print("Small snippet of example data set")
  print("[Dimension + Data] Whole snippet matrix before filtering")
  print(dim(patient_data_matrix_snippet))
  print(patient_data_matrix_snippet)
  print("[Dimension + Data] Resulting snippet matrix after filtering")
  print(dim(patient_data_matrix_filtered_snippet))
  print(patient_data_matrix_filtered_snippet)
  print("")
  # differences on the gene ids side
  print("[Data] The given gene ids by the database query (later KEGG)")
  print(gene_ids)
  print("[Data] Filtered gene ids that actually occur in the patient data")
  print(occuring_gene_ids)
}

print_results_details()

