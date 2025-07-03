if (!requireNamespace("KEGGREST", quietly = TRUE)) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  BiocManager::install("KEGGREST")
}
library(KEGGREST)

source("../service/csv.R")

ensure_gene_ids_are_strings <- function(gen_ids) {
  # checking given gene ids to be strings (else problems with indexing later)
  if (!is.character(gen_ids)) {
    # converting to strings
    temp_names <- names(gen_ids)
    gen_ids <- as.character(gen_ids)
    names(gen_ids) <- temp_names
  }
  gen_ids
}

filter_usable_gen_ids <- function(gen_ids, limit) {
  gen_ids <- gen_ids[gen_ids != "0"]
  # if given a limit of genes
  if (limit > 0) {
    gen_ids <- gen_ids[1:min(limit, length(gen_ids))]
  }
  gen_ids
}

get_filtered_matrix_genes <- function(data, data_gen_ids, query_gen_ids, limit = 0) { # nolint: line_length_linter.
  query_gen_ids <- ensure_gene_ids_are_strings(query_gen_ids)
  query_gen_ids <- filter_usable_gen_ids(query_gen_ids, limit)

  # mask for all occurring gene ids
  mask_gen_ids <- data_gen_ids %in% query_gen_ids
  # actual filtering
  data_gen_ids_filtered <- data_gen_ids[mask_gen_ids]
  # Select only rows where mask_gen_ids is TRUE
  data_filtered <- data[which(mask_gen_ids), , drop = FALSE]

  # Update query_gen_ids to only include those that were found in the data
  query_gen_ids <- query_gen_ids[query_gen_ids %in% data_gen_ids_filtered]

  # changing row names to correspond to the gen ids
  if (nrow(data_filtered) > 0) {
    rownames(data_filtered) <- data_gen_ids_filtered
  }

  # matrix patient data after feature selection
  # gen ids from matrix patient data (mapping to rows in matrix)
  # query gen ids & names used for feature selection
  list(data_filtered, data_gen_ids_filtered, query_gen_ids)
}

# Use KeggREST to retrieve genes from a KEGG pathway
retrieve_pathway_genes <- function(hsa_id) {
  pathway_id <- hsa_id
  pathway_data <- keggGet(pathway_id)
  pathway_genes <- pathway_data[[1]]$GENE
  pathway_genes
}

feature_selection <- function(csv_path, hsa_id) {
  filtered_patient_data <- get_processed_patient_data(csv_path)
  kegg_data <- retrieve_pathway_genes(hsa_id)
  gene_ids <- kegg_data[seq(1, length(kegg_data), 2)]
  gene_symbols <- kegg_data[seq(2, length(kegg_data), 2)]
  gene_df <- data.frame(ID = gene_ids, Name = gene_symbols)

  selection_results <- get_filtered_matrix_genes(
    filtered_patient_data[[1]],
    filtered_patient_data[[3]],
    gene_ids
  )

  list(
    data_filtered = t(selection_results[[1]]),
    gene_ids = selection_results[[2]],
    query_gene_ids = selection_results[[3]],
    gene_df = gene_df
  )
}
