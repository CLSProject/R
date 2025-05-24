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


find_all_occurring_genes <- function(rownames_data, genes, gene_names) {
  mask_occurring <- genes %in% rownames_data
  list(genes[mask_occurring], gene_names[mask_occurring])
}


get_filtered_matrix_genes <- function(data, genes, names) {
  genes <- ensure_gene_ids_are_strings(genes)

  occuring_genes_results <- find_all_occurring_genes(rownames(data), genes)
  data_filtered <- data[occuring_genes_results[[1]], ]

  list(data_filtered, occuring_genes_results[[1]], occuring_genes_results[[2]])
}
