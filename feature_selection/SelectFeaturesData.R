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

get_filtered_matrix_genes <- function(data, genes) {
  genes <- ensure_gene_ids_are_strings(genes)
  occuring_genes <- find_all_occurring_gene_ids(rownames(data), genes)
  data_filtered <- filter_matrix_by_genes(data, occuring_genes)
  list(data_filtered, occuring_genes)
}
