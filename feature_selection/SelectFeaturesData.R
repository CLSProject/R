###
# functions
ensure_gene_ids_are_strings <- function(gen_ids) {
  # checking given gene ids to be strings (else problems with indexing later)
  if (! is.character(gen_ids)) {
    # converting to strings
    temp_names <- names(gen_ids)
    gen_ids <- as.character(gen_ids)
    names(gen_ids) <- temp_names
  }
  return(gen_ids)
}


find_all_occurring_genes <- function(rownames_data, gen_ids, gene_names) {
  mask_occurring <- gen_ids %in% rownames_data
  list(gen_ids[mask_occurring], gene_names[mask_occurring])
}


get_filtered_matrix_genes <- function(data, gen_ids, gen_names) {
  gen_ids <- ensure_gene_ids_are_strings(gen_ids)

  occuring_genes_results <- find_all_occurring_genes(rownames(data),
                                                     gen_ids,
                                                     gen_names)
  data_filtered <- data[occuring_genes_results[[1]], ]

  list(data_filtered, occuring_genes_results[[1]], occuring_genes_results[[2]])
}
