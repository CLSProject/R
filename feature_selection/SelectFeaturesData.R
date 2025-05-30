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


filter_usable_gen_ids <- function(gen_ids, limit) {
  gen_ids <- gen_ids[gen_ids != "0"]
  # if given a limit of genes
  if (limit > 0) {
    gen_ids <- gen_ids[1:min(limit, length(gen_ids))]
  }
  return(gen_ids)
}


find_all_occurring_genes <- function(rownames_data, gen_ids) {
  gen_ids[gen_ids %in% rownames_data]
}


get_filtered_matrix_genes <- function(data, gen_ids, limit = 0) {
  gen_ids <- ensure_gene_ids_are_strings(gen_ids)
  gen_ids <- filter_usable_gen_ids(gen_ids, limit)

  gen_ids_filtered <- find_all_occurring_genes(rownames(data),
                                               gen_ids)
  data_filtered <- data[gen_ids_filtered, ]

  list(data_filtered, gen_ids_filtered)
}
