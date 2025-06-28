###
# functions
# wo kommen die gen ids her? Aus der CSV? Wer stellt diese bereit?
#Test funktioniert soweit, noch zu definieren: wo werden Outputdaten abgelegt und wie werden diese dann abgerufen?
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


get_filtered_matrix_genes <- function(data, data_gen_ids, query_gen_ids, limit = 0) { # nolint: line_length_linter.
  query_gen_ids <- ensure_gene_ids_are_strings(query_gen_ids)
  query_gen_ids <- filter_usable_gen_ids(query_gen_ids, limit)

  # mask for all occurring gene ids
  mask_gen_ids <- data_gen_ids %in% query_gen_ids
  # actual filtering
  data_gen_ids_filtered <- data_gen_ids[mask_gen_ids]
  data_filtered <- data[mask_gen_ids, ]
  query_gen_ids <- query_gen_ids[query_gen_ids %in% data_gen_ids_filtered]

  # changing row names to correspond to the gen ids
  rownames(data_filtered) <- as.character(1:nrow(data_filtered))

  list(data_filtered, data_gen_ids_filtered, query_gen_ids)
}
