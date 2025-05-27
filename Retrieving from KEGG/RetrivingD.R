if (!require("KEGGREST")) install.packages("KEGGREST")


retrieve_from_kegg <- function(occuring_gene_ids){
  library(KEGGREST)
# Convert Entrez IDs to KEGG format (hsa: + Entrez ID)
  kegg_ids <- paste0("hsa:", occuring_gene_ids)

# Fetch gene information
  kegg_data <- lapply(kegg_ids, function(id) {
   tryCatch({
      keggGet(id)[[1]]
   }, error = function(e) NULL)
  })

# Remove NULL entries for unavailable genes
  kegg_data <- kegg_data[!sapply(kegg_data, is.null)]
  
  print(kegg_data)
}

retrieve_from_kegg(occuring_gene_ids)
