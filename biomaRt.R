if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("biomaRt")
library(biomaRt)


# Connect to Ensembl's human gene dataset
ensembl <- useEnsembl(biomart = "genes", dataset = "hsapiens_gene_ensembl")

# Example gene symbols (BRCA2, TP53, BRAF)
gene_symbols <- c("BRCA2", "TP53", "BRAF")

# Get ALL available attributes (metadata fields) for genes
all_attributes <- listAttributes(ensembl)$name
#core_attributes <- c("hgnc_symbol", "ensembl_gene_id", "entrezgene_id",
                    # "chromosome_name", "start_position", "end_position",
                     #"gene_biotype", "description", "go_id", "kegg_enzyme")
core_attributes <- c("hgnc_symbol", "ensembl_gene_id", "entrezgene_id",
                     "chromosome_name", "start_position", "end_position",
                     "gene_biotype", "description")
# Retrieve all information (caution: returns >80 fields per gene)
gene_data <- getBM(
  attributes = core_attributes,  # Retrieve all available fields
  filters = "hgnc_symbol",
  values = gene_symbols,
  mart = ensembl
)

# Filter to show key columns (example)
gene_data[, c("hgnc_symbol", "ensembl_gene_id", "chromosome_name", 
              "start_position", "end_position", "gene_biotype", 
              "entrezgene_id", "description")]

listAttributes(ensembl)$name

ensembl <- useEnsembl(biomart = "genes", dataset = "hsapiens_gene_ensembl")

# Verified core attributes (no kegg_enzyme)


gene_data <- getBM(
  attributes = core_attributes,
  filters = "hgnc_symbol",
  values = c("BRCA2", "TP53", "BRAF"),
  mart = ensembl
)

head(gene_data)
write.csv(as.matrix(gene_data), file = "gene_data.csv", row.names = FALSE)

