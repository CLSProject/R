if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("AnnotationDbi")
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("org.Hs.eg.db")

library(AnnotationDbi)
library(org.Hs.eg.db)

# Define your list of Entrez gene IDs or gene symbols
gene_list <- c("7157", "675", "673") # Example: TP53, BRCA2, BRAF

# Retrieve gene symbol, Entrez ID, and full gene name
annotations <- select(
  org.Hs.eg.db,
  keys = gene_list,
  columns = c("SYMBOL", "GENENAME"),
  keytype = "ENTREZID"
)

print(annotations)
