
if (!requireNamespace("clusterProfiler", quietly = TRUE)) BiocManager::install("clusterProfiler")
if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) BiocManager::install("org.Hs.eg.db")
library(clusterProfiler)
library(org.Hs.eg.db)
if (!requireNamespace("KEGGREST", quietly = TRUE)) install.packages("KEGGREST")
library(KEGGREST)
kegg_pathway_names <- keggList("pathway", "hsa") #this works, it returns a list of all available human pathways
#why does this retrieve all human pathways and not just the one for the hsa ID which is selected by the User via the GUI?



pathway2gene_df <- kegg2gene$KEGGPATHID2EXTID #creates error: kegg2gene not found - is there a library or something missing?
pathway2name_df <- kegg2gene$KEGGPATHID2NAME

# 2. Your Entrez IDs
your_entrez_ids <- rownames(for_retreiving[[1]]) #where does this come from? for_retrieving is not defined anywhere
#are these the IDs which are taken from the CSV which are uploaded by the user? 

# Create a list: pathway ID -> all genes in that pathway
pathway_to_genes <- split(pathway2gene_df$to, pathway2gene_df$from)

# For each pathway, keep only genes present in the Data
pathway_genes_list <- lapply(pathway_to_genes, function(ids) intersect(your_entrez_ids, ids))

#  Map Entrez IDs to gene symbols (skip empty sets)
genes <- lapply(
  pathway_genes_list,
  function(entrez_vec) {
    if (length(entrez_vec) == 0) return(setNames(character(0), character(0)))
    gene_symbols <- mapIds(
      org.Hs.eg.db,
      keys = entrez_vec,
      column = "SYMBOL",
      keytype = "ENTREZID",
      multiVals = "first"
    )
    setNames(entrez_vec, gene_symbols)
  }
)

# 6. Make a named vector for pathway names
pathway_names <- setNames(pathway2name_df$to, pathway2name_df$from)

# 7. Example: print the first pathway's name and your genes in it
#first_pathway_id <- names(pathway_genes_list_symbols)[1]
#cat("Pathway:", pathway_names[first_pathway_id], "\n")
#print(pathway_genes_list_symbols[[first_pathway_id]])

# 8. (Optional) View results for a specific pathway
# Example: show genes for the first pathway
#first_pathway_id <- names(pathway_genes_list_symbols)[1]
#cat("Pathway ID:", first_pathway_id, "\n")
#print(pathway_genes_list_symbols[[first_pathway_id]])




#print(genes)