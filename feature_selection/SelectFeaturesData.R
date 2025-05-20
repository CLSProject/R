# Input:
# - patient_data_matrix (input & processed CSV)
# - gene_ids (numbers of relevant genes to choosen disease)

###
# example gene ids as nested vectors (name, id of one gene per inner vector)
proto_gene_ids <- c(c("AKT serine/threonine kinase 3", 10000),
                    c("FRAT regulator of WNT signaling pathway 1", 10023),
                    c("caspase 12 (gene/pseudogene)", 100506742),
                    c("NDUFC2-KCTD14 readthrough", 100532726),
                    c("peptidylprolyl isomerase F", 10105),
                    c("ADAM metallopeptidase domain 10", 102),
                    c("cyclin dependent kinase 5", 1020),
                    c("proteasome 26S subunit, non-ATPase 14", 10213),
                    c("TPTEP2-CSNK1E readthrough", 102800317),
                    c("APC regulator of WNT signaling pathway 2", 10297),
                    c("reticulon 3", 10313),
                    c("tubulin alpha 1b", 10376),
                    c("tubulin beta 3 class III", 10381),
                    c("tubulin beta 4A class IVa", 10382),
                    c("tubulin beta 4B class IVb", 10383),
                    c("ATP synthase peripheral stalk subunit d", 10476),
                    c("cytochrome c oxidase subunit NDUFA4-like", 107984365),
                    c("ubiquinol-cytochrome c reductase, complex III subunit XI", 10975), # nolint: line_length_linter.
                    c("P3R3URF-PIK3R3 readthrough", 110117499),
                    c("ADRM1 26S proteasome ubiquitin receptor", 11047),
                    c("frizzled class receptor 10", 11211),
                    c("tubulin alpha 3e", 112714),
                    c("cholinergic receptor muscarinic 1", 1128),
                    c("cholinergic receptor muscarinic 3", 1131),
                    c("cholinergic receptor muscarinic 5", 1133))
proto_gene_ids_small <- proto_gene_ids[1:5]
###
# example gene ids as named vector ([vector]names -> gene names, values -> ids)
proto_gene_ids_v2 <- c(
  10000, 10023, 100506742, 100532726, 10105, 102, 1020,
  10213, 102800317, 10297, 10313, 10376, 10381, 10382, 10383, 10476, 107984365,
  10975, 110117499, 11047, 11211, 112714, 1128, 1131, 1133
)
names(proto_gene_ids_v2) <- c(
  "AKT serine/threonine kinase 3",
  "FRAT regulator of WNT signaling pathway 1",
  "caspase 12 (gene/pseudogene)",
  "NDUFC2-KCTD14 readthrough",
  "peptidylprolyl isomerase F",
  "ADAM metallopeptidase domain 10",
  "cyclin dependent kinase 5",
  "proteasome 26S subunit, non-ATPase 14",
  "TPTEP2-CSNK1E readthrough",
  "APC regulator of WNT signaling pathway 2",
  "reticulon 3",
  "tubulin alpha 1b",
  "tubulin beta 3 class III",
  "tubulin beta 4A class IVa",
  "tubulin beta 4B class IVb",
  "ATP synthase peripheral stalk subunit d",
  "cytochrome c oxidase subunit NDUFA4-like",
  "ubiquinol-cytochrome c reductase, complex III subunit XI",
  "P3R3URF-PIK3R3 readthrough",
  "ADRM1 26S proteasome ubiquitin receptor",
  "frizzled class receptor 10",
  "tubulin alpha 3e",
  "cholinergic receptor muscarinic 1",
  "cholinergic receptor muscarinic 3",
  "cholinergic receptor muscarinic 5"
)
proto_gene_ids_v2_small <- proto_gene_ids_v2[1:5]
###
# same as previous, just a few specifically choosen gene ids & names
proto_gene_ids_specif <- c(5406, 4602, 3630, 10223, 27035, 10000, 10023)
names(proto_gene_ids_specif) <- c(
  "pancreatic triacylglycerol lipase",
  "transcriptional activator Myb",
  "insulin",
  "cell surface A33 antigen",
  "NADPH oxidase 1",
  # not in patient data example
  "AKT serine/threonine kinase 3",
  "FRAT regulator of WNT signaling pathway 1"
)
print(proto_gene_ids_specif)
###



###
# Reading and creating workable data matrix + labels
source(paste(getwd(),
             "/feature_selection/",
             "ReadInCSV.R",
             sep = "", collapse = ""))
patient_data_results <- get_processed_patient_data()
patient_data_matrix <- patient_data_results[[1]]
patient_data_labels <- patient_data_results[[2]]
gene_ids <- proto_gene_ids_specif

###
# checking given gene ids to be strings (else problems with indexing later)
print(gene_ids)
if (! is.character(gene_ids)) {
  temp_names <- names(gene_ids)
  gene_ids <- as.character(gene_ids)
  names(gene_ids) <- temp_names
  print(gene_ids)
}

###
# filtering genes
occuring_gene_ids <- gene_ids[gene_ids %in% rownames(patient_data_matrix)] # nolint: line_length_linter.
print(occuring_gene_ids)
patient_data_matrix_filtered <- patient_data_matrix[occuring_gene_ids, ]
print(dim(patient_data_matrix_filtered))
