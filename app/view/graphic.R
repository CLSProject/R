# konnte bisher nicht getestet werden, auch abhängig von Input und Output, muss noch final definiert werden
library(pheatmap)
library(gplots)
library(RColorBrewer)
library(dendextend)

create_heatmap <- function(
  daten_matrix,
  palette_colors,
  analysis_params = NULL
) {

  pheatmap(daten_matrix,
    main = paste("Analysis result for: ", analysis_params$file_name,
      "\nSelected disease: ", analysis_params$disease,
      "\nDistance measure: ", analysis_params$distance,
      "\nLinkage criterion: ", analysis_params$linkage,
      "\n", analysis_params$cluster_crit
    ),
    color = palette_colors,
    fontsize_number = 7,
    cellwidth = 15,
    cellheight = 12,
    border_color = "black"
  )
}

# # Visualisierung Fkt
# zeichne_heatmap <- function(daten_matrix, palette_colors, analysis_params = NULL, zeilen_dendrogramm = NULL, gene_labels = NULL) {
#   pheatmap(daten_matrix,
#     main = paste(
#       "Analysis result for: ", analysis_params$file_name,
#       "\nSelected disease: ", analysis_params$disease,
#       "\nDistance measure: ", analysis_params$distance,
#       "\nLinkage criterion: ", analysis_params$linkage,
#       "\n", analysis_params$cluster_crit
#     ),
#     color = palette_colors,
#     fontsize_number = 7,
#     cellwidth = 15,
#     cellheight = 12,
#     border_color = "black"
#   )
# }

# graphic_server <- function(input, output, session) {
#   cat("DEBUG: Server wurde gestartet\n")

#   # kegg_data <- reactive({
#   #   return(retrieve_pathway_genes(input$disease))
#   # })

#   #### Daten einlesen
#   selected_features <- reactive({
#     req(input$file)
#     cat("DEBUG: Datei wurde hochgeladen\n")

#     filtered_patient_data <- feature_selection(input$file$datapath, input$disease)
#     cat("DEBUG: Feature Selection abgeschlossen\n")

#     return(filtered_patient_data)
#   })

#   # data <- reactive({
#   #   df <- raw_data()[[1]]
#   #   cat("DEBUG: Dim of df:", dim(df), "\n")
#   #   req(df)

#   #   if (!is.matrix(df) && !is.data.frame(df)) {
#   #     cat("FEHLER: df ist kein data.frame oder matrix, sondern:", class(df), "\n")
#   #     cat("DEBUG: Klasse von df:", class(df), "\n")
#   #     str(df)

#   #     return(NULL)
#   #   }

#   #   # Fehlerprüfung: 'data' muss vom Typ vector sein, war 'NULL'
#   #   if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
#   #     cat("WARNUNG: df ist leer: keine Zeilen oder Spalten\n")
#   #     return(NULL)
#   #   }

#   #   m <- scale(df) # Normalisierung Test-Fkt
#   #   return(m)
#   # })



#   # gen_info <- reactive({
#   #   gene_ids <- kegg_data()[seq(1, length(kegg_data()), 2)]
#   #   processed_gene_data <- select_features(raw_data(), gene_ids)
#   #   gen_ids <- processed_gene_data[[3]]
#   #   gen_ids <- ensure_gene_ids_are_strings(gen_ids)

#   #   genenames <- kegg_data()[seq(2, length(kegg_data()), 2)]

#   #   list(
#   #     genenames = genenames,
#   #     geneids = processed_gene_data[[3]]
#   #   )
#   # })




#   clustering <- reactive({
#     mat <- selected_features()[[1]] # gefilterte Matrix
#     cat("DEBUG: Matrix für Clustering:\n")
#     print(dim(mat))

#     if (is.null(mat)) {
#       cat("FEHLER: Matrix ist NULL in clustering()\n")
#       return(NULL)
#     }
#     if (anyNA(mat)) {
#       cat("WARNUNG: Matrix enthält NA-Werte\n")
#     }
#     cat("DEBUG: input$normalizationYesNo:", input$normalizationYesNo, "\n")

#     if (input$normalizationYesNo == TRUE) {
#       cat("DEBUG: Matrix normalisiert mit Methode:", input$normalization, "\n")
#       mat <- get(input$normalization)(mat)
#       print(mat[1:5, 1:5]) # Debug-Ausgabe der ersten 5 Zeilen und Spalten
#       print(dim(mat)) # Debug-Ausgabe der Dimensionen der Matrix
#       # print(mat)
#     } else {
#       cat("DEBUG: Keine Normalisierung angewendet\n")
#     }
#     # dist-Objekt (Vektor) transponieren und Distanzmatrix
#     dist_pat <- as.matrix(dist(mat, method = tolower(input$distance)))
#     dist_gene <- as.matrix(dist(t(mat), method = tolower(input$distance)))
#     # print(dist_pat)
#     # print(dist_gene)
#     cat("DEBUG: Distanzmatrixen berechnet\n")
#     # Start a timer for clustering
#     cluster_start_time <- Sys.time()
#     cat("DEBUG: Starting clustering at", format(cluster_start_time), "\n")

#     print(paste("min:", min(mat), "max:", max(mat)))

#     cluster_raw <- cluster_both(
#       dist_pat = dist_pat,
#       dist_gene = dist_gene,
#       alpha_i = input$alpha_i,
#       alpha_j = input$alpha_j,
#       beta = input$beta,
#       gamma = input$gamma,
#       link_crit = input$linkage
#     )

#     hc_pat <- cluster_raw$pat_clustering
#     hc_gene <- cluster_raw$gene_clustering

#     # hc_pat <- hclust(as.dist(dist_pat), method = "single") # or "complete", "average", etc.
#     # hc_gene <- hclust(as.dist(dist_gene), method = "single")

#     # Calculate and display the time taken for clustering
#     cluster_end_time <- Sys.time()
#     cluster_duration <- difftime(cluster_end_time, cluster_start_time, units = "secs")
#     cat("DEBUG: Clustering completed at", format(cluster_end_time), "\n")
#     cat("DEBUG: Clustering took", round(as.numeric(cluster_duration), 2), "seconds\n")

#     # cat("DEBUG: Cluster-Objekt erstellt\n")
#     # if (is.null(cluster_raw$gene_clustering$order)
#     #     || !is.numeric(cluster_raw$gene_clustering$order)) {
#     #   cat("FEHLER: cluster_raw$gene_clustering$order ist NULL oder ungültig\n")
#     # }


#     gene_clustering <- as.dendrogram(hc_gene)
#     patient_clustering <- as.dendrogram(hc_pat)
#     cat("DEBUG: Dendrogramme erstellt\n")

#     #   list(
#     #     gene_clustering = as.dendrogram(cluster_raw$gene_clustering),
#     #     pat_clustering = as.dendrogram(cluster_raw$pat_clustering)
#     #   )
#     # })


#     # clustered_data <- eventReactive(input$submit, {
#     #   df <- data() # gefilterte Matrix
#     #   cluster <- clustering(df) # Dendrogramme

#     ## Zeilen & Spalten nach Clusterreihenfolge sortieren
#     row_order <- order.dendrogram(patient_clustering)
#     col_order <- order.dendrogram(gene_clustering)
#     df <- mat[row_order, col_order]

#     cat("DEBUG: Matrix nach Clustern sortiert\n")

#     # Gen-IDs und Namen aus der gefilterten Matrix
#     gene_df <- selected_features()[[4]]
#     # Rename columns using gene IDs from gene_df
#     if (!is.null(gene_df) && "ID" %in% colnames(gene_df)) {
#       colnames(df) <- gene_df$ID[col_order]
#     }


#     ## Gen-Labels
#     # info <- gen_info() # list(geneids, genenames)
#     # cat("Erste 10 geneids aus gen_info():\n")
#     # print(head(info$geneids, 10))
#     # cat("Erste 10 rownames(df):\n")
#     # print(head(rownames(df), 10))

#     # # gene_symbols <- kegg_data[seq(2, length(kegg_data), 2)] #to be used in plot to display gene names
#     # # Tabelle: GenName+ID
#     # info_gene_ids < selected_features()[[3]] # Gen-IDs aus der gefilterten Matrix
#     # id2name <- setNames(
#     #   paste(info$genenames, "(", info_gene_ids, ")"),
#     #   as.character(info_gene_ids)
#     # )
#     # # gen_ids nach Zeilennamen-Strings indexieren
#     # ids <- info_gene_ids

#     # with_index <- all(suppressWarnings(!is.na(as.integer(rownames(df)))))

#     # if (!with_index) {
#     #   warning("Nicht alle rownames lassen sich korrekt zu gen_ids mappen!")
#     #   index <- as.integer(rownames(df))
#     #   gene_ids_ordered <- ids[index]
#     # } else {
#     #   gene_ids_ordered <- rownames(df)
#     # }


#     # # labels = die Gene-IDs Index in der Heatmap-Reihenfolge
#     # gene_labels <- id2name[gene_ids_ordered]

#     # # Fallback: falls ein Name fehlt, die ID anzeigen
#     # missing <- is.na(gene_labels)
#     # gene_labels[missing] <- gene_ids_ordered[missing]

#     list(
#       df = df,
#       cluster = list(
#         gene_clustering = gene_clustering,
#         pat_clustering = patient_clustering
#       ),
#       gene_labels = NULL
#     )
#   })

#   output$heatmap_plot <- output$heatmap_plot_fullscreen <- renderPlot(
#     {
#       plot_data <- req(clustering())

#       # print("Gene-Labels für Heatmap:")
#       # print(plot_data$gene_labels)

#       zeichne_heatmap(
#         daten_matrix = plot_data$df,
#         palette_colors = farben_palette(),
#         analysis_params = list(
#           file_name = input$file$name,
#           disease = input$disease,
#           distance = input$distance,
#           linkage = input$linkage,
#           alpha_i = input$alpha_i,
#           alpha_j = input$alpha_j,
#           beta = input$beta,
#           gamma = input$gamma,
#           cluster_crit = input$clustercrit
#         ),
#         zeilen_dendrogramm = plot_data$cluster$gene_clustering,
#         # gene_labels = plot_data$gene_labels
#       )
#     },
#     height = function() {
#       n <- nrow(selected_features()[[1]])
#       max(400, (n * 12) + 300)
#     },
#     width = function() {
#       n <- ncol(selected_features()[[1]])
#       max(800, min((n * 15) + 300, 10000))
#     }
#   )

#   output$download_plot <- downloadHandler(
#     cat("DEBUG: Download-Handler für Plot wurde erstellt\n"),
#     filename = function() {
#       paste0("Analysis_Result_", Sys.Date(), ".pdf")
#     },
#     content = function(filename) {
#       cat("DEBUG: Download-Handler content-Funktion gestartet\n")
#       cat("DEBUG: Filename für Download:", filename, "\n")
#       plot_data <- req(clustering())
#       data <- plot_data$df

#       rows_per_page <- 35 # Adjust based on desired readability
#       total_rows <- nrow(data)
#       cat("DEBUG: Total rows in data:", total_rows, "\n")
#       num_pages <- ceiling(total_rows / rows_per_page)
#       total_cols <- ncol(data)

#       # n <- nrow(selected_features()[[1]])
#       # height <- max(400, (n * 12) + 300)

#       # n <- ncol(selected_features()[[data]])
#       # width <- max(800, min((n * 15) data 300, 10000))
#       # print(data[1:5, 1:5])
#       # Standard A4 dimensions in data (8.27 x 11.69)
#       pdf(filename, width = 8.27, height = 11.69)

#       for (i in seq_len(num_pages)) {
#         start_row <- (i - 1) * rows_per_page + 1
#         end_row <- min(i * rows_per_page, total_rows)

#         df_subset <- data[start_row:end_row, 1:total_cols, drop = FALSE]
#         # print(df_subset[1:5, 1:5])
#         # cat("DEBUG: Subset for page", i, "from row", start_row, "to", end_row, "\n")
#         # cat("DEBUG: Subset dimensions:", dim(df_subset), "\n")

#         # Using pheatmap library function directly to create plots in the PDF
#         pheatmap(df_subset,
#           main = paste(
#             "Analysis result for: ", input$file$name,
#             "\nSelected disease: ", input$disease,
#             " / Distance measure: ", input$distance,
#             "\nLinkage criterion: ", input$linkage,
#             " / ", input$clustercrit,
#             "\n", start_row, " to ", end_row, " (", i, " of ", num_pages, " pages)"
#           ),
#           color = farben_palette(),
#           fontsize = 6,
#           fontsize_row = 5,
#           fontsize_col = 5,
#           border_color = "black"
#         )
#       }
#       dev.off()
#     }
#   )
# }
