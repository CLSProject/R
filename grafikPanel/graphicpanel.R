#konnte bisher nicht getestet werden, auch abhängig von Input und Output, muss noch final definiert werden
library(shiny)

source("../feature_selection/ReadInCSV.R")
source("../feature_selection/SelectFeaturesData.R")
source("../clustern/clustering_base_algo.R") 
if (!exists("cluster_both")) stop("Fehler: cluster_both wurde nicht geladen.")

#Dummy-Clustering-Funktion (zum Testen)
# if (!exists("cluster_both")) {
#   message("Warnung: cluster_both nicht definiert – Dummy wird verwendet.")
#   cluster_both <- function(data, alpha_i, alpha_j, beta, gamma, dist_crit, link_crit) {
#     list(
#       gene_clustering = hclust(dist(data)),
#       pat_clustering = hclust(dist(t(data)))
#     )
#   }
# }


#Visualisierung Fkt 
zeichne_heatmap <- function(daten_matrix, palette_colors, zeilen_dendrogramm = NULL,gene_labels = NULL) {
  zeilen_anzahl <- nrow(daten_matrix)
  spalten_anzahl <- ncol(daten_matrix)
  
  cat("DEBUG: zeichne_heatmap() wurde aufgerufen\n")
  
  #Debug-Ausgabe
  cat("DEBUG: Matrixgröße:\n")
  cat("  Zeilen (Gene):", zeilen_anzahl, "\n")
  cat("  Spalten (Proben):", spalten_anzahl, "\n")
  cat("  Einzigartige Spaltennamen:", length(unique(colnames(daten_matrix))), "\n")
  cat("  Einzigartige Zeilennamen:", length(unique(rownames(daten_matrix))), "\n\n")
  
  #Min/Max z-transformierten Werten berechnen (Skalengrenze für Farben)
  min_wert<-min(daten_matrix, na.rm = TRUE)
  max_wert<-max(daten_matrix, na.rm = TRUE)
  
  print(paste("Min:", min_wert, "Max:", max_wert)) #Debug
  
  layout(matrix(c(1,2,3), nrow = 1), widths = c(1.5, 5, 0.5))
  
  
  # Dendrogramm
  par(mar = c(5,0,2,0))
  if (!is.null(zeilen_dendrogramm)) {
    # leere Achse (kein plot.dendrogram())
    plot(zeilen_dendrogramm, horiz = TRUE, axes = FALSE, xlab = "", ylab = "", main = "", leaflab = "none", ylim=c(0,zeilen_anzahl))
  } else {
    plot.new()
  }
  
  
  # Heatmap
  par(mar = c(5,0,2,5))
  plot(NA, xlim = c(0, spalten_anzahl), ylim = c(0, zeilen_anzahl),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "Heatmap", xaxs = "i",yaxs = "i")
  
  # Farben berechnen
  breaks <- seq(min_wert, max_wert, length.out = length(palette_colors) + 1)
  
  farben_vec <- cut(as.vector(daten_matrix), breaks = breaks, labels = FALSE)
  farben_matrix <- matrix(farben_vec, nrow = zeilen_anzahl, ncol = spalten_anzahl)
  
  
  #Heatmap zeichnen (Zelle für Zelle)
  for (i in 1:zeilen_anzahl) {
    for (j in 1:spalten_anzahl) {
      farbe_idx <- farben_matrix[i, j]
      if (!is.na(farbe_idx)) {
        # in Y-Richtung
           rect(xleft = j - 1,
                ybottom = zeilen_anzahl-i,
                xright = j,
                ytop = zeilen_anzahl-i+1,
                col = palette_colors[farbe_idx], border = "black")
    }
    }
  }
  
  # Achsenbeschriftung
  axis(1, at = 0:(spalten_anzahl - 1) + 0.5, labels = colnames(daten_matrix), las = 2, cex.axis = 0.7)
  axis(2, at = (1:zeilen_anzahl)-0.5,
       labels = gene_labels,
       las = 2, cex.axis = 0.6)
  
  
  for (k in 1:100) {
    rect(xleft = spalten_anzahl + 0.2,
         ybottom = (k - 1) * zeilen_anzahl / 100,
         xright = spalten_anzahl + 0.6,
         ytop = k * zeilen_anzahl / 100,
         col = palette_colors[k], border = "black")
  }
  
  axis(4, at = seq(0, zeilen_anzahl, length.out = 5),
       labels = round(seq(min_wert, max_wert, length.out = 5), 2), las = 1)
  mtext("Werteskala", side = 4, line = 3)
}

graphic_server <- function(input, output, session) {
  cat("DEBUG: Server wurde gestartet\n")
  
 #### Daten einlesen 
  raw_data <- reactive({
    
    ### Test-Fkt 
    #req(input$file)
    # df <- read.csv(input$file$datapath, row.names = 1)
    ## nur numerische Spalten
    # df <- df[, sapply(df, is.numeric)]
    # label <- rep("sample", ncol(df))  ## Dummy-Labels
    # gene_ids <- rownames(df)  ## rownames = IDs
    # list(df, label, gene_ids)
    
    
    
    req(input$file)
    cat("DEBUG: Datei wurde hochgeladen\n")
    
    result<- get_processed_patient_data(input$file$datapath)
    cat("DEBUG: Eingelesener Pfad:", input$file$datapath, "\n")
    if (is.null(df)) {
      cat("FEHLER: df ist NULL\n")
    }
    cat("DEBUG: Ergebnis von get_processed_patient_data():\n")
    str(result)
    
    return(result)
  })
  
  data<-reactive({
    df <- raw_data()[[1]]
    cat("DEBUG: Dim of df:", dim(df), "\n")
    
    if (!is.matrix(df) && !is.data.frame(df)) {
      cat("FEHLER: df ist kein data.frame oder matrix, sondern:", class(df), "\n")
      cat("DEBUG: Klasse von df:", class(df), "\n")
      str(df)
      
      return(NULL)
    }
    
    # Fehlerprüfung: 'data' muss vom Typ vector sein, war 'NULL'
    if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
      cat("WARNUNG: df ist leer: keine Zeilen oder Spalten\n")
      return(NULL)
    }
    
   
    
    m <- scale(df) # Z-Transformation
    cat("DEBUG: Matrix nach scale():", dim(m), "\n")
    return(m)
    
  })
  
  #df <- read.csv(file_path, header = TRUE, row.names = 1)
  #cat("DEBUG: Datei gelesen. Dimensionen: ", dim(df), "\n")
  
  #label_row <- grepl("^\\s*labels\\s*$", tolower(rownames(df)))
  #df <- df[!label_row, ]#Labels entfernen, falls geclustert
  
  #cat("Erste 10 rownames(df):\n")
  #print(head(rownames(df), 10))
  
  #df <- scale(df) #Z-Transformation
  #return(df)
  #})
  #example_data <- reactive({
  # if (is.null(input$file)) get_example_data() else NULL
  #})
  
  gen_info <- reactive({
    #if (!is.null(example_data())) {
    #   info <- example_data()[[3]]
    #   cat("DEBUG: Struktur von gen_info()\n")
    #   str(info)
    #   return(info)
    # }
    
    
    gen_ids <- raw_data()[[3]]
    gen_ids<-ensure_gene_ids_are_strings(gen_ids)
    
    genenames <- paste0("Gene", seq_along(gen_ids))  
    list(
      genenames = genenames,
      geneids = gen_ids
    )
  })  
  
  
  farben_palette <- reactive({
    if (input$colorpattern == "Rainbow") {
      colorRampPalette(c("red", "orange", "yellow", "green", "blue", "purple"))(100)
    } else if (input$colorpattern == "Heat") {
      colorRampPalette(c("darkred", "red", "orange", "yellow"))(100)
    } else if (input$colorpattern == "Topo") {
      colorRampPalette(c("darkgreen", "green", "lightblue", "blue", "darkblue"))(100)
    } else {
      colorRampPalette(c("red", "white", "green"))(100)
    }
  })
  clustering <- reactive({
    mat <- data()
    
    if (is.null(mat)) {
      cat("FEHLER: Matrix ist NULL in clustering()\n")
      return(NULL)
    }
    if (anyNA(mat)) {
      cat("WARNUNG: Matrix enthält NA-Werte\n")
    }
    
    # dist-Objekt (Vektor) transponieren und Distanzmatrix
    dist_pat <- as.matrix(dist(t(mat), method = tolower(input$distance)))
    dist_gene <- as.matrix(dist(mat, method = tolower(input$distance)))
    
    cluster_raw <- cluster_both(
      dist_pat = dist_pat,
      dist_gene = dist_gene,
      alpha_i = input$alpha_i,
      alpha_j = input$alpha_j,
      beta = input$beta,
      gamma = input$gamma,
      link_crit = input$linkage
    )
    if (is.null(cluster_raw$gene_clustering$order) || !is.numeric(cluster_raw$gene_clustering$order)) {
      cat("FEHLER: cluster_raw$gene_clustering$order ist NULL oder ungültig\n")
    }
    
    list(
      gene_clustering = as.dendrogram(cluster_raw$gene_clustering),
      pat_clustering = as.dendrogram(cluster_raw$pat_clustering)
    )
  })
  
  
  clustered_data <- eventReactive(input$submit, {
    df      <- data()        # gefilterte Matrix
    cluster <- clustering()  # Dendrogramme
    
    ##Zeilen & Spalten nach Clusterreihenfolge sortieren
    row_order <- order.dendrogram(cluster$gene_clustering)
    col_order <- order.dendrogram(cluster$pat_clustering)
    df <- df[row_order, col_order]
    
    ##Gen-Labels 
    info <- gen_info()  # list(geneids, genenames)
    cat("Erste 10 geneids aus gen_info():\n")
    print(head(info$geneids, 10))
    cat("Erste 10 rownames(df):\n")
    print(head(rownames(df), 10))
    
    # Tabelle: GenName+ID
    id2name <- setNames(
      paste(info$genenames, "(", info$geneids, ")"),
      as.character(info$geneids)
    )
    #gen_ids nach Zeilennamen-Strings indexieren
    ids <- gen_info()$geneids
    
    if (all(rownames(df) %in% as.character(seq_along(ids)))) {
      index <- as.integer(rownames(df))
      gene_ids_ordered <- ids[index]
    }else{
      gene_ids_ordered<-rownames(df)
    }
      
    # labels = die Gene-IDs Index in der Heatmap-Reihenfolge
    gene_labels <- id2name[gene_ids_ordered]
    
    # Fallback: falls ein Name fehlt, die ID anzeigen
    missing <- is.na(gene_labels)
    gene_labels[missing] <- gene_ids_ordered[missing]
    
    
    list(
      df= df,
      cluster= cluster,
      gene_labels = gene_labels   
    )
  })
  
  output$heatmap_plot <- renderPlot({
    plot_data <- req(clustered_data())
    
    print("Gene-Labels für Heatmap:")
    print(plot_data$gene_labels)
    
    zeichne_heatmap(
      daten_matrix = plot_data$df,
      palette_colors = farben_palette(),
      zeilen_dendrogramm = plot_data$cluster$gene_clustering,
      gene_labels = plot_data$gene_labels
    )
    
  }, 
  height = function() {
    n <- nrow(data())
    max(400, min(n * 25, 12000))
  },
  width = function() {
    n <- ncol(data())
    max(800, min(n * 10, 10000))
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("Heatmap_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # df <- data()
      # cluster <- clustering()
      # row_order <- order.dendrogram(cluster$gene_clustering)
      # col_order <- order.dendrogram(cluster$pat_clustering)
      # df <- df[row_order, col_order]
      # 
      # # Gene-Labels zu gen_info()
      # info <- gen_info()
      # # Reihenfolge-Indes aus numerierten rownames("1","2",)
      # index <- as.integer(row.names(df))
      # # Echte Gen-IDs über Index holen
      # gen_ids<-info$geneids[index]
      # 
      # # Mapping: Genname (GENE1 (GENE_ID)) nach gen_ids
      # id2name <- setNames(
      #   paste(info$genenames[index], "(", gen_ids, ")"),
      #   gen_ids
      # )
      # 
      # gene_labels <- id2name[gen_ids]
      # missing <- is.na(gene_labels)
      # gene_labels[missing] <- gen_ids[missing]
      
      plot_data<- clustered_data()
      df<-plot_data$df
      
      # PDF-Größe angepasst
      zeilen <- nrow(df)
      spalten <- ncol(df)
      cluster<- plot_data$cluster
      gene_labels<- plot_data$gene_labels
      pdf_width <- max(8, min(spalten * 0.05, 50))
      pdf_height <- max(8, min(zeilen * 0.15, 50))
      
      
      pdf(file, width = pdf_width, height = pdf_height)
      zeichne_heatmap(
        daten_matrix = plot_data$df,
        palette_colors = farben_palette(),
        zeilen_dendrogramm = cluster$gene_clustering,
        gene_labels = gene_labels
      )
      
        dev.off()
      
    }
  )
}



