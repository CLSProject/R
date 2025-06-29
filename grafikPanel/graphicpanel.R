#konnte bisher nicht getestet werden, auch abhängig von Input und Output, muss noch final definiert werden
library(shiny)

source("../feature_selection/ReadInCSV.R")
source("../feature_selection/SelectFeaturesData.R")
source("../normalization/Normalization.R")
source("../clustern/clustering_base_algo.R")
#Visualization function 
draw_heatmap <- function(data_matrix, palette_colors, row_dendrogram = NULL,gene_labels = NULL) {
  num_rows <- nrow(data_matrix)
  num_columns <- ncol(data_matrix)
  
  cat("DEBUG: draw_heatmap() was called\n")
  
 
  #if labels exist and need to be truncated
  if (!is.null(gene_labels)) {
    gene_labels <- gene_labels[1:min(nrow(data_matrix), length(gene_labels))]
  }
  
  
  num_rows <- nrow(data_matrix)
  num_columns <- ncol(data_matrix)
  
  #Debug-output
  cat("DEBUG: Matrixgröße:\n")
  cat("  rows (Genes):", num_rows, "\n")
  cat("  columns (Samples):", num_columns, "\n")
  cat("  Unique columnnames:", length(unique(colnames(data_matrix))), "\n")
  cat("  Unique rownames:", length(unique(rownames(data_matrix))), "\n\n")
  
  #Calculate min/max z-transformed values (color scale limits)
  min_value<-min(data_matrix, na.rm=TRUE)
  max_value<-max(data_matrix, na.rm = TRUE)
  
  print(paste("Min:", min_value, "Max:", max_value)) #Debug
  
  layout(matrix(c(1,2,3), nrow = 1), widths = c(2.5, 5, 2))
  
  ## Dendrogramm  
  # get_branches_heights <- function(dend) {
  #   heights <- c()
  #   if (!is.leaf(dend)) {
  #     heights <- c(heights, attr(dend, "height"))
  #     heights <- c(heights, get_branches_heights(dend[[1]]))
  #     heights <- c(heights, get_branches_heights(dend[[2]]))
  #   }
  #   return(heights)
  # }
  # 
  # # trim dendrogram
  # trim_dendrogram <- function(dend, h_cut) {
  #   if (is.leaf(dend)) return(dend)
  #   if (attr(dend, "height") <= h_cut) {
  #     dend[[1]] <- trim_dendrogram(dend[[1]], h_cut)
  #     dend[[2]] <- trim_dendrogram(dend[[2]], h_cut)
  #     return(dend)
  #   } else {
  #     # leaf nodes only 
  #     attr(dend, "height") <- h_cut
  #     return(dend)
  #   }
  # }
  # max_h <- max(get_branches_heights(row_dendrogram))
  # # Cut cluster height at the x-th hightest merge
  # cut_h <- sort(unique(get_branches_heights(row_dendrogram)), decreasing = TRUE)[6]
  # 
  # 
  # dend_trimmed <- trim_dendrogram(row_dendrogram, h_cut = cut_h)
  # 
  
  par(mar = c(5,4,2,0), xpd = NA) #xpd=NA Make sure nothing gets truncated
  plot(
    row_dendrogram,
    #dend_trimmed,
    horiz = TRUE,
    axes = FALSE,
    xlab = "", ylab = "", main = "",
    leaflab = "none",
    ylim = c(0, num_rows),
   
  )
  
  
  ## Heatmap
  par(mar = c(5,0,2,5))
  plot(NA, xlim = c(0, num_columns), ylim = c(0,num_rows),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "Heatmap", xaxs = "i",yaxs = "i")
  
  # Compute colors
  breaks <- seq(min_value, max_value, length.out = length(palette_colors) + 1)
  
  color_vec <- cut(as.vector(data_matrix), breaks = breaks, labels = FALSE)
  color_matrix <- matrix(color_vec, nrow = num_rows, ncol = num_columns)
  
  
  #Draw heatmap (cell by cell)
  for (i in 1:num_rows) {
    for (j in 1:num_columns) {
      color_idx <- color_matrix[i, j]
      if (!is.na(color_idx)) {
        # in Y-Direction
        rect(xleft = j - 1,
             ybottom = num_rows-i,
             xright = j,
             ytop = num_rows-i+1,
             col = palette_colors[color_idx], border = "black")
      }
    }
  }
  
  # Axis labeling
  axis(1, at = 0:(num_columns - 1) + 0.5, labels = colnames(data_matrix), las = 2, cex.axis = 0.7)
  axis(2, at = (1:num_rows)-0.5,
       labels = gene_labels,
       las = 2, cex.axis = 0.6)
  
  #color legend
  legend_left <- num_columns + 0.2
  legend_right <- num_columns + 2.0  
  
  for (k in 1:100) {
    rect(xleft = legend_left,
         ybottom = (k - 1) * num_rows / 100,
         xright = legend_right,
         ytop = k * num_rows / 100,
         col = palette_colors[k], border = "black")
  }
  
  axis(4, at = seq(0, num_rows, length.out = 5),
       labels = round(seq(min_value, max_value, length.out = 5), 2), las = 1)
  mtext("Valuescale", side = 4, line = 3)


graphic_server <- function(input, output, session) {
  cat("DEBUG: Server has started\n")
  
  #### load data 
  raw_data <- reactive({
    
    req(input$file)
    cat("DEBUG: File has been uploaded\n")
    
    result<- get_processed_patient_data(input$file$datapath)
    cat("DEBUG: Input path:", input$file$datapath, "\n")
    
    cat("DEBUG: Result of get_processed_patient_data():\n")
    str(result)
    
    return(result)
  })
  
  data<-reactive({
    df <- raw_data()[[1]]
    cat("DEBUG: Dim of df:", dim(df), "\n")
    req(df)
   
    
    if (!is.matrix(df) && !is.data.frame(df)) {
      cat("ERROR: df is neither a data.frame nor a matrix, but:", class(df), "\n")
      cat("DEBUG: Class of df:", class(df), "\n")
      str(df)
      
      return(NULL)
    }
    
    # Error check: 'data' must be from type vector - was 'NULL'
    if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
      cat("WARNING: df is empty: no rows or columns\n")
      return(NULL)
    }
    ###Limit number of columns (samples) if too large
    if (ncol(df) > 600) {
      cat("INFO: df is being reduced to 600 samples\n")
      df <- df[, 1:600]
    }
    
    return(df)
  })
  
 
  gen_info <- reactive({
    #if (!is.null(example_data())) {
    #   info <- example_data()[[3]]
    #   cat("DEBUG: Struktur von gen_info()\n")
    #   str(info)
    #   return(info)
    # }
    
    
    gen_ids <- raw_data()[[3]]
    ensure_gene_ids_are_strings <- function(ids) {
      as.character(ids)
    }
    
    
    # Adjust gene IDs to match reduced columns
    if (length(gen_ids) > 600) {
      gen_ids <- gen_ids[1:600]
    }
    
    genenames <- paste0("Genes", seq_along(gen_ids))  
    list(
      genenames = genenames,
      geneids = gen_ids
    )
  })  
  
  
  heatmap_palette <- reactive({
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
      cat("ERROR: Matrix is NULL during clustering()\n")
      return(NULL)
    }
    if (anyNA(mat)) {
      cat("WARNUNG: Matrix contains NA values\n")
    }
    
    # Transpose dist object (vector) transponieren and distance matrix
    dist_pat <- as.matrix(dist(t(mat), method = input$distance))
    dist_gene <- as.matrix(dist(mat, method = input$distance))
    
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
      cat("ERROR: cluster_raw$gene_clustering$order is NULL or ungültig\n")
    }
    
    list(
      gene_clustering = as.dendrogram(cluster_raw$gene_clustering),
      pat_clustering = as.dendrogram(cluster_raw$pat_clustering)
    )
  })
  
  
  clustered_data <- eventReactive(input$submit, {
     cat("Graphicpanel is loading...")
    df      <- data()        # filtered matrix
    cluster <- clustering()  # dendrograms
    
    ##Sort rows and columns according to clustering order
    row_order <- order.dendrogram(cluster$gene_clustering)
    col_order <- order.dendrogram(cluster$pat_clustering)
    df <- df[row_order, col_order]
    
    ##Gene-Labels 
    info <- gen_info()  # list(geneids, genenames)
    cat("First 10 geneids from gen_info():\n")
    print(head(info$geneids, 10))
    cat("First 10 rownames(df):\n")
    print(head(rownames(df), 10))
    
    # table: geneNames+ID
    id2name <- setNames(
      paste(info$genenames, "(", info$geneids, ")"),
      as.character(info$geneids)
    )
    # Index gen_ids by row name strings
    ids <- gen_info()$geneids

    if (all(rownames(df) %in% as.character(seq_along(ids)))) {
      index <- as.integer(rownames(df))
      gene_ids_ordered <- ids[index]
    }else{
      gene_ids_ordered<-rownames(df)
    }

    
    
    # #####
    # #Index from rownames (z.B. "1", "2", "3", ...) → 1, 2, 3, ...
    # row_index <- as.integer(rownames(df))
    # #Mapping over Gene-ID-Vector
    # gene_ids_ordered<- gen_info()$geneids[row_index]
    # #####
    # 
    
    # labels =  Gene-IDs Index from Heatmap Order
    gene_labels <- id2name[gene_ids_ordered]
    
    # Fallback: show ID if no name mapping is available
    missing <- is.na(gene_labels)
    gene_labels[missing] <- gene_ids_ordered[missing]
    
    
    list(
      df= df,
      cluster= cluster,
      gene_labels = gene_labels   
    )
  })
  
  output$heatmap_header <- renderText({
    req(input$file, input$disease, input$distance, input$linkage, input$clustercrit)
    
    paste0(
      "Analysis result for: ", input$file$name, "\n",
      "Selected disease: ", input$disease, "\n",
      "Distance measure: ", input$distance, "\n",
      "Linkage criterion: ", input$linkage, "\n",
      input$clustercrit
    )
  })
  
  output$heatmap_plot <- renderPlot({
    plot_data <- req(clustered_data())
    
    print("Gene-Labels for Heatmap:")
    print(plot_data$gene_labels)
    
    draw_heatmap(
      data_matrix = plot_data$df,
      palette_colors = heatmap_palette(),
      row_dendrogram = plot_data$cluster$gene_clustering,
      gene_labels = plot_data$gene_labels
    )
    
  }, 
  height = function() {
    n <- nrow(data())
    max(400, min(n * 25, 8000))
  },
  width = function() {
    n <- ncol(data())
    max(600, min(n * 10, 8000))
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("Heatmap_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      plot_data<- clustered_data()
      df<-plot_data$df
      
      # Set custom PDF dimensions
      rows <- nrow(df)
      columns <- ncol(df)
      cluster<- plot_data$cluster
      gene_labels<- plot_data$gene_labels
      pdf_width <- max(8, min(columns * 0.05, 50))
      pdf_height <- max(8, min(rows * 0.15, 50))
      
      
      pdf(file, width = pdf_width, height = pdf_height)
      draw_heatmap(
        data_matrix = plot_data$df,
        palette_colors = heatmap_palette(),
        row_dendrogram = cluster$gene_clustering,
        gene_labels = gene_labels
      )
      
      dev.off()
      
    }
  )
  
  
}}

