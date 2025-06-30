library(shiny)
library(shinyjs)
library(shinycssloaders)

source("graphic.R")
source("../service/color.R")
source("../service/feature.R")
source("../service/debug.R")
source("../service/normalisation.R")
source("../service/cluster.R")

if (!exists("create_color_palette")) stop("Error.")

options(shiny.maxRequestSize = 1000 * 1024^2) # 1GB max Upload

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML("
      .fullscreen {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        z-index: 9999;
        background-color: white;
        padding: 20px;
        overflow: auto;
      }
      .float-button {
        position: absolute;
        top: 10px;
        right: 10px;
        z-index: 10000;
      }
    "))
  ),
  titlePanel("CLS Analysis Tool"),
  sidebarLayout(
    sidebarPanel(
      h4("Upload & Disease selection"),
      fileInput("file", "Upload patient csv...",
        buttonLabel = "Search...", accept = ".csv"
      ),
      selectInput("disease", "Select disease...",
        choices = c(
          "Alzheimer's Disease" = "hsa05010",
          "Parkinson's disease" = "hsa05012",
          "Huntington's disease Blood" = "hsa05016",
          "Colorectal Cancer" = "hsa05210",
          "Renal Cancer Kidney" = "hsa05211",
          "Pancreatic Cancer Pancreas" = "hsa05212",
          "Glioma Brain & Spine" = "hsa05214",
          "Prostate Cancer Prostate" = "hsa05215",
          "Thyroid Cancer Thyroid" = "hsa05216",
          "AML Blood/Bone marrow" = "hsa05221",
          "Lung Cancer Lung" = "hsa05223",
          "Cardiomyopathy Heart" = "hsa05414"
        )
      ),
      hr(),
      h4("Algorithm Parameters"),
      selectInput("distance", "Distance measure...",
        choices = c(
          "Euclidean",
          "Manhattan",
          "Minkowski",
          "Canberra"
        )
      ),
      selectInput("linkage", "Linkage criterion...",
        choices = c(
          "Please Select...",
          "Single Linkage",
          "Complete Linkage",
          "UPGMA",
          "free parameter selection"
        )
      ),
      fluidRow(
        column(3, numericInput("alpha_i", "alpha_i:", value = 0.5, step = 0.1)),
        column(3, numericInput("alpha_j", "alpha_j:", value = 0.5, step = 0.1)),
        column(3, numericInput("beta", "beta:", value = 0, step = 0.01, max = 0.99)),
        column(3, numericInput("gamma", "gamma:", value = 0.5, step = 0.1))
      ),
      selectInput("clustercrit", "Cluster criterion...",
        choices = c(
          "Please Select...",
          "Group by: Patients",
          "Group by: Genes",
          "Group by: Patients and Genes"
        )
      ),
      checkboxInput(
        inputId = "normalizationYesNo",
        label = "Normalization wanted",
        value = FALSE
      ),
      selectInput("normalization", "Normalization...",
        choices = c(
          "Please Select..." = "Please Select...",
          "Z Score" = "z_score_norm",
          "Min Max" = "min_max_scale_norm"
        )
      ),
      hr(),
      h4("Visualization Settings"),
      # numericInput("n_clusters", "Number of clusters:", value = 0, min = 2, max = 10),
      selectInput("colorpattern", "Select Colorpattern...",
        choices = c("Please Select...", "Rainbow", "Heat", "Topo")
      ), # Farbpattern

      hr(),
      actionButton("submit", "Submit"),
      # actionButton("download", "Download Result")
      downloadButton("download_plot", "Download Result")
    ),
    mainPanel(
      h4("Section 2 - Result"),

      div(
        id = "normal_view",
        div(
          class = "float-button",
          actionButton("expand", " + ")
        ),
        wellPanel(
          h5("Normal View"),

          # Outer scroll container
          div(style = "min-height: 800px; overflow: auto; border: 1px solid #ccc; padding: 10px;",

            # Inner scroll container for wide content
            div(style = "min-width: 1200px; overflow-x: auto;",
                withSpinner(plotOutput("heatmap_plot", height = 1200), type = 6)
            )
          )
        )
      ),

      hidden(
        div(
          id = "fullscreen_view", class = "fullscreen", style = "height: 100vh; overflow: auto;",
          div(
            class = "float-button",
            actionButton("collapse", " - ")
          ),
          h3("Fullscreen View"),
          withSpinner(plotOutput("heatmap_plot_fullscreen"), type = 6),
        )
      )
    )
  )
)
server <- function(input, output, session) {
  observe({
    if (input$normalizationYesNo == TRUE) {
      shinyjs::enable("normalization")
    } else if (input$normalizationYesNo == FALSE) {
      shinyjs::disable("normalization")
    }
  })
  observe({ # Eingabe der Parameter nur bei freier Parameterwahl erlauben
    if (input$linkage == "Complete Linkage") {
      shinyjs::disable("alpha_i")
      shinyjs::disable("alpha_j")
      shinyjs::disable("beta")
      shinyjs::disable("gamma")

      updateNumericInput(session, "alpha_i", value = 0.5)
      updateNumericInput(session, "alpha_j", value = 0.5)
      updateNumericInput(session, "beta", value = 0)
      updateNumericInput(session, "gamma", value = 0.5)

      updateNumericInput(session, "gamma", value = 0.5)
    } else if (input$linkage == "Single Linkage") {
      shinyjs::disable("alpha_i")
      shinyjs::disable("alpha_j")
      shinyjs::disable("beta")
      shinyjs::disable("gamma")

      updateNumericInput(session, "alpha_i", value = 0.5)
      updateNumericInput(session, "alpha_j", value = 0.5)
      updateNumericInput(session, "beta", value = 0)
      updateNumericInput(session, "gamma", value = -0.5)
    } else if (input$linkage == "Please Select...") {
      shinyjs::disable("alpha_i")
      shinyjs::disable("alpha_j")
      shinyjs::disable("beta")
      shinyjs::disable("gamma")

      updateNumericInput(session, "alpha_i", value = 0)
      updateNumericInput(session, "alpha_j", value = 0)
      updateNumericInput(session, "beta", value = 0)
      updateNumericInput(session, "gamma", value = 0)
    } else if (input$linkage == "UPGMA") {
      shinyjs::disable("alpha_i")
      shinyjs::disable("alpha_j")
      shinyjs::disable("beta")
      shinyjs::disable("gamma")

      updateNumericInput(session, "alpha_i", value = 0.5)
      updateNumericInput(session, "alpha_j", value = 0.5)
      updateNumericInput(session, "beta", value = 0)
      updateNumericInput(session, "gamma", value = 0)

      updateNumericInput(session, "gamma", value = -0.5)
    } else if (input$linkage == "Please Select...") {
      shinyjs::disable("alpha_i")
      shinyjs::disable("alpha_j")
      shinyjs::disable("beta")
      shinyjs::disable("gamma")

      updateNumericInput(session, "alpha_i", value = 0)
      updateNumericInput(session, "alpha_j", value = 0)
      updateNumericInput(session, "beta", value = 0)
      updateNumericInput(session, "gamma", value = 0)
    } else if (input$linkage == "UPGMA") {
      shinyjs::disable("alpha_i")
      shinyjs::disable("alpha_j")
      shinyjs::disable("beta")
      shinyjs::disable("gamma")

      updateNumericInput(session, "alpha_i", value = 0.5)
      updateNumericInput(session, "alpha_j", value = 0.5)
      updateNumericInput(session, "beta", value = 0)
      updateNumericInput(session, "gamma", value = 0)
    } else {
      shinyjs::disable("alpha_i")
      shinyjs::disable("alpha_j")
      shinyjs::disable("alpha_i")
      shinyjs::disable("alpha_j")
      shinyjs::enable("beta")
      shinyjs::disable("gamma")

      shinyjs::disable("gamma")
    }
  })

  observe({
    req(input$linkage == "free parameter selection")

    # Berechne Parameter basierend auf Beta
    beta_val <- input$beta
    alphas <- 1 - beta_val
    alpha_i <- alphas / 2
    alpha_j <- alphas / 2
    gamma <- 0

    # Update der anderen Eingabefelder
    updateNumericInput(session, "alpha_i", value = round(alpha_i, 3))
    updateNumericInput(session, "alpha_j", value = round(alpha_j, 3))
    updateNumericInput(session, "gamma", value = gamma)
  })

  # Umstellung zwischen Vollbildschirm und Teilbildschirm
  observeEvent(input$expand, {
    hide("normal_view")
    show("fullscreen_view")
  })

  observeEvent(input$collapse, {
    hide("fullscreen_view")
    show("normal_view")
  })

  # Handle data analysis when submit button is clicked
  observeEvent(input$submit, {
    # Clear previous results
    output$test_result_normal <- renderText({
      ""
    })
    output$test_result_fullscreen <- renderText({
      ""
    })

    # Perform the analysis with more detailed progress steps
    withProgress(message = "Starting analysis...", value = 0, {
      process_start_time <- debug_time()
      # Step 0: Validate inputs
      incProgress(0.1, detail = "Validating inputs...")
      # Add validation logic here

      # Step 1: Create Color palette
      incProgress(0.1, detail = "Creating color palette...")
      color_palette <- reactive({
        cat("DEBUG: Color palette generated.\n")
        create_color_palette(input$colorpattern)
      })

      Sys.sleep(2)

      # print(color_palette)

      # Step 2: Data preprocessing
      start_time <- debug_time()
      incProgress(0.1, detail = "Preprocessing patient data...")
      patient_data <- feature_selection(input$file$datapath, input$disease)
      end_time <- time_diff(start_time)
      cat("DEBUG: Patient data preprocessing completed in", end_time, "seconds.\n")

      # set patient data for further processing
      patient_matrix <- patient_data$data_filtered

      # validate normalization input and apply normalization if selected
      if (input$normalizationYesNo && input$normalization != "Please Select...") {
        incProgress(0.0, detail = "Normalizing patient data...")
        start_time <- debug_time()
        patient_matrix <- normalise(patient_matrix, input$normalization)
        end_time <- time_diff(start_time)
        cat("DEBUG: Normalisation completed in", end_time, "seconds.\n")
      }

      # Step 3: Distance calculation
      incProgress(0.2, detail = "Calculating distances...")
      start_time <- debug_time()
      dist_pat <- as.matrix(dist(patient_matrix, method = tolower(input$distance)))
      dist_gene <- as.matrix(dist(t(patient_matrix), method = tolower(input$distance)))
      end_time <- time_diff(start_time)
      cat("DEBUG: Distance calculation completed in", end_time, "seconds.\n")



      # Step 4: Clustering
      incProgress(0.2, detail = "Performing clustering...")
      start_time <- debug_time()
      cluster_raw <- cluster_both(
        dist_pat = dist_pat,
        dist_gene = dist_gene,
        alpha_i = input$alpha_i,
        alpha_j = input$alpha_j,
        beta = input$beta,
        gamma = input$gamma,
        link_crit = input$linkage
      )
      end_time <- time_diff(start_time)
      cat("DEBUG: Clustering completed in", end_time, "seconds.\n")


      hc_pat <- cluster_raw$pat_clustering
      hc_gene <- cluster_raw$gene_clustering

      # Step 5: Visualization preparation
      incProgress(0.2, detail = "Preparing visualization...")
      start_time <- debug_time()
      gene_clustering <- as.dendrogram(hc_gene)
      patient_clustering <- as.dendrogram(hc_pat)
      row_order <- order.dendrogram(patient_clustering)
      col_order <- order.dendrogram(gene_clustering)
      graphic_data_frame <- patient_matrix[row_order, col_order]
      end_time <- time_diff(start_time)
      cat("DEBUG: Visualization preparation completed in", end_time, "seconds.\n")

      # Gen-IDs und Namen aus der gefilterten Matrix
      gene_df <- patient_data$gene_df
      # Rename columns using gene IDs from gene_df
      if (!is.null(gene_df) && "ID" %in% colnames(gene_df)) {
        colnames(graphic_data_frame) <- gene_df$ID[col_order]
      }


      incProgress(0.2, detail = "Finalizing...")
      start_time <- debug_time()
      output$heatmap_plot <- output$heatmap_plot_fullscreen <- renderPlot(
        {

          create_heatmap(
            daten_matrix = graphic_data_frame,
            palette_colors = color_palette(),
            analysis_params = list(
              file_name = input$file$name,
              disease = input$disease,
              distance = input$distance,
              linkage = input$linkage,
              alpha_i = input$alpha_i,
              alpha_j = input$alpha_j,
              beta = input$beta,
              gamma = input$gamma,
              cluster_crit = input$clustercrit
            )
          )
        },
        height = function() {
          n <- nrow(graphic_data_frame)
          max(400, (n * 12) + 300)
        },
        width = function() {
          n <- ncol(graphic_data_frame)
          max(800, min((n * 15) + 300, 10000))
        }
      )
      end_time <- time_diff(start_time)
      cat("DEBUG: Heatmap rendering completed in", end_time, "seconds.\n")

      process_end_time <- debug_time()
      total_time <- time_diff(process_start_time)
      cat("DEBUG: Total processing time:", total_time, "seconds.\n")
    })
  })
}

shinyApp(ui = ui, server = server)
