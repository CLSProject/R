library(shiny)
library(shinyjs)
library(shinycssloaders)
# install.packages("shinycssloaders")

source("../grafikPanel/graphicpanel.R") # Ermöglicht Zugriff auf Plot Funktionen

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
  titlePanel("CLS Analyseplattform"),
  sidebarLayout(
    sidebarPanel(
      h4("Upload & Disease selection"),
      fileInput("file", "Upload patient csv...",
        buttonLabel = "Search...", accept = ".csv"
      ),
      selectInput("disease", "Select disease (1 out of 24)...",
        choices = c(
          "Alzheimer's Disease – Hippocampal CA1" = "hsa05010",
          "Alzheimer's Disease – Entorhinal Cortex" = "hsa05010",
          "Alzheimer's Disease – Hippocampus" = "hsa05010",
          "Alzheimer's Disease – Visual Cortex" = "hsa05010",
          "Parkinson's disease – Lymphoblasts" = "hsa05012",
          "Parkinson's disease – Putamen" = "hsa05012",
          "Huntington's disease – Blood" = "hsa05016",
          "Colorectal Cancer – Mucosa" = "hsa05210",
          "Colorectal Cancer – Colon (1)" = "hsa05210",
          "Colorectal Cancer – Colon (2)" = "hsa05210",
          "Renal Cancer – Kidney (1)" = "hsa05211",
          "Renal Cancer – Kidney (2)" = "hsa05211",
          "Pancreatic Cancer – Pancreas (1)" = "hsa05212",
          "Pancreatic Cancer – Pancreas (2)" = "hsa05212",
          "Glioma – Brain" = "hsa05214",
          "Glioma – Brain & Spine" = "hsa05214",
          "Prostate Cancer – Prostate (1)" = "hsa05215",
          "Prostate Cancer – Prostate (2)" = "hsa05215",
          "Thyroid Cancer – Thyroid (1)" = "hsa05216",
          "Thyroid Cancer – Thyroid (2)" = "hsa05216",
          "AML – Blood/Bone marrow" = "hsa05221",
          "Lung Cancer – Lung (1)" = "hsa05223",
          "Lung Cancer – Lung (2)" = "hsa05223",
          "Cardiomyopathy – Heart" = "hsa05414"
        )),


      hr(),
      h4("Algorithm Parameters"),
      selectInput("distance", "Distance measure...",
        choices = c(
          "Euclidean", 
          "Manhattan", 
          "Minkowski", 
          "Canberra")),
      selectInput("linkage", "Linkage criterion...",
        choices = c(
          "Please Select...",
          "Single Linkage", 
          "Complete Linkage", 
          "UPGMA",
          "free parameter selection")),
      fluidRow(
        column(3, numericInput("alpha_i", "alpha_i:", value = 0.5, step = 0.1)),
        column(3, numericInput("alpha_j", "alpha_j:", value = 0.5, step = 0.1)),
        column(3, numericInput("beta", "beta:", value = 0, step = 0.1, max = 0.99)),
        column(3, numericInput("beta", "beta:", value = 0, step = 0.1, max = 0.99)),
        column(3, numericInput("gamma", "gamma:", value = 0.5, step = 0.1))
      ),
      selectInput("clustercrit", "Cluster criterion...",
                  choices = c("Please Select...", "Group by: Patients", "Group by: Genes", "Group by: Patients and Genes")
                  ),
      
      checkboxInput(inputId = "normalizationYesNo", 
                    label = "Normalization wanted", 
                    value = FALSE),
      
      selectInput("normalization", "Normalization...",
                  choices = c("Please Select...", "Z Score", "Min Max")
      ),
      
      hr(),
      h4("Visualization Settings"),

      numericInput("n_clusters", "Number of clusters:", value = 0, min = 2, max = 10),
      
      selectInput("colorpattern", "Select Colorpattern...",
                  choices = c("Please Select...", "Rainbow", "Heat", "Topo")
      ), # Farbpattern 
      
      hr(),
      actionButton("submit", "Submit"),
      actionButton("download", "Download Result")
    ),
    mainPanel(
      h4("Section 2 - Ausgabe"),

       # Teilbild
      div(id = "normal_view",
          div(class = "float-button",
              actionButton("expand", " + ")
          ),
         wellPanel(
            h5("Teilbild"),
            withSpinner(plotOutput("heatmap_plot"), type = 6),
          )
      ),

      # Vollbild
      hidden(
        div(
          id = "fullscreen_view", class = "fullscreen",
          div(
            class = "float-button",
            actionButton("collapse", " - ")
          ),
          h3("Vollbild"),
          withSpinner(plotOutput("heatmap_plot_fullscreen"), type = 6),
        )
      )
    )
  )
)
server <- function(input, output, session) {
  observe({
    if (input$normalizationYesNo == TRUE){
      shinyjs::enable("normalization")
    } else if (input$normalizationYesNo == FALSE){
      shinyjs::disable("normalization")
    }
    
  })
  observe({  # Eingabe der Parameter nur bei freier Parameterwahl erlauben
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
      
    } else if (input$linkage == "Single Linkage"){
      shinyjs::disable("alpha_i")
      shinyjs::disable("alpha_j")
      shinyjs::disable("beta")
      shinyjs::disable("gamma")
      
      updateNumericInput(session, "alpha_i", value = 0.5)
      updateNumericInput(session, "alpha_j", value = 0.5)
      updateNumericInput(session, "beta", value = 0)
      updateNumericInput(session, "gamma", value = -0.5)
    
    } else if (input$linkage == "Please Select..."){
      shinyjs::disable("alpha_i")
      shinyjs::disable("alpha_j")
      shinyjs::disable("beta")
      shinyjs::disable("gamma")
      
      updateNumericInput(session, "alpha_i", value = 0)
      updateNumericInput(session, "alpha_j", value = 0)
      updateNumericInput(session, "beta", value = 0)
      updateNumericInput(session, "gamma", value = 0)
      
    } else if (input$linkage == "UPGMA"){
      shinyjs::disable("alpha_i")
      shinyjs::disable("alpha_j")
      shinyjs::disable("beta")
      shinyjs::disable("gamma")
      
      updateNumericInput(session, "alpha_i", value = 0.5)
      updateNumericInput(session, "alpha_j", value = 0.5)
      updateNumericInput(session, "beta", value = 0)
      updateNumericInput(session, "gamma", value = 0)
      
      updateNumericInput(session, "gamma", value = -0.5)
    
    } else if (input$linkage == "Please Select..."){
      shinyjs::disable("alpha_i")
      shinyjs::disable("alpha_j")
      shinyjs::disable("beta")
      shinyjs::disable("gamma")
      
      updateNumericInput(session, "alpha_i", value = 0)
      updateNumericInput(session, "alpha_j", value = 0)
      updateNumericInput(session, "beta", value = 0)
      updateNumericInput(session, "gamma", value = 0)
      
    } else if (input$linkage == "UPGMA"){
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
    req(input$linkage == "free parameter selection")  # Nur bei freier Auswahl aktiv
    
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
  
  observe({
    req(input$linkage == "free parameter selection")  # Nur bei freier Auswahl aktiv
    
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
    output$test_result_normal <- renderText({""})
    output$test_result_fullscreen <- renderText({""})
    
    # Perform the analysis (replace with your actual analysis function)
    withProgress(message = 'Analyzing data...', value = 0, {
      # Here you would call your actual analysis function
      # For example: results <- analyze_patient_data(patient_data(), selected_disease, params)
      results <- graphic_server(input = input, output = output, session = session)
      # Update progress
      incProgress(0.5)
      
      # Simulate processing for now
      Sys.sleep(2)
      
      # Format results
      # result_text <- paste("Analysis completed for", selected_disease, 
      #                      "using", distance_method, "distance and", 
      #                      ifelse(linkage_method == "free parameter selection",
      #                             "custom parameters", linkage_method),
      #                      "with", cluster_count, "clusters")
      
      # # Update outputs with results
      # output$test_result_normal <- renderText({result_text})
      # output$test_result_fullscreen <- renderText({result_text})
    })
  })
}

shinyApp(ui = ui, server = server)
