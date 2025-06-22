library(shiny)
library(shinyjs)
library(shinycssloaders)
# install.packages("shinycssloaders")

source("../grafikPanel/graphicpanel.R") # Ermöglicht Zugriff auf Plot Funktionen

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
          "GSE1297 – Alzheimer's Disease – Hippocampal CA1" = "hsa05010",
          "GSE5281 – Alzheimer's Disease – Entorhinal Cortex" = "hsa05010",
          "GSE5281 – Alzheimer's Disease – Hippocampus" = "hsa05010",
          "GSE5281 – Alzheimer's Disease – Visual Cortex" = "hsa05010",
          "GSE20153 – Parkinson's disease – Lymphoblasts" = "hsa05012",
          "GSE20291 – Parkinson's disease – Putamen" = "hsa05012",
          "GSE8762 – Huntington's disease – Blood" = "hsa05016",
          "GSE4107 – Colorectal Cancer – Mucosa" = "hsa05210",
          "GSE8671 – Colorectal Cancer – Colon (1)" = "hsa05210",
          "GSE9348 – Colorectal Cancer – Colon (2)" = "hsa05210",
          "GSE14762 – Renal Cancer – Kidney (1)" = "hsa05211",
          "GSE781 – Renal Cancer – Kidney (2)" = "hsa05211",
          "GSE15471 – Pancreatic Cancer – Pancreas (1)" = "hsa05212",
          "GSE16515 – Pancreatic Cancer – Pancreas (2)" = "hsa05212",
          "GSE19728 – Glioma – Brain" = "hsa05214",
          "GSE21354 – Glioma – Brain & Spine" = "hsa05214",
          "GSE6956 – Prostate Cancer – Prostate (1)" = "hsa05215",
          "GSE6956 – Prostate Cancer – Prostate (2)" = "hsa05215",
          "GSE3467 – Thyroid Cancer – Thyroid (1)" = "hsa05216",
          "GSE3678 – Thyroid Cancer – Thyroid (2)" = "hsa05216",
          "GSE9476 – AML – Blood/Bone marrow" = "hsa05221",
          "GSE18842 – Lung Cancer – Lung (1)" = "hsa05223",
          "GSE19188 – Lung Cancer – Lung (2)" = "hsa05223",
          "GSE3585 – Cardiomyopathy – Heart" = "hsa05414"
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
          "Single Linkage", 
          "Complete Linkage", 
          "free parameter selection")),
      fluidRow(
        column(3, numericInput("alpha_i", "alpha_i:", value = 0.5, step = 0.1)),
        column(3, numericInput("alpha_j", "alpha_j:", value = 0.5, step = 0.1)),
        column(3, numericInput("beta", "beta:", value = 0, step = 0.1)),
        column(3, numericInput("gamma", "gamma:", value = 0.5, step = 0.1))
      ),
      selectInput("clustercrit", "Cluster criterion...",
        choices = c(
          "Group by: Patients", 
          "Group by: Genes", 
          "Group by: Patients and Genes"
          )),
      hr(),
      h4("Visualization Settings"),
      numericInput("n_clusters", "Number of clusters:", value = 3, min = 2, max = 10),
      selectInput("colorpattern", "Select Colorpattern...",
        choices = c("Rainbow", "Heat", "Topo")
      ), # Farbpattern

      hr(),
      actionButton("submit", "Submit")
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
          withSpinner(textOutput("test_result_fullscreen"), type = 6),
        )
      )
    )
  )
)
server <- function(input, output, session) {
  observe({ # Eingabe der Parameter nur bei freier Parameterwahl erlauben
    if (input$linkage == "Complete Linkage") {
      shinyjs::disable("alpha_i")
      shinyjs::disable("alpha_j")
      shinyjs::disable("beta")
      shinyjs::disable("gamma")

      updateNumericInput(session, "alpha_i", value = 0.5)
      updateNumericInput(session, "alpha_j", value = 0.5)
      updateNumericInput(session, "beta", value = 0)
      updateNumericInput(session, "gamma", value = -0.5)
    } else if (input$linkage == "Single Linkage") {
      shinyjs::disable("alpha_i")
      shinyjs::disable("alpha_j")
      shinyjs::disable("beta")
      shinyjs::disable("gamma")

      updateNumericInput(session, "alpha_i", value = 0.5)
      updateNumericInput(session, "alpha_j", value = 0.5)
      updateNumericInput(session, "beta", value = 0)
      updateNumericInput(session, "gamma", value = 0.5)
    } else {
      shinyjs::enable("alpha_i")
      shinyjs::enable("alpha_j")
      shinyjs::enable("beta")
      shinyjs::enable("gamma")
    }
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
