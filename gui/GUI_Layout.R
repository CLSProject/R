library(shiny)
library(shinyjs)
library(shinycssloaders)
# install.packages("shinycssloaders")

options(shiny.maxRequestSize = 1000*1024^2) # 1GB max Upload

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
                buttonLabel = "Search...", accept = ".csv"),
      
      selectInput("disease", "Select disease (1 out of 24)...", 
                  choices = c(
                    "Please Select..." = "SelectDesease",
                    "Alzheimer's Disease – Hippocampal CA1" = "GSE1297",
                    "Alzheimer's Disease – Entorhinal Cortex" = "GSE5281_1",
                    "Alzheimer's Disease – Hippocampus" = "GSE5281_2",
                    "Alzheimer's Disease – Visual Cortex" = "GSE5281_3",
                    "Parkinson's disease – Lymphoblasts" = "GSE20153",
                    "Parkinson's disease – Putamen" = "GSE20291",
                    "Huntington's disease – Blood" = "GSE8762",
                    "Colorectal Cancer – Mucosa" = "GSE4107",
                    "Colorectal Cancer – Colon (1)" = "GSE8671",
                    "Colorectal Cancer – Colon (2)" = "GSE9348",
                    "Renal Cancer – Kidney (1)" = "GSE14762",
                    "Renal Cancer – Kidney (2)" = "GSE781",
                    "Pancreatic Cancer – Pancreas (1)" = "GSE15471",
                    "Pancreatic Cancer – Pancreas (2)" = "GSE16515",
                    "Glioma – Brain" = "GSE19728",
                    "Glioma – Brain & Spine" = "GSE21354",
                    "Prostate Cancer – Prostate (1)" = "GSE6956_1",
                    "Prostate Cancer – Prostate (2)" = "GSE6956_2",
                    "Thyroid Cancer – Thyroid (1)" = "GSE3467",
                    "Thyroid Cancer – Thyroid (2)" = "GSE3678",
                    "AML – Blood/Bone marrow" = "GSE9476",
                    "Lung Cancer – Lung (1)" = "GSE18842",
                    "Lung Cancer – Lung (2)" = "GSE19188",
                    "Cardiomyopathy – Heart" = "GSE3585"
                  )),
      
      
      hr(),
      h4("Algorithm Parameters"),
      
      selectInput("distance", "Distance measure...",
                  choices = c("Please Select...", "Euclidean", "Manhattan", "Minkowski", "Canberra")
                  ),
      
      selectInput("linkage", "Linkage criterion...",
                  choices = c("Please Select...", "Single Linkage", "Complete Linkage", "UPGMA", "free parameter selection")
                  ),
      fluidRow(
        column(3, numericInput("alpha_i", "alpha_i:", value = 0.5, step = 0.1)),
        column(3, numericInput("alpha_j", "alpha_j:", value = 0.5, step = 0.1)),
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
      actionButton("reload", "Reload Visualization"),
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
            textOutput("test_result_normal"),
          )
      ),
      
      # Vollbild
      hidden(
        div(id = "fullscreen_view", class = "fullscreen",
            div(class = "float-button",
                actionButton("collapse", " - ")
            ),
             h3("Vollbild"),
            textOutput("test_result_normal"),
        )
      )
      
    )
  )
)
server <- function (input, output, session) {
  
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
      
    } else {
      shinyjs::disable("alpha_i")
      shinyjs::disable("alpha_j")
      shinyjs::enable("beta")
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


  # Umstellung zwischen Vollbildschirm und Teilbildschirm
  observeEvent(input$expand, {
    hide("normal_view")
    show("fullscreen_view")
  })
  
  observeEvent(input$collapse, {
    hide("fullscreen_view")
    show("normal_view")
  })
  
  
  
  
  # reaktive Berechnung mit eventReactive
  #trigger <- reactive({
  #  input$submit
  #  input$reload
  #})
  
  #test_result <- eventReactive(trigger(), {
  #  Sys.sleep(5)  # Berechnung simulieren
  #  "Test erfolgreich"
  #})
  
  test_result <- eventReactive(input$submit, {
    
    if (is.null(input$file) ||
        input$disease == "SelectDesease" ||
        is.null(input$alpha_i) ||
        is.null(input$alpha_j) ||
        is.null(input$beta) ||
        is.null(input$gamma) ||
        input$distance == "Please Select..." ||
        input$linkage == "Please Select..." ||
        input$clustercrit == "Please Select..." ||
        input$n_clusters == "Please Select..." ||
        input$colorpattern == "Please Select..." ||
        (input$normalization == "Please Select..." && input$normalizationYesNo == TRUE)){
      
      showModal(modalDialog(
        title = "Missing Inputs",
        "Please Fill in the remaining fields",
        easyClose = TRUE
      ))
      
    } else {
    
    #Sys.sleep(5)
    
    csv_datapath <<- input$file$datapath  #für Lucy
    val_distance <<- input$distance
    val_linkage <<- input$linkage
    val_alpha_i <<- input$alpha_i
    val_alpha_j <<- input$alpha_j
    val_beta <<- input$beta
    val_gamma <<- input$gamma
    val_clustercrit <<- input$clustercrit
    val_n_clusters <<- input$n_clusters
    val_colorpattern <<- input$colorpattern
    val_disease <<- input$disease
    if(input$normalizationYesNo == FALSE){
      val_normalization <<- "none"
    }else{
      val_normalization <<- input$normalization
    }
    
    
    "Test erfolgreich"
    # Werte aus input als "nicht-reaktive" R-Variablen übernehmen
    }
    
  })

  reload_result <- eventReactive(input$reload, {
    #Sys.sleep(5)
    
    val_n_clusters <<- input$n_clusters
    val_colorpattern <<- input$colorpattern
    
    "Reload erfolgreich"
  })
    
  'observeEvent(input$submit,{
  # Werte aus input als "nicht-reaktive" R-Variablen übernehmen
  val_distance <<- input$distance
  val_linkage <<- input$linkage
  val_alpha_i <<- input$alpha_i
  val_alpha_j <<- input$alpha_j
  val_beta <<- input$beta
  val_gamma <<- input$gamma
  val_clustercrit <<- input$clustercrit
  val_n_clusters <<- input$n_clusters
  val_colorpattern <<- input$colorpattern
  val_disease <<- input$disease
})'
 
# Ausgabe-Elemente
 ' output$test_result_normal <- renderText({
    test_result()
  })
  
  output$test_result_fullscreen <- renderText({
    test_result()
  })'
  
  # Dynamisch unterscheiden, was zuletzt gedrückt wurde
  output$test_result_normal <- renderText({
    if (input$submit > input$reload) {
      test_result()
    } else if (input$reload > 0) {
      reload_result()
    } else {
      "Bitte Submit oder Reload drücken"
    }
  })
  
  'output$test_result_fullscreen <- renderText({
    if (input$submit > input$reload) {
      submit_result()
    } else if (input$reload > 0) {
      reload_result()
    } else {
      "Bitte Submit oder Reload drücken"
    }
  })'
}

shinyApp(ui = ui, server = server)
