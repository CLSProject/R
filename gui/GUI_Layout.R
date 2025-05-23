library(shiny)

ui <- fluidPage(
  titlePanel("CLS Analyseplattform"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Upload & Disease selection"),
      
      fileInput("file", "Upload patient csv...", 
                buttonLabel = "Search...", accept = ".csv"),
      
      selectInput("disease", "Select disease (1 out of 24)...", 
                  choices = c(
                    "GSE1297 – Alzheimer's Disease – Hippocampal CA1" = "GSE1297",
                    "GSE5281 – Alzheimer's Disease – Entorhinal Cortex" = "GSE5281_1",
                    "GSE5281 – Alzheimer's Disease – Hippocampus" = "GSE5281_2",
                    "GSE5281 – Alzheimer's Disease – Visual Cortex" = "GSE5281_3",
                    "GSE20153 – Parkinson's disease – Lymphoblasts" = "GSE20153",
                    "GSE20291 – Parkinson's disease – Putamen" = "GSE20291",
                    "GSE8762 – Huntington's disease – Blood" = "GSE8762",
                    "GSE4107 – Colorectal Cancer – Mucosa" = "GSE4107",
                    "GSE8671 – Colorectal Cancer – Colon (1)" = "GSE8671",
                    "GSE9348 – Colorectal Cancer – Colon (2)" = "GSE9348",
                    "GSE14762 – Renal Cancer – Kidney (1)" = "GSE14762",
                    "GSE781 – Renal Cancer – Kidney (2)" = "GSE781",
                    "GSE15471 – Pancreatic Cancer – Pancreas (1)" = "GSE15471",
                    "GSE16515 – Pancreatic Cancer – Pancreas (2)" = "GSE16515",
                    "GSE19728 – Glioma – Brain" = "GSE19728",
                    "GSE21354 – Glioma – Brain & Spine" = "GSE21354",
                    "GSE6956 – Prostate Cancer – Prostate (1)" = "GSE6956_1",
                    "GSE6956 – Prostate Cancer – Prostate (2)" = "GSE6956_2",
                    "GSE3467 – Thyroid Cancer – Thyroid (1)" = "GSE3467",
                    "GSE3678 – Thyroid Cancer – Thyroid (2)" = "GSE3678",
                    "GSE9476 – AML – Blood/Bone marrow" = "GSE9476",
                    "GSE18842 – Lung Cancer – Lung (1)" = "GSE18842",
                    "GSE19188 – Lung Cancer – Lung (2)" = "GSE19188",
                    "GSE3585 – Cardiomyopathy – Heart" = "GSE3585"
                  )),
      
      
      hr(),
      h4("Algorithm Parameters"),
      
      selectInput("distance", "Distance measure...",
                  choices = c("Euclidean", "Manhattan", "Minkowski", "Canberra")
                  ),
      
      selectInput("linkage", "Linkage criterion...",
                  choices = c("Single Linkage", "Complete Linkage", "Average Linkage")
                  ),
      
      selectInput("clustercrit", "Cluster criterion...",
                  choices = c("Group by: Patients", "Group by: Genes", "Group by: Patients and Genes")
                  ),
      
      
      hr(),
      h4("Visualization Settings"),
      
      selectInput("colorpattern", "Select Colorpattern...",
                  choices = c("Rainbow", "Heat", "Topo")
      ), # Farbpattern 
      
      hr(),
      actionButton("submit", "Submit")
      
    ),
    
    
    
    mainPanel(
      h4("Section 2 - Ausgabe")
    )
  )
)
server <- function (input, output, session) {
  
}

shinyApp(ui = ui, server = server)