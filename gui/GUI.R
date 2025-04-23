library(shiny)
# UI = was der User sieht ######################################################
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .centered {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        height: 100vh;
        text-align: center;
      }
    "))
  ),
  div(class = "centered",
      h2("Welcome to the GUI"),
      actionButton("test_btn", "Test")
  )
)

# SERVER = logig ###############################################################

server <- function(input, output, session) {
  observeEvent(input$test_btn, {
    showModal(modalDialog(
      title = "Info",
      "Button has been pressed!",
      easyClose = TRUE
    ))
  })
}
# startet die GUI ##############################################################

shinyApp(ui, server)