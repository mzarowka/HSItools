simpleShiny <- function(){
  ui <- fluidPage(
    fluidRow(
    "Hello, world!",
    actionButton('begin','Save Selections and Proceed')
    )
  )
  server <- function(input, output, session) {
    observeEvent(input$begin, {
      print("button clicked")
      shinyalert::shinyalert("clicked!")
    })
  }
  shinyApp(ui, server)
}
