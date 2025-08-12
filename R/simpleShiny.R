simpleShiny <- function(){
  ui <- shiny::fluidPage(
    shiny::fluidRow(
    "Hello, world!",
    shiny::actionButton('begin','Save Selections and Proceed')
    )
  )
  server <- function(input, output, session) {
    shiny::observeEvent(input$begin, {
      print("button clicked")
      shinyalert::shinyalert("clicked!")
    })
  }
  shiny::shinyApp(ui, server)
}
