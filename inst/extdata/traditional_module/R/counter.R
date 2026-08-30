counter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("step"), "Step", value = 1),
    textOutput(ns("total"))
  )
}

counter_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    running <- reactive({
      input$step * 2
    })

    output$total <- renderText({
      format_count(running())
    })

    running
  })
}
