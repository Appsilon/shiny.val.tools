#' @export
counter_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::numericInput(ns("step"), "Step", value = 1),
    shiny::textOutput(ns("total")),
    readout_ui(ns("readout"))
  )
}

#' @export
counter_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    running <- shiny::reactive({
      input$step * 2
    })

    output$total <- shiny::renderText({
      paste("total:", running())
    })

    readout_server("readout", value = running)
  })
}
