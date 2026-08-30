# Internal module: deliberately absent from NAMESPACE. It must not become a
# top-level module record, but it must still appear as a child instance
# inside the exported module that uses it.

readout_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::textOutput(ns("echo"))
}

readout_server <- function(id, value) {
  shiny::moduleServer(id, function(input, output, session) {
    output$echo <- shiny::renderText({
      paste("readout:", value())
    })
  })
}
