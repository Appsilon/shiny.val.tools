box::use(
  shiny[bootstrapPage, moduleServer, NS],
)

box::use(
  app/view/counter,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    counter$ui(ns("counter"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    counter$server("counter")
  })
}
