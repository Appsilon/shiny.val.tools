box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput],
)

box::use(
  app/view/home,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    uiOutput(ns("message"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$message <- home$render_message(output)
  })
}
