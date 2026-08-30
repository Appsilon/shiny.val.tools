box::use(
  shiny[moduleServer, NS, renderText, tagList, textOutput],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(textOutput(ns("label")))
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$label <- renderText("present")
  })
}
