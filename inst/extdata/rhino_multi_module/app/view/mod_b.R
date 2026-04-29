box::use(
  shiny[moduleServer, NS, renderText, tagList, textOutput],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("echo"))
  )
}

#' @export
server <- function(id, selected) {
  moduleServer(id, function(input, output, session) {
    output$echo <- renderText({
      paste("you picked:", selected())
    })
  })
}
