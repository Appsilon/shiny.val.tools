box::use(
  shiny[moduleServer, NS, reactive, selectInput, tagList],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("which"), "Pick", choices = c("x", "y"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({ input$which })
  })
}
