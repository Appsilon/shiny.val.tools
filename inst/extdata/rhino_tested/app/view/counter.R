box::use(
  shiny[actionButton, moduleServer, NS, reactive, renderText, tagList, textOutput],
)

box::use(
  app/logic/arithmetic,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("bump"), "Bump"),
    textOutput(ns("count"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    bumped <- reactive({
      arithmetic$step(input$bump)
    })

    output$count <- renderText({
      bumped()
    })
  })
}
