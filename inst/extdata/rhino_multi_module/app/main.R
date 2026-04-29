box::use(
  shiny[bootstrapPage, div, moduleServer, NS, tags, uiOutput],
)

box::use(
  app/view/mod_a,
  app/view/mod_b,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    mod_a$ui(id = ns("a")),
    mod_b$ui(id = ns("b")),
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    selected <- mod_a$server(id = "a")
    mod_b$server(id = "b", selected = selected)
  })
}
