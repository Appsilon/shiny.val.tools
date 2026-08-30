box::use(
  shiny[bootstrapPage, moduleServer, NS, textOutput],
)

box::use(
  app/view/present,
  # `app/view/absent` is imported and instantiated below, but no such file
  # exists in the enumerated source -- a missing file or a renamed module.
  # The instantiation resolves to nothing: SVT-W104.
  app/view/absent,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    present$ui(id = ns("p")),
    absent$ui(id = ns("a")),
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    present$server(id = "p")
    absent$server(id = "a")
  })
}
