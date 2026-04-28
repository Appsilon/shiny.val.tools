box::use(
  shiny[renderUI, div, tags],
)

#' @export
render_message <- function(output) {
  renderUI({
    div(
      style = "display: flex; justify-content: center; align-items: center; height: 100vh;",
      tags$h1("Hello from rhino!")
    )
  })
}
