library(shiny)

# Calls whose head is itself a call (`f(a, b)(x)`), e.g. the
# `leaflet::colorNumeric(...)(values)` palette-factory idiom. The walkers
# must treat these as unresolvable heads rather than erroring.
helpers <- list(
  make_formatter = function(prefix, sep) function(x) paste(prefix, x, sep = sep)
)

ui <- fluidPage(
  numericInput("n", "n", value = 1),
  textOutput("label")
)

server <- function(input, output, session) {
  output$label <- renderText({
    a <- (helpers$make_formatter("n", "="))(input$n)
    b <- base::Reduce(function(f, g) g, list(identity, toupper))(a)
    b
  })
}

shinyApp(ui, server)
