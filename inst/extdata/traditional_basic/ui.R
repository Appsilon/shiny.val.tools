library(shiny)

fluidPage(
  numericInput("x", "x", value = 1),
  textOutput("doubled")
)
