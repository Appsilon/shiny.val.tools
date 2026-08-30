library(shiny)

fluidPage(
  numericInput("n", "n", 10),
  textOutput("summary")
)
