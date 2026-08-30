library(shiny)

fluidPage(
  selectInput("dataset", "Dataset", choices = c("a", "b")),
  uiOutput("controls"),
  textOutput("summary")
)
