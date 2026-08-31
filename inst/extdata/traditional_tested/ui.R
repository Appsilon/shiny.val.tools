library(shiny)

fluidPage(
  numericInput("x", "x", value = 1),
  numericInput("factor", "factor", value = 3),
  textOutput("doubled"),
  textOutput("scaled"),
  textOutput("summary"),
  textOutput("ratio"),
  textOutput("notes")
)
