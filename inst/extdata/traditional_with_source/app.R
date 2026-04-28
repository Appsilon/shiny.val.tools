library(shiny)

source("helpers/foo.R")

if (Sys.getenv("DEBUG") == "1") {
  source("helpers/dev.R")
}

shinyApp(ui, server)
