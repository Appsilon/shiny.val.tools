library(shiny)

# Shiny's autoload sources R/*.R non-recursively, so nested helpers must be
# sourced explicitly or they are never available to the app.
source("R/utils/format.R")
