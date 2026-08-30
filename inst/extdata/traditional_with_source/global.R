library(shiny)

# chdir = TRUE so that foo.R's own `source("bar.R", chdir = TRUE)` resolves
# relative to helpers/ rather than the app root.
source("helpers/foo.R", chdir = TRUE)

if (Sys.getenv("DEBUG") == "1") {
  source("helpers/dev.R")
}
