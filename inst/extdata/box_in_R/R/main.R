box::use(
  shiny[...],
  dplyr[filter, mutate],
  log = logger,
  ../shared/util,
)

ui <- shiny$fluidPage()

server <- function(input, output, session) {
  log$info("started")
}
