function(input, output, session) {
  # The threshold input does not exist until this UI renders, so no static
  # definition site describes it. This is the SVT-W002 case.
  output$controls <- renderUI({
    numericInput("threshold", "Threshold", value = 1)
  })

  output$summary <- renderText({
    describe(input$dataset, input$threshold)
  })
}
