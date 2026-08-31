function(input, output, session) {
  doubled_value <- reactive({
    double_it(input$x)
  })

  output$doubled <- renderText({
    format_label(doubled_value())
  })

  output$scaled <- renderText({
    format_label(scale_by(input$x, input$factor))
  })

  output$summary <- renderText({
    summarise_values(input$x, input$factor)
  })

  output$ratio <- renderText({
    ratio_of(input$x, input$factor)
  })

  output$notes <- renderText({
    paste("x is", input$x)
  })
}
