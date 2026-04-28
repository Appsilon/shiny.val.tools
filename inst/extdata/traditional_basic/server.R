function(input, output, session) {
  output$doubled <- renderText({
    format_label(double_it(input$x))
  })
}
