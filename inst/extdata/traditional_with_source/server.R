function(input, output, session) {
  output$out <- renderText({
    paste("foo_helper:", foo_helper(input$n))
  })
}
