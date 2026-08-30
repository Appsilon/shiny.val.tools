function(input, output, session) {
  output$summary <- renderText({
    paste(mid(seq_len(input$n)), spread(seq_len(input$n)))
  })
}
