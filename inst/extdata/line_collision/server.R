# Both outputs are defined on ONE line. `dynamic` reads a runtime-built
# input id (SVT-W001); `plain` reads nothing. Attributing the warning by
# (file, line) cannot tell the two apart -- the node_id can.
function(input, output, session) {
  output$dynamic <- renderText({ input[[paste0("k", 1)]] }); output$plain <- renderText({ "static" })
}
