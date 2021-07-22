server <- function(input, output, session) {
  
  # first observer has lower priority so it runs second and will
  # overwrite the other observer
  observe({
    txtA <- paste("First observer", input$mytext)
    updateTextInput(session, inputId = "myresults", value = txtA)
  }, priority = 1)
  
  # second observer has higher priority so it will run first and
  # then be overwritten
  observe({
    txtB <- paste("Second observer", input$mytext)
    updateTextInput(session, inputId = "myresults", value = txtB)
  }, priority = 2)
  
}

ui <- basicPage(
  
  h3("Priority is higher for second observer so it runs first and then gets written over by the first observer"),
  textInput("mytext", "Input goes here"),
  textInput("myresults", "Results will be printed here", "")
)

shinyApp(ui = ui, server = server)