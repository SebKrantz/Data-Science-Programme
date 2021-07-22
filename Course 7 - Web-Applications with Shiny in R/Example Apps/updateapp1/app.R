server <- function(input, output, session) {
  
  observe({
    txt <- paste("Value above is:", input$mytext)
    
    # here I'm essentially writing a result to the text box
    # called myresults
    updateTextInput(session, "myresults", value=txt)
  })
  
}

ui <-   basicPage(
  h3("An example of an update* function"),
  textInput("mytext", "Input goes here"),
  textInput("myresults", "Results will be printed here", "Initial value")
)

shinyApp(ui = ui, server = server)