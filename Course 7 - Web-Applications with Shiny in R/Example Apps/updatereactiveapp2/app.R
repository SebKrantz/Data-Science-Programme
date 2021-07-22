server <- function(input, output, session) {
  
  # since both mytext and myslider are in the reactive
  # they both trigger the code to run
  myresults <- reactive({
    paste(input$mytext, input$myslider)
  })
  
  # eventReactive here tells Shiny only to trigger this code
  # when mytext changes
  myresults_lim <- eventReactive(input$mytext, {
    paste(input$mytext, input$myslider)
  })
  
  observe(updateTextInput(session, "myresults", value = myresults()))
  observe(updateTextInput(session, "myresults_lim", value = myresults_lim()))
  
  
}

ui <- basicPage(
  
  h3("Using eventReactive to limit reactions."),
  sliderInput("myslider", "", min=0, max=1000, value=500),
  textInput("mytext", "Input goes here"),
  textInput("myresults", "Text box + slider (updates when either changes)", "Initial value"),
  textInput("myresults_lim", "Text box + slider (updates when text box changes)", "Initial value")
  
)

shinyApp(ui = ui, server = server)