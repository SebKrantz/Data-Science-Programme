server <- function(input, output, session) {
  
  # Now if you change the slider only the slider result changes
  # and the text box result stays the same. This is because
  # we isolated the reactive values in their own reactive function
  
  txt <- reactive({paste(input$mytext, sample(1:100, 1))})
  val <- reactive({paste(input$myslider, sample(1:100, 1), sep="-")})
  
  
  observe({
    res <- paste0(txt(), " | Slider ", val())
    updateTextInput(session, "myresults", value = res)
  })  
  
  
}

ui <- basicPage(
  
  h3("Changes to the text box and slider are separated so that a change to the text box will not affect the slider part of the results textbox"),
  sliderInput("myslider", "A slider:", min=0, max=1000, value=500),
  textInput("mytext", "Input goes here", "Text"),
  textInput("myresults", "Results will be printed here", "Initial value")
  
)

shinyApp(ui = ui, server = server)