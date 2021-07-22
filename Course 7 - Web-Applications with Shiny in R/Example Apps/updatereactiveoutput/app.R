server <- function(input, output, session) {
  
  # when select box changes, we get a different dataset
  dat <- reactive(get(input$dataset))

  # Based on the dataset, this updates so we can see the variables of the dataset
  observeEvent(input$dataset, {
    
    updateSelectizeInput(session, "vars", choices = names(dat()))
    
  })
  
  # This re-executes if either dat() or input$vars changes
  output$summ <- renderPrint({
    
     # input$vars is NULL if no variables selected, otherwise a character string
     vars <- input$vars 
     data <- dat()
     
     # This is meant to prevent errors from empty vars or when switching datasets
     if(length(vars) && all(vars %in% names(data))) 
       print(summary(data[vars])) 
     
  })
  
}

ui <- fluidPage(
  
  titlePanel("An app using an observe, reactive and render"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select Dataset", choices = c("iris", "mtcars", "airquality"), selected = "iris"),
      selectizeInput("vars", "Select Variables", choices = names(iris),
                     options = list(placeholder = "Choose variables"), multiple = TRUE)
    ), # end sidebar panel
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",  verbatimTextOutput("summ"))
      )
    ) # end main panel
  )
)

shinyApp(ui = ui, server = server)