# Load packages ----
library(shiny)
library(quantmod)

# Source helpers ----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("stockVis"),

  sidebarLayout(
    sidebarPanel(
      helpText("Select a stock to examine.

        Information will be collected from Yahoo finance."),
      textInput("symb", "Symbol", "SPY"),

      dateRangeInput("dates",
                     "Date range",
                     start = "2013-01-01", 
                     min = "2013-01-01", # Because we load all data for a stock and subset after, we need to restrict the users choices by setting minimum and maximum date
                     end = as.character(Sys.Date()), 
                     max = as.character(Sys.Date())),

      br(),
      br(),

      checkboxInput("log", "Plot y axis on log scale",
                    value = FALSE),

      checkboxInput("adjust",
                    "Adjust prices for inflation", value = FALSE)
    ),

    mainPanel(plotOutput("plot"))
  )
)

# Server logic
server <- function(input, output) {

  # Efficient modularization: For each stock, we only load the data once
  dataInput <- reactive({  
    getSymbols(input$symb, src = "yahoo",
               from = "2013-01-01",
               to = as.character(Sys.Date()),
               auto.assign = FALSE)
  })
  # If adjustment, we adjust the entire data for inflation
  adjInput <- reactive({
    if (!input$adjust) return(dataInput())
    adjust(dataInput())
  })
  # We can now subset the date range without the need to reload the data or readjust (see course 2 session on 'xts' time series for subsetting 'xts' using two dates)
  finalInput <- reactive({
    adjInput()[paste(input$dates, collapse = "/")]
  })
  # taking the log is relatively inexpensive, so we can do that on the fly in the plot function. 
  output$plot <- renderPlot({
    chartSeries(finalInput(), theme = chartTheme("white"),
                type = "line", log.scale = input$log, TA = NULL)
  })

}

# Run the app
shinyApp(ui, server)
