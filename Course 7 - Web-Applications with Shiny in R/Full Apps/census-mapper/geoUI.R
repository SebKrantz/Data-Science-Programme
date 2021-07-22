shinyUI(fluidPage(div(class="outer",
                      
                      tags$head(
                        # Include our custom CSS
                        includeCSS("styles.css")
                      ),
                      
                      # If not using custom CSS, set height of leafletOutput to a number instead of percent
                      leafletOutput("map", width="100%", height="100%"), 
                      
                      # Shiny versions prior to 0.11 should use class = "modal" instead.
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = FALSE, top = 130, left = "auto", right = 20, bottom = "auto",
                                    width = 335, height = "auto", cursor = "auto",
                                    
                                    h2("Uganda 2014 Census", style = "color: #262626; padding-top: 0;"),
                                    
                                    tags$style(HTML("
                                    .tabbable > .nav > li > a                  {background-color: transparent;  color:#0067e6;}
                                    .tabbable > .nav > li[class=active]    > a {background-color: #e6e6e6; color:#0067e6;}
                                    ")),
                                    
                                    tabsetPanel(id="geotab",
                                                tabPanel("Data",
                                                         div(id='myDiv',
                                                             selectInput("topic", "Topic", choices = names(cens_vars_list)), # c("Select a Topic" = "", names(cens_vars_list)) # , selected = "Composite Indices"
                                                             selectInput("color", "Indicator", choices = c("Select an Indicator" = "")), # c("Human Development Index" = "HDI")  # cens_vars_list[["Composite Indices"]], selected = "HDI"),
                                                             conditionalPanel("input.topic != 'Composite Indices' && input.topic != ''",
                                                               radioButtons("unit", "Unit", choices = "Number", inline = TRUE)
                                                             ),
                                                             tags$hr(),
                                                             selectInput("agglev", "Level of Aggregation", choices = agglabs, selected = "District") 
                                                             
                                                         )
                                                ),
                                                tabPanel("Appearance", 
                                                         div(id='myDiv',
                                                           selectInput("palette", "Color Palette", choices = list(`Viridis Palettes` = c("viridis","magma","inferno","plasma"),
                                                                                                               `R Color Brewer - Sequential` = rev(row.names.data.frame(brewer.pal.info)[brewer.pal.info$category == "seq"]),
                                                                                                               `R Color Brewer - Divergent`  = rev(row.names.data.frame(brewer.pal.info)[brewer.pal.info$category == "div"])),
                                                                    selected = "viridis")
                                                         ),
                                                        numericInput("opacity", "Opacity", min = 0, max = 1, value = 0.7, step = 0.1),
                                                        tags$hr(),
                                                        checkboxInput("logcolor", "Log10 Scale"),
                                                        checkboxInput("discbreaks", "Discrete Breaks"),
                                                        checkboxInput("stroke", "Stroke Polygon Shapes", TRUE)
                                                        
                                               )
                                    )

                      ))))
