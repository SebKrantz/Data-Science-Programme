#############################################################
# *************** R BASED DATA SCIENCE TRAINING *************
# Course 7: Web-Applications with Shiny 
# -----------------------------------------------------------
# Prepared by: Sebastian Krantz, ODI Fellow, MEPD, MoFPED
# Permanent Contact: sebastian.krantz@graduateinstitute.ch
#############################################################

# Course Aim: To familiarize participants with web-application development  
#             using shiny and related packages, including custom HTML and CSS elements


# This Course:
# (1) Intro to Shiny
# (2) UI and Server-Side Programming
# (3) Reactive Expressions
# (4) Observers, Update Statements and Full Reactivity
# (5) R Packages Extending Shiny
# (6) Custom HTML and CSS Elements
# (7) Case-Study: Shiny Data Portal
# (8) Case-Study: Census Mapping App
# (9) Publishing Shiny Applications

# Today: We'll try to cover topics 1-6.

setwd("Course 7 - Web-Applications with Shiny in R")

# (1) Intro to Shiny -------------------------------------------------------
#***************************************************************************

# Shiny (https://shiny.rstudio.com/) is a framework for creating web applications using R code. 
# It is designed primarily with data scientists in mind, and to that end, 
# you can create pretty complicated Shiny apps with no knowledge of HTML, 
# CSS, or JavaScript. On the other hand, Shiny doesn’t limit you to creating 
# trivial or prefabricated apps: its user interface components can be easily 
# customized or extended, and its server uses reactive programming to let you 
# create any type of back end logic you want. 
# (Hadley Wickham, Mastering Shiny: https://mastering-shiny.org/index.html)

# Main resources are all on the website: https://shiny.rstudio.com/
# In particular: 
# Turorials (Video and Written + Mastering Shiny Book): https://shiny.rstudio.com/tutorial/
# Resources from tutorial here: https://github.com/rstudio-education/shiny.rstudio.com-tutorial
# Articles: https://shiny.rstudio.com/articles/
# Function refrence: https://shiny.rstudio.com/reference/shiny/1.6.0/



# Here I will follow the written tutorial: https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/

install.packages("shiny")
library(shiny)

# Let's look at the first Example App:  
runExample("01_hello") # Code saved as app1.R under Example Apps Folder

runApp("Example Apps/app1", display.mode = "showcase")

# This is the server function:
source("Example Apps/app_1_server.r")
r <- server(input = list(bins = 1), output = list())
str(r)

# This simply executes the code inside the server function:
x    <- faithful$waiting
bins <- seq(min(x), max(x), length.out = 30 + 1)

hist(x, breaks = bins, col = "#75AADB", border = "white",
     xlab = "Waiting time to next eruption (in mins)",
     main = "Histogram of waiting times")

# Every Shiny app has the same structure: an app.R file that contains ui and server. 
# You can create a Shiny app by making a new directory and saving an app.R file inside it. 
# It is recommended that each app will live in its own unique directory.

# You can run a Shiny app by giving the name of its directory to the function runApp:
runApp("Example Apps/app1") # Normal mode
runApp("Example Apps/app1", display.mode = "showcase") # Showcase mode

#****************************
### In-Class Exercise 1 -----
#****************************

# Click escape and make some changes to this app:
  
# - Change the title from “Hello Shiny!” to “Hello World!”.
# - Set the minimum value of the slider bar to 5.
# - Change the histogram border color from "white" to "orange".



# Shiny Gallery: https://shiny.rstudio.com/gallery/



# (2) UI and Server-Side Programming ---------------------------------------
#***************************************************************************

# Now Let's buld an app from scratch: 
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/

# Layout
# Shiny uses the function fluidPage to create a display that automatically adjusts to the dimensions of your user’s browser window. 
# (Other alternatives are fixedPage(), bootstrapPage() / basicPage(), fillPage() and navbarPage(), see https://shiny.rstudio.com/reference/shiny/1.6.0/)
# You lay out the user interface of your app by placing elements in the fluidPage function.

# titlePanel and sidebarLayout are the two most popular elements to add to fluidPage. They create a basic Shiny app with a sidebar.
# sidebarLayout always takes two arguments:
#  - sidebarPanel function output
#  - mainPanel function output

# More options in the application layout guide: https://shiny.rstudio.com/articles/layout-guide.html
# You can add content to your Shiny app by placing it inside a *Panel function.

# HTML tags are provided by the htmlbuilder package. names arguments become attributes
p("hello")  # Paragraph
p("hello", style = "color:blue") # paragraph with blue text 
img(src = "my_image.png", height = 72, width = 72) # Image + width and height in pixels
# The img function looks for your image file in a specific place: 
# Your file must be in a folder named www in the same directory as the app.R script. 
# Shiny treats this directory in a special way. 
# Shiny will share any file placed here with your user’s web browser, which makes www a great place to put images, 
# style sheets, and other things the browser will need to build the web components of your Shiny app.

# More about HTML customization at: 
# https://shiny.rstudio.com/articles/html-tags.html
# https://shiny.rstudio.com/articles/tag-glossary.html

# Note: Tags placed inside a *Panel function need to be separated with commas. 
# the omission of which is a major source of errors in Shiny apps!!


#****************************
### In-Class Exercise 2 -----
#****************************

# Do the exercise at the bottom of this page: 
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
# (try to recreate the layout)

runApp("Example Apps/lesson2", display.mode = "showcase")



# Widgets: https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
# What’s a widget? A web element that your users can interact with. Widgets provide a way for your users to send messages to the Shiny app.
# Shiny comes with a family of pre-built widgets, each created with a transparently named R function. 
# For example, Shiny provides a function named actionButton that creates an Action Button and a function named sliderInput that creates a slider bar.

# You can add widgets to your web page in the same way that you added other types of HTML content in Lesson 2. 
# To add a widget to your app, place a widget function in sidebarPanel or mainPanel in your ui object.

# Each widget function requires several arguments. The first two arguments for each widget are

# - a name for the widget: The user will not see this name, but you can use it to access the widget’s value. The name should be a character string.
# - a label: This label will appear with the widget in your app. It should be a character string, but it can be an empty string "".

# In this example, the name is “action” and the label is “Action”: actionButton("action", label = "Action")
# The remaining arguments vary from widget to widget, depending on what the widget needs to do its job. They include things like initial values, ranges, and increments. You can find the exact arguments needed by a widget on the widget function’s help page, (e.g., ?selectInput).

runApp("Example Apps/app3", display.mode = "showcase")

#****************************
### In-Class Exercise 3 -----
#****************************

# Do the exercise at the end of lesson 3: https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/

# Look at the widget gallery: https://shiny.rstudio.com/gallery/widget-gallery.html
# Play around with different widgets and comprehend the output they return. 


# Reactive Output
#****************

# Reactive output automatically responds when your user toggles a widget.
# Create a folder in your working directory named census-app. 
# You can create reactive output with a two step process.

# (1) Add an R object to your user interface.
# (2) Tell Shiny how to build the object in the server function. The object will be reactive if the code that builds it calls a widget value.

#  Each of the *Output functions require a single argument: a character string that Shiny will use as the name of your reactive element. 

# The server function plays a special role in the Shiny process:
# it builds a list-like object named output that contains all of the code needed to update the R objects in your app. Each R object needs to have its own entry in the list.
# You can create an entry by defining a new element for output within the server function, like below. The element name should match the name of the reactive element that you created in the ui.
# You do not need to explicitly state in the server function to return output in its last line of code. R will automatically update output through reference class semantics.
# Each entry to output should contain the output of one of Shiny’s render* functions. These functions capture an R expression and do some light pre-processing on the expression. Use the render* function that corrresponds to the type of reactive object you are making.
# Each render* function takes a single argument: an R expression surrounded by braces, {}. The expression can be one simple line of text, or it can involve many lines of code, as if it were a complicated function call.
# Think of this R expression as a set of instructions that you give Shiny to store for later. Shiny will run the instructions when you first launch your app, and then Shiny will re-run the instructions every time it needs to update your object.
# For this to work, your expression should return the object you have in mind (a piece of text, a plot, a data frame, etc.). You will get an error if the expression does not return an object, or if it returns the wrong type of object.

# Shiny tracks which outputs depend on which widgets. When a user changes a widget, Shiny will rebuild all of the outputs that depend on the widget, using the new value of the widget as it goes. As a result, the rebuilt objects will be completely up-to-date.
# This is how you create reactivity with Shiny, by connecting the values of input to the objects in output. Shiny takes care of all of the other details.

runApp("Example Apps/census-app", display.mode = "showcase")

#****************************
### In-Class Exercise 4 -----
#****************************

# Do the exercise at the end of lesson 4: https://shiny.rstudio.com/tutorial/written-tutorial/lesson4/

runApp("Example Apps/census-app", display.mode = "showcase")

# Now: Building A US Census Visualization App: https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/
#********************************************

counties <- readRDS("Example Apps/census-app/data/counties.rds")
head(counties)
# counties <- tidyr::separate(counties, name, c("state", "countr"), ",")
library(maps)
library(mapproj)
source("Example Apps/census-app/helpers.R")
percent_map(counties$white, "darkgreen", "% White", 0, 100)

# App Operation: 

# - The shinyApp function is run once, when you launch your app
# - The server function is run once each time a user visits your app
# - The R expressions inside render* functions are run many times. 
#   Shiny runs them once each time a user change the value of a widget.

# Thus: 

# Source scripts, load libraries, and read data sets at the beginning of app.R outside of the server function. 
# Shiny will only run this code once, which is all you need to set your server up to run the R expressions contained in server.

# Define user specific objects inside server function, but outside of any render* calls. 
# These would be objects that you think each user will need their own personal copy of. 
# For example, an object that records the user’s session information. This code will be run once per user.

# Only place code that Shiny must rerun to build an object inside of a render* function. 
# Shiny will rerun all of the code in a render* chunk each time a user changes a widget mentioned in the chunk. 
# This can be quite often.

# You should generally avoid placing code inside a render function that does not need to be there. Doing so will slow down the entire app.

# Your server can rely on a single global copy of counties.rds and percent_map to do all of the R execution necessary for all of the users. 
# You only need to build a separate object for each user if the objects will have different values for each of your users.


#****************************
### In-Class Exercise 5 -----
#****************************

# Do the exercise at the end of lesson 5: https://shiny.rstudio.com/tutorial/written-tutorial/lesson4/


runApp("Example Apps/census-app", display.mode = "showcase")

# (3) Reactive Expressions -------------------------------------------------
#***************************************************************************

# Reactive expressions let you control which parts of your app update when, 
# which prevents unnecessary computation that can slow down your app.

runExample("03_reactivity") # a reactive expression

# Stock Visualization App: https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/
#************************

# The stockVis app looks up stock prices by ticker symbol and displays the results as a line chart.
# It uses R’s quantmod package, so you’ll need to install quantmod with install.packages("quantmod") if you do not already have it.
runApp("Example Apps/stockVis", display.mode = "showcase")

# Note that the “Adjust prices for inflation” check box doesn’t work yet. One of our tasks in this lesson is to fix this check box.
# By default, stockVis displays the SPY ticker (an index of the entire S&P 500). 
# Some common symbols are GOOG (Google), AAPL (Apple), and GS (Goldman Sachs). More tickers at: https://finance.yahoo.com/lookup/

# stockVis relies heavily on two functions from the quantmod package:
  
# - getSymbols to download financial data straight into R from websites like Yahoo finance and the Federal Reserve Bank of St. Louis.
# chartSeries to display prices in an attractive chart.

# stockVis also relies on an R script named helpers.R, which contains a function that adjusts stock prices for inflation.

# Reactive Expressions
#*********************

# You can limit what gets re-run during a reaction with reactive expressions.
# A reactive expression is an R expression that uses widget input and returns a value. The reactive expression will update this value whenever the original widget changes.
# To create a reactive expression use the reactive function, which takes an R expression surrounded by braces (just like the render* functions).

# Reactive expressions are a bit smarter than regular R functions. They cache their values and know when their values have become outdated. 
# What does this mean? The first time that you run a reactive expression, the expression will save its result in your computer’s memory. 
# The next time you call the reactive expression, it can return this saved result without doing any computation (which will make your app faster).

# Shiny keeps track of which reactive expressions an output object depends on, as well as which widget inputs. Shiny will automatically re-build an object if
 
# - an input value in the objects’s render* function changes, or
# - a reactive expression in the objects’s render* function becomes obsolete

# Think of reactive expressions as links in a chain that connect input values to output objects. 
# The objects in output will respond to changes made anywhere downstream in the chain. 
# (You can fashion a long chain because reactive expressions can call other reactive expressions.)

# Only call a reactive expression from within a reactive or a render*function.


#****************************
### In-Class Exercise 6 -----
#****************************

# Do the exercise at the end of lesson 6: https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/

# Extra: look at google trend index app:
runApp("Example Apps/google-trend-index", display.mode = "showcase")


# (4) Observers, Update Statements and Full Reactivity ---------------------
#***************************************************************************

# From: http://zevross.com/blog/2016/04/19/r-powered-web-applications-with-shiny-a-tutorial-and-cheat-sheet-with-40-example-apps/

# The functions that can handle the reactive values are observe({}), reactive({}) and the suite of render* functions.

# The values associated with your UI inputs (like the text box, input$mytext) are called reactive values. 
# It is tempting to include input$mytext directly in the server. Try running the code below in your own console 
# and you will get an error operation not allowed without an active reactive context. 
# This means that to read the reactive value you need to wrap it in a function designed to listen to the reactive elements.

# Observers will get triggered in response to reactive values. 
# They were designed to listen to reactive elements and respond by causing side effects, 
# like updates to text boxes or pull-downs. Unlike the reactive function, which we cover next, 
# they should not be used to return data or values.

# The suite of update* functions are designed to allow you to update existing Shiny widgets – 
# for example, update a text box or the list of items in a pulldown menu.

runApp("Example Apps/updateapp1", display.mode = "showcase")

# There are two flavors of observe. With observe the code inside will get triggered when any of the reactive values inside change. 
# With observeEvent code will only be triggered by specified reactive values. 
# I would suggest that you use observeEvent whenever possible because observeEvent forces you 
# to think through and specify the reactions you want to see.

# Using observeEvent instead of observe allows you to specify the reactive values to listen for and react to. In this case, 
# we’re only listening for one reactive (input$mytext) but you can include more than one.

runApp("Example Apps/updateapp2", display.mode = "showcase")

# observeEvent is a wrapper around observer() and isolate() see ?isolate on how to natively do this. 
# Basically, reactive values wrapped in isolate({}) inside an observe({}) statement cannot trigger the observe({})
# statement to be re-executed when they change. 


# Observer priority: which observers run first
# With bigger apps, you may have situations where you want one observer to run before others. 
# By default (it seems), observers are run in the order they appear in the document. 
# You can use an observe function priority argument to do this. The default priority is 0 and higher numbers mean higher priority (and you can use negative numbers).
#

runApp("Example Apps/updateapp3", display.mode = "showcase")

# Now lets revisit reavtive({}):

# use a reactive function to isolate code and generate output with no side effects
# A reactive function is used in the same way you would use an R function except that it gets 
# triggered by a reactive element. Because using reactive creates a function and returns results you 
# generally save a reactive as an object and use it elsewhere in your server as you would use any R function. 
# There is one major distinction from a function, however, the function can only be executed within a 
# “reactive context” (so in another reactive, an observe or a render* function). 
# The reactives are NOT supposed to generate side effects, they should essentially be self-contained.


runApp("Example Apps/updatereactiveapp1", display.mode = "showcase")

# Sometimes you only want your reactive function to listen for specific reactive values and this is when 
# you use eventReactive or observeEvent. In this app we have a reactive function that responds to both 
# reactive values and an eventReactive that only reacts to changes in input$mytext.


runApp("Example Apps/updatereactiveapp2", display.mode = "showcase")


# In summary: 
# a reactive value in your observe or reactive functions will trigger that function to run if the user 
# interacts with it even if the reactive element is not part of the calculations.

# If you want to prevent that:
# - use eventReactive() or observeEvent() to only execute them when certain values change
# - modularize your code into more observe() and reactive() expressions. 
# - use isolate() inside observer() or reactive() to prevent reactive values to trigger re-execution of the 
#   entire interior code.

# Two examples: 
runApp("Example Apps/updatereactive-comparison/together", display.mode = "showcase")
runApp("Example Apps/updatereactive-comparison/separated", display.mode = "showcase")

# In practive: using observer() and reactive() also has a small computational cost,
# so heavy modularization as shown here only makes sense if the computations are potentially expensive. 

runApp("Example Apps/updatereactiveoutput", display.mode = "showcase")


# Advanced topics: 

renderUI() # Generate / Export UI elements from server -> can do more than simply update widgets
# Some examples here: http://zevross.com/blog/2016/04/19/r-powered-web-applications-with-shiny-a-tutorial-and-cheat-sheet-with-40-example-apps/

reactiveValues() # Create reactive values like input$value inside server code (without input widget)
debounce()       # Slow down reactive expressions  



  





# (5) R packages Extending Shiny -------------------------------------------
#***************************************************************************

library(shinythemes)
# See https://rstudio.github.io/shinythemes/
# Bootstrap offers a number of pre-created themes that allow for a complete change of style with limited coding. 
# The shinythemes package from RStudio allows you to take advantage of this capability. 
# The only change you need to make to your app is to add a line of code theme=shinytheme("cosmo") to your 
# fluidPage or fixedPage function and the entire look of you app will change 
# (you cannot apply themes to the basicPage because basicPage is not based on Bootstrap).

library(shinyjs)
# https://deanattali.com/shinyjs/
# The shinyjs package, created by Dean Attali, allows you to use common JavaScript operations in your Shiny applications such as hiding an element, delaying code etc. The package provides more than a dozen useful functions that are described in a page on GitHub. His talk on the package at the 2016 Shiny Developers Conference is also worth watching and will be posted by RStudio in the near future.
# In order to use the functionality you need to load the package and then activate it in the UI with the useShinyjs function.

library(shinyBS)
# https://ebailey78.github.io/shinyBS/
# The shinyBS package, created by Eric Bailey, offers some nice additional Bootstrap-related components like modals, 
# collapses and related. Unfortunately, it’s not clear that it’s being maintained (last commit in April 2015) and many 
# of the components do not work with the most recent versions of Shiny. 

library(DT) # https://rstudio.github.io/DT/
# Powerful interfact to JavaScript DataTables library (https://datatables.net/), 
# for enhanced interactive tables, as you can see in the macro data portal and other applications, 
# just pass a data.frame to DT::renderDT. Even allows for data entry into the tables. 

# Interactive plots and maps using HTML widgets: http://www.htmlwidgets.org/
# HTML widgets allow you to use JavaScript visualization libraries to create interactive graphics in R, 
# including Shiny web applications. For details on HTML widgets generally you can visit the htmlwidgets site.
# Recommended: 
library(plotly)      # Interactive plots: https://plotly.com/r/ (See Course 5 where we created some nice charts)
library(highcharter) # Interactive plots: https://jkunst.com/highcharter/
library(apexcharter) # Interactive plots: https://dreamrs.github.io/apexcharter/articles/apexcharter.html
library(rChars)      # Interactive plots: https://ramnathv.github.io/rCharts/ (remotes::install_github('rCharts', 'ramnathv'), lets you interface various JavaScript libraries, somewhat advanced)
library(leaflet)     # Interactive maps:  https://rstudio.github.io/leaflet/


library(shinyWidgets) 
# http://shinyapps.dreamrs.fr/shinyWidgets/
# https://dreamrs.github.io/shinyWidgets/index.html
# More (and more fancy) widgets for shiny apps. 

library(shinydashboard)
# https://rstudio.github.io/shinydashboard/
# In order to create a dashboard you need a header, sidebar and body. 
# Within the body you create a series tabItems and fill them with the controls you want to see included. 
# For more detail you should refer directly to the shiny dashboard page on GitHub.


 
# (6) Custom HTML and CSS --------------------------------------------------
#***************************************************************************

# Use HTML tags
?tags
names(tags)
HTML("<b> bold text </b>") # Use HTML to write raw HTML

# See Rstudio article about HTML in Shiny apps: 
# https://shiny.rstudio.com/articles/html-tags.html

# See Rstudio article about CSS in Shiny apps: 
# https://shiny.rstudio.com/articles/css.html

# Also see various examples here:
# http://zevross.com/blog/2016/04/19/r-powered-web-applications-with-shiny-a-tutorial-and-cheat-sheet-with-40-example-apps/

# Here is your full list of options.

# - Include your raw CSS directly inline in the head of your app
# - Include CSS within specific HTML tags
# - With a two page app (ui.R and server.R) you can add a folder called www and the app will automatically read any CSS files you’ve included there.
# - The shinyjs package (discussed below) also has a useful function called inlineCSS that you can use to add CSS
# - Use the includeCSS function to read an external CSS file

# Best practice for larger apps would be to include all your CSS in a single, 
# external file so that it’s easy to find and change settings. 
# For smaller apps including CSS in the head or HTML tags would be acceptable but be careful, 
# if you include styling in the HTML tags it can be difficult to prioritize and organize your styles.


# Advanced topics:

# HTML templates: https://shiny.rstudio.com/articles/templates.html
# Let you write your entire UI directly in HTML

# Some developers feel more comfortable writing HTML code directly rather than writing in R 
# and having Shiny compile code to HTML. To meet this need, RStudio introduced the concept of HTML 
# templates that allow you to write the HTML and include placeholders for Shiny input using curly braces. 
# If you’re comfortable with web development, the concept will be familiar (e.g. mustache, handlebars).


runExample("08_html")       # Shiny app built from HTML



# (7) Case-Study: Shiny Data Portal ----------------------------------------
#***************************************************************************

runApp("Full Apps/shiny-data-portal")

# NOTE: This is an open-source version, the code together with detailed setup instructions has 
# also been made available online at: https://github.com/SebKrantz/shiny-data-portal

# An advanced version of this (that loads data from a database) has been deployed at 
# https://mepd.finance.go.ug/apps/macro-data-portal
# to provide you and the general public with timely and tidy macroeconomic data for Uganda. 

# To get data from an existing database you can use DBI::dbConnect and DBI::dbGetquery
# More info at https://cran.r-project.org/web/packages/DBI/vignettes/DBI.html 
# and https://shiny.rstudio.com/articles/#data or https://db.rstudio.com/getting-started/


#****************************
### In-Class Exercise 7 -----
#****************************

# In the published version of the portal at mepd.finance.go.ug/apps/macro-data-portal,
# there are a number of additional features. One of them, is that for "Excel" downloads
# there is an additional checkbox "Transpose / Row-Based Format" which enables the user 
# to get the data in a row-based excel file. Implement this in the data portal here, 
# using a conditionalPanel() in the user interface so the option only becomes available if
# "Exel" is chosen as download format. write only the numeric variables using 
# collapse::num_vars and transpose the data using data.table::transpose. Use the first column as
# variable names. 

# e.g.:
setNames(data.table::transpose(collapse::num_vars(iris)), as.character(iris[[1]]))

# Bonus: Try to write a more sophisticated implementation that prints numeric and categorical 
# variables (collapse::cat_vars) to different sheets. 


# (8) Case-Study: Census Mapping App ----------------------------------------
#***************************************************************************

runApp("Full Apps/census-mapper")

# NOTE: This is the full version of the app, as it is deployed at mepd.finance.go.ug/apps/census-map
# This code has also been made available open source at: https://github.com/SebKrantz/Census-Mapper

# There is some processing involved from getting the raw shapefile from UBOS, generating consistent 
# column names, aggregating it to different administrative levels and simplifying the geometries so they
# can be visualized efficiently with leaflet. I have just provided you the final datasets here. 
# I can provide details about these computations during private consultations or a course on geospatial computing 
# (If there is significant interest in that, it was planned at the end of the data science programme)


#****************************
### In-Class Exercise 8 -----
#****************************

# Please add a "Download" tab to the Census-Map control panel, 
# where a user is able to download the shapefile at any available aggregation level. 
# you can use sf::st_write(data, "DATA.shp") to write shapefiles. 



# (9)  Publishing Shiny Applications --------------------------------------
#***************************************************************************

# https://shiny.rstudio.com/tutorial/written-tutorial/lesson7/
# https://shiny.rstudio.com/deploy/




# Extras: -------------------------------------------------------------------


#****************************
### In-Class Exercise A1 -----
#****************************

# Comprehend these example Apps built into shiny: 

runExample("02_text")       # tables and data frames
runExample("03_reactivity") # a reactive expression
runExample("04_mpg")        # global variables
runExample("05_sliders")    # slider bars
runExample("06_tabsets")    # tabbed panels
runExample("07_widgets")    # help text and submit buttons
runExample("08_html")       # Shiny app built from HTML
runExample("09_upload")     # file upload wizard
runExample("10_download")   # file download wizard
runExample("11_timer")      # an automated timer


# Additional Links:

# DreamRs shiny gallery: http://shinyapps.dreamrs.fr/
# Dean Attali: https://deanattali.com/blog/advanced-shiny-tips/
# How to get help: https://shiny.rstudio.com/articles/help.html
