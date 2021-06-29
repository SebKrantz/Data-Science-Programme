#############################################################
# *************** R BASED DATA SCIENCE TRAINING *************
# Course 6: Computable Documents and Interactive Dashboards 
# -----------------------------------------------------------
# Prepared by: Sebastian Krantz, ODI Fellow, MEPD, MoFPED
# Permanent Contact: sebastian.krantz@graduateinstitute.ch
#############################################################

setwd("Course 6 - Computable Documents and Interactive Dashboards in R")

# (7) Interactive Dashboards with 'flexdashboard' --------------------------
#***************************************************************************

# Dashboards are a useful way to communicate large amounts of information visually and quickly. 
# Create one with the flexdashboard::flex_dashboard output format

install.packages('flexdashboard')

# Links: 
# https://pkgs.rstudio.com/flexdashboard/
# https://bookdown.org/yihui/rmarkdown/dashboards.html


# (8) Case study: Make your own COVID-19 Dashboard -------------------------
#***************************************************************************

# Task: Make a COVID dahboard for Uganda, East Africa or the whole world, including vaccination progress. 
# Use interactive visualizations provided by https://www.htmlwidgets.org/ (in particular plotly)

# COVID Dashboard Example (using plotly for visualization): 
# https://github.com/RamiKrispin/coronavirus_dashboard

# Worldwide Data (Including Vaccination Progress)
install.packages("COVID19")
library(COVID19)
COVID19 <- covid19() # get data for whole world, or for some countries e.g. country == "UGA"
View(COVID19)


### Publishing your Dashboard ----------------------------------------------

# Publishing at shinyapps.io (need to make free account and add 'runtime: shiny' to dashboard header), 
# then you can publish from Rstudio:
# https://www.shinyapps.io/
# https://community.rstudio.com/t/how-to-deploy-a-shiny-rmd-file-to-shinyapps-io/30605
# https://community.rstudio.com/t/flexdashboard-publishing/40476

# Alternatively (for static dashboards which do not need to be updated), you can render the dashboard
# to a simple website using rmarkdown::render, and then publish using github pages: 
# https://pages.github.com/
# https://www.r-bloggers.com/2020/09/deploying-flexdashboard-on-github-pages/


# My COVID Dashboard published at shinyapps.io: 
# https://mepd.shinyapps.io/COVID-19/
