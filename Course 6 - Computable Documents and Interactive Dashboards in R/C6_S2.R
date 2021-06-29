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

COVID19 <- covid19()
View(COVID19)
