readxl_web <- function(x, ...) {
  temp <- tempfile()
  download.file(x, temp, mode = "wb")
  on.exit(file.remove(temp))
  readxl::read_excel(temp, ...)
}

BOU_I <- readxl_web("https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/InterestRates/Interest_rates.xlsx", sheet = 2)

# Problem 2: Accessing data from an API package ----------------------------------------
library(wbstats)
library(tradestatistics)
# Download the following API packages and get some interesting development and trade related data using them.

# Using the R data viewer, find out which was the most exported and imported product of Uganda in 2018

# Problem 3: Analyzing Macroeconomic Data from BoU -------------------------------------

# Using the readxl_web function above, import the Monthly macroeconomic indicators dataset from the Bank of Uganda
# Tipp: You want to first download the file manually, then 

# Create a plot...

# Problem 4: Learning R Using Swirl ----------------------------------------------------
