##########################################################
# ************ R BASED DATA SCIENCE TRAINING *************
# Course 1: Basic Data Manipulation and Visualization in R
# --------------------------------------------------------
# Prepared by: Sebastian Krantz, ODI Fellow, MEPD, MoFPED
# Permanent Contact: sebastian.krantz@graduateinstitute.ch
##########################################################

# Session 2

# Today:
# (1) Recap Apply Functions + Extensions
# (2) Packages
# (3) Importing Data Into R + Some Analysis
# (4) Basic Graphics (More Formally)
# (5) Basic Statistical Analysis
# (6) Control Flow
# Extra: Effectively working with R and Rstudio


# (1) Apply Functions: Recap and extensions -----------------------------

# (a) apply: Applies a function over the margins (rows / columns) of a matrix / array
m <- as.matrix(mtcars) # mtcars is a dataset about cars supplied with R. Here we create a matrix from it
m
str(m)
dim(m)  # This shows you the dimensions of the matrix
apply(m, 1L, median) # Apply the median to each row (first dimension)
apply(m, 2L, median) # Apply the median to each column (second dimension)

# Note that for common functions we have efficient alternatives
rowSums(m) # same as apply(m, 1L, sum), but more efficient
rowMeans(m)
colSums(m)
colMeans(m)
# more functions like this (colMedians etc. are available in the matrixStats package)

str(iris) # Remember this dataset 
View(iris)
str(iris3) # iris3 is the same dataset in array format
apply(iris3, 2:3, median)

# A good thing about apply functions is that we can apply any function with them
apply(m, 2L, function(x) c(mean = mean(x), median = mean(x), sd = sd(x), quantile(x)))

# (b) lapply: applies a function to every element in a list
str(mtcars) # recall that a data.frame is stored as a list of columns
is.list(mtcars)

lapply(mtcars, sum) # applying the sum to each columns
mtcars[] <- lapply(mtcars, log) # Replacing each column in mtcars with it natural log
mtcars
rm(mtcars)

str(airquality) # Another dataset, with some missing values
lapply(airquality, sum, na.rm = TRUE) # we can pass further arguments to sum like this using the ellipsis ... 
?lapply
?sum
?"..."

# (c) sapply: Similar to lapply: 
sapply # look at it

sapply(mtcars, sum)      # vector
sapply(mtcars, quantile) # matrix
# Sometimes it's hard to say from the print if something is a matrix or data.frame, but str() or is.data.frame() / is.matrix() can tell you
str(m) 
str(mtcars)

# (d) vapply: Faster version of sapply, but you need to specify the output format 
vapply(mtcars, sum, numeric(1L)) # telling vapply that the function (sum) returns a numeric vector of length 1

# (e) rapply: Recursive version of sapply / lapply for lested lists
rapply(list(airquality, mtcars), sum)
str(rapply(list(airquality, mtcars), sum, how = "list"))

# (f) mapply: Apply a function to multiple arguments
mapply(rep, 1:4, 4:1) # multiple arguments passed to a function simultaneously.
mapply(paste, c("a","b","c","d"), c("a","b","c","d"))
mapply(paste, c("a","b","c","d"), c("a","b","c","d"), SIMPLIFY = FALSE)
mapply(paste, c("a","b","c","d"), c("a","b","c","d"), SIMPLIFY = FALSE, USE.NAMES = FALSE)

# What does this do?
mapply(`+`, mtcars, 1:11)
mapply(`+`, mtcars, 1:11, SIMPLIFY = FALSE)

# (g) tapply: Split an object, and apply functions to each group
str(iris)
tapply(iris$Sepal.Length, iris$Species, sum)

# (h) For data frames: aggregate
aggregate(iris[1:4], iris["Species"], sum)
# There is also a formula interface, but this is more arcane. 
aggregate(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species, iris, sum)

# 3 online references for apply:
# https://nicercode.github.io/guides/repeating-things/
# https://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/
# https://www.datacamp.com/community/tutorials/r-tutorial-apply-family#gs.ShAYp7Q
# But as always: learning by doing is best!!

#****************************
### In-Class Exercise 1 -----
#****************************

# (a) Consider the following two objects. Compute the sum along the columns of these objects in the most efficient way so that the result is a vector
state.x77
longley

# (b) Show an alternative way of summing along the columns of these objects, again resulting in a vector. 

# (c) Consider the Titanic data. Compute the percent that survived by Sex and Age
Titanic

# (d) Consider these two datasets. Aggregate 'infert' by education and induced, and 'airquality' by Month
infert
airquality

# (e) Using the iris dataset, compute the quantiles of Sepal.Width by Species and simplify the result to a matrix. 
iris


# (2) Packages ----------------------------------------------------------
# Packages are Extensions to the basic R features introduced so far.
# They are written by individuals and companies around the World. 
# Many packages can be downloaded and installed easily from the 
# Comprehensive R Archive Network CRAN using the install.packages() function. 
# More packages can be found on Github or Bioconductor. 

# To view available packages: Look at packages plane in Rstudio
# alternatively call
library()
.packages(all.available = TRUE) # This is simpler: returns a character vector of installed packages
# A more detailed View
View(installed.packages()) 

# To call a function, R first has to find it. 
# R does this by first looking in the global environment. (the objects and functions you have created in the current session)
# If R doesn’t find it there, it looks in the search path, the list of all the packages you have attached. 
# You can see this list by running search().
search()

# To use the data and function supplied with a package that is not attached by default, we need to attach it
ldahist(iris[1:4], iris$Species) # ldahist is a function in MASS package, if the package is no loaded, evaluating this gives an error

library(MASS) # This loads the MASS package and attaches it to the namespace
search()      # It is there, in second position, right after the global environment
ldahist(iris[1:4], iris$Species) # Now this works..

# To unload a package:
detach("package:MASS", unload = TRUE)
search() # It is gone again
ldahist(iris[1:4], iris$Species) # Fails again. 

# Usually loading the packages we work with at the beginning of an R script is a good idea.
# However if we only require a single function from a package, or if the functionality of the
# package significantly interferes with the ones we have loaded (rare), we can call a function
# directly without attaching it using the `::` function:

MASS::ldahist(iris[1:4], iris$Species) # This works
?"::" # Check out the documentation for this

# We can check if a certain package is available using
isTRUE(requireNamespace("MASS"))

# Getting basic info about a package: Look at the description file in the documentation
packageDescription("stats") # This programmatically prints the description
# These functions extract information from the description file
packageDate("stats")
packageVersion("stats")


# Installing a New Package:
install.packages("data.table") # One package
install.packages(c("xts", "collapse")) # Multiple packages. 

# See all packages available on CRAN...
View(available.packages()) 
View(tools::CRAN_package_db()) # This downloads a more extensive package database

# More functions like this: 
installed.packages() # We had this before: Lists all installed packages
old.packages()       # old.packages indicates packages which have a (suitable) later version on the repositories whereas 
update.packages()    # update.packages offers to download and install such packages.
new.packages()       # new.packages looks for (suitable) packages on the repositories that are not already installed, and optionally offers them for installation.

download.packages()  # Download a set of packages from CRAN or the web but do not install them (rare, better use install.packages() or update.packages())
remove.packages()    # Delete packages (the opposite of install.packages())

# This function can show you how a package relates to other packages:
tools::package_dependencies()

# Searching available packages on CRAN and beyond, with the help of RWsearch
install.packages("RWsearch")
library(RWsearch)
crandb_down()                                     # Download CRAN Database
pck <- s_crandb(complex, survey, mode = "and")    # Search CRAN Database for packages relating to complex survey analysis
pck
p_table5(pck)                                     # Get a table with the information of the packages
p_display5(pck)                                   # View the table in a browser
# See more at: 
browseVignettes("RWsearch")

# See also these websites providing online documentation for R: 
# https://www.rdocumentation.org/
# https://rdrr.io/


#****************************
### In-Class Exercise 2 -----
#****************************

# (a) Go to the CRAN Task Views and look for interesting packages in a task view

# (b) Using RWsearch, search for packages in the field you find most interesting


# (2.1) Recap on Finding Help + Extensions for Packages --------------------------------------------------
?plot # Recall we get the help for a function like this
help(plot) # or like this
# Execute examples provided on how to use the plot function: At the bottom of this page. 
# This executes them:
example("plot")

# Look up documentation for a package
help(package = "data.table")
help(package = "collapse")

# If we don't know the exact name of the function or package we are looking for, can use help.search
help.search("classification")
??"classification"  # Same thing but more compact
??plot

# Also try the 'Search Engine & Keywords' in the help panel of RStudio. Check demos, examples, vignettes
help.start() # Start html help, or click on the 'help' panel (The house)

## Demos and Vignettes:

# Vignettes are supplementary documents that explain the functionality of a package. 
# They are more useful than the general package documents available on CRAN which just describe whats included in the package.
# All Vignettes available:
vignette()
vignette(package = "data.table") # all vignettes for a package
vignette("datatable-intro", package = "data.table")  # View a specific one
vignette(package = "stats") # No vignettes for this one

# View them in a Web-Browser
browseVignettes()
browseVignettes(package = "data.table")
browseVignettes(package = "stats")

# Demos are R programs that you can click through. they will demonstrate something.
# all Available demos:
demo(package = .packages(all.available = TRUE))
# demos of packages currently loaded:
demo()
# This is a nice one
colors() # This prints the names of all installed colors
demo("colors") # This demonstrates them in a series of plots:


#****************************
### In-Class Exercise 3 -----
#****************************

# (a) Find the introduction to 'data.table' vignette and the introduction to 'xts' vignette.

# (b) Install the packages 'magrittr', 'matrixStats' and 'ggplot2' and find the vignettes

# (c) Find the website for 'collapse', access the introductory vignette 
#     and look for the section on advanced data aggregation

# (d) Find the cheatsheets for 'collapse', 'data.table' and 'ggplot2' on the Rstudio website.

# (e) Execute all examples given for the 'lm' function


# (3) Data Import ----------------------------------------------------------

# (a) Pasting data into R from Excel or CSV (handy but not recommended for issues of reproducibility)
# When you copy something on windows, it is saved to a place in memory called the clipboard. 

# The clipboard can be read like this: From CSV file (Amount of Water Supplied and billed: Open, select with ctrl + A, copy with ctrl + C, then execute this line)
data <- read.csv("clipboard")
str(data)
View(data)
summary(data)
# Removing the comma in the water supply data and coercing to character
data[2:4] <- lapply(data[2:4], function(x) as.numeric(gsub(",", "", x)))

# From Excel: (Pupil Teacher Ratio by District: Open, select data cells including first row, copy with ctrl + C, then execute this line)
data <- read.delim("clipboard")
data <- read.delim("clipboard", check.names = FALSE) # Without fixing the names
str(data)
View(data)
summary(data)
# Coercing all numeric columns to numeric
data[-1L] <- lapply(data[-1L], as.numeric)

# (b) For reproducible research we want to import Excel and CSV by reading the files directly from disc
# There are several options here: 

# I. Setting the working directory to the location of the file and then read the file (note that the dashes are / not \ as in windows)
setwd("C:/Users/Sebastian Krantz/Documents/R/Data-Science-Programme/Course 1 - Basic Data Manipulation and Visualization in R/data")
data <- read.csv("Amount_of_Water_Supplied_and_Billing_efficiency_by_NWSC.csv")

# II. Setting the working directory to the project location and specifying a partial path (preferred because we also need to save outputs in the project directory)
setwd("C:/Users/Sebastian Krantz/Documents/R/Data-Science-Programme/Course 1 - Basic Data Manipulation and Visualization in R")
data <- read.csv("data/Amount_of_Water_Supplied_and_Billing_efficiency_by_NWSC.csv")

# III. Reading directly using the full file path (preferred if we are using files from different locations on the computer)
data <- read.csv("C:/Users/Sebastian Krantz/Documents/R/Data-Science-Programme/Course 1 - Basic Data Manipulation and Visualization in R/data/Amount_of_Water_Supplied_and_Billing_efficiency_by_NWSC.csv")

# In general, for smaller files we prefer to have them in the project directory in a "data" folder. 
# Larger files or files read from the internet we want to specify the full path / URL.

# Now read.delim() was made for tab-separated (tsv), or other separated data,
# It can read excel cells copied from the clipboard but not a full excel notebook.
# To read excel we need to install a package: install.packages("readxl")
library(readxl) 
?`readxl-package`
getwd() # we set the working directory to the project folder as in option II. above
excel_format("data/Pupil_Teacher_Ratio_by_District.xlsx") # can show if xlsx or xls (old excel)
excel_sheets("data/Pupil_Teacher_Ratio_by_District.xlsx") # can show the excel sheets
?read_excel
data <- read_excel("data/Pupil_Teacher_Ratio_by_District.xlsx", .name_repair = "none")
View(data) # This includes the source information (see last row). 
# can use the range argument to specify the rectangle directly
data <- read_excel("data/Pupil_Teacher_Ratio_by_District.xlsx", 
                   range = "A1:G123", .name_repair = "none")
# Alternatively we can use some helper functions:
?cell_rows
# This excludes the source information and national estimates
data <- read_excel("data/Pupil_Teacher_Ratio_by_District.xlsx", 
                   range = cell_rows(1:123),  .name_repair = "none")
View(data)

## Now lets do some analysis of this data by writing a reproducible piece of code that generates an output:

# Step1: Attach required packages and set working directory
library(readxl)
setwd("C:/Users/Sebastian Krantz/Documents/R/Data-Science-Programme/Course 1 - Basic Data Manipulation and Visualization in R")
# Step2: read data and process if needed
data <- read_excel("data/Pupil_Teacher_Ratio_by_District.xlsx", 
                   range = cell_rows(1:123),  .name_repair = "none")
str(data)   # Note the some columns are still character format
class(data) # This is a tibble, a kind of variant of a data frame which we we will learn about in Course 2.
# Coercing all numeric columns to numeric
data[-1L] <- lapply(data[-1L], as.numeric)
str(data)
summary(data)

# Creating a matrix from this data to better analyze it
class(data) <- "data.frame"   # first, we make this a normal data.frame again (tibble's do not support row.names)
row.names(data) <- data[[1L]] # Using the first column as row names
ptr <- as.matrix(data[-1L]) # matrix of pupil teacher ratios
str(ptr)
dimnames(ptr)
summary(ptr)

# Now Some basic analysis

# Calculating average pupil-teacher ratio by district
ptr_district_means <- rowMeans(ptr, na.rm = TRUE)
sort(ptr_district_means)
summary(ptr_district_means)

# Compute a histogram
hist(ptr_district_means, 
     breaks = 30, xlab = "Pupils per Teacher") 


# Saving sorted version and compute a dotchart
ptr_district_means <- sort(ptr_district_means) 
dotchart(ptr_district_means) 

# Nicer version
dotchart(ptr_district_means, 
         cex = 0.6, # cex = character expansion factor (the size of text relative to the default size, here making everything smaller)
         pch = 19,  # The point type, here using a round dot. (see cheatsheet for base graphics)
         ylab = "District", 
         main = "Average Pupils Per Teacher, 2012-2017")

# lattice is another charting library included in the basic R set of packages. I think this chart is nicer:
lattice::dotplot(ptr_district_means, 
                 space = "left", scales = list(tick.number = 10, alternating = 3),
                 lab = ptr_district_means,
                 ylab = "District", xlab = "Pupils Per Teacher",
                 main = "Average Pupils Per Teacher, 2012-2017")

# For a perfect result we need to get into the nitty gritty of graphics libraries (not for this course)
library(lattice) # loading here because we access more functions from it
o <- order(rowMeans(ptr, na.rm = TRUE)) # get the order of the means
dotplot(ptr_district_means, xlim = c(min(ptr, na.rm = TRUE) - 5, max(ptr, na.rm = TRUE) + 5),
        space = "left", scales = list(x = list(tick.number = 10, alternating = 3), y = list(cex = 0.5, alpha = 0.7)),
        lab = ptr_district_means, 
        ylab = "District", xlab = "Pupils Per Teacher",
        main = "Average Pupils Per Teacher, 2012-2017",
        panel = function(x, y, ...) {
             panel.dotplot(x, y, ...)
             panel.arrows(x0 = apply(ptr, 1L, min, na.rm = TRUE)[o], # Here we use the ordering to calculate min and max
                          x1 = apply(ptr, 1L, max, na.rm = TRUE)[o], 
                          y0 = as.numeric(y), y1 = as.numeric(y), 
                          length = 0.02, angle = 90, code = 3, offset )
             panel.xyplot(x, y, ..., pch = 19, cex = 1.3)
             panel.text(x, y, format(round(ptr_district_means, 1)), 
                        cex = 0.3, col = "white", fontface = 2) # adj = c(-0.5, 0.5)
         })

# let's now save this plot. 
# I created a 'figures' folder in the working directory beforehand.
# This is the way to save the plot to pdf. We copy the current graphics device to a pdf file and close / save that file
dev.copy(pdf, "figures/ptr_district_dotchart.pdf", width = 7, height = 16) # Width and height in inches
dev.off() # This closes the graphics device, which is to say it saves the pdf file. 

# Note: an even more fancy version can be created using ggplot2, which we will learn about soon. 
# See more at: https://uc-r.github.io/cleveland-dot-plots

# Not let's look at the average pupil teacher ratio over time
ptr_year_means <- colMeans(ptr, na.rm = TRUE)
ptr_year_means
barplot(ptr_year_means)

# With a bit of effort we can again make a publication quality plot.
# If we just want to save the plot without first viewing it, we can save it like this:
pdf("figures/ptr_year_barplot.pdf", width = 5, height = 5.8)
bp <- barplot(ptr_year_means, col = "orange", border = "orange",
              main = "Uganda Pupils Per Teacher, 2012-2017", 
              sub = "Source: Uganda Bureau of Statistics",
              ylab = "Pupils Per Teacher", xlab = "Year", 
              ylim = c(0, 55),  font.sub = 3, cex.sub = 0.8)
text(bp, ptr_year_means + 2, labels = round(ptr_year_means, 2))
dev.off()

# Looking at the data in more detail..
# Correlations between districts for each year
cor(ptr, use = "pairwise.complete.obs")
# Heatmap of correlations
heatmap(cor(ptr, use = "pairwise.complete.obs"), 
        Rowv = NA,  Colv = NA, scale = "none")

# A more fancy version is given by pheatmap package
pheatmap::pheatmap(cor(ptr, use = "pairwise.complete.obs"), cluster_rows = FALSE, cluster_cols = FALSE, 
                   scale = "none", border_color = NA, display_numbers = TRUE, number_color = "white",
                   color = viridis::plasma(10), cex = 1.2, fontsize_number = 12)

# This is also nice
ggcorrplot::ggcorrplot(cor(ptr, use = "pairwise.complete.obs"),  
                       outline.color = NA, lab = TRUE) +
  ggplot2::theme(panel.grid = ggplot2::element_blank())

# Heatmap of the entire data
heatmap(ptr, Rowv = NA,  Colv = NA, scale = "none")
# with hierarchical clustering of rows:
heatmap(ptr, Colv = NA, scale = "none") # https://www.r-graph-gallery.com/215-the-heatmap-function.html

# Again pheatmap version
pheatmap::pheatmap(ptr, cluster_rows = FALSE, cluster_cols = FALSE, 
                   fontsize_number = 6, fontsize = 6, scale = "none", border_color = NA,
                   color = viridis::viridis(10))

# Now there is a package called superheat which allows us to combine charts
ptr_district_means <- rowMeans(ptr, na.rm = TRUE) # Need unsorted version again
superheat::superheat(ptr, yr = ptr_district_means, yt = ptr_year_means,
                     yt.axis.name = "Annual Averages", yr.axis.name = "District Averages",
                     yt.plot.type = "scatterline", yr.plot.type = "bar", # yr.bar.col = "white",
                     yt.num.ticks = 5, yr.num.ticks = 5, yt.plot.size = 0.2, yr.plot.size = 0.3,
                     padding = 0.2, grid.hline = FALSE, grid.vline = FALSE,
                     force.left.label = TRUE, left.label.text.size = 2, left.label.size = 0.21,
                     left.label.text.alignment = "right",
                     order.rows = order(ptr_district_means),
                     column.title = "Year", row.title = "District",
                     title = "Uganda Pupils Per Teacher, 2012-2017")  +
  ggplot2::theme(plot.title = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 0, b = 1, l = 0)))

dev.copy(pdf, "figures/superheat.pdf", height = 18, width = 10)
dev.off()

# That's it, so here we have written a little analysis program that imports a spreadsheet that was downloaded from UBOS,
# generates some nice charts and saves them. 


# More data import and export functionality is provided by some packages:

# Delimited Data:
library(readr)
# CSV
read_csv()
write_csv()
# Tab
read_tsv()
write_tsv()
# Other Delimited Data:
read_delim()
write_delim()

# Excel:
library(readxl)
read_xlsx()
library(writexl)
write_xlsx()

# Recommended for STATA, SPSS, SAS:
library(haven)
# STATA
read_dta()
write_dta( , version = 12)
# SPSS
read_sav()
write_sav()
# SAS
read_sas()
write_sas()

library(foreign) # This also contains functions to read and write STATA, SPSS or SAS, and other formats.

# Other useful functions:
utils::download.file()  # Download files from the web
utils::choose.dir()     # Set the working directory interactively (only on Windows)
utils::choose.files()   # Choose a list of files interactively (only on Windows)


#****************************
### In-Class Exercise 4 -----
#****************************

# Using the haven library, import the STATA File "sachs 2003 institutions don't rule.dta".

# (a) Summarise the data

# (b) Delete the columns 'AJR' and 'ME' from the data

# (c) Save the file as a STATA 10 file in the 'data' folder. 

# (d) Save the file as an Excel file in the 'data' folder.

# (e) Save the file as a CSV file in the 'data' folder. 


# (4) Basic Graphics more Formally -------------------------------------------------------

# http://www.sthda.com/english/wiki/r-base-graphs
# See Cheatsheet

library(MASS) # Several datasets used in this section 

# In data visualization we distinguish broadly between Exploratory and Explanatory data visualizations. 

# calling plot() on a data.frame gives a scatterplot matrix (same as calling pairs())
plot(whiteside)

# Plot Gas vs. Temp
plot(x = whiteside$Gas, 
     y = whiteside$Temp, 
     xlab = "Heating gas consumption",
     ylab = "Outside temperature")

# One of the key features of the plot() function is that it is generic, 
# meaning that the results of applying the function depend on the nature of the object to which it is applied.

# Apply the plot() function to Insul factor
plot(whiteside$Insul)


# Scatter Plots with two sets of points
str(Cars93)
# Plot Max.Price vs. Price as red triangles
plot(Cars93$Price,
     Cars93$Max.Price, pch = 17, col = "red")
# Add Min.Price vs. Price as blue circles
points(Cars93$Price, 
       Cars93$Min.Price, pch = 16, col = "blue")
# Add an equality reference line with abline()
abline(a = 0, b = 1, lty = 2)


# Plot Arrays
data(Animals2, package = "robustbase") # Need to install.packages("robustbase") to get this dataset
# Animals2 is in your workspace
str(Animals2)
# Set up the side-by-side plot array
par(mfrow = c(1, 2))
# First plot: brain vs. body in its original form
plot(Animals2)
# Add the first title
title("Original representation")
# Second plot: log-log plot of brain vs. body
plot(Animals2, log = "xy")
# Add the second title
title("Log-log plot")


# Bar and Pie charts
data(dataCar, package = "insuranceData")
str(dataCar)
# Set up a side-by-side plot array
par(mfrow = c(1, 2))
# Create a table of veh_body record counts and sort
tbl <- sort(table(dataCar$veh_body),
            decreasing = TRUE)
# Create the pie chart
pie(tbl)
# Give it a title
title("Pie chart")
# Create the barplot with perpendicular, half-sized labels
barplot(tbl, las = 2, cex.names = 0.5)
# Add a title
title("Bar chart")

# Univariate Exploratory Plots
# ******************************

# Histograms: 
  # hist() is part of base R and its default option yields a histogram based on the number of times a record falls into each of the bins on which the histogram is based.
  # truehist() is from the MASS package and scales these counts to give an estimate of the probability density.
  # Set up a side-by-side plot array
  par(mfrow = c(1, 2))
  # Create a histogram of counts with hist()
  hist(Cars93$Horsepower, main = "hist() plot")
  # Create a normalized histogram with truehist() from MASS
  truehist(Cars93$Horsepower, main = "truehist() plot")
  
# Density plots:
  # While they are probably not as well known as the histogram, density estimates may be regarded as smoothed histograms, 
  # designed to give a better estimate of the density function for a random variable.
  par(mfrow = c(1, 1)) # Setting back to only one plot
  # Create index16, pointing to 16-week chicks
  index16 <- which(ChickWeight$Time == 16)
  # Get the 16-week chick weights
  weights <- ChickWeight[index16, "weight"]
  # Plot the normalized histogram
  truehist(weights)
  # Add the density curve to the histogram
  lines(density(weights))

# Normal QQ (Quantile-Quantile) Plot
  # A practical limitation of both histograms and density estimates is that, if we want to know whether the Gaussian distribution assumption is reasonable for our data, it is difficult to tell.
  # The quantile-quantile plot, or QQ-plot, is a useful alternative: we sort our data, plot it against a specially-designed x-axis based on our reference distribution (e.g., the Gaussian "bell curve"), and look to see whether the points lie approximately on a straight line. In R, several QQ-plot implementations are available, but the most convenient one is the qqPlot() function in the car package.
  car::qqPlot(weights)
  
# Bivariate Exploratory Plots
# ******************************
  
# Scatter and Sunflower Plots: Two Numeric Variables
  # Set up a side-by-side plot array
  par(mfrow = c(1, 2))
  # Create the standard scatterplot
  plot(rad ~ zn, data = Boston)
  # Add the title
  title("Standard scatterplot")
  # Create the sunflowerplot
  sunflowerplot(rad ~ zn, data = Boston)
  # Add the title
  title("Sunflower plot")
  
# Add smooth trend line to a scatterplot
  par(mfrow = c(1, 1)) # Only one plot in the window
  # Create a scatterplot of MPG.city vs. Horsepower
  plot(MPG.city ~ Horsepower, data = Cars93)
  # Call supsmu() to generate a smooth trend curve, with default bass
  trend1 <- supsmu(Cars93$Horsepower,
                   Cars93$MPG.city)
  # Add this trend curve to the plot
  lines(trend1)
  # Call supsmu() for a second trend curve, with bass = 10
  trend2 <- supsmu(Cars93$Horsepower,  
                   Cars93$MPG.city, bass = 10)
  # Add this trend curve as a heavy, dotted line
  lines(trend2, lty = 3, lwd = 2)
  
# Boxplots: Categorical vs. Numeric
  par(mfrow = c(1, 1))
  # Create a variable-width boxplot with log y-axis & horizontal labels
  boxplot(crim ~ rad, data = Boston, varwidth = TRUE, log = "y", las = 1)
  # Add a title
  title("Crime rate vs. radial highway index")

# Mosaic Plots: Two Categorical Variables
  # A mosaic plot may be viewed as a scatterplot between categorical variables and it is supported in R with the mosaicplot() function.
  # Create a mosaic plot using the formula interface
  mosaicplot(carb ~ cyl, data = mtcars)
  
# Multivariate Exploratory Plots
# ******************************
  
# Correlation plots
  # Load the corrplot library for the corrplot() function
  library(corrplot)
  # Extract the numerical variables from UScereal
  numericalVars <- UScereal[sapply(UScereal, is.numeric)]
  # Compute the correlation matrix for these variables
  corrMat <- cor(numericalVars)
  # Generate the correlation ellipse plot
  corrplot(corrMat, method = "ellipse")
  
# Decision Tree model plots
  # Load the rpart library
  library(rpart)
  # Fit an rpart model to predict medv from all other Boston variables
  tree_model <- rpart(medv ~., data = Boston)
  # Plot the structure of this decision tree model
  plot(tree_model)
  # Add labels to this plot
  text(tree_model, cex = 0.7)
  
  
# Introduction to the par() function
# **********************************
  
  # You already saw how the mfrow parameter to the par() function could be used to plot multiple graphs in one pane. 
  # The par() function also allows you to set many other graphics parameters, whose values will remain in effect until they are reset by a subsequent par() call.
  # Assign the return value from the par() function to plot_pars
  plot_pars <- par()
  # Display the names of the par() function's list elements
  names(plot_pars)
  # Display the number of par() function list elements
  length(plot_pars)

  
#****************************
### In-Class Exercise 5 -----
#****************************
  
# Using the iris Dataset, do the following:
  iris

# (a) Create a side-by-side plot array of the histogram and density of Sepal.Length 

# (a) Create a scatterplot matrix of the data using the pairs() function

# (b) Create a correlation plot of the numeric variables in the data using the corrplot() function

# (c) Create a scatter plot of Sepal.Length against Sepal.Width, coloured by Species. 

# (d) Add a regression line using abline()
  
# (e) Add a blue coloured smooth trend line using lines() and the supsmu() function. 

# (f) Create a boxplot of Sepal.Length by Species

# (g) Compute the average Sepal.Length by species and create a barchart. 

  
# (5) Basic Statistical Analysis ---------------------------------------------------------

# t-test: One sample
t.test(iris$Sepal.Length)
# t-test: two sample
t.test(iris$Sepal.Length, iris$Petal.Length)

# Correlation test
cor.test(iris$Sepal.Length, iris$Petal.Length)

# OLS Regression:
reg1 <- lm(Sepal.Length ~ Petal.Length, data = iris)
str(reg1)
reg1 # Just call and coefficients
coef(reg1)
summary(reg1) # Full output
reg1$residuals
resid(reg1) # same
reg1$fitted.values
fitted(reg1) # same

# regress on all other variables in the data frame
reg2 <- lm(Sepal.Length ~ ., data = iris) # Note that the factor (Species) is coded as a set of two dummy variables.
summary(reg2)
View(reg2)

# Regression diagnostics courtesy the car package
plot(reg2) # plots can help check some OLS assumptions (we want all 1 through 5 plots offered)


# Principal component analysis using eigenvalue decomposition on Correlation Matrix:
pca1 <- princomp(scale(mtcars)) # some functions standardize the data automatically, some don't (if in doubt look up the documentation). I always standardize manually using the scale function, to be sure.
summary(pca1)
# Loadings of principal components
loadings(pca1)
# or pca1$loadings
# Scree plot of eigenvalues
plot(pca1)
screeplot(pca1, type = "line", main = "Scree Plot")
# Biplot of score variables
biplot(pca1)
# Scores of the components
pca1$scores
# Rotation
varimax(pca1$loadings)
promax(pca1$loadings)


# (6) Control Flow -----------------------------------

# For loops
for (i in 1:3)  print(i)

for (i in 1:3)  {
  print(i)
}

for (i in 1:3)  print(mean(mtcars[, i]))

# For loops are inefficient and discouraged in R, a better way to do the same thing is:
sapply(mtcars[1:3], mean)
# In general, we want to use apply functions instead of for loops.

# while loops:
i <- 0L
while (i < 5L){
  print(i)
  i <- i + 1L
}

# Both for and while loops are best to be avoided in favor of apply functions which are more efficient. 

library(mosaic)
D = rnorm(100)
genmod <- function(D){ Y = D + rnorm(100); lm(Y ~ D)}
res = do(1000) * genmod(D) # this runs 1000 linear models and simplifies the output so that each model becomes a row in a data frame.
View(res)
class(res)
dim(res)
hist(res$D) # The OLS Estimator of D is Consistent!!

# Replicate:
res2 <- replicate(1000, coefficients(genmod(D))) # unlike do, replicate does not simplify the linear model output. It would output a huge list of linear model objects. We therefore just take the coefficients of the regression.
class(res2)
dim(res2)
res2 <- as.data.frame(t(res2))


# Extras about Working Effectively with R --------------------------------------------------

# Rstudio + Projects Workflow:
  # https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects
  # https://r4ds.had.co.nz/workflow-projects.html
  # https://www.r-bloggers.com/2020/01/rstudio-projects-and-working-directories-a-beginners-guide/
  # https://www.upgrad.com/blog/rstudio-projects-for-beginners/
  # https://bookdown.org/ndphillips/YaRrr/projects-in-rstudio.html

# Version control with Git and github: https://happygitwithr.com/

# Environments: http://adv-r.had.co.nz/Environments.html
# More advanced programming topics: http://adv-r.had.co.nz/
