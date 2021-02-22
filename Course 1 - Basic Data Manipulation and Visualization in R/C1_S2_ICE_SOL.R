############################################
# Data Science Training
# Solution to the In-Class Exercises of
# Course 1 Session 2
############################################

#****************************
### In-Class Exercise 1 -----
#****************************

# (a) Consider the following two objects. Compute the sum along the columns of these objects in the most efficient way so that the result is a vector
state.x77
longley

str(state.x77) # This is a matrix
str(longley)   # This is a data.frame

colSums(state.x77) # Most efficient matrix column sums: Using colSums()
vapply(longley, sum, numeric(1L)) # Most efficient data.frame column sums: Using vapply(), where we also need to declare that the result for each column will be a numeric vector of lenth 1. 

# (b) Show an alternative way of summing along the columns of these objects, again resulting in a vector. 
apply(state.x77, 2L, sum) # apply() supports any function but is substantially slower than colSums()
sapply(longley, sum)      # sapply() applies a function to each column in a data.frame and simplifies the result. 
# It needs to perform some checks to figre out how to simplify things, so it is slower than vapply() where we declare what the result will be.

# (c) Consider the titanic data. Compute the percent that survived by Sex and Age
Titanic
str(Titanic) # 4D array
ag_class <- apply(Titanic, 2:4, sum) # First we sum over class
ag_class[,,"Yes"] / apply(ag_class, 1:2, sum) * 100 # Then divide survived matrix by total matrix, multiply by 100 to get percentages

# (d) Consider these two datasets. Aggregate 'infert' by education and induced, and 'airquality' by Month
infert
aggregate(infert[-c(1L, 4L)], by = infert[c(1L, 4L)], FUN = mean)
airquality
aggregate(airquality[-5L], by = airquality[5L], FUN = mean, na.rm = TRUE) # Ned to add na.rm = TRUE here as columns Ozone and Solar.R have missing values. 

# (e) Using the iris dataset, compute the quantiles of Sepal.Width by Species and simplify the result to a matrix. 
iris
do.call(rbind, tapply(iris$Sepal.Width, iris$Species, quantile)) # Can use simplify2array(...) instead of do.call(rbind, ...), but the latter is more efficient.


#****************************
### In-Class Exercise 2 -----
#****************************

# (a) Go to the CRAN Task Views and look for interesting packages in a task view

# CRAN Task Views are available online at: https://cran.r-project.org/web/views/

# (b) Using RWsearch, search for packages in the field you find most interesting


#****************************
### In-Class Exercise 3 -----
#****************************

# (a) Find the introduction to 'data.table' vignette and the introduction to 'xts' vignette.
vignette("datatable-intro")
vignette("xts")

# (b) Install the packages 'magrittr', 'matrixStats' and 'ggplot2' and find the vignettes
install.packages(c("magrittr", "matrixStats", "ggplot2"))
vignette("magrittr")
vignette("matrixStats-methods")
browseVignettes("ggplot2")

# (c) Find the website for 'collapse', access the introductory vignette 
#     and look for the section on advanced data aggregation
# Website: https://sebkrantz.github.io/collapse/index.html
# Section on Advanced Data Aggregation: https://sebkrantz.github.io/collapse/articles/collapse_intro.html#5-advanced-data-aggregation

# (d) Find the cheatsheets for 'collapse', 'data.table' and 'ggplot2' on the Rstudio website.
# Can be downloaded from here: https://rstudio.com/resources/cheatsheets/

# (e) Execute all examples given for the 'lm' function
example("lm")


#****************************
### In-Class Exercise 4 -----
#****************************

# Using the haven library, import the STATA File "sachs 2003 institutions don't rule.dta".
data <- haven::read_dta("data/sachs 2003 institutions don't rule.dta")

# (a) Summarise the data
summary(data)

# (b) Delete the columns 'AJR' and 'ME' from the data
data[c("AJR", "ME")] <- NULL

# (c) Save the file as a STATA 10 file in the 'data' folder. 
haven::write_dta(data, "data/sachs_STATA10.dta", version = 10)

# (d) Save the file as an Excel file in the 'data' folder.
writexl::write_xlsx(list(data = data, namlab = collapse::namlab(data)), 
                    "data/sachs.xlsx") # Passing a list of data.frame's to write_xlsx() creates an excel workbook with multiple sheets.

# (e) Save the file as a CSV file in the 'data' folder. 
readr::write_csv(data, "data/sachs.csv")


#****************************
### In-Class Exercise 5 -----
#****************************

# Using the iris dataset, do the following:
iris

# (a) Create a side-by-side plot array of the histogram and density of Sepal.Length 
par(mfrow = c(1, 2))
hist(iris$Sepal.Length)
plot(density(iris$Sepal.Length))
par(mfrow = c(1, 1)) # Need to undo this for the next

# (a) Create a scatterplot matrix of the data using the pairs() function
pairs(iris)

# (b) Create a correlation plot of the numeric variables in the data using the corrplot() function
corrplot(cor(iris[1:4]), method = "ellipse")

# (c) Create a scatter plot of Sepal.Length against Sepal.Width, coloured by Species. 
plot(Sepal.Length ~ Sepal.Width, data = iris, col = iris$Species, pch = 16) # Using point shape 16 giving filled dots
# (Optionally) adding a legend. 
legend("topleft", legend = levels(iris$Species), 
       col = unique(iris$Species), pch = 16)

# (d) Add a regression line using abline()
abline(lm(Sepal.Length ~ Sepal.Width, data = iris))

# (e) Add a blue coloured smooth trend line using lines() and the supsmu() function. 
trend <- supsmu(iris$Sepal.Width,
                iris$Sepal.Length, bass = 10)
# Add this trend curve to the plot
lines(trend, col = "blue")

# (f) Create a boxplot of Sepal.Length by Species
boxplot(Sepal.Length ~ Species, data = iris)

# (g) Compute the average Sepal.Length by species and create a barchart. 
barplot(tapply(iris$Sepal.Length, iris$Species, mean))
