############################################
# Data Science Training
# Homework Assignment of Course 1 Session 1
# Solution
############################################

# Problem 1: Basic Statistical Programming -------------------
# consider the following vector of random numbers drawn from a 
# normal distribution with mean 10 and standard deviation 2
x <- rnorm(100, mean = 10, sd = 2)

# (a) plot a histogram of x using hist()
hist(x)

# (b) compute the sum, mean, median, standard deviation, minimum and maximum values and quantiles of this vector
sum(x)
mean(x)
median(x)
sd(x)
min(x)
max(x)
quantile(x)

# (c) manually compute the mean using sum() and length()
sum(x) / length(x)

# (d) check your computation of the mean, median, minimum, maximum, 2nd and 4th quartile against:
summary(x)

# (e) write a function that takes x as an input and returns the same output as summary(x)
my_summary <- function(x) c(`Min.` = min(x), `1st Qu.` = unname(quantile(x, 0.25)),
                            Median = median(x), Mean = mean(x), `3rd Qu.` = unname(quantile(x, 0.75)),
                            `Max.` = max(x))
my_summary(x)

# (f) This inserts 10 missing values at random into x.
x[sample.int(100L, 10L)] <- NA
x
?sample.int  # See the help page of this function, make sure you understand the code above

summary(x)  # see the output now

# (g) Modify your function in part (e) to also replicate this output
my_summary <- function(x) c(`Min.` = min(x, na.rm = TRUE), 
                            `1st Qu.` = unname(quantile(x, 0.25, na.rm = TRUE)),
                            Median = median(x, na.rm = TRUE), 
                            Mean = mean(x, na.rm = TRUE), 
                            `3rd Qu.` = unname(quantile(x, 0.75, na.rm = TRUE)),
                            `Max.` = max(x, na.rm = TRUE),
                            `NA's` = sum(is.na(x)))
my_summary(x)

# (h) Add an argument 'na.rm' to your function, and modify your function so that it can produce both outputs of (e) and (g) depending on na.rm = TRUE or FALSE
my_summary <- function(x, na.rm = FALSE) {
  if(na.rm) nas = c(`NA's` = sum(is.na(x)))
  c(`Min.` = min(x, na.rm = na.rm), 
    `1st Qu.` = unname(quantile(x, 0.25, na.rm = na.rm)),
    Median = median(x, na.rm = na.rm), 
    Mean = mean(x, na.rm = na.rm), 
    `3rd Qu.` = unname(quantile(x, 0.75, na.rm = na.rm)),
    `Max.` = max(x, na.rm = na.rm), if(na.rm) nas)
}

my_summary(rnorm(100, mean = 10, sd = 2))
my_summary(x, na.rm = TRUE)

# (i) Consider this dataset that comes with R
mtcars
View(mtcars)

# make sure you understand this code. Look up the individual functions i.e. ?t, ?sapply
summ <- t(sapply(mtcars, summary))
summ

# Examine 'summ'. What kind of object is it?, see the structure and class of it
str(summ)
class(summ)

# (j) apply your function to each column of the data in the same way as shown above. Do you get the same result?
t(sapply(mtcars, my_summary))
summ

all.equal(t(sapply(mtcars, my_summary)), summ)

# (k) using the matrix() and sample.int() functions, insert missing values into mtcars and repeat.
mat <- matrix(TRUE, nrow = nrow(mtcars), ncol = ncol(mtcars))
# Here setting 30 values to missing at random
mat[sample.int(length(mat), 30L)] <- FALSE
mat
mtcars[mat] <- NA

t(sapply(mtcars, summary, na.rm = TRUE))
t(sapply(mtcars, my_summary, na.rm = TRUE))

# (l) Consider the iris dataset printed below. Create a generic version of your function in part (h), 
#     adding a default method that that tabulates non-numeric data and apply it to the columns of iris using lapply(). 
head(iris)
str(iris)
summary(iris)

# Generic version of the function
my_summary <- function(x, na.rm = FALSE) UseMethod("my_summary")

# The function we had before becomes the method for numeric data
my_summary.numeric <- function(x, na.rm = FALSE) {
  if(na.rm) nas = c(`NA's` = sum(is.na(x)))
  c(`Min.` = min(x, na.rm = na.rm), 
    `1st Qu.` = unname(quantile(x, 0.25, na.rm = na.rm)),
    Median = median(x, na.rm = na.rm), 
    Mean = mean(x, na.rm = na.rm), 
    `3rd Qu.` = unname(quantile(x, 0.75, na.rm = na.rm)),
    `Max.` = max(x, na.rm = na.rm), if(na.rm) nas)
}

# The default method applied to all other data
my_summary.default <- function(x, na.rm = FALSE) {
  if(na.rm) nas = c(`NA's` = sum(is.na(x)))
  c(table(x), if(na.rm) nas)
}

# Applying the function to the columns in the dataset
lapply(iris, my_summary)

# Compare this to the output of summary:
summary(iris)

# Note: These are possible alternative definitions of the generic function
my_summary <- function(x, ...) UseMethod("my_summary")
my_summary <- function(x, na.rm = FALSE) UseMethod("my_summary", x)
# The first allows a variable number of arguments to be passed to different methods (not applicable here),
# and the second just makes it explicit that we want methods will be chosen based on the type of the 'x' argument, 
# which is also not necessary here as UseMethod() dispatches on the first function argument by default. 




# Problem 2: Strings and Dates -----------------------------------
# consider the following vector of fiscal years
y <- 1999:2017
fy <- paste(y - 1L, substr(as.character(y), 3L, 4L), sep = "/")

# (a) make sure you understand how these fiscal year strings were generated by looking up the relevant functions, e.g. ?substr
?substr

# (b) wrap this code into a function called FY(), that can create sequences of fiscal years from arbitrary starting and ending calendar years 
FY <- function(start, end) {
  y <- start:end
  paste(y - 1L, substr(as.character(y), 3L, 4L), sep = "/")
}

FY(1999, 2017)
FY(2001, 2020)

# (c) now create a function FY_to_Date() that takes a vector of fiscal years and converts it to a vector of Dates corresponding to the last day (30th of June) of each fiscal year
FY_to_Date <- function(fy) as.Date(paste0(as.integer(substr(fy, 1L, 4L)) + 1L, "-06-30"))
FY_to_Date(fy)

# (d) generate a vector of fiscal years and apply your function to turn them into dates. 
dates <- FY_to_Date(FY(2001, 2020))

# (e) using substr(), get the month from these generated dates. 
as.integer(substr(dates, 6L, 7L))

