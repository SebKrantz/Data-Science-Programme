#############################################################
# *************** R BASED DATA SCIENCE TRAINING *************
# Course 2: Advanced Data Manipulation and Visualization in R
# -----------------------------------------------------------
# Prepared by: Sebastian Krantz, ODI Fellow, MEPD, MoFPED
# Permanent Contact: sebastian.krantz@graduateinstitute.ch
#############################################################

# Course Aim: To familiarize participants with state-of-the art R packages for 
#             statistical computing, data manipulation and visualization

# Today:
# (0) Rstudio Projects, Git and Github
# (1) Matrix statistics with 'matrixStats'
# (2) Pipe operators with 'magrittr'
# (3) Advanced data transformation using 'collapse'
# (4) Data manipulation with 'data.table'
# (5) Time series with 'xts' + rolling statistics with 'roll' and 'dygraph' charts


setwd("Course 2 - Advanced Data Manipulation and Visualization in R")

# (0) Projects Workflow in Rstudio, Git and Github -------------------------
#***************************************************************************

# Rstudio + Projects Workflow:
# https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects
# https://r4ds.had.co.nz/workflow-projects.html
# https://www.r-bloggers.com/2020/01/rstudio-projects-and-working-directories-a-beginners-guide/
# https://www.upgrad.com/blog/rstudio-projects-for-beginners/
# https://bookdown.org/ndphillips/YaRrr/projects-in-rstudio.html

# Version control with Git and github: https://happygitwithr.com/


# (1) Matrix Statistics with 'matrixStats' ---------------------------------
#***************************************************************************

# Remember the basic R functions rowSums(), colSuma() and rowMeans(), colMeans()?
data() # Set of built-in datasets in R
# Lets use this matrix of US State facts and figures
?state.x77
m <- state.x77 # Giving it a simple name

# Then we can compute efficient row and column-wise sums and means
rowSums(m)
colSums(m)
rowMeans(m)
colMeans(m)

# Now the matrixStats package extends the set of row-and column-wise statistical functions
install.packages("matrixStats")
library(matrixStats)
?`matrixStats-package`
vignette(package = "matrixStats")
vignette("matrixStats-methods")

# So now we can do
rowMins(m)
colMins(m)
rowMaxs(m)
colMaxs(m)
rowMedians(m)
colMedians(m)
rowSds(m)
colSds(m)
# etc...

# There are also weighted versions:
colWeightedMeans(m, w = m[, "Population"])
colWeightedMedians(m, w = m[, "Population"])

# Unfortunately matrixStats functions at this point don't preserve the dimension names of the matrices.
# This will be fixed soon, but at the moment we need to copy those names using e.g.

setNames(rowMins(m), rownames(m))
setNames(colMins(m), colnames(m))

# Another nice feature of the matrixStats package is that you can compute on subsets of rows and columns.
setNames(rowMaxs(m, cols = 1:3), rownames(m))
rowMaxs(m[, 1:3]) # Same thing but much slower as we first need to materialize the subsetted matrix

rowSums2(m, cols = 1:3)

# To see the use of this lets get some real world data like a Social Accounting Matrix recording the 
# transactions between different agents in the economy. Every column in the matrix shows the payments 
# the respective agent/account makes to all row accounts.

library(readxl)
# Read an aggregated version of a social accounting matrix, with random errors introduced before distributing it in the framework of this course. 
SAM <- read_xlsx("data/Ag_SAM_modified.xlsx") 
View(SAM)
str(SAM)

# Lets create a matrix from the SAM:
SAM_mat <- as.matrix(SAM[-(1:2)])
rownames(SAM_mat) <- SAM$Index
View(SAM_mat)
str(SAM_mat)

# Production in the SAM is disaggregated into activities and commodities (following the SUT), where an activity can
# produce multiple commodities, and taxes are paid on commodities while the activity (producers) earn an income at basic prices.

# lets check whether the rows and column sums match the totals
all.equal(rowSums2(SAM_mat, cols = -ncol(SAM_mat)),
          unname(SAM_mat[, ncol(SAM_mat)]))

all.equal(colSums2(SAM_mat, rows = -nrow(SAM_mat)),
          unname(SAM_mat[nrow(SAM_mat), ]))

# This computes all Intermediate commodidtes bought by activities

res <- colSums2(SAM_mat, 
                rows = startsWith(rownames(SAM_mat), "C_"),
                cols = startsWith(colnames(SAM_mat), "A_"))

names(res) <- grep("^A_", colnames(SAM_mat), value = TRUE)
res

# The less efficient base R way
colSums(SAM_mat[startsWith(rownames(SAM_mat), "C_"), 
                startsWith(colnames(SAM_mat), "A_")])


#****************************
### In-Class Exercise 1 -----
#****************************

# (a) using rowSums2(), compute the total intermediate use of each commodity

# (b) repeat step (a) using rowSums() where you subset the matrix beforehand

# (c) compute the taxes and transport margins paid on commodities

# (d) compute the total expenditure of different households on all commodities



# (2) Pipe operators with 'magrittr' ---------------------------------------
#***************************************************************************

# The magrittr package, introduced in 2014, provides a forward 
# pipe operator that radically changed the way most people nowadays
# write R code. 

# Consider this code:
transform(aggregate(mpg ~ cyl, 
                    data = subset(mtcars, hp > 100), 
                    FUN = function(x) round(mean(x), 2)), 
          newcol = log(abs(mpg)) * 0.4251)
# What does this code do?

# Now let's see the version using magrittr pipes:
library(magrittr)
mtcars %>%
  subset(hp > 100) %>%
  aggregate(mpg ~ cyl, data = ., FUN = . %>% mean %>% round(2)) %>%
  transform(newcol = mpg %>% abs %>% log %>% `*`(0.4251)) 

# This example shows a few neat features of the pipe:
  
# (1) By default the left-hand side (LHS) will be piped in as the first argument of the function appearing on the right-hand side (RHS). 
  mtcars %>%
    subset(hp > 100)
  # is the same as
  subset(mtcars, hp > 100)

# (2) %>% may be used in a nested fashion, e.g. it may appear in expressions within arguments. 
  transform(mtcars, newcol = abs(mpg))
  # is the same as
  mtcars %>%
    transform(newcol = mpg %>% abs) 
  # Note that the %>% inside transform is nested

# (3) Whenever only one argument (the LHS) is needed, one can omit the empty parentheses.
  mpg <- mtcars$mpg
  # So these are all equivalent:
  abs(mpg)
  mpg %>% abs
  mpg %>% abs()
  mpg %>% abs(.)
  
# (4) When the LHS is needed at a position other than the first, one can use the dot,'.', as placeholder.
  mtcars %>%
    aggregate(mpg ~ cyl, data = ., FUN = mean)
  # is the same as  
  aggregate(mpg ~ cyl, data = mtcars, FUN = mean)
  
# (5) A pipeline with a dot (.) as the LHS will create a unary function.
  # Normally we define a function ad-hoc:
  myFUN <- function(x) log(abs(x))
  myFUN # look at it
  myFUN(mpg)
  # We could now also do
  myFUN = . %>% abs %>% log
  myFUN # look at it (you cannot really see what it does)
  myFUN(mpg) # But gives Same result 
  
  # In the above example
  aggregate(mpg ~ cyl, data = mtcars, FUN = . %>% mean %>% round(2))
  # is the same as
  aggregate(mpg ~ cyl, data = mtcars, FUN = function(x) round(mean(x), 2))
  
  # NOTE: I don't recommend using this feature of the pipe, it is very confusing to understand, 
  #       because when you see a "." you usually expect it to be a placeholder for some object piped from
  #       code above it.
  
# So now we are able to fully comprehend what this code is doing: 
  mtcars %>%
    subset(hp > 100) %>%
    aggregate(mpg ~ cyl, data = ., FUN = . %>% mean %>% round(2)) %>%
    transform(newcol = mpg %>% abs %>% log %>% `*`(0.4251)) 
  
# There is one necessary feature of piped code that is not demonstrated in the above example:

# consider this code
head(cbind(abs_mpg = abs(mtcars$mpg), 
           log_mpg = log(mtcars$mpg)))
# We could want to create a piped version like this:
mtcars$mpg %>% cbind(abs_mpg = abs(.), 
                     log_mpg = log(.)) %>% head

# However this is not quite what we wanted. The reason is that the placeholder "." 
# does not enter cbind() directly but only the functions abs() and log(), therefore it is added as a first argument
mtcars$mpg %>% cbind(., abs_mpg = abs(.), # This is what happens in the above code
                        log_mpg = log(.)) %>% head

# We can omit this 'first argument injection' by adding curly braces:
mtcars$mpg %>% {
  cbind(abs_mpg = abs(.), 
        log_mpg = log(.))
  } %>% head


#****************************
### In-Class Exercise 2 -----
#****************************

# Rewrite these expressions using magrittr pipes:

# (a)
sapply(subset(transform(mtcars, mpg2 = mpg * 2),
              cyl > 4 & mpg > 20, vs:mpg2), median)

# (b)
str(rapply(list(airquality, mtcars), sum, how = "list"))

# (c)
do.call(rbind, tapply(iris$Sepal.Width, iris$Species, quantile)) 





# Now: Some more pipes

# I. Assignment pipe `%<>%`

  # Say we want to modify a data frame by transforming a column:
  mtcars <- transform(mtcars, cyl_vs_am = cyl + vs + am)
  
  # We can rewrite this more compactly as
  mtcars %<>% transform(cyl_vs_am = cyl + vs + am)


# II. Exposition pipe `%$%`
  # Recall that a convenient way to access columns from a data frame is by using the `$` operator
  cor(iris$Sepal.Length, iris$Sepal.Width)
  iris %$% cor(Sepal.Length, Sepal.Width)

  tapply(iris$Sepal.Width, iris$Species, quantile)
  
  # Recall also that the with() function in base R lets us write code that is evaluated in the data environment
  with(iris, cor(Sepal.Length, Sepal.Width)) # Same as above
  
  # In a piped call we might want do to some more complex manipulation beforhand such as subsetting beforehand 
    # We could use the "." placeholder, but here we also need curly {} braces to prevent first-argument injection.
    iris %>% subset(Species == "setosa") %>% 
      {cor(.$Sepal.Length, .$Sepal.Width)}   # pretty arcane code!
    # The with() function makes things easier, but we still have some nesting in the code.
    iris %>% subset(Species == "setosa") %>% 
      with(cor(Sepal.Length, Sepal.Width))   # Ok, but we now have nested with(cor())...
    # The `%$%` pipe allows us to write this in the cleanest way
    iris %>% subset(Species == "setosa") %$% 
      cor(Sepal.Length, Sepal.Width)  
    
    # And similarly for this code...
    iris %$% tapply(Sepal.Width, Species, quantile)
    
    # We can think of `%$%` as exposing the names of all variables in a list / data frame in the following function call
    
# III. Tee pipe `%T>%`
    # Say we want an intermediate step in out analysis that summarizes the data or plots it:
    iris %<>% subset(Species == "setosa")
    plot(iris)                               # Plot the entire data   
    iris %$% plot(Sepal.Length, Sepal.Width) # Plot the relationship between two variables
    iris %$% cor(Sepal.Length, Sepal.Width)  # Correlation coefficient
    iris %<>% transform(log_width = log(Sepal.Width))
    iris %$% plot(Sepal.Length, log_width)   # Plot again
    
    # Note that we have to interrupt the piped workflow, which leads to the iris dataset being referred to 6 times in the above code.
    # The `%T>%` allows us to pipe an object into a piece of code, evaluate that code, but then return the original object again.
    rm(iris) # Deleting modified dataset so we can start over..
    # This expression does exactly the same thing, and we only need to type `iris` 1 time.

    iris %<>% 
      subset(Species == "setosa") %T>%
      plot %T>%                                           # Plotting functions always display the result when executed
      with(cor(Sepal.Length, Sepal.Width) %>% print) %T>% # But for functions that are supposed to print something, we need to call print() to see the output, done here using a nested pipe.
      with(plot(Sepal.Length, Sepal.Width)) %>%           # In piped calls the with() function in still useful for exposition.
      transform(log_width = log(Sepal.Width)) %$%        # But at the end of the call we use the exposition pipe.  
      plot(Sepal.Length, log_width)
    
    rm(iris)
    
# Some Concluding Remarks: Once you have mastered them, 
#    pipes are extremely useful for expressive and quick code which 
#    avoids complex nesting and the need to create intermediate objects. 
#    A downside of pipes however is that it may be more difficult to debug code and to 
#    execute single fragments of code in the call. I thus recommend only connecting pieces of 
#    code with pipes that you are confident will work, or that you have tested, and to not write
#    too long and too complex piped expressions. 
    
    

# (3) Advanced data transformation with 'collapse' -------------------------
#***************************************************************************

# Up to now we have done rather simple statistical computations, such as column or row-wise
# statistics, using base functions like rowSums, colSums, the matrixStats extensions (rowSums2, rowMedians etc..),
# the base R apply family (apply, sapply, lapply, mapply, tapply, covered in depth in course 1), the aggregate() function for
# elementary data.frame aggregation, and the functions subset(), transform(), with() and magrittr pipe operators 
# for flexible and expressive coding. 
    
# This toolset is convenient, but severely limited if we want to perform complex computations 
# on large real world datasets (such as (weighted) aggregation, aggregation of categorical and mixed type e.g. survey 
# data, and computations on time series and panel data, and general by-group operations. 
    
# In the following part of the course we will see 3 high-performing packages: collapse, data.table and xts that
# allow is to do most of the complex computations we are interested in as economists and statisticians working in government. 
# Many data scientist using R rely on a different set of packages called the `tidyverse`, were the `dplyr` package is very popular. 
# These packages are also useful, but they are mostly designed to deal well with cross-sectional datasets as they occur a lot in industry, 
# and have severe limitations when it comes to dealing with weights, irregular time series / panels, as well as performance bottlenecks on large data.

install.packages("collapse")
library(collapse)    
?`collapse-package`    
?`collapse-documentation`        
.COLLAPSE_TOPICS    

# A first major contribution of collapse is a set of fast statistical functions that support 
# groupwise and/or weighted statistics and transformations on vectors, matrices and data frames.
help(.COLLAPSE_TOPICS[2L])
str(airquality) # remember this data.frame
# To calculate the sum of each column in base R, we need to write
sapply(airquality, sum)
sapply(airquality, sum, na.rm = TRUE) # Need additionally to pass na.rm = TRUE to sum() function

# With collapse, we can simply write
fsum(airquality)  # na.rm = TRUE by default in this package

# We can also calculate a grouped sum:
fsum(airquality, g = airquality$Month)
# Note that the unique groups are now in the row-names, and Month is also aggregated...
# This way of aggregating is more convenient for matrices where we provide an external grouping variable
# Remember the SAM:
str(SAM_mat)
# This defines a grouping that will give us a macro-SAM
g <- colnames(SAM_mat) %>% 
     strsplit("_") %>%          # Splitting by "_"
     sapply(`[`, 1L) %>%        # Taking the first element after split
     factor(levels = unique(.)) # Coercing this to factor with ordering given by the unique levels (otherwise alphabetic ordering after aggregation)
g
# Aggregate columns of the matrix
fsum(SAM_mat, g)                 
# This now gives us the Macro-SAM
MACRO_SAM <- SAM_mat %>% fsum(g) %>% t %>% fsum(g) %>% t 
MACRO_SAM

# For data frames, we can create a grouped data frame
airquality %>% fgroup_by(Month)
airquality %>% gby(Month)       # Some functions in collapse can be abbreviated...

airquality %>% gby(Month) %>% fsum # Aggregating by month
# -> Looks better, the grouping columns go in front (by default sorted) followed by columns containing the aggregated values.

# Now another nice feature of these functions is the `TRA` argument, which let's us do 
# groupwise replacing or sweeping operations. 
# Let's take again the SAM. Say we want to calculate expenditure shares of activities and households on different commodities:

SAM_mat %>% fsum(g, TRA = "/") %>% replace_NA(0) %>% View # TRA = "/" here means divide all values by the group sum.
# There is also a TRA() function which let's us do that explicitly
SAM_mat %>% fsum(g) %>% TRA(SAM_mat, . , "/", g) %>% replace_NA(0)
?TRA

# This also works with data frames...
# What if we wanted to just replace data with the grouped statistic:
airquality %>% gby(Month) %>% fsum(TRA = "replace")
airquality %>% gby(Month) %>% fsum(TRA = "replace_fill")

# To see more useful functionality of collapse let's consider some panel data
data(GGDC10S) # A dataset that comes with collapse
View(GGDC10S)
?GGDC10S

# Aggregating: last value (note that also na.rm = TRUE in flast)
GGDC10S %>% gby(Country, Variable) %>% flast(na.rm = FALSE) %>% View
# Note that this data has variable labels. We can actually see that it was imported from STATA using haven::read_dta
str(GGDC10S)
# We can see retrieve these labels by calling vlabels
vlabels(GGDC10S)
vlabels(GGDC10S)["AGR"] <- "Agriculture" # Just removing a redundant trailing space
vlabels(GGDC10S, "format.stata") # This gives the STATA encoding
# The function namlab lets us see both the variable name and the label
namlab(GGDC10S)
namlab(GGDC10S, class = TRUE) # We can add the class
# Now what if we want to compute some descriptive statistics about this data...
# In basic R we saw the summary() function
summary(GGDC10S) # Can be a bit challenging to understand..
# The function qsu() gives a quick summary of the data
qsu(GGDC10S)
qsu(GGDC10S, vlabels = TRUE)
# descr() gives us a detailed statistical summary
descr(GGDC10S)
# If we have many variables (e.g. a survey), it can also be convenient to coerce and view this as a data frame
descr(GGDC10S) %>% as.data.frame %>% View
# See also:
help(.COLLAPSE_TOPICS[10L])

# Now there are 2 challenges with this data: 
# The first problem in summarizing this data is that value added (VA) is in local currency, 
# the second that it contains 2 different Variables (VA and EMP) stacked in the same column. 

# One way of solving the first problem could be converting the data to percentages through dividing by the overall VA and EMP contained in the last column.
# collapse provides a function ftransform, which is a faster and more versatile version of trasnform()
ftransform(GGDC10S, AGR_Share = AGR / SUM) # This calculates the Share of Agriculture in total employment and VA
ftransformv(GGDC10S, 6:16, `/`, SUM)       # ftransformv allows us to calculate shares from all the columns
settransformv(GGDC10S, 6:16, `/`, SUM)     # settransformv can be used to modify the data frame in-place

# In general, many collapse functions starting with `f` are faster and more versatile equivalents of functions in base R or other packages.
help(.COLLAPSE_TOPICS[4L])

# The Problem of the stacked variables can be solved in several ways. 

  # (1) If we only require a simple set of statistics, the qsu() function directly allows us to perform grouped statistics
  qsu(GGDC10S, by = ~ Variable, vlabels = TRUE)
  qsu(GGDC10S, by = ~ Variable + Region, vlabels = TRUE)

  # (2) We subset the data and summarize each quantity separately
  fsubset(GGDC10S, Variable == "VA", AGR:SUM) %>% descr
  fsubset(GGDC10S, Variable == "EMP", AGR:SUM) %>% descr
  # ... obviously this can get tedious if we have many groups...
  
  # (3) We can split the data into chunks using rsplit()
  rsplit(GGDC10S, ~ Variable) %>% str(give.attr = FALSE)
  rsplit(GGDC10S, ~ Variable) %>% lapply(descr)
  rsplit(GGDC10S, ~ Variable) %>% lapply(. %>% descr %>% as.data.frame)
  # This is a quite flexible approach, which can be generalized to multiple dimensions of the data
  rsplit(GGDC10S, ~ Variable + Region) %>% str(give.attr = FALSE)
  rsplit(GGDC10S, ~ Variable + Region) %>% rapply2d(descr) # Recursively applying descriptive summary to these data subsets
  rsplit(GGDC10S, ~ Variable + Region) %>%      # Split data
    rapply2d(. %>% descr %>% as.data.frame) %>% # Apply descriptive statistics, coerce them to data frame 
    unlist2d(.c(Var, Region)) %>% View          # row-bind result back to data frame and View  
  # To learn about rsplit(), rapply2d() and unlist2d(), see
  help(.COLLAPSE_TOPICS[9L])
  # We will soon see an even simpler approach to do this last computation where we first split and then 
  # combine the data again using the data.table package. 
  
# Another thing we can do with qsu() is compute panel-data statistics with the pid argument
qsu(GGDC10S, by = ~ Variable, pid = ~ Country, cols = is.numeric) %>% aperm(c(4L, 2L, 3L, 1L))
# Output as a list of matrices and binding to data frame      
qsu(GGDC10S, by = ~ Variable, pid = ~ Country, cols = is.numeric, array = FALSE) %>% 
  unlist2d(.c(Variable, Trans), row.names = "Var") %>% View

# Now Another option for quick visualization of this data is to transform it into a time-series array,
# using the function psmat()
GGDC10s_ar <- GGDC10S %>% fsubset(Variable == "VA") %>% 
              psmat(~ Country, ~ Year, cols = is.numeric)
str(GGDC10s_ar)
plot(GGDC10s_ar, legend = TRUE)

# Looking at so many series in parallel is tedious though. Let's aggregate over regions
GGDC10S %>% fsubset(Variable == "VA") %>% 
  gby(Region, Year) %>% fselect(AGR:SUM) %>% 
  fmean %>% psmat(~ Region, ~ Year) %>%
  plot(legend = TRUE)

# Apart from sectoral shares, we might be interested in the growth rates of employment and value added in different countries and sectors
rm(GGDC10S)
help(.COLLAPSE_TOPICS[8L])

# The eastest way to calculate growth rates is using the growth operator G(). By default it renames columns adding a G1 prefix
G(GGDC10S, by = ~ Country + Variable, t = ~ Year) %>% qTBL # qTBL coerces the data to a 'tibble', which is also a data.frame, but has a pretty printout method
G(GGDC10S, by = ~ Country + Variable, t = ~ Year, stubs = FALSE) %>% qTBL # No renaming
G(GGDC10S, c(1L, 10L), 1L, ~ Country + Variable, ~ Year) %>% qTBL # Same thing, adding decade over decade growth rates.

# We can also do this more formally using pipes
GGDC10S %>% 
  fsubset(Variable == "VA", -Variable, -Regioncode) %>% # Subset
  fgroup_by(Country, Region) %>%                        # Group
  fgrowth(10, t = Year, power = 1/10) %>%               # Calculate annualized decade-over-decade growth rates (to smooth volatility)
  fgroup_by(Region, Year) %>%                           # Group again by Region
  fselect(-Country) %>%                                 # Remove Country column
  fmedian %>%                                           # Aggregate using the median (again to remove outliers which would feed into the mean)
  psmat(~ Region, ~ Year) %>%                           # Transform this into a time-series array
  plot(legend = TRUE)                                   # Plot the array


#****************************
### In-Class Exercise 3 -----
#****************************

# The collapse package comes with another dataset: wlddev 
View(wlddev)
?wlddev
# In this exercise you will explore this dataset

# (a) using namlab(), see the names, variable labels and classes of variables in this data

# (b) compute a detailed statistical summary of variables using descr(). 

# (c) using qsu(), compute panel-data summary statistics of this data.

# (d) Summarise these variables for EAC countries

# (e) Compute the annualized 10-year growth rates of these variables

# (f) Save the GDP, LIFE Expectancy and ODA for EAC variables in a matrix and plot it

# (g) Repeat part (f) and plot the annualized growth rates computed in (e)

# (h) We want to aggregate this data to provide regional statistics. 
  # To better represent the populations in the different regions, we will do a population weighted aggregation.
  # Since the data does not contain a population variable, we download it from the world bank and merge it to the data
  install.packages("WDI")
  wlddev %<>% merge(
         WDI::WDI(country = as.character(unique(wlddev$iso3c)), # API for the world bank development indicators:, see ?WDI::WDI. For more indicators see als install.packages("wbstats")
                indicator = "SP.POP.TOTL", extra = TRUE) %>% 
         frename(SP.POP.TOTL = POP) %>%
         fselect(iso3c, year, POP))
  # Relabelling data
  vlabels(wlddev) <- c(vlabels(collapse::wlddev), "Total population")
  View(wlddev)
  qsu(wlddev, vlabels = TRUE)
  
  # Aggregate this data to the world region level using Population weights. See ?fmean
  
# (i) transform this aggregated data from part (h) into a time series array and plot


  
# Advanced data aggregation with collapse --------------------------------
# Let's look again at this dataset
View(wlddev)
# We can easily aggregate the numeric columns of this dataset using any function (such as weighted means)
wlddev %>% fgroup_by(region, year) %>% num_vars %>% fmean(POP)
# We can also aggregate all variable using a function that can deal with categorical data, such as the mode (e.g. the most frequent value)
wlddev %>% fgroup_by(region, year) %>% fmode
# However ideally we would like to combine these approaches to aggregate mixed-type data with both numeric and categorical variables

# the collap() function let's us do just that.
help(.COLLAPSE_TOPICS[6L])

collap(wlddev, ~ region + year, FUN = fmean, catFUN = fmode, w = ~ POP) %>% View
# By default this preserves the column order. 
collap(wlddev, ~ region + year, fmean, fmode, w = ~ POP, 
       keep.col.order = FALSE) %>% View
# Can apply multiple functions
collap(wlddev, ~ region + year, list(fmean, fsd), fmode, w = ~ POP, 
       keep.col.order = FALSE) %>% View
# If we do that we can also get a stacked format
collap(wlddev, ~ region + year, list(fmean, fsd), fmode, w = ~ POP, 
       keep.col.order = FALSE, return = "long_dupl") %>% View

# The above is still is not quite satisfying, as we would optimally compute a simple sum of ODA received in each region
wlddev %>%
  collap( ~ region + year, w = ~ POP,
         custom = list(fmode = cat_vars(., "indices"), 
                       fmean = .c(PCGDP, LIFEEX, GINI),
                       fsum_uw = "ODA")) %>% View

# Let's look at a more elaborate example using your Census Data
CENS <- readRDS("data/UBOS 2014 census.rds") # Uganda National Population and Housing Census 2014

fdim(CENS)
table(vclasses(CENS))
descr(CENS, table = FALSE) %>% as.data.frame %>% View
# The specialty of this data is that some variables are recorded in population totals, and some in percentage terms.

# Making sure all percentage variables are indeed on a percentage scale (gvr is a shorthand for get_vars(..., regex = TRUE))
gvr(CENS, "_P$") %>% fmax %>% range

# The population counts are easily aggregated by simply computing a sum, but variables providing percentages of the population need to be aggregated using a weighted mean, where the total population serves as the weighting variable.
# To aggregate this data with collap, we need to supply the names or indices of both percentage and non-percentage variables together with the corresponding aggregator functions in a list passed to the custom argument.
perc_vars <- gvr(CENS, "_P$", return = "indices")
pop_vars <- num_vars(CENS, "indices") %>% setdiff(perc_vars)
# Now aggregating to district level
CENS_district <- 
  collap(CENS, ~ Region + District, w = ~ POP,
         custom = list(fmean = perc_vars, fsum_uw = pop_vars), 
         keep.w = FALSE)

View(CENS_district)

# Variants of collap: 
  # - collapv for programming (using column names or indices of id and weight variables)
  ids <- c("Region", "District")
  w <- "POP"
  collapv(CENS, ids, w = w,
          custom = list(fmean = perc_vars, fsum_uw = pop_vars), 
          keep.w = FALSE)
  
  # - collapg for chained syntax and Non-Standard Evaluation (NSE)
  CENS %>% fgroup_by(Region, District) %>%
          collapg(w = POP, 
                  custom = list(fmean = perc_vars, fsum_uw = pop_vars), 
                  keep.w = FALSE)


# collap() lets us efficiently aggregate many different columns with many different functions including weights,
# and is the function to use for large and complex aggregations with multiple functions. 

# Sometimes we are however interested in much simpler tasks where we want to aggregate only a few columns but with 
# different functions. See:
?fsummarise
  
CENS %>% fgroup_by(Region, District) %>% 
    fsummarise(POP_TOT = fsum(POP), 
               male_HH_perc = fmean(HHEAD_M_P, w = POP))

  

#****************************
### In-Class Exercise 4 -----
#****************************

# In the data folder there is a STATA file with poverty estimates from the Uganda National Household Survey 2016/17

# (a) import this data using the haven library. Coerce all labeled variables to R factors by calling haven::as_factor on the imported dataset
library(haven)
UNHS_pov <- read_dta("data/UNHS_pov16.dta") %>% as_factor

# (b) describe the data, if possible utilize the weights 'finwalwgt' in the summary

# (c) Summarize the variable 'fexp30' and 'welfare' by 'urban', and panel-decompose the variation by 'district', use the weights 'finalwgt'

# (d) Compute the median, 1st and 2rd quartile of 'welfare' by 'regurb'. 
# TIP: Use fsummarise and fnth, also utilize the weights 'finalwgt' 

# (e) Compute an identifier indicating whether a household is wealthier than the median household in the same 'regurb' group.

# (f) Aggregate the data to the district level. Remove the columns 'hhid' and 'ea' beforehand,
#     utilize the weights 'finalwgt'. 




# Concluding remarks: 
# collapse is a nice and very fast package that allows us to do advanced statistical computations
# on vectors, matrices and data frames. However, it has some limitations that kick in 
# especially if we want to perform arbitrary computations on data by groups, combine (join) or reshape
# datasets etc., compute rolling statistics or to efficiently read and write files. 

# In the following we will see two more important packages that help us improve on data management and 
# more flexible computing, and with time series analysis.   
  

# (4) Data manipulation and management with 'data.table' -------------------
#***************************************************************************

library(data.table)
?`data.table-package`
?data.table

GGDC10S <- collapse::GGDC10S
setDT(GGDC10S)
GGDC10S

# data.table builds on base R functionality to reduce 2 types of time:
# programming time (easier to write, read, debug and maintain), and
# 
# compute time (fast and memory efficient).
# 
# The general form of data.table syntax is:
#   DT[ i,  j,  by ] # + extra arguments
#      |   |   |  
#      |   |   -------> grouped by what?
#      |    -------> what to do?
#      ---> on which rows?
#   The way to read this out loud is: "Take DT, subset rows by i, then compute j grouped by by. Here are some basic usage examples expanding on this definition. See the vignette (and examples) for working examples.
# 
#     X[, plot(a, b), by=c]       # j accepts any expression, generates plot for each group and returns no data


# Some simple examples
  # (1) Subsetting (same as collapse::fsubset)
  GGDC10S[Country == "BWA", ]
  fsubset(GGDC10S, Country == "BWA")
  # Same thing (similar to pandas in python). 
  GGDC10S[Country == "BWA"]
  qDF(GGDC10S)[GGDC10S$Country == "BWA", ] # Need to do this with data.frame (collapse::qDF() is a fast converter to data.frame)
  
  GGDC10S[1:3]           # Note that this give the first 3 rows for a data.table
  qDF(GGDC10S)[1:3]      # If we have a data.frame, this gives the first 3 columns !! (so know what objects you are dealing with. The different print methods can tell you, and there is a function is.data.table() to check.)
  
  
  GGDC10S[Regioncode == "SSA" & Year > 1990L]
  # Also selecting columns
  GGDC10S[Regioncode == "SSA" & Year > 1990L, AGR:SUM]
  GGDC10S[Regioncode == "SSA" & Year > 1990L, c("Regioncode", "Year", "AGR")]
  GGDC10S[Regioncode == "SSA" & Year > 1990L, list(Regioncode, Year, AGR)]
  GGDC10S %>% fsubset(Regioncode == "SSA" & Year > 1990L, Regioncode, Year, AGR:SUM) # collapse way
  # .() is an alias for list() in data.table
  GGDC10S[Regioncode == "SSA" & Year > 1990L, .(Regioncode, Year, AGR)]
  # Can rename columns (see also collapse::frename)
  GGDC10S[Regioncode == "SSA" & Year > 1990L, 
          .(Country, Regioncode, Year, Agriculture = AGR, Mining = MIN)] %>% View
  # Ordering rows
  GGDC10S[order(Country, -Year)] %>% View        # Using data.table
  GGDC10S %>% roworder(Country, -Year) %>% View  # Using collapse (supports pipes)
  setorder(GGDC10S, Country, -Year)     # Another data.table way: Ordering by reference, most efficient!
  
  # Some simple Computations (can use base R functions, but need to use na.rm = TRUE). Computations are internally optimized 
  GGDC10S[, .(sum_AGR = sum(AGR, na.rm = TRUE), mean_AGR = mean(AGR, na.rm = TRUE))]
  GGDC10S %>% fsummarise(sum_AGR = fsum(AGR), mean_AGR = fmean(AGR)) # collapse way
  # Grouped computations  
  GGDC10S[, .(sum_AGR = sum(AGR, na.rm = TRUE), 
              mean_AGR = mean(AGR, na.rm = TRUE)), keyby = Region]
  GGDC10S %>% fgroup_by(Region) %>%  # collapse way
    fsummarise(sum_AGR = fsum(AGR), mean_AGR = fmean(AGR)) 
  # Using 'keyby' instructs data.table to order the result. We can also use 'by' which will order results in order of first appearance.
  GGDC10S[, .(sum_AGR = sum(AGR, na.rm = TRUE), 
              mean_AGR = mean(AGR, na.rm = TRUE)), by = Region]
  GGDC10S %>% fgroup_by(Region, sort = FALSE) %>%  # collapse way
    fsummarise(sum_AGR = fsum(AGR), mean_AGR = fmean(AGR)) 
  
  # A nice feature of data.table is that we can compactly compute columns by reference using `:=`
  ?`:=`
  GGDC10S[, sum_AGR := sum(AGR, na.rm = TRUE)]
  GGDC10S[, c("sum_AGR", "sum_MIN") := list(sum(AGR, na.rm = TRUE), sum(MIN, na.rm = TRUE))] # Multiple columns
  GGDC10S[, `:=`(sum_AGR = sum(AGR, na.rm = TRUE),     # Same thing
                 sum_MIN = sum(MIN, na.rm = TRUE))]
  
  GGDC10S
  settransform(GGDC10S, sum_AGR = fsum(AGR), 
                        sum_MIN = fsum(MIN)) # collapse way
  
  # by groups
  GGDC10S[, sum_AGR := sum(AGR, na.rm = TRUE), by = Region]
  GGDC10S
  settransform(GGDC10S, sum_AGR = fsum(AGR, Region, TRA = "replace_fill")) # collapse way (provides more options through the `TRA` argument, but also more tedious)
  
  GGDC10S[, `:=`(sum_AGR = sum(AGR, na.rm = TRUE),     # Multiple columns
                 sum_MIN = sum(MIN, na.rm = TRUE)), by = Region]

  settransform(GGDC10S, list(sum_AGR = AGR, sum_MIN = MIN) %>% 
                        fsum(Region, TRA = "replace_fill")) # collapse way)
  
  # Another option:
  GGDC10S[, c("sum_AGR", "sum_MIN") := lapply(.SD, sum, na.rm = TRUE), .SDcols = c("AGR", "MIN")] # Multiple columns
  # And another collapse way..  
  settransform(GGDC10S, list(AGR, MIN) %>% 
                        lapply(sum, na.rm = TRUE) %>% 
                        setNames(c("sum_AGR", "sum_MIN")))

    
  # Demeaning a variable by group
  GGDC10S[, demean_AGR := AGR - mean(AGR, na.rm = TRUE), by = .(Variable, Country)]
  GGDC10S
  settransform(GGDC10S, demean_AGR = fwithin(AGR, list(Variable, Country))) # In this case collapse is more efficient
  # Computing a lagged variable by group
  setorder(GGDC10S, Country, Variable, Year)
  GGDC10S[order(Year), lag_AGR := shift(AGR), by = .(Variable, Country)]
  GGDC10S
  settransform(GGDC10S, lag_AGR = flag(AGR, 1, list(Variable, Country), Year)) # In this case collapse is also more efficient
  
  # Grouping by multiple columns:
  GGDC10S[, .(median_AGR = median(AGR, na.rm = TRUE)), by = .(Variable, Country, Region)]
  GGDC10S[, .(median_AGR = median(AGR, na.rm = TRUE)), by = c("Variable", "Country", "Region")]
  
  # What if we want to do multiple operations?: Chaining 
  GGDC10S[, 
    .(median_AGR = median(AGR, na.rm = TRUE)), by = Region
  ][
    order(median_AGR)
  ][
    Region == "Europe"
  ]
  
  # data.table is powerful if we want to mix subsetting, grouping and some computation
  GGDC10S[Variable == "VA" & Year > 1990L, .(median_AGR = median(AGR, na.rm = TRUE)), by = Region][order(median_AGR)]
  # The collapse equivalent is much longer and also less efficient as we subset the entire data.frame first whereas data.table internally optimizes to subset only 'Region' and 'AGR'
  GGDC10S %>% fsubset(Variable == "VA" & Year > 1990L) %>% fgroup_by(Region) %>% fsummarise(median_AGR = fmedian(AGR)) %>% roworder(median_AGR)
  # This would be a more efficient collapse solution, but for these kind of things data.table is really more flexible and parsimonious
  GGDC10S %>% fsubset(Variable == "VA" & Year > 1990L, Region, AGR) %>% fgroup_by(Region) %>% fsummarise(median_AGR = fmedian(AGR)) %>% roworder(median_AGR)
  
  # What if we want to aggregate over multiple columns using data.table?  
  GGDC10S[Variable == "VA" & Year > 1990L, 
          lapply(.SD, median, na.rm = TRUE), 
          by = Region, .SDcols = AGR:SUM]
  # .SD means "subset of data.frame". Below again an equivalent collapse expression.
  GGDC10S %>% fsubset(Variable == "VA" & Year > 1990L, Region, AGR:SUM) %>% 
    fgroup_by(Region) %>% fmedian
  
  # Now the real advantage of data.table comes with computing arbitrary expressions by group
  
    # This computes pairwise correlations between the 10 sectors by variable and country
    GGDC10S[, qDT(pwcor(.SD)), by = .(Variable, Country), .SDcols = AGR:SUM] %>% View
    # This Estimates a grouped linear regression of Agricultural employment on employment in mining and manufacturing
    library(lmtest, include.only = "coeftest") # Importing only the coeftest function from the lmtest library
    GGDC10S[Variable == "EMP", qDT(coeftest(lm(AGR ~ MIN + MAN)), "Coef"), by = Country] %>% View
    # Same thing in growth rates, mixing data.table and collapse
    GGDC10S[Variable == "EMP", qDT(coeftest(lm(G(AGR) ~ G(MIN) + G(MAN))), "Coef"), by = Country]
    # Another way of doing the same thing. Note that I use fgrowth instead of G() because G() by default adds a "G1" prefix to columns.  
    GGDC10S[Variable == "EMP", qDT(coeftest(lm(AGR ~ ., fgrowth(.SD, t = Year))), "Coef"), by = Country, .SDcols = .c(AGR, MIN, MAN)]
    # This runs the regressions by region using country fixed effects (centering columns by country)
    GGDC10S[Variable == "EMP", qDT(coeftest(lm(AGR ~., fwithin(.SD, Country))), "Coef"), by = Region, .SDcols = .c(AGR, MIN, MAN)]
    # Or growth rates
    GGDC10S[Variable == "EMP", qDT(coeftest(lm(AGR ~., fgrowth(.SD, g = Country, t = Year))), "Coef"), by = Region, .SDcols = .c(AGR, MIN, MAN)]
    # We can also use pipes inside the data.table
    GGDC10S[Variable == "EMP", 
            .SD %>% fgrowth(g = Country, t = Year) %>% lm(formula = AGR~.) %>% coeftest %>% qDT("Coef"), 
            by = Region, .SDcols = .c(AGR, MIN, MAN)]
    # Final example: Also harmonizing the growth rates in terms of mean and standard deviations
    GGDC10S[Variable == "EMP", 
            .SD %>% fgroup_by(Country) %>% fgrowth(t = Year) %>% 
              fscale(sd = "within.sd") %>%
              lm(formula = AGR ~ MIN + MAN) %>% coeftest %>% qDT("Coef"), 
            by = Region, .SDcols = .c(Country, AGR, MIN, MAN)] %>% View
    
    # We have seen the special symbol .SD allowing us to to operations on a subset of the data.table
    # Another special symbol is .N, which refers to the number of rows (or the last row).
    ?.N
    GGDC10S[, .N] # Number of rows
    GGDC10S[.N]   # Subsetting i with the number of rows gives the last row
    GGDC10S[, .N, by = Region]      # Number of obs by region
    GGDC10S[order(Country, Year), .SD[.N], by = Region] # Last row by region
    # A neat example of this: run the grouped regression only for countries with more than 60 obs of data    
    GGDC10S[Variable == "EMP", 
            if (.N > 60L) qDT(coeftest(lm(AGR ~ MIN + MAN)), "Coef"), 
            by = Country] %>% View
    
    # Now: remember this summarizing exercise:
    rsplit(GGDC10S, ~ Variable + Region) %>%      # Split data
      rapply2d(. %>% descr %>% as.data.frame) %>% # Apply descriptive statistics, coerce them to data frame 
      unlist2d(.c(Var, Region), DT = TRUE)        # row-bind result back to data table
    # Everything that results in a matrix or data.frame can be more efficiently done with data.table
    GGDC10S[, as.data.frame(descr(.SD)), by = .(Var = Variable, Region)]
    # So we only need to actually split if we want output other than a data frame 
    desc <- rsplit(GGDC10S, ~ Variable + Region) %>%      # Split data
            rapply2d(descr)                               # Apply descriptive statistics, with printout
    
    desc$VA$Asia
    desc$VA$Europe
    # Recall our grouped linear regressions
    GGDC10S[Variable == "VA", qDT(coeftest(lm(AGR ~ MIN + MAN)), "Coef"), by = Country] %>% View
    # If we want the full models with a printout, we can use either summary() on the lm object, or the nicer jtools::summ()
    modlist <- GGDC10S %>% 
      fsubset(Variable == "VA", Country, AGR, MIN, MAN) %>%
      rsplit( ~  Country) %>% 
      rapply2d(lm, formula = AGR ~ MIN + MAN)
    
    summary(modlist$TZA)
    jtools::summ(modlist$KEN) # Need to install.packages("jtools")

   # A final example: Grouped estimation and plotting
    par(mfrow = c(2, 3))
    GGDC10S[Variable == "VA", .(MIN_coef = coef(lm(AGR ~ MIN + MAN))["MIN"]), keyby = .(Region, Country)][, 
            if(.N > 2L) plot(density(MIN_coef), main = .BY[[1L]][1L]), by = Region]
    par(mfrow = c(1,1))
    # see ?.BY
    
        
#****************************
### In-Class Exercise 5 -----
#****************************

# Data Table also provides an efficient file reader for delimited data (CSV files)
?fread
# This reads CSV from the Bureau of Transporation Statistics for all the flights that departed from New York City airports in 2014 
flights <- fread("https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv")

# (a) Save this file in the data subfolder of this course under the name 'flights14.csv' using the function fwrite
?fwrite

# (b) Work through the 'Introduction to data.table' vignette which is based on this data.
# make sure you get the same results and understand them.

# Bonus: Also work through the 'Reference semantics' vignette



# Joining tables ----------------------------------------------------------------------------------

X <- data.frame(id = rep(1:3, each = 2L),
                a = sample(letters, 6L),
                b = sample(LETTERS, 6L)) %>% setDT

Y <- data.frame(id = 1:3,
                c = month.name[1:3],
                d = month.abb[1:3])  %>% setDT

# By default merge() does inner join on common variables
merge(X, Y)
X[Y, on = .NATURAL] # data.table way
merge(Y, X)
Y[X, on = .NATURAL]
# Alternatively: Specifying variables:
merge(X, Y, by = "id")
X[Y, on = "id"]
merge(Y, X, by = "id")
Y[X, on = "id"]
# If different names, can specify:
setrename(Y, id = idy)
merge(X, Y, by.x = "id", by.y = "idy")
X[Y, on = c("id" = "idy")]
merge(Y, X, by.y = "id", by.x = "idy")
Y[X, on = c("idy" = "id")]

# From the FAQ vignette: 1.11
vignette('datatable-faq')
# X[Y] is a join, looking up X’s rows using Y as an index.
# Y[X] is a join, looking up Y’s rows using X as an index.
# 
# merge(X,Y) does both ways at the same time. The number of rows of X[Y] and Y[X] usually differ, whereas the number of rows returned by merge(X, Y) and merge(Y, X) is the same.

# The latter is more flexible
setrename(Y, idy = id)
X[Y, on = "id", mult = "first"] 
X[Y, on = "id", mult = "last"]  

# No match
Y[id == 3L, id := 4L][] # Note: I just add [] to print the result, the column is modified by reference. See the vignette on reference semantics. 
X[id == 1L, id := NA][]

# The match way:
merge(X, Y)                # Inner join: Only keeps matching rows
merge(X, Y, all.x = TRUE)  # Left join: Keeps rows in X
merge(X, Y, all.y = TRUE)  # Right join: Keeps rows in Y
merge(X, Y, all = TRUE)    # FUll join: Keeps all rows from all tables
# What if we want to do a full join and generate a 'merge' variable as in STATA?
X$merge_X <- 1
Y$merge_Y <- 2
Z <- merge(X, Y, all = TRUE)    # FUll join: Keeps all rows from all tables
Z[, merge := kit::psum(merge_X, merge_Y, na.rm = TRUE)] # This generates 'merge' variable

# The data.table way:
X[Y, on = "id", nomatch = NULL]
Y[X, on = "id"]
X[Y, on = "id"]
# Full join is difficult in the data.table way: use merge()


# Now: A real world example:
# This downloads a package containing a database of flights for 3 airports in New York City in 2013
install.packages("nycflights13")
# It is similar to the data in Exercise 5, but here we have more 
# This gets the table of flights and coerces in to a data.table    
airlines <- as.data.table(nycflights13::airlines)
airports <- as.data.table(nycflights13::airports)
flights <- as.data.table(nycflights13::flights)
planes <- as.data.table(nycflights13::planes)
weather <- as.data.table(nycflights13::weather)

airlines
airports
flights
planes
weather

# There are 2 ways we can join tables using data.table: either using merge(), or using the 'on' argument inside data.table

?merge.data.table  # Describes merge()
?data.table        # Describes natural data.table syntax

# Joining the airlines table to flights 
merge(flights, airlines)  %>% View         # by default merge() figures out common columns between the two tables, and then returns all the non-missing rows where these columns match (= inner join in SQL language)
flights[airlines, on = .NATURAL]   # The native data.table syntax is more arcane. Basically the idea is to subset flights with the matching rows of airlines on columns on.

merge(flights, airlines, by = "carrier") # Explicitly specifying the shared columns
flights[airlines, on = "carrier"]        # Same thing

# The latter is more flexible
flights[airlines, on = "carrier", mult = "first"] %>% View # Joining the first flight by carrier
flights[airlines, on = "carrier", mult = "last"]  # Joining the last flight by carrier

# We can join the Other way around
merge(airlines, flights, by = "carrier")                 
airlines[flights, on = "carrier"]        # Same thing

# Now we introduce a missing value for one carrier in airlines, and flights
airlines[carrier == "UA", carrier := NA][] # Note: I just add [] to print the result, the column is modified by reference. See the vignette on reference semantics. 
flights[carrier == "MQ", carrier := NA][]

# Merge by default gives us an inner join, where NA is match against NA. So this is wrong!!
merge(flights, airlines)
# We have to remove the missing value from one of the data table's to get the inner join without missing values. 
merge(flights, airlines[!is.na(carrier)]) # It is best to do this on the smaller dataset
flights[airlines[!is.na(carrier)], on = "carrier"]

# We can do a left join, which keeps all the rows of flights (the left table in merge)
merge(flights, airlines[!is.na(carrier)], by = "carrier", all.x = TRUE)
airlines[!is.na(carrier)][flights, on = "carrier"] # Same thing, just need to index by flights to retain all the rows of flights

# The opposite operation is the right join which retains all rows in airlines with matching valies in flights
merge(flights, airlines[!is.na(carrier)], by = "carrier", all.y = TRUE)
flights[airlines[!is.na(carrier)], on = "carrier"]   # Same thing, note that the carrier column is not moved to the front as in merge. 

# The final join operation we want to examine is the full join, which returns all rows from both datasets, hether they match or not
merge(flights, airlines[!is.na(carrier)], by = "carrier", all = TRUE) # Adds one extra row for the airline not in flights
# There is not straightforward way to do a full join in the native way. 



#****************************
### In-Class Exercise 6 -----
#****************************

# Join all the tables in this database together into one big table. 
# (Note: there are multiple ways to do it, any that combines all tables will suffice)



# Concluding remarks:
# data.table is a very powerful and flexible package and it is widely used. It's strength are data management,
# subsetting, arbitrary computations by groups (aggregations, transformations, estimations etc.), and for 
# joining and reshaping data (we will cover reshaping tomorrow). It has some shortcomings in terms of 
# statistical computing (doesn't support matrices), complex and weighted aggregation, and panel data 
# where collapse is needed. collapse on the other hand cannot compute arbitrary expressions by groups and 
# cannot be used to join datasets. Fortunately both packages work very well together, and you should be using both.





# (5) Time series (with 'xts') ---------------------------------------------
#***************************************************************************

# Now we will get some Ugandan time-series data into R, with the help of both collapse and data.table and magrittr
library(collapse)
library(data.table)
library(magrittr)
library(readxl)
# This downloads the Monthly macroeconomic indicators excel sheet from Bank of Uganda and saves it in the data folder
download.file("https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/MacroeconomicIndicators/Disseminated-Indicators-file_Web-version.xlsx", 
              destfile = "data/BOU_MMI.xlsx", mode = 'wb')
# This reads the monthly sheet
MMI <- read_xlsx("data/BOU_MMI.xlsx", sheet = "Monthly", skip = 3L, .name_repair = "none")
setDT(MMI)                                           # Setting as data.table
MMI <- MMI[rowSums(!is.na(MMI[, -1L])) > 0]          # Removing rows missing on all but the first column 
MMI_labs <- read_xlsx("data/BOU_MMI_labels.xlsx")    # Getting an excel sheet with variable codes and labels
MMI %<>% add_vars(MMI_labs, pos = "front") %T>% View # This sheet was set up to match the data, but we can still visually check
MMI %<>% get_vars(-(2:3)) %>%                        # This is key, we only retain the 'Variable' identifier,...
 transpose(keep.names = "Date", make.names = "Variable") %>%  # This transposes the data table (data.table::transpose)
 dapply(as.numeric)                                  # This makes sure all columns are numeric
MMI[, Date := as.Date(Date, origin = "1899-12-30")]  # This coerces the Date variable to a proper date. Note that the origin setting is Windows specific, see ?as.Date.numeric
vlabels(MMI)[-1L] <- MMI_labs[[2L]]                  # This assigns the labels 

View(MMI)
qsu(MMI)  # Sumamrise the data

# So this is a data.table of time series, which we can manipulate using data.table or collapse, or a combination of both
MMI[Date > "2000-01-01", .(Date, fgrowth(.SD, 12L, t = Date)), .SDcols = -1L]   # Computing growth rates, the data.table way
MMI %>% fsubset(Date > "2000-01-01") %>% 
  G(12L, t = ~ Date, stubs = FALSE) %>% View      # The collapse way

# data.table also has some functions for rolling statistics e.g. this calculates 12-month rolling averages
MMI[Date > "2000-01-01", c(list(Date = Date), lapply(.SD, frollmean, n = 12L)), .SDcols = -1L] %>% View
?frollmean  # See documentation

# We can plot this whole data using ts.plot, but that is rather tedious... we need to standardize all columns first to put series on the same scale, and manually draw the legend
num_vars(MMI) %>% STD %T>% 
  ts.plot(col = rainbow(fncol(.)), main = "Standardized Monthly Macroeconomic Indicators") %T>% {
    legend("topleft", names(.), col = rainbow(fncol(.)),
           ncol = 4, cex = 0.7, lty = 1, bty = "n")
  }

# Plotting selected series: Inflation, CIEA, BTI and Trade Balance
MMI[Date > "2010-01-01", .(CPI_HL_09, CIEA, BTI, TB)] %>% 
  plot.ts(main = "Selected Series")

# So ts.plot plots all series on one chart and plot.ts creates a facettes chart
# Note however that we are missing proper labels for the time axis.

# To get these, we need to coerce this data to base R's time series class called 'ts'. 
range(MMI$Date) # range gives the minimum and maximum
# For ts() we need to specify the starting and ending year and month and the frequency
MMI_ts <- ts(num_vars(MMI), start = c(1986, 1), end = c(2021, 2), frequency = 12)
MMI_ts
str(MMI_ts)
# This selects series, and plots them from 2010
MMI_ts[, .c(CPI_HL_09, CIEA, BTI, TB)] %>% 
  window(start = 2010) %>% 
  plot(main = "Selected Series")
# Note that now we have the proper axis
# This plots the annual growth ofselected series since 2015:
MMI_ts[, .c(CPI_HL_09, CIEA, BTI, TB)] %>% 
  fgrowth(12L) %>%
  window(start = 2015) %>% 
  plot(main = "Selected Series Growth", col = "blue")

# So base R's time series class gives us some options to subset time series using the window() function,
# and to plot them using the basic plot() function (which calls the method plot.ts), or, alternatively, ts.plot which plots all series in one chart
# However, the conversion to this class is tedious, we need to manually specify stard and end date and month, 
# and we need to make sure out data is sorted by time. Subsetting and plotting capabilities of 'ts' are also limited.

# So this is where 'xts', shorthand for 'extensible time series' comes in. 
install.packages("xts")
library(xts)
# xts is a truly time-based class, and conversion from data.table is very easy if you have a 'Date' column:
MMI_xts <- as.xts(MMI)
MMI_xts
str(MMI_xts) # The 'xts' class is basically a matrix where a date variable attached in an 'index' attribute provides for the time dimensions and exact ordering of the data
str(attributes(MMI_xts))
index(MMI_xts) # This gets the 'xts' index, which is simply the date column in the data. 
# Now look at that plot
MMI_xts %>% STD %>% plot(main = "Standardized Series")
# xts can be subset like a data.frame or data.table, with the difference that we can specify sequences in quotes
MMI_xts["2015/", .c(CPI_HL_09, CIEA, BTI, TB)] %>% 
  plot(multi.panel = TRUE, yaxis.same = FALSE)     # in 'xts', the default for plot() is to plot all series in one chart, we have to specify these arguments to get a facetted chart
?plot.xts
# We can coerce a suitable data.table with a 'Date' column to xts using as.xts. For other cases there is the constructor function xts()
?xts
# Getting out 4 series, calling it X
X <- MMI %$% xts(cbind(CPI_HL_09, CIEA, BTI, TB), order.by = Date, frequency = 12L)
X
# 'xts' subsetting more formally:
X['2007']  # all of 2007
X['2007/']  # 2007 to end of dataset
X['2007-03/']  # March 2007 to the end of the data set
X['2007-03/2007']  # March 2007 to the end of 2007
X['2007-03/2008-04']
X['/'] # the whole data set
X['/2007'] # the beginning of the data through 2007
X['2007-01-01'] # just the 3rd of January 2007

# 'xts' also provides some time series manipulation functions
nyears(X)
nmonths(X)
na.omit(X) # Remove any rows with missing values
# Aggregating the time series to quarterly frequency using the mean
apply.quarterly(X, mean, na.rm = TRUE) 
# Aggregate to annual
apply.yearly(X, mean, na.rm = TRUE)  
# First and last value
last(MMI_xts)
first(MMI_xts)
# Lag and first-difference
lag(MMI_xts)
diff(MMI_xts)

# more function that apply to 'xts' are found in the 'zoo' package on which xts is based
?zoo
na.fill(X, 0) # This is a zoo function to pad NA's with some value
na.spline(X)  # Some interpolation options as well..
na.aggregate(X) # Replace NA by aggregation..
# Summary
summary(X)
# Rolling functions
rollmean(X, 12L)      # rolling annual means
rollapply(X, 12L, sd) # rollung standar deviations (cal roll any function in this way)
# Original:
plot(na.omit(X), multi.panel = TRUE, yaxis.same = FALSE)     
# Rolling mean:
rollmean(X, 12L) %>% na.omit %>% plot(multi.panel = TRUE, yaxis.same = FALSE)

# collapse functions also apply to xts:
qsu(X)
descr(X)
fmean(X)
fmedian(X)
fsd(X)
fscale(X)  # Standardizing, can also use STD() operator which adds a 'STD.' prefix to variable names
fwithin(X) # Centering, can also use W() operator which adds a 'W.' prefix to variable names
flag(X)    # lag , can also use L() operator, F() for leads
flag(X, c(1,12)) # multiple lags
fdiff(X, c(1,12)) # difference, cna also use D() operator
fgrowth(X, c(1,12)) # growth rates (montly and YoY), can also use G() operator
fgrowth(X, 12, power = 1/12) # Compounded annual growth rate
fgrowth(X, power = 12)       # Annualized monthly growth rate
replace_outliers(X, 3)       # Replace outliers above or below 3 standard deviations from the mean. etc..

# Plot annual growth rates: See the COVID shock
MMI_xts["2015/", .c(CPI_HL_09, CIEA, BTI, TB)] %>% 
  fgrowth(12L) %>%
  plot(multi.panel = TRUE, yaxis.same = FALSE)     

# Another useful thing about xts is the time-based merge functionality
# This creates another xts containing exports and imports of goods and services
Y <- MMI %$% xts(cbind(EX_G, IM_G, EX_S, IM_S), order.by = Date, frequency = 12L)
qsu(Y)
plot(na.omit(Y), legend.loc = "topleft")
# Simple time-based merge: Nothing to worry about as long as the format of the index() is the same
merge(X, Y) %>% View
na.omit(merge(X, Y)) %>% plot
# Let's see what happens when we merge a monthly and a quarterly series...
merge(X, apply.quarterly(Y, mean)) %>% View

# Other packages:
library(roll) # Super fast rolling functions (for many time series), also apply to xts
roll_mean(X, 12L) %>% na.omit %>% plot(multi.panel = TRUE, yaxis.same = FALSE)
roll_median(X, 12L) %>% na.omit %>% plot(multi.panel = TRUE, yaxis.same = FALSE)
roll_quantile(X, 12L, p = 0.75) %>% na.omit %>% plot(multi.panel = TRUE, yaxis.same = FALSE)
# Rolling standard deviations
roll_sd(X, 12L) %>% na.omit %>% plot(multi.panel = TRUE, yaxis.same = FALSE)

library(dygraphs) # dynamic time series charts
dygraph(na.omit(X))
fgrowth(X, 12L) %>% na.omit %>% dygraph

# Concluding remarks: 
# 'xts' is a very useful matrix-based time series class, easy to create, manipulate, subset, merge and plot,
# it interfaces well with 'collapse' and can easily be created from a data.table. 
# It also has some supporting packages like 'roll' or 'dygraphs'. 





