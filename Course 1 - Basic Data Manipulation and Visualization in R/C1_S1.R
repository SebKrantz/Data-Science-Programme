##########################################################
# ************ R BASED DATA SCIENCE TRAINING *************
# Course 1: Basic Data Manipulation and Visualization in R
# --------------------------------------------------------
# Prepared by: Sebastian Krantz, ODI Fellow, MEPD, MoFPED
# Permanent Contact: sebastian.krantz@graduateinstitute.ch
##########################################################

# (1) What is R ? ----------------------------------------
# ********************************************************

#  R is an interpreted object-oriented programming language based on C/C++ and Fortran (mostly C). 
#  To understand computations in R, two slogans are helpful:
#  Everything that exists is an object, all objects are in the Global Environment!
#  Everything that happens is a function call (everything you do with an object).
#
#  Being interpreted means you can execute code on the fly. When you call a function on 
#  and object in R, the interpreter will interpret and execute the code of that function
#  (which usually contains calls to other lower-level functions). All pieces of code are 
#  eventually composed of some low-level functions (written in C/C++ or Fortran)
#  which carry out the actual computations. These so called "primitive" or "internal" functions
#  written in C/C++ or Fortran were compiled beforehand, which means the computer knows 
#  exactly what to do when that function is called.
#
#  Object orientation means that a specific function (called the 'generic' function) can call
#  different internal functions (called 'methods') depending on the R object the function is applied to.
#  The process of selecting the right method for an object is called 'method dispatch' and also
#  carried out via the interpretor.
#
#  By the end of this course it will be clear to you what this means.
#

## First things first -------------------------------------------------------
# First, lets set the Working Directory
getwd() # find out where your current directory is
wd <- choose.dir()
setwd(wd) # Navigate to the folder with the training materials. 

# A hashtag `#` in front of a line indicates a comment which is not evaluated
# You evaluate code by highlighting it and presssing ctrl + Enter of a PC (cmd + Enter on a mac),
# or by typing it directly into the console and pressing enter.
# R is case sensitive, so check your spelling of objects and function names.

# (2) Basic R Objects and Functions ----------------------------------------
#***************************************************************************

# In the R landscape, there are 3 basic types of objects which can contain data: 
# atomic vectors, lists and environments.
# Apart from that there are functions which perform tasks on data, and some special
# objects like formulas or unevaluated expressions which we will discuss much later. 

## (2.1) Atomic Vectors and Associated Functions ----------------------------

# Vectors are 1-dimensional sequences of individual (atomic) elements. 
# Vectors can be of different data types: numeric (integer / double), logical or character
# A vector can only have one data type

### Numeric Vectors ---------------------------------------------------------
## Double Precision Vector
5.6                     # All numbers in R are by default stored as double prevision values (so values where we can have decimal points behind the comma, not necessarily whole numbers, shich are also called integers)
x <- c(1, 2.5, 3.5)     # the'c' function conactenates elements into a vector
x                       # We just created a numeric vector x
rm(x)                   # This removes x again

x = c(1, 2.5, 3.5)      # '=' or '<-' have the same function of assigning a value to an object / creating objects
x
c(1, 2.5, 3.5) -> x     # the advantage of <- is that it can be used to assign in both directions
x                       # here we have recreated x by assigning the same values to it

y <- c(1, 2, 3)         # this also creates a numeric vector called y
mode(y)                 # this gives the type of the object
is.numeric(y)           # this function checks if the vector is numeric
typeof(y)               # this gives the type of the C-array storing the object
is.double(y)            # the equivalent function to check the storage type

## Integer Vector
y <- c(1L, 2L, 3L)      # adding `L` to a whole number in R indicates that it is integer 
mode(y)                 # (i.e. without decimal places). this therefore creates an integer vector
typeof(y)
is.numeric(y)           # check the mode, also numeric (as distinct from character or logical)
is.double(y)            # this is not stored as a double precision number
is.integer(y)           # but it is stored as an integer

z <- c(1L, 2L, 3)       # if one number in the vector is non integer, 
is.integer(z)           # the whole vector is stored as a C-array of double precision numbers
is.integer(as.integer(z)) # as.functions can be used to coerce vector types. 

x <- 1:3              # generating large sequences of integers is simplified by the colon `:` operator
x
typeof(x)
typeof(1:3)             # Same thing. If we use `:` we do not need to add `L` to declare values as integer
seq(1, 3)               # Another way to generate a sequence of numbers is using seq
identical(1:3, seq(1, 3)) # The function identical checks that these two vectors are identical
seq(2000, 2030, 2)      # seq is more flexible.

                        # In general, storing integer values in integer and not double precision vectors 
                        # has the advantage that integer vectors are only half as large:
y_l <- 1:1e7            # 10 million observations
object.size(y_l)        # Size of the object
object.size(c(y_l, 1L)) # Adding an integer value increases the size a bit
object.size(c(y_l, 1))  # But adding a double precision value (omitting L) doubles the size of the whole vector.
rm(y_l)                 # this is because vectors can only be of one type, and integers can be stored as doubles but not vice versa

### Some general stuff about coding -----------------------------------------
x                       # typing x and evaluating it is actually the same as calling the print function on the object
print(x)

y <- 1:3; y             # commands in same line must be separated with ';'
z <- c(x, y); z         # This concatenates x and y and assigns it to z (previous z is overwritten)

# Important functions to create vectors
`<-`; `=`       # Assignment (create or replace object)
c               # concatenate objects
rm              # remove objects
`:`             # Generate sequences of integers
seq             # More general sequences
                # Note: Apart from seq which is generic, all these functions are primitive that is they are called directly from C
`(`             # Even the brace operator to group values / contain expressions is a function in R. 

# Some important functions to examine atomic vectors
is.atomic(z)    # this function checks if an object is an atomic vector
length(z)       # Get or set the length of the vector (the number of elements)
mode(z)         # Get or set the R type / mode of an object.
typeof(z)       # Determines the (internal) type or C-level storage mode of an object
str(z)          # Compactly display the internal structure of any R object
is.numeric(z)   # and similarly is.otherdatatypes (e.g. is.character, is.logical).
is.double(z)    # We can further check if a numeric vector is stored as double precision or integer
as.double(z)    # 'as.-' functions convert vector types (any many other objects in R)
print(z)        # Prints to the console (only needs to be called explicitly in functions)
View(z)         # Open Object in Data viewer (calls as.data.frame() on the object)

#****************************
### In-Class Exercise 1 -----
#****************************

# (a) Generate an arbitrary numeric vector "v" and an integer vector "vi" of length 5 using the c() function

# (b) Check the mode and the storage type of the vectors using mode(), typeof() and the corresponding is._() functions

# (c) Check the structure with str() and the length of the vectors with length()

# (d) Replace "vi" with an integer sequence of values from 1 to 10 in steps of 2, using the seq() function

# (e) remove "v" and "vi" using rm()


### Mathematical Operations ------------------------------------------------
6.7 - 3.4               # you can use R as a calculator for numeric calculations
q <- 2L + 7L            # this creates an integer value q. This is stored as an integer vector of length 1.

z <- x + y; z           # Mathematical operations in R are vectorized
z <- x - y; z
-x                      # This simply negates all elements of x
z <- x * y; z
x * y + z               # As usual multiplication takes presence over addition
(x * y) + z             # Same thing 
x * (y + z)             # This is different
x / y^2                 # powers take precedence over multiplication and division
z <- (1:1000)^2         # This creates a sequence of squared values
plot(1:1000, z)         # This plots the sequence

# Mathematical functions operating on numeric vectors
x <- 1:10
y <- rnorm(10L) # This draws 10 values at random from a standard normal distribution (Gauss curve)
## scalar valued (statistical) functions
sum(x)
prod(x)
mean(x)
sum(x) / length(x) # Equivalent, faster way of calculating the mean
median(x)
quantile(x)
min(x)
max(x)
var(x)
sum((x-mean(x))^2) / (length(x)-1) # This manually computes the variance
sd(x)
sqrt(var(x)) # The sd is the square root of the variance
cov(x, y) # covariance of x and y
sum((x-mean(x))*(y-mean(y))) / (length(x)-1) # Manually computing the covariance of x and y
cor(x, y) # correlation of x and y: The covariance is normalized to take values between 1 and -1
cov(x, y) / (sd(x) * sd(y)) # This manually computes the correlation coefficient

## Vector valued functions
sqrt(x)      # Square root
x^(1/2)      # same thing
abs(-x)      # Absolute value
exp(x)       # exponentiate e^x
exp(1)^x     # same thing
log(x)       # natural logarithm
all.equal(exp(log(x)), x) # identical() here gives false because of numeric imprecision. we can use all.equal instead
log10(x)     # logarithm base 10
log(x, base = 10) # same thing
diff(x)      # first difference
scale(x)     # Standardize (center and scale = subtract mean and divide by standard deviation)
round(x + 0.54, 1) # Round to digits
floor(x + 0.5)     # Round down to nearest integer
ceiling(x + 0.5)   # Round up to nearest integer

# Other functions
y <- c(1L, 3L, 1L, 2L, 3L, 4L)
rep(1L, 4L)       # replicate values or vectors
rep(y, 10L)        
rep(y, each = 2L)
rev(y)            # reverse the order of elements in a vector
rle(rep(y, each = 2L)) # run-length encoding: computes the number of consecutive equal values
sort(y)           # sort the elements of y
order(y)          # compute the order of the elements = the index that would sort the data
unique(y)         # get unique values of a vector
table(y)          # tabulate (i.e. count the frequencies of unique values in a vector)
tabulate(y)       # Same thing, just returns the frequencies

### !!! Finding help on an R object !!! -------------------------------------
help(log)
?log         # Same thing more compact
?all.equal

?mtcars      # Help on a dataset

?"<-"        # We need to quote operators to find their help pages
?"+"

# For more general helo, start html help, or click on the 'help' panel in Rstudio
help.start()

### Special values ---------------------------------------------------------

x <- c(1, NA, 1/0, Inf/Inf, -Inf, NULL) 
x # NULL has disappeared -> It is Its own object, the null object
# NA = "Not Available" (missing value). NA is a logical constant which contains a missing value indicator. Any operation on it gives NA.
sum(NA)
NA + 1
mean(c(NA, 1, 2, 4.5)) # Statistical computations on NA give NA
mean(c(NA, 1, 2, 4.5), na.rm = TRUE) # But many statistical functions have an argument to remove missing values
# NAN = "Not a Number". NaN are the result of some infeasible mathematical operations, e.g. 0/0.
0/0
# Infinite values are generated when a computation unambigously yields infinity
1/0
-1/0
# There is also NULL, which is different from NA, NaN, Inf, -Inf. 
# NULL is the NULL object. It cannot be stored in a vector and has a length of 0 (i.e. it is empty).
length(NA)
length(NULL)
y <- NULL          # Creating an empty object
is.na(x)           # These functions check all vector elements
is.nan(x)          # Not that is.na also includes NaN's
is.finite(x)       # is.finite excludes all values that are not a normal number
is.infinite(x)     # is.infinite only includes infinite values (positive or negative)
is.null(x)         # NULL is its own object 
is.null(y)
# The important distinction between NA and NULL is that NA is a 'logical' value that when evaluated in an expression, yields NA.  
# This is the expected behavior of a value that handles logical indeterminacy.
# NULL is its own thing and does not yield any response when evaluated in an expression. 
# NULL is typically used to delete vectors in list-like structures (as we will see later)
# More here: https://www.r-bloggers.com/r-na-vs-null/


#****************************
### In-Class Exercise 2 -----
#****************************

# Problem 1: Consider two vectors of the weight and height of 5 people
weights <- c(80.5, 63.9, 75.3, 68.1, 79.3) # weights in kg
heights <- c(1.94, 1.80, 1.75, 1.71, 1.85) # heights in m

# (a) calculate the Body Mass Index, BMI = weight divided by the square of height, save it in a vector called "bmi"

# (b) Compute the mean, median and standard deviation of the BMI

# (c) Sort the BMI values, and compute the order of them

# (d) round the BMI values to the full integer, and compute a frequency table of them

# (e) look up the documentation of the trunc() function. When do floor() and trunc() give different results?

# (f) The R-Squared is a popular statistic for the goodness of fit of a regression model. 
#     Compute the R-Squared of the regression of weights on heights using the cor() function. 

# Problem 2: Consider three vectors of standardized characteristics of people
p1 <- c(0.06843254, -0.13153881,  0.76021149,  0.7880306775)
p2 <- c(0.79301235, -0.13153881,  0.81685914,  1.0504160307)
p3 <- c(1.03453895, -0.13153881,  0.81685914,  1.4439940605)

# (a) Use the Euclidian Distance = The square root of the sum of the squared differences between the vector elements
#     to compare person 1 to person 2 and person 3. Which of the two persons is more similar to person 1?

# (b) check your results against the following code
dist(rbind(p1, p2, p3))

# (c) Do another comparison of p1 with p2 and p3 by computing the correlation between them. 
#     Do you get the same result as from your distance calculation?

# (d) Consider these versions of p2 where some strange values are inserted. 
#     Compute the Euclidian distance between p1 and p2 as in part (a) for each p2 and make sure you understand the outcome.
p2 <- c(0.79301235, -0.13153881,  NA,  1.0504160307)

p2 <- c(0.79301235, -0.13153881,  -Inf,  1.0504160307)

p2 <- c(0.79301235, -0.13153881,  NULL,  1.0504160307)


### Character Vectors ------------------------------------------------------
chr <- c("Hello", "how", "are", "you", "?", "")
is.character(chr)
chr         # same as print(chr)
cat(chr)    # concatenate and print
length(chr)
nchar(chr)  # number of characters
nzchar(chr) # detects non-zero elements in the character vector
toupper(chr) 
tolower(chr) 
            # trimws() removes trailing or leading white spaces:
c(" dfdf ", trimws(" dfdf "))
            # Can also use single quotes
chr2 <- c('hello', 'how', 'are', 'you', '?', '') 
b <- "I'm"; b # to display ticks, we need to nest them.
b <- 'he"l"lo'; b

avec <- rep("a", 10L) # replicate character values
avec

chr3 <- c(chr, chr2) # concatenating character values
chr3

letters     # Some special character vectors present in R
LETTERS
month.abb
month.name
            # The paste function allows more flexible combining of vectors
paste(month.abb, month.name)
paste0(month.abb, month.name) # without the space
paste(month.abb, month.name, sep = ". - ") # custom separator
paste(month.abb, collapse = " ") # paste function with collapse argument to join elements of the vector to a single string
paste0(month.abb, " (", month.name, ")") # constructing stings 
paste("Today is", date()) # date() returns the current date as a character string

            # We can create month.abb ourselves by taking the characters 1 through 3 of each vector element
m.abb <- substr(month.name, 1L, 3L)
m.abb
identical(m.abb, month.abb)
            # match elements at the start and end point of strings
startsWith(month.name, "J")
endsWith(month.name, "ber")
grepl("ctob", month.name)  # we can search for characters within strings 
grepl("ctob|cembe", month.name) # searching for 'ctob' and 'cembe'
gsub("ctob|cembe", "", month.name) # removing these elements from the string  
# many more options for pattern matching and replacement in strings, see ?gsub

            # alphabetic sorting also works with strings
sort(month.abb)
order(month.abb) # order again gets the index of elements that would sort the data
            # match returns a vector of the positions of (first) matches of its first argument in its second.
match(c("b", "w"), letters) 


#****************************
### In-Class Exercise 3 -----
#****************************

# Consider The following vector of 4 Ugandan districts
districts <- c(" Abim.", "bulisa", "MUKONO? ", "   Arua__")

# (a) Remove all leading and trailing white space, save your result replacing districts

# (b) remove all irregular characters (".", "?" and "__"). Note that you have to escape some characters by preceding them with "\\" 

# (c) Turn all strings into upper case letters

# (d) Now capitalize only the first letter using the function tools::toTitleCase()

# (e) Check the number of characters in each district name, and which names start with "A"

# (f) Sort the cleaned districts in descending alphabetic order


### Logical Vectors: -------------------------------------------------------
# => Contain logical statements TRUE or FALSE or missing values

lvec <- c(TRUE, FALSE, NA, NA)
          # Any comparison using >, <, <=, >=, == or !=, 
          # or using functions starting with 'is.-' e.g. is.na, is.numeric etc. generates logical vectors
lvec2 <- c(3 > 8, 9 + 2 == 10, 15 >= 3 * 5, FALSE)
lvec2

lvec == lvec2 # These logical comparisons are also vectorized. Comparison with NA values always returns NA. 
1:3 != 1:3
month.abb == substr(month.name, 1L, 3L)
              # Note that you need == for logical evaluation, simple = means assignment (same as <-)
lvec3 = lvec == lvec2 # What does this do?
    # There are a number of logical operators, see ?"&"
lvec & lvec2  # Logical AND. 
TRUE & TRUE
TRUE & FALSE
TRUE & NA
FALSE & NA    # Note this behavior: NA is a missing / undetermined TRUE or FALSE value, so this is always FALSE not matter what NA is
TRUE && FALSE # If we know both vectors are of length 1, we should use &&
lvec && lvec2 # Using && on vectors of length > 1 compares only the first elements to the two vectors

lvec | lvec2  # Similarly there is logical OR
TRUE | TRUE
TRUE | FALSE
TRUE | NA     # Note this behavior (same explanation as above: TRUE or NA (=undetermined) is TRUE)
FALSE | NA    
TRUE || FALSE 
lvec || lvec2 

isTRUE(TRUE)  # A safe way to check if an individual value is logical TRUE or logical FALSE is given by these functions
isFALSE(TRUE)

0L == FALSE  # This is convenient because Logical Vectors are actually stored as integer vectors at the C-level
1L == TRUE   # 0L is FALSE, 1L is TRUE
isTRUE(1L)   # These are FALSE, because isTRUE detects that we passed an integer value, not 'TRUE'
isFALSE(0L)

!TRUE       # The ! operator negates a logical vector
!lvec
is.na(lvec) 
!is.na(lvec)
            # Since TRUE values are equal to 1L and FALSE equal to 0L, it is also possible to perform
TRUE + TRUE # calculation on them. 
sum(!is.na(lvec)) # This calculates the number of non-missing values in the vector

             # Analogous to the match() function, there is the %in% operator which returns a logical vector
match(c("b", "w"), letters) 
c("b", "w") %in% letters
letters %in% c("b", "w") # Elements 1 and 23 of this vector are TRUE

which(letters %in% c("b", "w")) # The which() function returns the integer indices of the elements that are TRUE

### Subsetting Vectors: ----------------------------------------------------
# All atomic vectors can be subset using integer or logical vectors
x <- rnorm(10L) # This draws 10 values at random from a standard normal distribution (Gauss curve)
x[4L]           # The fourth element.
x[-4L]          # All but the fourth.
x[2:4]          # Elements two to four.
x[-(2:4)]       # All elements except two to four.
x[c(1L, 5L)]    # Elements one and five.
x[x > 0]        # greater than 0
x[abs(x) > 1]   # greater than 1 in absolute value
x[order(x)]     # remember the order() function?
sort(x)         # Same thing
y <- rnorm(10L)
y[order(x)]     # order() is more flexible than sort() which can only be used to sort the vector itself: Here we order y based on the order of x using order() 

LETTERS[letters %in% c("a", "b")] # Extracting the capital letters corresponding to these lower case ones. 

# Vector Constructors: Create and empty vector of a specified size
numeric(10L)   # Same as double(10)
integer(10L)   # Numeric vectors are initialized as 0 (0L for integer)
character(10L) # Initialized as ""
logical(10L)   # Initialized as FALSE

# Replacing vector elements: using <- or = 
y <- integer(10L)
y[4:7] <- 6L  # Replacements can either have length one
y             
y[c(1L, 3L, 9L)] <- 6:8 # or a length equal to the replaced elements
y
y[4:7] <- 6:8 # Failure to comply with this will yield an error
              # Subsetting and replacing can flexibly be combined with computations. 
x[x < 0] <- -x[x < 0] # This replaces the negative values of x with their absolute values
x <- abs(x)   # The above is just an example though, normally we would just do this: apply abs() to all values, positive or negative


#****************************
### In-Class Exercise 4 -----
#****************************

# Problem 1: Consider again the vector of weights used earlier
weights <- c(80.5, 63.9, 75.3, 68.1, 79.3) # weights in kg

# (a) Get the weight of the second and 4th person using integers, and again using a logical statement

# (b) Get the weight of all but the second and 4th persons using integers, and again using a logical statement

# Problem 2: Consider the following integer vector
z <- c(1L, 4L, 6L)

# (a) create a logical vector of length 10 that has TRUE in the elements given by z

# (b) Get z back again from the logical vector created in part (a)


## (2.2) Object Attributes and Objects Built on Atomic Vectors ---------------
# Atomic vectors are the basic structure to contain data in R, and as we have seen they are
# very flexible and powerful, but also quite limited still in terms of high-level functionality.
# Fortunately, R lets us attach attributes (such as other vectors) to and R object, which
# enables much more complex objects and functionality.

### Named Vectors ----------------------------------------------------------
# This constructs a named vector
econ_score <- c(75.4, 80.1, 85.8)
names(econ_score) <- c("bob", "paul", "ashley")
econ_score
econ_score <- c(bob = 75.4, paul = 80.1, ashley = 85.8) # same thing more compact
econ_score
View(econ_score)
str(econ_score)  # see the structure
names(econ_score) <- NULL # This deletes the names attribute again
# as str() suggests. names() is actually a shortcut for the following code:
attr(econ_score, "names") <- c("bob", "paul", "ashley")
econ_score
econ_score["paul"] # An added advantage of named vectors is that we can also subset them using the name tag 

### Assigning arbitrary attributes to objects ------------------------------
# we can add arbitrary other attributes, but only some (predefined) attributes trigger additional functionality
attr(econ_score, "examdate") <- "2020-03-02"
econ_score
str(econ_score)

### The class attribute ----------------------------------------------------
# A particularly important attribute is the 'class' attribute, which can create different objects in R
# to which different function methods apply. The class uniquely identifies a certain type of R object

class(econ_score) <- "bla" # same as:
attr(econ_score, "class") <- "bla"
inherits(econ_score, "bla")       # Inherits checks if an object is of a certain class
any(class(econ_score) %in% "bla") # Same thing as what inherits does..
any(class(econ_score) == "bla")   # Just note that == is better than %in% if we are only comparing a vector with one element. However %in% works for multiple elements, and an object can have more than one class 

### Factors ----------------------------------------------------------------
# An important type of vector in R is a factor - analogous to an encoded categorical variable in STATA or other software
v <- rep(1:2, c(10L, 5L))          # Underlying integer values
levels(v) <- c("Male", "Female")   # same as attr(v, "levels") <- c("Male", "Female")
class(v) <- "factor"               # same as attr(v, "class") <- 
v
str(v)
is.object(v)  # If on object in R has a class attribute, it is no longer a plain vector. This can be checked with is.object
sum(v)        # This is now no longer an integer vector, but a categorical variable. You cannot just take the sum of it anymore
is.factor(v)  # We can check if it is a factor
inherits(v, "factor")

levels(v)     # returns the levels (= value labels)
class(v)      # returns the class of v
v2 <- rep(1:2, c(10L, 5L))
is.object(v2) # A plain vector is not a (classed) object
class(v2)     # calling class() on a plain vector returns the type of it (same as mode())
oldClass(v2)  # An earlier version of the class() function (still available under the name oldClass()) just returned NULL
unclass(v)    # unclass removes the class and returns the vector
is.object(unclass(v))
v             # but v is still unchanged
class(v) <- NULL # If we permanently want to remove the class from v, we can assign the NULL object (can also do oldClass(v) <- NULL)
v
levels(v) <- NULL # similarly for the levels attribute
v
identical(v, v2)  # This is now again plain integer, same as v2. 

# Since it can be tedious to first create a plain vector and the assign attributes to it, the structure() function lets us to all the in one go
v <- structure(rep(1:2, c(10L, 5L)), 
               levels = c("Male", "Female"), 
               class = "factor")
v
# For most classes, there is also a constructor function that makes the creation of this particular type of object even simpler:
factor(rep(c("Male", "Female"), c(10L, 5L))) # By default the factor levels are alphabetically ordered

# If we want to make explicit that the variable is not just categorical but ordinal (ordered categories), we can generate ordered factors
f <- factor(c("1997/98", "1998/99", "1999/00"), ordered = TRUE)
f
is.ordered(f)
is.ordered(v)
class(f)  # Notice the class ordered added. In general you can assign as many classes as you like to an object
# We could also make v ordered:
class(v) <- c("ordered", class(v))
v
is.ordered(v)
is.ordered    # let's look at the definition of this function 
inherits(v, "ordered") # we can check if an object inherits a certain class this way, for any class 
inherits(v, "bla")      
class(v) <- c(class(v), "bla")
inherits(v, "ordered")
inherits(v, "bla")

### Dates ------------------------------------------------------------------
# Another special class is the Date class
d <- as.Date("2000-07-16")
class(d)
inherits(d, "Date")
# Functions have different methods for different classes. This is the seq method for the "Date" class
d <- seq(as.Date("2000-07-16"), as.Date("2012-07-01"), by = "month")
d
inherits(d, "Date")
str(d)
# Logical comparisons
d > as.Date("2005-01-01")

#****************************
### In-Class Exercise 5 -----
#****************************

# Problem 1: Consider again the vector of weights used earlier
weights <- c(80.5, 63.9, 75.3, 68.1, 79.3)        # weights in kg
persons <- c("Dan", "Paul", "Anna", "Tim", "Tom") # names of the persons

# (a) assign the person names to the weights vector, and get the weight of Paul

# (b) compare the weight of Paul to the weights of Tim and Tom

# Problem 2: consider the following vector
k <- c("Yes", "Yes", "No", "Yes", "Don't Know", "No")

# (a) Coerce it to a factor, examine the structure of the factor

# (b) Get the levels, number of levels, and compute a frequency table for the levels

# (c) Coerce the factor to integer

# (d) Coerce the factor to character using as.character

# (e) Manually coerce the factor to character using the integer values and the levels. 

# (f) Manually recreate the factor from the character vector using using match(), sort() and unique()


### Matrices --------------------------------------------------------------
# Apart from the 'class' attribute which determines the type of an object,
# we can create N-dimensional structures from vectors simply by attaching a dimension ('dim') attribute:

econ_score <- c(75.4, 80.1, 85.8)
math_score <- c(72.5, 67.6, 80.9)
m <- c(econ_score, math_score) # combining the two vectors
m
dim(m) <- c(3L, 2L) # This now adds a dimension attribute (same as attr(m, "dim") <- c(3L, 2L))
  # The added dimension attribute tells R that this object has  more than one dimension
m # so this is a matrix of 3 rows and 2 columns, note that the data enter the matrix column-wise
is.object(m) # There is still no class assigned
is.matrix(m) # but the dimension attribute is enough for R to know this is a matrix
class(m)     # accordingly, this returns the implicit class

str(m)  # Note that nothing here has changed about the way the data is stored...
c(econ_score, math_score) # the data is still stored in a numeric vector. 
# the added 'dim' attribute just tells R that this is a 2-dimensional structure and it treats it as such

# Similarly to factors, there is a constructor function for matrices: 
matrix(c(econ_score, math_score), nrow = 3L, ncol = 2L)

# For matrices, we can also assign row- and column-names:
rownames(m) <- c("bob", "paul", "ashley")
colnames(m) <- c("econ_score", "math_score")
m
dimnames(m) # This returns the dimension names in a list (which we will discuss in a bit)

# Since this is a matrix, we can now subset both rows and columns (in the same way as we can subset a (named) vector)
m["paul", "econ_score"] # m[rows, columns], same as for vectors which only allow 1D subsetting e.g. v[indices]
m[2L, 1L]  # Same thing using integer indices instead of names
m[c("paul", "ashley"), ] # Subsetting rows only
m[2:3, ]   # Same thing
m[-1L, ]   # Same thing
m[1L, ]    # Only subsetting rows. 
str(m[1L, ]) # Note that this is again a named vector
m[1L, , drop = FALSE] # using drop = FALSE we can prevent R to drop the dimensions of the object. 
str(m[1L, , drop = FALSE]) # So this remains a matrix, even though we extract only one column

# Note that the data is still stored in a vector, so we don't need to use matrix subsetting:
m[1:4] # Although m is now a matrix, it retains all essential properties of a vector, including 1D subsetting
sum(m) # Basic statistical function can also be called on the matrix as if it were a vector.

# Useful functions for matrices: cbind = column-bind: Here column-binding two vectors also constructs a matrix
cbind(econ_score, math_score)
cbind(m, m) # can also be used to combine 2 matrices column-wise

rbind(econ_score, math_score) # similarly there is rbind for row-binding
rbind(m, m) 

# There are a number of built-in functions that operate on matrices
dim(m)
nrow(m)
ncol(m)
length(m) # length() is for vectors, so applies to the underlying vector 

colSums(m) # Some statistical functions for matrices also exist
rowSums(m)
colMeans(m)
rowMeans(m)

# For other functions, we can use apply
?apply # See the help page
apply(m, 2L, median) # 2 = second dimension = columns (so this does the same as a function colMedians() would do)
apply(m, 1L, median) # 1 = first dimension = rows

# Basic matrix operations:
t(m) # transpose
m * 5         # Multiplying matrix and scalar (similarly for + - /)
m * (1:3)     # Multiplying matrix and vector (multiplies each column of the matrix with a vector, similarly for + - /)
m %*% t(m)       # Matrix multiplication 
m %*% m[1L, ] # Matrix multiplication with a vector
cm <- crossprod(m)  # Cross-product (more efficient way to write t(m) %*% m)
t(m) %*% m
cm            # this is a square matrix
d <- diag(cm) # gets the matrix diagonal
diag(cm) <- 0 # replace the diagonal with 0's
cm
diag(cm) <- d # Assign again the original diagonal
lower.tri(cm) # Lower and Upper Triangular Part of a Matrix
cm[upper.tri(cm)]
diag(10)      # diag can also be used to generate identity matrices of a given size
row(cm)       # row and col replace elements with their respective row and column indices
col(cm)
outer(1:3, 1:3)   # Outer product (multiplying all elements of the first lector with all elements of the second..)
kronecker(cm, cm) # Kronecker product: Multiplying all elements of the first matrix with the entire second matrix
# Neat application: outer() also works with character values:
outer(c("a","b"), c("a","b"), paste)

solve(cm)     # Matrix inverse cm^(-1)
n = 1:2
solve(cm, n)  # Find x in: cm %*% x = n
chol2inv(chol(cm)) # More efficient inverse of a symmetric PD matrix based on choleski factorization (if matrix is not symmetric or PD, use solve())
eigen(cor(m)) # Eigen-decomposition (of correlation matrix, useful for PCA)
svd(m)        # Singular-Value decomposition (more efficient way to do PCA in the absence of missing values)
qr(cm)        # QR decomposition (good for linear model solving), also gives the rank of the matrix


#****************************
### In-Class Exercise 6 -----
#****************************

# In this exercise we will use some data in matrix form built into R
data() # This shows all datasets built into R

# Problem 1: Consider this matrix of US personal expenditures
USPersonalExpenditure

# (a) See its structure using str(), look at it in the data viewer using View(), compute the dimensions using dim()

# (b) Get the health and private education expenditure for 1940, 1950 and 1960, using both names and indices

# (c) Compute the total expenditure in each year, and the average expenditure per item over time

# (d) Compute the share of different items in total consumption in each year

# (e) Compute the standard deviation in the consumption share of each item over time

# Bonus: These are some similar matrices that come with R, explore them using the tools shown above
VADeaths
state.x77

# Problem 2: Consider this time series of US quarterly log revenue from (1962,2Q) to (1971,4Q).
freeny.y

# (a) Look it up in the documentation, see the structure of it. and plot it using plot()

# (b) Plot the first difference of it using diff()

# (c) Consider this matrix of explanatory variables (view it, see the structure)
freeny.x

# (d) Create a matrix 'freeny' combining freeny.y and freeny.x

# (e) Compute the correlations of the variables in the matrix, in levels and first-differences

# (f) Manually regress freeny.y on freeny.x, after adding an intercept column to freeny.x. 
#     Note that that OLS coefficients are computed as: (X'X)^(-1)X'y

# (g) Compare your coefficients to those reported by R's linear modeling command:
lm(freeny.y ~ freeny.x)


### Arrays: Same as matrix but with 3 or more dimensions -------------------
# Again we have a constructor function
a <- array(1:27, dim = c(3L,3L,3L), 
           dimnames = list(c("1a", "1b", "1c"), 
                           c("2a", "2b", "2c"), 
                           c("3a", "3b", "3c")))
str(a)
dim(a)
dimnames(a)
is.array(a)
a # This is the print method for arrays: prints the first two dimensions for every instance of the third dimension

a[1L, 1L, ] # select 3rd dimension vector corresponding to first row and first column 
a[1L, 1L, , drop = FALSE] # Same, preserving dimensions
drop(a[1L, 1L, , drop = FALSE]) # using drop() 
a[2:3, c("2a", "2b"), "3c"] # mixed subsetting
a[2:3, c("2a", "2b"), c(1L, 3L)] # mixed subsetting

a[1:10]  # Again, an array is nothing but a vector with attached dimension ('dim') and dimension names ('dimnames') attributes
sum(a)   # Thus all functions that apply to atomic vectors also apply to arrays

apply(a, 1L, sum) # sum all elements of dimensions 2 and 3 along dimension 1
apply(a, 2L, sum)
apply(a, 3L, sum)
apply(a, 1:2, sum) # Sum all elements in the 3rd dimension along dimensions 1 and 2
apply(a, c(1L, 3L), sum) # Sum all elements in the second dimension

aperm(a, c(3L, 1L, 2L)) # Permuting array (reshuffling dimensions)
aperm(a) # Be default reverses dimensions, same as aperm(a, c(3L, 2L, 1L))

# Arrays are useful particularly because we can very easily access the elements
# and we can easily compute statistics along different dimensions

# There is no exercise on arrays, but you could explore the following arrays supplied by R
iris3
Titanic


## (2.3) Lists and Data Frame's --------------------------------------------
# All objects we have seen so far were based on atomic vectors. 
# While we can create 2D, 3D, 4D etc. matrices and arrays from vectors by simply
# attaching appropriate 'dim' attributes or using the matrix() or array() constructors,
# a fundamental limitation remains that all data in the vector needs to be of the same type,
# e.g. either integer or double or character or logical. 

# Thus we need a kind of container object that allows us to pack vectors of different types in one objects
# this is what the List does in R.

### Lists ------------------------------------------------------------------
# A list is a 1D container object that can contain all kinds of other objects
# including vectors, functions and other lists. 

# This creates a list of vectors of different lengths and types. list() is the constructor function
l1 <- list(7.5, 9:10, "word", 3 + 8 != 5) # the list function combines vectors of different lengths and types into a list
l1 # This is how the printout looks like
str(l1) # see the structure of it
is.list(l1)
is.atomic(l1)
class(l1)  # The implicit class is list
length(l1) # This returns the length of the list
seq_along(l1) # This returns the indices of the list elements (same as 1:length(l1))

# A Named list:
l2 <- list(avec = 1:10, bvec = c("a", "b", "c"), cvec = c(TRUE, FALSE))
l2
str(l2)
names(l2)

# Again we can also assign names
names(l1) <- letters[seq_along(l1)] 
l1
names(l1)
# We can also concatenate lists using the c() function. 
c(l1, l2)

# removing the names again.
names(l1) <- NULL  
l1

# We can coerce atomic vectors to list using as.list
x <- 1:4
as.list(x)

# Lists are the preferred vehicle to package information into a single object, (e.g. results from a linear model estimation) ..coming up

# Apart from allowing different vector lengths and types to be packed into a list, a list itself can be packed into a list 
l2 <- list(avec = 1:10, bvec = c("a", "b", "c"), cvec = c(TRUE, FALSE),
           sublist = list(d = "q", e = 2L, f = TRUE), func = sum) # here putting the sum function into the list as well
l2         # This is a named list containing vectors of different types as well as a sublist and a function
str(l2)
l2$avec    # Use $ to pool out list elements by their name
l2$bvec
l2$sublist 
l2$sublist$e 

l2[2L] # gives second element but data type is still list.
l2["bvec"] # same thing (subsetting by name as we have seen before with vectors)
l2[[2L]] # we need double brackets to get just the content, generally this is what we want
l2[["bvec"]] # Same thing
l2$bvec == l2[[2L]] # These give identical output ($ <=> [[]])

# [[]] is useful for programming...
x <- "bvec"
l2[[x]]

# To make this clear:
str(l2[[2L]])   # this is a vector!!
class(l2[[2L]])
length(l2[[2L]])
str(l2$bvec)    # this is the same vector!!
class(l2$bvec)
length(l2$bvec)
str(l2[2L])     # but this is a sublist of length 1 containing the vector!!
class(l2[2L])   
length(l2[2L])   

l2[[4L]]
class(l2[[4L]]) # This is also list, which we nested inside l2

# list functions:
length(l2)
lengths(l2) # gives the length of every list element

# for lists of equally typed values (numeric or character)
list3 = list(a = 1:3, b = 4:7, c = list(8:10))
str(list3)
unlist(list3) # This creates a numeric vector from this list
unlist(list3, recursive = FALSE) # This only does one step of unlisting
unlist(list3, use.names = FALSE) # without names

# Recursively apply functions to list elements
rapply(list3, mean)
rapply(list3, mean, how = "list") # This keeps the list structure

# lapply (list-apply) is non-recursive
lapply(list3[1:2], mean)
lapply(list3, mean)
# gives warning because the mean can only be applied to a numeric vector, not to a list
mean(list(8:10))
mean(8:10) # This works, thus if we want to compute statistics on a nested list, need to use rapply()

m # Remember the matrix m we create above?
# This creates a named list replicating m three times
ml <- list(`2010` = m + rnorm(6L), 
           `2011` = m, 
           `2012` = m + rnorm(6L))
ml
# suppose we canted to combine all the matrices in these lists row- or column wise:
# The function do.call constructs a function call by passing a list of arguments to a function
do.call(cbind, ml) # same as cbind(m, m, m)
do.call(rbind, ml) # same as rbind(m, m, m)

# Since this is a list of 3 matrices of the same dimensions, we might also 
# want to create a 3D arraay from this. This can be done 
# using the function simplify2array() = unlist to more than 1 dimensions
ma <- simplify2array(ml)
str(ma)


#****************************
### In-Class Exercise 7 -----
#****************************

# Consider the list ml created above
str(ml)

# (a) Add scores for the year 2013 in a similar way I created them for 2012, look up the rnorm() function. 

# (b) Look at the structure of the list, view it in the data viewer, and print the scores for 2012 and 2013

# (c) Compute the average math and econ score and the average score for each student in each year using lapply()

# (d) Simplify your calculation result from part (c) using simplify2array()

# (e) Do steps (c) and (d) in one step using the sapply() function. Look it up in the documentation

# (f) Delete the matrices for 2010 and 2013 from the ml list. 


### Date Frame's -----------------------------------------------------------
# Similarly to Matrices which we learned are just vectors with a dimension 
# attribute and additional methods and properties, 
# A data frame is a list of equal-length column-vectors with added attributes attached. 
# It is the fundamental structure to contain datasets in R:

# Using the data.frame constructor
students <- data.frame(weights = c(60.5, 72.5, 45.2), # Numeric vector
                       genders = factor(c("Male", "Male", "Female")), # Factor
                       econ_score = econ_score, # Integer vector
                       math_score = math_score, 
                       row.names = c("bob", "paul", "ashley"))
str(students)
is.list(students)
is.data.frame(students)

# Lets construct it manually:
students <- list(weights = c(60.5, 72.5, 45.2), 
                 genders = factor(c("Male", "Male", "Female")), 
                 econ_score = econ_score, 
                 math_score = math_score)
attr(students, "row.names") <- c("bob", "paul", "ashley")
class(students) <- "data.frame"
str(students)

# Same thing again using the structure function 
students <- structure(list(weights = c(60.5, 72.5, 45.2), 
                 genders = factor(c("Male", "Male", "Female")), 
                 econ_score = econ_score, 
                 math_score = math_score),
                 row.names = c("bob", "paul", "ashley"), class = "data.frame")

students
View(students)

# We can subset a data frame like a matrix
students[, 2L] 
students[, "genders"]
str(students[, "genders"])
students[, "genders", drop = FALSE]
str(students[, "genders", drop = FALSE])

# Or like a list (again this is analogous to matrices and arrays which maintain the properties of a vector: The data frame maintains the properties of a list which it is based upon)
students[2L] 
students["genders"] 
students[[2L]] 
students$genders

# All these therefore give identical output:
identical(students[, 2L], students[, "genders"])
identical(students[, 2L], students[[2L]])
identical(students[, 2L], students[["genders"]])
identical(students[, 2L], students$genders)

# these are also equivalent
identical(students[, 2L, drop = FALSE], students[2L])
identical(students[, 1:2], students[1:2])
identical(students[, -2L], students[-2L])

# Row-subsetting also works in a similar way
students[1L, ] # Since the data frame is a list of columns whose data types can be different, this preserves the data frame (i.e. no dropping dimensions for rows)
students[2:3, ] 
students[1:2, ] 

# Subsetting based on content of the data.frame
students[students$genders == "Male", ]
students[students$genders %in% c("Male", "Female"), ] # This is non-sensible here, but if there are more than two choices you can do like that
students[students$weights > 56, ] 
students[students$econ_score > 80 & students$math_score > 80, ] # Can combine arbitrary statements using & | ! and () to group statements

# A more user friendly way to do this is with the subset() function
subset(students, genders == "Male")
subset(students, econ_score > 80 & math_score > 80)
subset(students, econ_score > 80 & math_score > 80, weights:genders) # also selecting columns

# Computing or deleting columns
students$new = 1
students$sum_score = students$econ_score + students$math_score
students
students$new = NULL # deleting a column
students
students[["new"]] = 1 # same thing
students[["new"]] = NULL 
# Multiple columns
students[["new"]] = 1 
students[, c("new", "sum_score")] = NULL # note that students[c("new", "sum_score")] also works as a data.frame is also a list
students

# Again a more user friendly way to do this is offered by transform()
transform(students, new = 1)
transform(students, sum_score = econ_score + math_score)
# Can also transform multiple columns, and save result
students <- transform(students, 
                      sum_score = econ_score + math_score, 
                      mean_score = (econ_score + math_score) / 2)
students
# Deleting columns this way
students <- transform(students, sum_score = NULL, mean_score = NULL)

# Ordering
students[order(students$math_score), ] # Default: Ascending order
students[order(students$math_score, decreasing = TRUE), ]
# First by gender, then by econ score, in ascending order
students[order(students$genders, students$econ_score), ]

# Note that there is no user friendly function to do this in basic R, 
# but we will learn those supplied by packages in Course 2 (dplyr::arrange, collapse::roworder and data.table::setorder)

# With allows computing in a list or data.frame environment without 
with(students, (math_score + econ_score) / 2)
with(l2, sum(avec))

# Functions for data frames:
names(students)
row.names(students) # rownames() also works, but is meant for matrices
nrow(students)
ncol(students)
length(students)  # gives the length of the underlying list = the number of columns
summary(students) # summarize each column in the data frame

# Row and column binding also works
cbind(students, students)
rbind(students, students)

# Excursus: Datasets and other data objects included in basic R:
data()  # shows all datasets in all packages
help(package = "datasets") # Datasets in the datasets package that is always there when you install R

# Applying functions to data.frame columns: Base R provides 2 options: lapply() or sapply()
View(airquality) # This is a dataset about New York Air Quality Measurements: Daily air quality measurements in New York, May to September 1973.
summary(airquality)
lapply(airquality, sum)
lapply(airquality, sum, na.rm = TRUE) # na.rm = TRUE because Ozone and Solar.R contain some missing values
unlist(lapply(airquality, sum, na.rm = TRUE))
sapply(airquality, sum, na.rm = TRUE) # sapply = simplify after list-apply. 

# Converting to and from matrix: 
as.data.frame(m)
as.matrix(students[colnames(m)]) # if converting to matrix, all columns must be of homogeneous type

# Important functions
?na.omit # removes missing values or rows from vectors, matrices or data frames
na.omit(airquality)
View(na.omit(airquality))
nrow(airquality)
nrow(na.omit(airquality))

?complete.cases # This shows us the complete cases / rows in a matrix or data frame
complete.cases(airquality) # default: logical output
which(complete.cases(airquality)) # This shows the complete rows
which(!complete.cases(airquality)) # This shows the rows where at least one column has a missing value

airquality[complete.cases(airquality), ] # Same thing as na.omit(airquality)

?split # Splits a vector / matrix or data frame into groups based on vectors / factors
View(iris) # 	Edgar Anderson's Iris Data: gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.
l <- split(iris[1:4], iris$Species)
View(l)
str(l)
# This reassembles the list again
do.call(rbind, l)
# This instead creates a 3D array
arl <- simplify2array(lapply(l, as.matrix))
str(arl)


#****************************
### In-Class Exercise 8 -----
#****************************

# Consider the following dataset. 
esoph
?esoph

# (a) Examine it's structure and summarize it using summary()

# (b) Compute the percentage of cancer cases on the dataset

# (c) Subset the dataset to display only the 25-34 and 35-44 year old's with both above average alcohol and tobacco consumption

# (d) Repeat the exercise in part (c), now with the people older than 65. What do you notice?

# (e) Make a copy of the dataset where you have coerced all factors to integer, and compute a correlation matrix of the variables. What do you notice?

# (f) Using lm(), and the data from part (e), run a regression of he percentage of cases on agregp, alcgp, tobgp. 
#     Use summary() to summarize the model and interpret the results.

# (g) Repeat part (f) using the original data. How do you interpret the results?

# (h) Repeat part (g), now using an interaction between alcgp and tobgp using alcgp:tobgp. How do you interpret the results?

# (i) Repeat part (h), now using a full interaction between alcgp and tobgp using alcgp * tobgp. How do you interpret the results?

# Extra: More datasets to explore:
longley
infert
USJudgeRatings
USArrests
iris



# (3) Functional Programming -----------------------------------------------
#***************************************************************************

# Remeber what we said at the beginning: 

#  To understand computations in R, two slogans are helpful:
#  Everything that exists is an object, all objects are in the Global Environment!
#  Everything that happens is a function call (everything you do with an object).

# Up to this point we have focussed on data objects and we have seen all the essential 
# objects to operate R by now. Let us now focus on functions.
# Functions are themselves a kind of object (a function object), that can be used to
# perform computations on other objects.

### Intro to Functions -----------------------------------------------------
# lets take a function like this one. We can see the content of every function by evaluating its name
tabulate
str(tabulate) # see the structure
class(tabulate) # this is a function
is.function(tabulate) # this is a function
typeof(tabulate) # It is stored at the C-level in something called a closure
length(tabulate) # Functions always have length 1.

View(tabulate) # We can also view the function content in the viewer
# a function is definition starts with the word 'function', followed in round brackets ()
# by a list of arguments, and opening and closing curly brackets {} deliminating the function body
# The function body is the composed of other functions doing something to the inputted objects (the function arguments)
# and some control flow statements if() else () etc...

# This is a basic function called hello:

hello <- function(yourname) { # This defines the function
  paste("Hello", yourname)
}
hello("Sebastian") # This executes the hello() function using my name

# Now lets create a small statistical summary function called su():
su <- function(x) c(Mean = mean(x), SD = sd(x))
# Note that we don't actually need the curly braces if just one line of code...
su(1:10) # applying the function to an integer vector. 

# Let's revise out hello() function by adding a second argument 'goodbye'. 
# The second argument has a default, so it is not necessary to supply a value to 'goodbye' 
# This is the most readable way to write the function 
hello <- function(yourname, goodbye = FALSE) {
  if (goodbye) { # or if(isFALSE(goodbye)) if you want to make sure a logical FALSE is passed to the argument
    paste("Goodbye", yourname)
  } else {
    paste("Hello", yourname)
  }
}

# Note: A better alternative to an if-clause if we want to dispatch on the value of an argument is using a switch statement:
arg <- "c"
switch (arg,
  a = 1,
  b = TRUE,
  c = FALSE
)


hello("Sebastian", goodbye = TRUE)
hello("Sebastian", goodbye = FALSE)
hello("Sebastian") # Same thing, argument default is FALSE

# Again if it's one line of code, we don't need curly braces
hello <- function(yourname, goodbye = FALSE) if (goodbye) paste("Goodbye", yourname) else paste("Hello", yourname)
# This is also one line of code, because each line apart from the last ends with a control flow statement expecting a value,
# e.g. the first line ends on function(), the second on else. 
hello <- function(yourname, goodbye = FALSE) 
  if (goodbye) paste("Goodbye", yourname) else 
    paste("Hello", yourname)
# But this is considered bad style. If you use multiple lines you should at least wrap your function body in curly braces
hello <- function(yourname, goodbye = FALSE) {
  if (goodbye) paste("Goodbye", yourname) else 
    paste("Hello", yourname)
}
# The most compact way to write this function is actually to put the if clause inside paste():
hello <- function(yourname, goodbye = FALSE) 
  paste(if (goodbye) "Goodbye" else "Hello", yourname)
# But for now you should write functions in the first way I have shown you above, using if(){} else {} and spaces between these statements

# So far we have created function and saved them by creating function objects 
# e.g. myfun <- function(x, y, z, ..) {...}
# We can also define ad-hoc functions i.e. functions that are defined instantly to do a specific task but not saved.
sapply(mtcars, function(x) c(N = sum(!is.na(x)), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x)))
sapply(mtcars, summary) # Compare to summary function
# Same after first defining the function
my_summary <- function(x) c(N = sum(!is.na(x)), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))
sapply(mtcars, my_summary)

### The return Argument ----------------------------------------------------
# by default, the function returns the last object evaluated in its body.
# We can also use return statements to terminate function execution early.
# return() simply means: return the object and stop the function execution

# So a smarter way to write our hello function would actually be:
hello <- function(yourname, goodbye = FALSE) {
  if (goodbye) return(paste("Goodbye", yourname))
  paste("Hello", yourname)
}

# The return statement is very useful to simplify code in larger functions

### Generic functions and methods ------------------------------------------
# let's look at many of the functions we have been dealing with:
mean
unique
summary

# These functions are generic, which means that the parent function (called the 'generic' function) 
# calls different child functions (called 'methods') depending on the R object it is evaluated on

# We can see the methods defined for each generic function like this:
methods(mean)
methods(unique)
methods(summary)

# We can get the method function directly by appending the generic with a dot, followed by the name of the method
unique.data.frame # unique function for data frames
unique.matrix # unique function for matrices
unique.default # The default method is a method that applies to all other objects that do not inherit a class for which a specific method is defined.

# To drive this point home, let's create a generif summary function:
su <- function(x) UseMethod("su") # This creates the generic
# Default method (for numeric data)
su.default <- function(x) c(Mean = mean(x), SD = sd(x))
# method for factors: tabulate values
su.factor <- function(x) table(x)

# remember this dataset:
str(iris) # it has 4 numeric columns and one factor column

# we can now apply the su function to every column
lapply(iris, su)

summary(iris) # This is actually very similar to what summarize does. 

# Note that this is only possible with the generic version. using the numeric method gives an error for the factor.
lapply(iris, su.default) # recall su.default() is identical to the non-generic su() which we created above

# Regarding the use of generic functions: A generic function is handy, but it obscures a bit
# what the function does because you cannot see the code directly but need to look at the methods
# individually, and there is also a small computational cost associated with 
# method dispatch = the process of choosing the fight method to call on the object done by UseMethod("generic function name")

# So for a so simple function like su(), we would not create a generic version but define it as:
su_simple <- function(x) {
  if(is.factor(x)) return(table(x))
  c(Mean = mean(x), SD = sd(x))
}

identical(lapply(iris, su_simple), lapply(iris, su)) # same thing

# However for larger functions like unique (where each method can have 100 lines of code),
# it makes a lot of sense to create a generic version.

# Note finally that because of the generic.method syntax, it is very bad practice to create 
# a non-generic function with a "." in the name. For your own functions you should use "_" for 
# multi-word function names, such as su_simple which I created above. 
# (If I named the function su.simple, you could think it is a method for the generic 'su' defined for an object of class 'simple' )

### The ellipsis (...) argument --------------------------------------------

# The ellipsis (...) argument to a function allows for a variable number of arguments being
# passed to a generic function or to functions called within a function
su <- function(x, ...) UseMethod("su") 
su.default <- function(x, na.rm = FALSE, ...) {
  c(Mean = mean(x, na.rm = na.rm), SD = sd(x, na.rm = na.rm), 
    quantile(x, na.rm = na.rm, ...))
}
su.factor <- function(x, ...) table(x)

# The na.rm argument is passed down to mean, sd and quantile in su.default, but disregarded in su.factor
lapply(airquality, su, na.rm = TRUE)

# Now we also pass an argument probs to the quantile function
?quantile # see hwta the probs argument does
# So here we remove missing values and only calculate the median, 2nd and 4th quartile
lapply(airquality, su, na.rm = TRUE, probs = c(0.25, 0.5, 0.75))

# Note that we don't require a generic function to use the ellipsis argument:
su2 <- function(x, ...) c(Mean = mean(x), SD = sd(x), quantile(x, ...))
su2(airquality$Temp, probs = c(0.25, 0.5, 0.75))

# Note that if we pass the ellipsis to multiple functions, we might get an error:
su2 <- function(x, ...) c(Mean = mean(x, ...), SD = sd(x, ...), quantile(x, ...))
su2(airquality$Ozone, na.rm = TRUE) # This works because all 3 functions (mean, sd, quantile have a 'na.rm' argument)
# but this gives an error because mean and sd don't have a 'probs' argument.
su2(airquality$Ozone, na.rm = TRUE, probs = c(0.25, 0.5, 0.75))

# If we recreate functions like this:
mean2 <- function(x, na.rm = FALSE, ...) mean(x, na.rm = na.rm)
sd2 <- function(x, na.rm = FALSE, ...) sd(x, na.rm = na.rm)

su3 <- function(x, ...) c(Mean = mean2(x, ...), SD = sd2(x, ...), quantile(x, ...))
# This now works, because mean2 and sd2 have the ellipsis argument, but it is not passed to any otehr internal function
# which micght give an error because it does not have a 'probs' argument. 
su3(airquality$Ozone, na.rm = TRUE, probs = c(0.25, 0.5, 0.75))
# Thus we say here that the 'probs' argument has been 'silently swallowed' in the ellipsis passed to mean2 and sd2

# Note that this is the same behavior as we observed in the generic su.factor method above:
str(iris$Species) # Recall this is a factor
su(iris$Species, bla = 3) # Passing a random argument here does nothing because the ellipsis is not used in su.factor

# Ending the R Session -----------------------------------------------------
ls() # list all objects currently in the workspace (global environment)
rm(avec, chr, x, y, students) # remove some of them
rm(list = ls()) # remove all of them
gc()            # garbage collection (recycling memory used by these objects, only needed when we manipulated very big objects and we want to do something else now.)






