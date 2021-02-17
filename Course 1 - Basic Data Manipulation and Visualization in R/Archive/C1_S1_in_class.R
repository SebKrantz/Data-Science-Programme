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

## First tings first -------------------------------------------------------
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

# Numeric Vectors 
## Double Precision Vector
5.6                     # All numbers in R are by default stored as double prevision values
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

z <- c(1L, 2L, 3L)      # if one number in the vector is non integer, 
is.integer(z)           # the whole vector is stored as a C-array of double precision numbers
is.double(z)
is.integer(as.integer(z)) # as.functions can be used to coerce vector types. 

x <- 1:3                # generating large sequences of integers is simplified by the colon `:` operator
x
typeof(x)
typeof(1:3)           # Same thing. If we use `:` we do not need to add `L` to declare values as integer
seq(1, 10)               # Another way to generate a sequence of numbers is using seq
identical(1:3, seq(1, 3)) # The function identical checks that these two vectors are identical
seq(2000, 2030, 2)      # seq is more flexible.

                        # In general, storing integer values in integer and not double precision vectors 
                        # has the advantage that integer vectors are only half as large:
y_l <- 1:1e7            # 10 million observations
object.size(y_l)        # Size of the object
object.size(c(y_l, 1L)) # Adding an integer value increases the size a bit
object.size(c(y_l, 1))  # But adding a double precision value (omitting L) doubles the size of the whole vector.
rm(y_l)                 # this is because vectors can only be of one type, and integers can be stored as doubles but not vice versa

# Some general stuff about coding
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
mode(z)         # Get or set the type / mode of an object.
typeof(z)       # Determines the (internal) type or C-level storage mode of an object
str(z)          # Compactly display the internal structure of any R object
is.numeric(z)   # and similarly is.otherdatatypes (e.g. is.character, is.logical).
is.double(z)    # We can further check if a numeric vector is stored as double precision or integer
as.double(z)    # 'as.-' functions convert vector types (any many other objects in R)
print(z)        # Prints to the console (only needs to be called explicitly in functions)
View(z)         # Open Object in Data viewer

# Mathematical Operations
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
sum((x-mean(x))^2) / (length(x)-1)
sd(x)
sqrt(sum((x-mean(x))^2) / (length(x)-1))
y <- rnorm(10)
cov(x, y*100) # covariance of x and y
sum((x-mean(x))*(y-mean(y))) / (length(x)-1)
cor(x, y*100) # correlation of x and y
plot(x, -x)
cor(x, -x)
cov(x, y) / (sd(x) * sd(y))
## Vector valued functions
sqrt(x)
x^(1/2)      # same thing
abs(-x)
exp(x)       # exponentiate e^x
exp(1)^x     # same thing
log(x)       # natural logarithm
all.equal(exp(log(x)), x) # identical() here gives false because of numeric imprecision. we can use all.equal instead
log10(x)     # logarithm base 10
log(x, base = 10) # same thing
diff(x)      # first difference

# Other functions
y <- c(1L, 3L, 1L, 2L, 3L, 4L)
rep(1L, 4L)       # replicate values or vectors
rep(y, 2L) 
c(y, y)
rep(y, each = 2L)
rev(y)            # reverse the order of elements in a vector
rle(rep(y, each = 2L)) # run-length encoding: computes the number of consecutive equal values
sort(y)           # sort the elements of y
order(y)          # compute the order of the elements = the index that would sort the data
sort(unique(y))         # get unique values of a vector
table(y)          # tabulate (i.e. count the frequencies of unique values in a vector)
tabulate(y)       # Same thing, just returns the frequencies

# !!! Finding help on a function !!!
help(log)
?log         # Same thing more compact
?all.equal

?"<-"        # We need to quote operators to find their help pages
?"+"

# For more general helo, start html help, or click on the 'help' panel
help.start()

## Special numeric values 

x <- c(1, NA, 1/0, Inf/Inf, -1/0, NULL) 
x # NULL has disappeared -> It is Its own object, the null object
# NA = "Not Available" (missing value). NA is a logical constant which contains a missing value indicator. Any operation on it gives NA.
sum(NA)
NA + 1
mean(c(NA, 1, 2, 4.5), na.rm = TRUE)
mean(c(1, 2, 4.5, Inf))
# NAN = "Not a Number". NaN are the result of some mathematical operations, e.g. 0/0.
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
is.infinite(x)     # is.infinite only includes ininite values (positive or negative)
is.null(x)         # NULL is its own object 
is.null(y)
# The important distinction between NA and NULL is that NA is a 'logical' value that when evaluated in an expression, yields NA.  
# This is the expected behavior of a value that handles logical indeterminacy.
# NULL is its own thing and does not yield any response when evaluated in an expression. 
# NULL is typically used to delete vectors in list-like structures (as we will see later)
# More here: https://www.r-bloggers.com/r-na-vs-null/


# Now let's consider the character vector

chr <- c("Hello", "how", "are", "you", "?", "")
is.character(chr)
chr         # same as print(chr)
cat(chr)    # concatenate and print
length(chr)
nchar(chr)  # number of characters
nzchar(chr) # non-zero
toupper(chr) 
tolower(chr) # twimws() removes trailing or leading white spaces. 
c(" dfdf ", trimws(" dfdf "))
            # Can also use single quotes
chr2 <- c('hello', 'how', 'are', 'you', '?', '') 
b <- "he'l'lo"; b # to display ticks, we need to nest them.
b <- 'he"l"lo'; b

avec <- rep("a", 10) # replicate character values
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
paste(month.abb, collapse = "-") # paste function with collapse argument to join elements of the vector to a single string
paste0(month.abb, " (", month.name, ")") # constructing stings 
paste("Today is", date())

            # We can create month.abb ourselves by taking the characters 1 through 3 of each element
m.abb <- substr(month.name, 1L, 3L)
m.abb
identical(m.abb, month.abb)
            # match elements at the start and end point of strings
startsWith(month.name, "J")
endsWith(month.name, "ber")
grepl("ctob", month.name)  # we can search for characters within strings 
grepl("ctob|cembe", month.name) # searching for 'ctob' and 'cembe'
gsub("ctob|cembe", "dfg", month.name) # removing these elements from the string  
# many more options for pattern matching and replacement in strings, see ?gsub

            # alphabetic sorting also works with strings
sort(month.abb)
order(month.abb)
            # match returns a vector of the positions of (first) matches of its first argument in its second.
match(c("b", "w"), letters) 

# Logical Vector: Contains logical statements TRUE or FALSE or missing values

lvec <- c(TRUE, FALSE, NA, NA)
          # Any comparison using >, <, <=, >=, == or !=, 
          # or using functions starting with is e.g. is.na, is.numeric etc. generates logical vectors
lvec2 <- c(3 > 8, 9 + 2 == 10, 15 >= 3 * 5, FALSE)
lvec2

lvec == lvec2 # These logical comparisons are also vectorized. Comparison with NA values always returns NA. 
1:3 == 1:3
month.abb == substr(month.name, 1L, 3L)
              # Note that you need == for logical evaluation, simple = means asssignment (same as <-)
lvec3 = lvec == lvec2
    # There are a number of logical operators, see ?"&"
lvec & lvec2  # Logical AND. 
TRUE & TRUE
TRUE & FALSE
TRUE & NA
FALSE & NA    # Note this behavior: NA is a missing / undetermined TRUE or FALSE value, so this ise always FALSE not matter what NA is
TRUE && FALSE # If we know both vectors are of length 1, we should use &&
lvec && lvec2 # Using && on vectors of length > 1 comparies only the first elements to the two vectors

lvec | lvec2  # Similarly there is logical OR
TRUE | TRUE
TRUE | FALSE
TRUE | NA     # Note this behavior (same explanation as above)
FALSE | NA    
TRUE || FALSE 
lvec || lvec2 

isTRUE(TRUE)  # A safe way to check is an individual value is logical TRUE or logical FALSE is given by these functions
isFALSE(TRUE)

0L == FALSE  # This is convenient since Logical Vectors are actually stored as integer vectors at the C-level
1L == TRUE   # 0L is FALSE, 1L is TRUE
isTRUE(1L)   # These are FALSE
isFALSE(0L)

!TRUE       # The ! operator negates a logical vector
!lvec
is.na(lvec) 
!is.na(lvec)
            # Since TRUE values are equal to 1L and FALSE equal to 0L, it is also possible to perform
TRUE + TRUE # Calculation on them. 
sum(!is.na(lvec)) # This calculates the number of non-missing values in the vector
x <- c(NA, 1:5)
mean(x, na.rm = TRUE)
sum(is.na(x)) # This calculates the number of non-missing values in the vector
             # Analogous to the match() function, there is the %in% operator which returns a logical vector
match(c("b", "w"), letters) 
c("b", "w") %in% letters
letters %in% c("b", "w") # Elements 1 and 23 of this vector are TRUE

which(letters %in% c("b", "w")) # The which() function returns the indices of the elements that are TRUE

# Subsetting Vectors: All atomic vectors can be subset using integer or logical vectors
x <- rnorm(10)  # This draws 10 values at random from a standard normal distribution (Gauss curve)
x[4L]           # The fourth element.
x[-4L]          # All but the fourth.
x[2:4]          # Elements two to four.
x[-(2:4)]       # All elements except two to four.
x[c(1L, 5L)]    # Elements one and five.
x[x > 0]        # greater than 0
x[abs(x) > 1]   # greater than 1 in absolute value
x[order(x)]     # remember the order() function?
sort(x)         # Same thing
y <- rnorm(10)
y[order(x)]     # But order() is more flexible, we could order y based on x 

LETTERS[letters %in% c("a", "b")] # Extracting the capital letters corresponding to these lower case ones. 

# Vector Constructors: 
numeric(10)   # Same as double(10)
integer(10)   # Numeric vectors are initialized as 0 (0L for integer)
character(10) # Initialized as ""
logical(10)   # Initialized as FALSE

# Replacing vector elements: using <- or = 
y <- integer(10)
y[4:7] = 6L  # Replacements can either have length one
y             
y[c(1L, 3L, 9L)] <- 6:8 # or a length equal to the replaced elements
y
y[4:7] <- 6:8 # Failure to comply with this will yield an error
              # Subsetting and replacing can flexibly be combined with computations. 
x[x < 0] <- abs(x[x < 0])
x <- abs(x)



## (2.2) Attributes and Objects Built on Atomic Vectors -------------------------------------------
# Atomic vectors are the basic structure to contain data in R, and as we have seen they are
# very flexible and powerful, but also quite limited still in terms of high-level functionality.
# Fortunately, R lets us attach attributes (such as other vectors) to and R object, which
# enables much more complex objects and functionality

# This constructs a named vector
econ.score <- c(75.4, 80.1, 85.8)
names(econ.score) <- c("bob", "paul", "ashley")
econ.score
econ.score <- c(bob = 75.4, paul = 80.1, ashley = 85.8) # same thing more compact
econ.score
View(econ.score)
str(econ.score)  # see the structure
names(econ.score) <- NULL # This deletes the names attribute again
# as str() suggests. names() is actually a shortcut for the following code:
attr(econ.score, "names") <- c("bob", "paul", "ashley")
econ.score
econ.score[c("paul", "bob")] # An added advantage of named vectors is that we can also subset them using the name tag 


# we can add arbitrary other attributes, but only some (predefined) attributes trigger additional functionality
attr(econ.score, "examdate") <- "2020-03-02"
econ.score
str(econ.score)

# A particularly important attribute is the 'class' attribute, which can create different objects in R
# to which different function methods apply. 

# Factors: An important type of vector in R is a factor - analogous to an encoded categorical variable in STATA or other software
v <- rep(1:2, c(10L, 5L))  # Underlying integer values
levels(v) <- c("Male", "Female")   # same as attr(v, "levels") <- 
class(v) <- "factor"               # same as attr(v, "class") <- 
v
str(v)
is.object(v)  # If on object in R has a class attribute, it is no longer a plain vector. This can be checked with is.object
sum(v)        # This is now no longer an integer vector, but a categorical variable. You cannot just take the sum of it anymore
is.factor(v)  # We can check if 

levels(v)     # returns the levels (labels)
class(v)      # returns the class of v
v2 <- rep(1:2, c(10L, 5L))
is.object(v2) # A plain vector is not a (classed) object
class(v2)     # calling class() on a plain vector returns the type of it (same as mode())
mode(v2)
oldClass(v2)  # An earlier version of the class() function (still available under the name oldClass()) just returned NULL
unclass(v)    # unclass removes the class and returns the vector
is.object(unclass(v))
v <- unclass(v)             # but v is still unchanged
class(v) <- NULL # If we permanently want to remove the class from v, we can assign the null object (can also do oldClass(v) <- NULL)
v
levels(v) <- NULL # similarly for the levels attribute
v
identical(v, v2)  # This is now again plain integer, same as v2. 

# Since it can be tedious to first create a plain vector and the assign attributes to it, the structure() function lets us to all the in one go
v <- structure(rep(1:2, c(10L, 5L)), levels = c("Male", "Female"), class = "factor")
v
# For must classes, there is also a constructor function that makes the creation of this particular type of object even simpler:
as.factor(rep(c("Male", "Female"), c(10L, 5L))) # By default the factor levels are alphabetically ordered

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

# Another special class is the Date class
d <- as.Date("2000-07-16")
class(d)
inherits(d, "Date")
# Functions have differnt methods for different classes. This is the seq method for the "Date" class
d <- seq(as.Date("2000-07-16"), as.Date("2012-07-01"), by = "day")
d
inherits(d, "Date")
str(d)

### Matrices ------------------------------------------
  # Apart from the 'class' attribute which determines the type of an object,
  # we can create N-dimensional structures from vectors simply by attaching a dimension ('dim') attribute:

econ.score <- c(75.4, 80.1, 85.8)
math.score <- c(72.5, 67.6, 80.9)
m <- c(econ.score, math.score)
m
dim(m) <- c(3L, 2L) # same as attr(m, "dim") <- c(3L, 2L)
m # so this is a matrix of 3 rows and 2 columns, note that the data enter column-wise
is.object(m) # There is still no class assigned
is.matrix(m) # but the dimension attribute is enough for R to know this is a matrix
class(m)     # accordingly, this returns the implicit class

str(m)  # Note that nothing here has changed about the way the data is stored...
c(econ.score, math.score) # the data is still stored in a numeric vector. 
# the added 'dim' attribute just tells R that this is a 2-dimensional structure and it treats it as such

# similarly there is a constructor function: 
matrix(c(econ.score, math.score), nrow = 3L, ncol = 2L)

# For matrices, we can also assign row- and column-names:
rownames(m) <- c("bob", "paul", "ashley")
colnames(m) <- c("econ.score", "math.score")
m
dimnames(m)

# Since this is a matrix, we can now subset both rows and columns (same as vector)
m["paul", "econ.score"] # m[rows, columns], same as for vectors...
m[2L, 1L]  # Same thing
m[c("paul", "ashley"), ] # Subsetting rows only
m[2:3, ]   # Same thing
m[-1L, ]   # Same thing
m[, 1L]  # Only subsetting columns. Note that th this is again a names vector
str(m[, 1L])
m[, 1L, drop = FALSE] # using drop = FALSE we can prevent R to drop the dimensions of the object. 
str(m[, 1L, drop = FALSE])
m1 <- m[, 1L, drop = FALSE]
m1
drop(m1) # Dimensoins can also be dropped in post using drop()

# Note that the data is still stored in a vector, so we don't need to use matrix subsetting:
m[1:5]

# Other useful functions: cbind = column-bind: Here also constructs a matrix
cbind(econ.score, math.score)
cbind(m, m) # can also be used to combine 2 matrices column-wise

rbind(econ.score, math.score) # similarly there is rbind for row-binding
rbind(m, m) 

# There are a number of built-in functions that operate on matrices
dim(m)
nrow(m)
ncol(m)
length(m) # length() is for vectors, so applies to the underlying vector 
          
colSums(m) # Some statistical functions
rowSums(m)
colMeans(m)
rowMeans(m)

# For other functions, we can use apply
?apply
apply(m, 2L, median) # 2 = second dimension = columns (so this does the same as a function colMedians() would do)
apply(m, 1L, median) # 1 = first dimension = rows


### Arrays: Same as matrix but with 3 or more dimensions ---------------------------
a <- array(1:27, dim = c(3L,3L,3L), dimnames = list(c("1a", "1b", "1c"), c("2a", "2b", "2c"), c("3a", "3b", "3c")))

a[1L, 1L, ] # select 3rd dimension vector corresponding to ffirst row an d first column 
a[1L, 1L, , drop = FALSE] # Same, reservin dimensions
drop(a[1L, 1L, , drop = FALSE]) # using drop() 

apply(a, 1L, sum) # sum all elements of dimensions 2 and 3 along dimension 1
apply(a, 2L, sum)
apply(a, 3L, sum)
apply(a, 1:2, sum) # Sum all elements in the 3rd dimension
apply(a, c(1L, 3L), sum) # Sum all elements in the second dimension

aperm(a, c(3L, 1L, 2L)) # Permuting array (reshuffling dimensions)
aperm(a) # Be default reverses dimensions, same as aperm(a, c(3L, 2L, 1L))

### Lists ------------------------------------------
# All objects we have seen so far were based on atomic vectors. 
# While we can create 2D and 3D etc. matrices and arrays from vectors by simply
# attaching appropriate 'dim' attributes or using the matrix() or array() constructors,
# a fundamental limitation remains that all data in the vector needs to be of the same type. 

# Thus we need a kind of container object that allows us to pack vectors of different types in one objects
# this is what the List does in R:

list1 <- list(7.5, 9:10, "word", 3 + 8 != 5) # the list function combines vectors of different lengths and types into a list
list1
list2 <- list(1:10, c("a", "b", "c"), TRUE, FALSE) 
list2
str(list2)
is.list(list2) 
# Named list:
list2 <- list(avec = 1:10, bvec = c("a", "b", "c"), cvec = c(TRUE, FALSE))
list2
str(list2)
# Lists are the preferred vehicle to package information into a single object, (e.g. results from a linear model estimation) ..coming up

# Apart from allowing different vector lengths and types to be packed into a list, a list itself can be packed into a list 
list2 <- list(avec = 1:10, bvec = c("a", "b", "c"), cvec = c(TRUE, FALSE),
              sublist = list(d = "q", e = 2L, f = TRUE))
list2
list2$avec    # Use $ to pool out list elements by their name
list2$sublist 
list2$sublist$e 

list2[2L] # gives second element but data type is still list.
list2["bvec"] # same thing (subsetting by name as we have seen before with vectors)
list2[[2L]] # we need double brackets to get just the content, generally this is what we want
list2[["bvec"]] # Same thing
list2$bvec == list2[[2L]] # These give identical output ($ <=> [[]])
list2$bvec == list2[["bvec"]]

class(list2[[2L]]) # this is a vector!!
class(list2[2L])   # This is a sublist!!

list2[[4L]]
class(list2[[4L]]) # This is also list, which we nested inside list2

# list functions
length(list2)
lengths(list2)

# for lists of equal values
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
lapply(list3, mean)
# gives warning because
mean(list(8:10))

### Date Frame's ------------------------------------------
# Similarly to Matrices which we learnt are just vectors with a dimension attribute and additional methods and properties, #
# A data frame is a list of equal-length columns with added properties. It is the fundamental structure to contain datasets in R:

# Using the data.frame constructor
students <- data.frame(weights = c(60.5, 72.5, 45.2), # Numeric vector
                       genders = factor(c("Male", "Male", "Female")), # Factor
                       econ.score = econ.score, # Integer vector
                       math.score = math.score, 
                       row.names = c("bob", "paul", "ashley"))
str(students)
# Lets construct it manually:
students <- list(weights = c(60.5, 72.5, 45.2), 
                 genders = factor(c("Male", "Male", "Female")), 
                 econ.score = econ.score, 
                 math.score = math.score)
attr(students, "row.names") <- c("bob", "paul", "ashley")
class(students) <- "data.frame"

# Same thing using the structure function 
students <- structure(list(weights = c(60.5, 72.5, 45.2), 
                 genders = factor(c("Male", "Male", "Female")), 
                 econ.score = econ.score, 
                 math.score = math.score),
                 row.names = c("bob", "paul", "ashley"), class = "data.frame")

students
View(students)

# We can subset a data frame like a matrix
students[, 2L] 
students[, "genders"]
str(students[, "genders"])
students[, "genders", drop = FALSE]
str(students[, "genders", drop = FALSE])

# Or like a list
students[2L] 
students[[2L]] 
students$genders

# All these give identical output:
identical(students[, 2L], students[, "genders"])
identical(students[, 2L], students[[2L]])
identical(students[, 2L], students[["genders"]])
identical(students[, 2L], students$genders)

# these are also equivalent
identical(students[, 2L, drop = FALSE], students[2L])
identical(students[, 1:2], students[1:2])
identical(students[, -2L], students[-2L])

# Row-subsetting also works in a similar way
students[1L, ] # Since since data types can be different, this preserves the data frame (i.e. no dropping dimensions for rows)
students[2:3, ] 

# Subsetting based on content of the data.frame
students[students$genders == "Male", ]
students[students$genders %in% c("Male", "Female"), ] # This is non-sensible here, but if there are more than two choices you can so like that
students[students$weights > 56, ] 
students[students$econ.score > 80 & students$math.score > 80, ] # Can combine arbitrary statements using & | ! and () to group statements

# A more user friendly way to do this is with the subset() function
subset(students, genders == "Male")
subset(students, econ.score > 80 & math.score > 80)
subset(students, econ.score > 80 & math.score > 80, weights:genders) # also selecting columns

# Computing or deleting columns
students$new = 1
students$sum.score = students$econ.score + students$math.score
students
students$new = NULL # deleting a column
students
students[["new"]] = 1 # same thing
students[["new"]] = NULL 
# Multiple columns
students[["new"]] = 1 
students[, c("new", "sum.score")] = NULL # not also students[c("new", "sum.score")] works as a data.frame is also a list
students

# Again a more user friendly way to do this is offered by transform()
transform(students, new = 1)
transform(students, sum.score = econ.score + math.score)
# Can also transform multiple columns, and save result
students <- transform(students, sum.score = econ.score + math.score, 
                                mean.score = (econ.score + math.score) / 2)
students
# Deleting columns this way
students <- transform(students, sum.score = NULL, mean.score = NULL)

# Ordering
students[order(students$math.score), ] # Default: Ascending order
students[order(students$math.score, decreasing = TRUE), ]
# First by gender, then by econ score, in ascending order
students[order(students$genders, students$econ.score), ]

# Note that there is no user friendly function to do this in basic R, 
# but we will learn those supplied by packages in Course 2 (dplyr::arrange, collapse::roworder and data.table::setorder)

# Computing in a list or data.frame environment without 
with(students, (math.score + econ.score) / 2)
with(list2, sum(avec))

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

# Excursus: Datasets and objects included in R:
data()  # shows all datasets in all packages
help(package = "datasets") # Datasets in the datasets package

# Applying functions to data.frame columns: Base R provides 2 options: lapply() or sapply()
View(airquality) # This is a dataset about New York Air Quality Measurements: Daily air quality measurements in New York, May to September 1973.
summary(airquality)
lapply(airquality, sum)
lapply(airquality, sum, na.rm = TRUE) # na.rm because Ozon and Solar.R contain some missing values
unlist(lapply(airquality, sum, na.rm = TRUE))
sapply(airquality, sum, na.rm = TRUE)

# Converting to and from matrix: 
as.data.frame(m)
as.matrix(students[colnames(m)]) # if converting to matrix, all columns must be of homogeneous type

# Important functions
?na.omit # removes missing values or rows from vectors, matrices or data frames
na.omit(airquality)

?split # Splits a vector / matrix or data frame into groups based on vectors / factors
View(iris) # 	Edgar Anderson's Iris Data: gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.
split(iris[1:4], iris$Species)


## (2.3) Functional Programming ------------------------------------------------------

hello <- function(yourname) {
  paste("Hello", yourname)
}
hello("Sebastian")

# don't need the curly braces if just one line of code...
su <- function(x) c(Mean = mean(x), SD = sd(x))

su(airquality$Wind)

# This is another function with 2 arguments. The second argument has a default, so it is not necessary to supply a value to 'goodbyeÄ 
# This is the most readible way to write the function 
hello <- function(yourname, goodbye = FALSE) {
  if (goodbye) { # or if(isFALSe(goodbye)) if you want to make sure a logical FALSE is passed to the argument
    paste("Goodbye", yourname)
  } else {
    paste("Hello", yourname)
  }
}
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
# But this is cnsidered bad style. If you use multiple lines you should at least wrap your function body in curly braces
hello <- function(yourname, goodbye = FALSE) {
  if (goodbye) paste("Goodbye", yourname) else 
    paste("Hello", yourname)
}

# The most compact way to write this function is actually to put the if clause inside paste():
hello <- function(yourname, goodbye = FALSE) paste(if (goodbye) "Goodbye" else "Hello", yourname)

# We can also define and ad-hov function.
apply(mtcars, 2L, function(x) c(N = sum(!is.na(x)), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x)))
# Note that apply() was ment for matrices, not data.frames. so this first (internally) coerces the data frame to matrix

# This is more explicit as to what is happening internally
apply(as.matrix(mtcars), 2L, function(x) c(N = sum(!is.na(x)), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x)))

# How do I know this? -> look at the source code of the function 
apply
View(apply)
class(apply)


# Ending the Session ----------------------------------------
ls() # list all objects currently in the workspace (global environment)
rm(avec, chr, x, y, students) # remove some of them
rm(list = ls()) # remove all of them
gc()            # garbage collection (recycling memory used by these objects)






