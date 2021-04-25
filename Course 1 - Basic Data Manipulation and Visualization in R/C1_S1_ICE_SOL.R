############################################
# Data Science Training
# Solution to the In-Class Exercises of
# Course 1 Session 1
############################################

#****************************
### In-Class Exercise 1 -----
#****************************

# (a) Generate an arbitrary numeric vector "v" and an integer vector "vi" of length 5 using the c() function
v <- c(1.4, 3.5, 456.6, 3.4, 3.3)
vi <- c(3L, 5L, 1L, 1000L, 46L)

# (b) Check the mode and the storage type of the vectors using mode(), typeof() and the corresponding is._() functions
mode(v)
mode(vi)
typeof(v)
typeof(vi)
is.numeric(v)
is.numeric(vi)
is.double(v)
is.double(vi)
is.integer(v)
is.integer(vi)

# (c) Check the structure with str() and the length of the vectors with length()
str(v)
str(vi)
length(v)
length(vi)

# (d) Replace "vi" with an integer sequence of values from 1 to 10 in steps of 2, using the seq() function
vi <- seq(1L, 10L, 2L)
vi

# (e) remove "v" and "vi" using rm()
rm(v, vi)


#****************************
### In-Class Exercise 2 -----
#****************************

# Problem 1: Consider two vectors of the weight and height of 5 people
weights <- c(80.5, 63.9, 75.3, 68.1, 79.3) # weights in kg
heights <- c(1.94, 1.80, 1.75, 1.71, 1.85) # heights in m

# (a) calculate the Body Mass Index, BMI = weight divided by the square of height, save it in a vector called "bmi"
bmi <- weights / heights^2
bmi

# (b) Compute the mean, median and standard deviation of the BMI's
mean(bmi)
median(bmi)
sd(bmi)

# (c) Sort the BMI values, and compute the order of them
sort(bmi)
order(bmi)

# (d) round the BMI values to the full integer, and compute a frequency table of them
table(round(bmi))

# (e) look up the documentation of the trunc() function. When do floor() and trunc() give different results?
?trunc
floor(1.5); trunc(1.5)
floor(-1.5); trunc(-1.5)

# (f) The R-Squared is a popular statistic for the goodness of fit of a regression model. 
#     Compute the R-Squared of the regression of weights on heights using the cor() function. 
cor(weights, heights)^2
summary(lm(weights ~ heights)) # Check with lm

# Problem 2: Consider three vectors of standardized characteristics of people
p1 <- c(0.06843254, -0.13153881,  0.76021149,  0.7880306775)
p2 <- c(0.79301235, -0.13153881,  0.81685914,  1.0504160307)
p3 <- c(1.03453895, -0.13153881,  0.81685914,  1.4439940605)

# (a) Use the Euclidian Distance = The square root of the sum of the squared differences between the vector elements
#     to compare person 1 to person 2 and person 3. Which of the two persons is more similar to person 1?
sqrt(sum((p1 - p2)^2)) # Person 2 is more similar to person 1, the Euclidian Distance is smaller.
sqrt(sum((p1 - p3)^2))

# (b) check your results against the following code
dist(rbind(p1, p2, p3))

# (c) Do another comparison of p1 with p2 and p3 by computing the correlation between them. 
#     Do you get the same result as from your distance calculation?
cor(p1, p2)
cor(p1, p3)

# (d) Consider these versions of p2 where some strange values are inserted. 
#     Compute the Euclidian distance between p1 and p2 as in part (a) for each p2 below and make sure you understand the outcome
p2 <- c(0.79301235, -0.13153881,  NA,  1.0504160307)
sqrt(sum((p1 - p2)^2)) # NA propagates
p2 <- c(0.79301235, -0.13153881,  -Inf,  1.0504160307)
sqrt(sum((p1 - p2)^2)) # As (-Inf)^2 = Inf
p2 <- c(0.79301235, -0.13153881,  NULL,  1.0504160307)
sqrt(sum((p1 - p2)^2)) # Warning because NULL is removed from the vector, so p2 is shorter than p1


#****************************
### In-Class Exercise 3 -----
#****************************

# Consider The following vector of 4 Ugandan districts
districts <- c(" Abim.", "bulisa", "MUKONO? ", "   Arua__")

# (a) Remove all leading and trailing white space, save your result replacing districts
districts <- trimws(districts)

# (b) remove all irregular characters (".", "?" and "__"). Note that you have to escape some characters by preceding them with "\\" 
help("regex")
districts <- gsub("\\.|\\?|_", "", districts)

# (c) Turn all strings into upper case letters
districts <- toupper(districts)

# (d) Now capitalize only the first letter using the function tools::toTitleCase()
districts <- tools::toTitleCase(tolower(districts))

# (e) Check the number of characters in each district name, and which names start with "A"
nchar(districts)
startsWith(districts, "A")

# (f) Sort the cleaned districts in descending alphabetic order
sort(districts, decreasing = TRUE)


#****************************
### In-Class Exercise 4 -----
#****************************

# Problem 1: Consider again the vector of weights used earlier
weights <- c(80.5, 63.9, 75.3, 68.1, 79.3) # weights in kg

# (a) Get the weight of the second and 4th person using integers, and again using a logical statement
weights[c(2L, 4L)]
weights[weights < 70]

# (b) Get the weight of all but the second and 4th persons using integers, and again using a logical statement
weights[-c(2L, 4L)] # Or:
weights[c(1L, 3L, 5L)]
weights[weights > 70]

# Problem 2: Consider the following integer vector
z <- c(1L, 4L, 6L)

# (a) create a logical vector of length 10 that has TRUE in the elements given by z
lv <- logical(10L)
lv[z] <- TRUE
lv

# (b) Get z back again from the logical vector created in part (a)
which(lv)
z


#****************************
### In-Class Exercise 5 -----
#****************************

# Problem 1: Consider again the vector of weights used earlier
weights <- c(80.5, 63.9, 75.3, 68.1, 79.3)        # weights in kg
persons <- c("Dan", "Paul", "Anna", "Tim", "Tom") # names of the persons

# (a) assign the person names to the weights vector, and get the weight of Paul
names(weights) <- persons
weights

# (b) compare the weight of paul to the weights of Tim and Tom
weights["Paul"] < weights[c("Tim", "Tom")]

# Problem 2: consider the following vector
k <- c("Yes", "Yes", "No", "Yes", "Don't Know", "No")

# (a) Coerce it to a factor, examine the structure of the factor
k <- as.factor(k) # or k <- factor(k)
str(k)

# (b) Get the levels, number of levels, and compute a frequency table for the levels
levels(k)  # or attr(k, "levels")
nlevels(k) # or length(levels(k))
table(k)

# (c) Coerce the factor to integer
as.integer(k)

# (d) Coerce the factor to character using as.character
as.character(k)

# (e) Manually coerce the factor to character using the integer values and the levels. 
kchar <- levels(k)[as.integer(k)]
kchar

# (f) Manually recreate the factor from the character vector using using match(), sort() and unique()
suk <- sort(unique(kchar))
kfac <- match(kchar, suk)
levels(kfac) <- suk # or attr(kfac, "levels") <- suk
class(kfac) <- "factor"
kfac
identical(kfac, k)


#****************************
### In-Class Exercise 6 -----
#****************************

# In this exercise we will use some data in matrix form built into R
data() # This shows all datasets built into R

# Problem 1: Consider this matrix of US personal expenditures
USPersonalExpenditure
upe <- USPersonalExpenditure # Just using a more convenient name

# (a) See its structure using str(), look at it in the data viewer using View(), compute the dimensions using dim()
str(upe)
is.matrix(upe)
View(upe)
dim(upe)

# (b) Get the health and private education expenditure for 1940, 1950 and 1960, using both names and indices
upe[c("Medical and Health", "Private Education"), c("1940", "1950", "1960")]
upe[c(3L, 5L), c(1L, 3L, 5L)]
upe[c(3L, 5L), c("1940", "1950", "1960")]

# (c) Compute the total expenditure in each year, and the average expenditure per item over time
colSums(upe)
rowMeans(upe)

# (d) Compute the share of different items in total consumption in each year
upes <- t(t(upe) / colSums(upe)) # Recall that arithmetic operations divide each column of the matrix with a vector. We need to divide each row.
colSums(upes)
upe / outer(rep(1L, nrow(upe)), colSums(upe)) # More efficient alternative, but more difficult to code

# (e) Compute the standard deviation in the consumption share of each item over time
apply(upes, 1L, sd)

# Bonus: These are some similar matrices that come with R, explore them using the tools shown above
VADeaths
state.x77

# Problem 2: Consider this time series of US quarterly log revenue from (1962,2Q) to (1971,4Q).
freeny.y

# (a) Look it up in the documentation, see the structure of it. and plot it using plot()
?freeny.y
str(freeny.y)
is.matrix(freeny.y)
plot(freeny.y)

# (b) Plot the first difference of it using diff()
plot(diff(freeny.y))

# (c) Consider this matrix of explanatory variables (view it, see the structure)
freeny.x
View(freeny.x)
str(freeny.x)

# (d) Create a matrix 'freeny' combining freeny.y and freeny.x
freeny <- cbind(freeny.y, freeny.x)
freeny
plot(freeny)
plot(diff(freeny))

# (e) Compute the correlations of the variables in the matrix, in levels and first-differences
round(cor(freeny), 2L)
round(cor(diff(freeny)), 2L)

# (f) Manually regress freeny.y on freeny.x, after adding an intercept column to freeny.x. 
#     Note that that OLS coefficients are computed as: (X'X)^(-1)X'y
X <- cbind(Intercept = 1, freeny.x)
solve(t(X) %*% X) %*% t(X) %*% freeny.y     # Estimating OLS coefficients manually
solve(crossprod(X), crossprod(X, freeny.y)) # More efficient way to write the same thing

# (g) Compare your coefficients to those reported by R's linear modeling command:
lm(freeny.y ~ freeny.x)
summary(lm(freeny.y ~ freeny.x))


#****************************
### In-Class Exercise 7 -----
#****************************

# This recreates ml
econ_score <- c(75.4, 80.1, 85.8)
math_score <- c(72.5, 67.6, 80.9)
m <- cbind(econ_score, math_score) 
rownames(m) <- c("bob", "paul", "ashley")
ml <- list(`2010` = m + rnorm(6L), 
           `2011` = m, 
           `2012` = m + rnorm(6L))

# Consider the list ml created above
str(ml)

# (a) Add scores for the year 2013 in a similar way I created them for 2012, look up the rnorm function. 
ml$`2013` <- m + rnorm(6L)
ml[["2013"]] <- m + rnorm(6L) # Same thing

# (b) Look at the structure of the list, view it in the data viewer, and print the scores for 2012 and 2013
str(ml)
str(ml, give.attr = FALSE) # More compact: does not print attributes
View(ml)
ml[c("2012", "2013")] # Or: 
ml[3:4]

# (c) Compute the average math and econ score and the average score for each student in each year using lapply()
avg_score <- lapply(ml, colMeans)
avg_student <- lapply(ml, rowMeans)

# (d) Simplify your calculation result from part (c) using simplify2array
simplify2array(avg_score)
do.call(cbind, avg_score) # Actually for a list of vectors do.call(cbind, ...) retains the names of the list and is the more efficient way to do this
do.call(cbind, avg_student)

# (e) Do steps (c) and (d) in one step using the sapply function. Look it up in the documentation
?sapply
sapply(ml, colMeans)
sapply(ml, rowMeans)

# (f) Delete the matrices for 2010 and 2013 from the ml list. 
ml[c("2010", "2013")] <- NULL
str(ml)


#****************************
### In-Class Exercise 8 -----
#****************************

# Consider the following dataset. 
esoph
?esoph

# (a) Examine it's structure and summarize it using summary()
str(esoph)
View(esoph)
summary(esoph)

# (b) Compute the percentage of cancer cases on the dataset
esoph <- transform(esoph, perc_cases = ncases / (ncases + ncontrols) * 100)

# (c) Subset the dataset to display only the 25-34 and 35-44 year olds with both above average alcohol and tobacco consumption
subset(esoph, agegp %in% c("25-34", "35-44") &
              alcgp %in% c("80-119", "120+") & 
              tobgp %in% c("20-29", "30+"))
# Alternative ways, using the fact that factors have integers underlying the levels
subset(esoph, agegp %in% c("25-34", "35-44") &
              unclass(alcgp) > 2L & 
              unclass(tobgp) > 2L)
subset(esoph, agegp %in% c("25-34", "35-44") &
              unclass(alcgp) > mean(unclass(alcgp)) & 
              unclass(tobgp) > mean(unclass(tobgp)))

# (d) Repeat the exercise in part (c), now with the people older than 65. What do you notice?
subset(esoph, agegp %in% c("65-74", "75+") &
              alcgp %in% c("80-119", "120+") & 
              tobgp %in% c("20-29", "30+"))

# (e) Make a copy of the dataset where you have coerced all factors to integer, and compute a correlation matrix of the variables. What do you notice?
esoph_copy <- esoph
esoph_copy[1:3] <- lapply(esoph_copy[1:3], as.integer)
round(cor(esoph_copy), 2L)

# (f) Using lm(), and the data from part (e), run a regression of he percentage of cases on agregp, alcgp, tobgp. 
#     Use summary() to summarize the model and interpret the results.
mod1 <- lm(perc_cases ~ agegp + alcgp + tobgp, data = esoph_copy)
summary(mod1)

# (g) Repeat part (f) using the original data. How do you interpret the results?
mod2 <- lm(perc_cases ~ agegp + alcgp + tobgp, data = esoph)
summary(mod2)

# (h) Repeat part (g), now using an interaction between alcgp and tobgp using alcgp:tobgp. How do you interpret the results?
mod3 <- lm(perc_cases ~ agegp + alcgp:tobgp, data = esoph)
summary(mod3)

# (i) Repeat part (h), now using a full interaction between alcgp and tobgp using alcgp * tobgp. How do you interpret the results?
mod4 <- lm(perc_cases ~ agegp + alcgp*tobgp, data = esoph)
summary(mod4)

# Extra: More datasets to explore:
longley
infert
USJudgeRatings
USArrests
iris

