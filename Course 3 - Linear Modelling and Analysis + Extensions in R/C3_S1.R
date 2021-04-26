#############################################################
# *************** R BASED DATA SCIENCE TRAINING *************
# Course 3: Linear Modelling and Analysis + Extensions in R
# -----------------------------------------------------------
# Prepared by: Sebastian Krantz, ODI Fellow, MEPD, MoFPED
# Permanent Contact: sebastian.krantz@graduateinstitute.ch
#############################################################

# Course Aim: To familiarize participants with linear regression modelling 
#             in R, including specification, testing and various extensions to 
#             the classical linear model.

# Today:
# (0) Rstudio Projects, Git and Github
# (1) Linear models with lm() and some built in tools and diagnostics
# (2) Testing linear models with 'lmtest' and 'car' (and 'performance')
# (3) Measuring model performance
# (4) Robust standard errors and bootstrapping
# (5) Reporting results from linear models in Latex, Excel, Word and Plots
# (6) Binary response models (Logit and Probit)
# (7) Some special linear models for causal inference (IV, RDD, DID)
# (8) Additional useful packages


setwd("Course 3 - Linear Modelling and Analysis + Extensions in R")

# (0) Projects Workflow in Rstudio, Git and Github -------------------------
#***************************************************************************

# Rstudio + Projects Workflow:
# https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects
# https://r4ds.had.co.nz/workflow-projects.html
# https://www.r-bloggers.com/2020/01/rstudio-projects-and-working-directories-a-beginners-guide/
# https://www.upgrad.com/blog/rstudio-projects-for-beginners/
# https://bookdown.org/ndphillips/YaRrr/projects-in-rstudio.html

# Version control with Git and github: https://happygitwithr.com/

# See also attached file on how to configure GitHub SSH access.


# (1) Linear models with lm() and Parametric Inference ---------------------
#***************************************************************************

# Note: Presentation taken from: (great resource)
# https://sites.google.com/site/econometricsacademy

library(carData) # Some datasets for the 'car' package
?Chile
str(Chile)
head(Chile)
View(Chile)

# First whenever you have a new dataset: Exploratory Analysis:
library(collapse)
qsu(Chile)
hist(Chile$statusquo)
descr(Chile)
pwcor(Chile, P = TRUE)
fNobs(Chile)
pwNobs(Chile)     # May be important for omitting variables that take out a lot of observations
fNdistinct(Chile) # Any dummies? 
pairs(Chile)      # Plotting

# Some advanced Visualization tools
DataExplorer::introduce(Chile)
DataExplorer::profile_missing(Chile)
DataExplorer::create_report(Chile) # Generates a nice report visually summarising the data
# Correlations and relationships
ggcorrplot::ggcorrplot(pwcor(Chile), lab = TRUE)
PerformanceAnalytics::chart.Correlation(num_vars(Chile))
GGally::ggpairs(Chile)

# Also see DescTools package for exploratory analysis and statistics + Tables. 

# Now on to linear models. Say we want to predict the statusquo variable 
qsu(Chile, statusquo ~ vote) # Summarising it by voting outcome: Clearly a strong relationship
barplot(fmean(Chile$statusquo, Chile$vote)) # People who votes yes favor the status quo

# R is an object oriented language, so when we fit a model, we can create an model object to save the results
model <- lm(statusquo ~ sex + age + income, data = Chile)
class(model)
str(model)
str(model, give.attr = FALSE) # More compact structure: without attributes: see what's stored inside the model object
names(model)
View(model)         # Can also view the content in the data viewer
print(model)        # Print method for lm() just shows the coefficients
summary(model)      # This is probably what you expect

# Alternative regression summaries. 
jtools::summ(model, digits = 4) # A more legible version # https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
olsrr::ols_regress(model) # Even more advance version. # https://cran.r-project.org/web/packages/olsrr/vignettes/intro.html

# As you can see, the model object itself already contains various statistics
model$coefficients
model$residuals
model$fitted.values
model$model         # Including the data used to estimate the model

# There also exist a few generic functions in R which can be used to extract this information (from a broader set of models)
coef(model)
resid(model)
fitted(model)
model.frame(model)
model.matrix(model) # model.matrix gives a matrix of the predictors with an intercept column in front. 

# More statistics can be extracted when we create a model summary object
model_smr <- summary(model)
names(model_smr)
model_smr$coefficients
model_smr$df        # degrees of freedom = number of observations - number of coefficients estimated (3 + constant)
model_smr$r.squared # R-squared: 1 means perfect fit, 0 means no fit

# So lm() fits a linear model which is also known as the Ordinary Least Squares (OLS) estimator. 
# But let's take a step back and ask: what do we mean by least squares?
x <- rnorm(100)                         # Random x
y <- 2 + 0.5 * x + rnorm(100, sd = 0.1) # Linear model: y = ß0 + ß1 x + e, where I have set ß0 = 2 and ß1 = 0.5, and e are random normal errors with a standard deviation of 0.1
plot(x, y)                                
# In reality we don't know the true model, and our goal is to estimate optimal coefficients ß0 and ß1 in order to
# predict the value of y from x using a linear relationship y = ß0 + ß1 x.

# We could try a model with ß0 = 1.8 and ß1 = 0.8. 
yhat <- 1.8 + 0.8 * x   
lines(x, yhat, col = "red")
# But how do we know that these are the optimal coefficients?
# One thing we can do to evaluate the model is to compute the deviation between y and the line (the predicted y from the model)
y - yhat
# Evidently some of these errors are positive if y is above the line (yhat) and some are negative if y is below the line. 
# We can turn negative errors into positive ones by squaring them, which also has the effect of aggravating large errors, 
# and we can calculate the sum of those squared errors (also called residuals) to give us a measure of the accuracy of our model.
sum((y - yhat)^2) # SSR (sum of squared residuals, in some books SSE for sum of squared errors)

# The lm() function finds the optimal coefficients ß0 and ß1 to draw a line that minimize the SSR
mod <- lm(y ~ x)
mod$coefficients
sum(mod$residuals^2)

# This function displays a regression line + equation in a plot
add_regline <- function(x, pos = "topleft", col = "blue") {
  abline(x, col = col)
  cf <- coef(x)
  formula_text <- sprintf("y = %.1f + %.1f x, R2 = %.3f", cf[1L], cf[2L], summary(x)$r.squared)
  legend(pos, formula_text, col = col, lty = 1, bty = "n")
}
# Adding the line to the plot
add_regline(mod)

# Mathematically, if we only have two variables x and y, the optimal coefficients ß0 and ß1 are easily obtained as follows:
ß1 <- cov(x, y) / var(x)
ß0 <- mean(y) - ß1 * mean(x)

# In multivariate settings with more than one predictor, the coefficients are given by
# coef = (X'X)^(-1)X'y where X is a matrix of predictors in the columns preceded by a column of 1's for the intercept, and ' denotes the matrix transpose. 
# Using this, we can manually calculate the coefficients of the model fitted above:
coef(model)
X <- model.matrix(model)   # This gets X from the model
y <- model$model[[1]]      # This gets y: The first variable in the model frame
# This calculates the coefficients
solve(t(X) %*% X) %*% t(X) %*% y  
# In practice for programming we would use one of 2 more efficient alternatives:
qr.coef(qr(X), y) # Calculates the coefficients using a QR decomposition: Recommended for general settings
chol2inv(chol(crossprod(X))) %*% crossprod(X, y) # Using a Choleski Decomposition: More efficient but requires crossprod(X) (which is an efficient way to write t(X) %*% X) to be positive definite (= All columns of X must be strictly linearly independent, which also should normally be the case)

# The R-Squared is a measure of the models fit and represents the percentage of variation in Y explained by the model. 
# This computes the R^2 manually
SSR <- sum(model$residuals^2)
SST <- sum((y - mean(y))^2) 
R2 <- 1 - SSR/SST
# For programming we can simply write ...
1 - var(model$residuals) / var(y)
# ...Or Another way: This shows you why it is called the r-squared
cor(y, fitted(model))^2

# Another statistic is the Adjusted R-squared, which penalizes the models fit for the number of predictors (more in section 3) 
model_smr$adj.r.squared 
# Manually calculating adjusted R2: 
1 - (var(model$residuals) * (nobs(model)-1))/(var(y) * model$df.residual)
# The models F-statistic allows us to do inference on the whole model: Does it account for variation in the outcome?
model_smr$fstatistic

# To do inference on the coefficients, we consider the variance-covariance matrix of the coefficients.
vcov(model)  
# The standard error visible in the coefficients table (model_smr$coefficients) is the square root of the diagonal elements in this matrix
SE <- sqrt(diag(vcov(model)))
# To do inference on the coefficient, we use a t-statistic, which is is simply the coefficient divided by the standard error. 
# It measures how confident we are that the coefficient is different from 0. 
# If abs(tstat) > 1.96 for nobs(model) > 100, this indicates that 95% of samples drawn from the population would result in a smaller tstat if the true coefficient was 0, which gives us confidence that it is greater than 0. 
tstat <- coef(model) / SE
# the t-statistic follows a t-distribution, from which we can compute the probability of observing a t-statistic as large as this one if the true regression coefficient is 0 (Also known as P-Value). 
# By default the coefficints table reports a 2-sided hypothesis test, where we test the Null hypothesis of no effect against the alternative of either a large positive or a large negative t-statistic.
P <- 2 * pt(abs(tstat), model$df.residual, lower.tail = FALSE) # Using a t-distribution with model$df.residual = n - k - 1 degrees of freedom (n = number of observations, k = number of predictors)
# Compare out manual computations to the coefficients table calculated by summary(model)
cbind(coef = model$coefficients, SE = SE, t = tstat, P = P)
model_smr$coefficients

# Frequentist (classical) parametric inference revolves all around the concept the the sampling distribution. 
# This is the hypothetical distribution of coefficient values we would obtain if we could repeatedly draw a 
# sample of observations from the population and calculate a regression coefficient. Each sample gives a slightly
# different regression coefficient. Normally we only have one sample of observations, thus we cannot observe
# the sampling distribution, but for illustration purposes we can simulate it: 

# This function runs a simple regression sampling 100 observations at random from a population
mysamplereg <- function() {
  x <- rnorm(100)
  y <- 2 + 0.02 * x + rnorm(100, sd = 0.1)
  lm(y ~ x)
}

mysamplereg()$coefficients # Each execution gives a different coefficient

# We can execute this function 1000 times and save the coefficients from each run in a matrix called 'simul'
simul <- t(replicate(1000, mysamplereg()$coefficients))
# The histogram of the coefficent on x (ß1) resembles the sampling distribution of ß1
hist(simul[, 'x'], breaks = 100)
# There are two important results in statistics that tell us something about the sampling distribution:
# - The Law of Large Numbers (LLN) says the sampling distribution is centered on the true population mean and will converge to the mean as the sample size n increases towards infinity.
# - The Central Limit Theorem (CLT) says that the shape of the sampling distribution is that of a normal distribution. 

# The standard error (SE) reported in the coefficients table, is simply an estimate of the standard deviation of the coefficient estimates that make up the sampling distribution
sd(simul[, 'x'])
# Normally we only have one sample, from with the SE is estimated using a formula which relies on the LLN and CLT:
summary(mysamplereg())$coefficients['x', 'Std. Error']
# The P-value likewise is an estimate of the number of coefficients in the sampling distribution that are greater/smaller than 0 if the hypothesized effeect is positive/negative
# If we have the sampling distribution as in this simulation, we can simply compute it for a positive coefficient using
1 - mean(simul[, 'x'] > 0)

# Other functions allow us to calculate: Confidence intervals
confint(model, level = 0.95) # 95% confidence interval.
# regarding interpretation, note that a 95% confidence level does not mean that there is a 95% probability that the population parameter lies within the interval,
# rather, the population parameter will lie within the confidence intervals computed from 95% of random samples drawn from the population.  
# See https://en.wikipedia.org/wiki/Confidence_interval
jtools::summ(model, confint = TRUE) # Regression summary with confidence intervals

# How do we make predictions on new data?:
predict(model, newdata = Chile[1:10, ])

# Testing exclusion restrictions for exclusion of one or more variables in the model. H0: income is irrelevant 
restricted_model <- lm(statusquo ~ sex + age, data = na.omit(Chile))
full_model <- lm(statusquo ~ sex + age + income, data = na.omit(Chile))
anova(restricted_model, full_model) # We fail to reject H0 at the 5% level (P > 0.05).

# A faster function to do this without first estimation two models is collapse::fFtest: 
library(magrittr) # Pipe operators
na_omit(Chile) %$% fFtest(statusquo, income, cbind(sex, age))


#****************************
### In-Class Exercise 1 -----
#****************************

library(collapse)
library(jtools)

# Load the 2014 Census and define and run a regression model of your choosing, where you predict
# one census variable using 2-4 other variables. The model should be sensible, so come up with a theory 
# how each variable you include in the model affects the outcome. 
CENS <- readRDS("Course 2 - Advanced Data Manipulation and Visualization in R/data/UBOS 2014 census.rds")
View(namlab(CENS))

# My model predicts the percentage of the 6-12 years not attending school from 2 variable indicating poverty levels and the distance to a primary school, respectively: 
vars <- c('EDU_6_12_NAS_P', 'WEA_L2MAD_P', 'SDL_EDU_PRI_P')
# Some summary statistics
qsu(CENS, cols = vars, vlabels = TRUE)
descr(CENS, cols = vars)
pwcor(CENS[vars])
# Histograms
CENS %$% hist(EDU_6_12_NAS_P, xlab = vlabels(EDU_6_12_NAS_P), breaks = 50)
CENS %$% hist(WEA_L2MAD_P, xlab = vlabels(WEA_L2MAD_P), breaks = 50)
CENS %$% hist(SDL_EDU_PRI_P, xlab = vlabels(SDL_EDU_PRI_P), breaks = 50)

# This estimates the model
my_model <- lm(EDU_6_12_NAS_P ~ WEA_L2MAD_P + SDL_EDU_PRI_P, CENS)
summ(my_model)


####################################################
# Excursus: Running regressions by district

district_models <- CENS %>% 
  rsplit(~ District, cols = vars) %>% # Split the data into district chuncks
  lapply(lm, formula = EDU_6_12_NAS_P ~ WEA_L2MAD_P + SDL_EDU_PRI_P) # Estimate a linear model for each district

# Summarise a single district model
summ(district_models$Agago)
# Summarise all models
lapply(district_models, summ)
# View coefficients in a single long table
district_models %>% 
  lapply(lmtest::coeftest) %>% 
  unlist2d("District", row.names = "Coef") %>% View


####################################################
# Excursus: The ceteris paribus (all factors held constant) interpretation of linear regression with multiple RHS variables

# The coefficient on x (ß1) in the multivariate linear regression of y on x and z amounts to first regressing y on z, taking 
# the residuals res_y, regressing x on z and taking residuals res_x and then regressing res_y on res_x, which yields ß1. 
# Below an example from the census where y = EDU_6_12_NAS_P, x = WEA_L2MAD_P, and z = SDL_EDU_PRI_P. 

# Regressing y on z
res_mod_ED <- lm(EDU_6_12_NAS_P ~ SDL_EDU_PRI_P, CENS)
res_ED <- resid(res_mod_ED)
plot(EDU_6_12_NAS_P ~ SDL_EDU_PRI_P, CENS)
abline(res_mod_ED, col = "red")
plot(res_mod_ED$model$SDL_EDU_PRI_P, res_ED)
# Regressing x on z
res_mod_POV <- lm(WEA_L2MAD_P ~ SDL_EDU_PRI_P, CENS)
res_POV <- resid(res_mod_POV)
plot(WEA_L2MAD_P ~ SDL_EDU_PRI_P, CENS)
abline(res_mod_POV, col = "red")
plot(res_mod_ED$model$SDL_EDU_PRI_P, res_POV)
# Regressing res_y on res_x...
lm(res_ED ~ res_POV)
# ... gives the same coefficient as regressing y on x and controlling for z:
lm(EDU_6_12_NAS_P ~ WEA_L2MAD_P + SDL_EDU_PRI_P, CENS)


####################################################
# Excursus: Dummy variables, interactions, and variable transformations

# A dummy is a variable that has only 2 distinct values, 0 and 1.
# In R, factor variables are coded as dummies:
summary(model)
# Note the sex factor variables used in this model
model$model$sex
# model.matrix gives us the matrix of predictors. Here the factor was turned as a sexM dummy indicating male gender.
head(model.matrix(model))
# Factors with more than one category are coded as a set of dummies, where the first factor level (here central region) becomes the base category.
factor(CENS$Region)
mod <- lm(EDU_6_12_NAS_P ~ WEA_L2MAD_P + SDL_EDU_PRI_P + factor(Region), CENS)
summary(mod) # jtools::summ(mod) or summary(mod) (whatever you prefer)
head(model.matrix(mod)) # See the dummies created for all regions apart from Central which is the base category.

# Categorical variables coded as dummies are to be interpreted as changes in the dependent variable relative to the base category, 
# and holding constant the level of all other variables in the regression (ceteris paribus). For example we can say that
# holding constant age and income, males have a 0.15 smaller support for the status quo than females.
round(coef(model), 2)
# For the second model we can say that holding constant poverty and distance to school, the eastern region has 2.3% higher absenteism 
# compared to the central region, whereas the northern region has 12.5% higher absenteism. 
round(coef(mod), 2)
# Note that holding constant poverty and distance to school means these remaining differences between the regions must be due to
# other unobservable factors such as cultural factors in the north. If these factors are correlated with poverty or distance to primary school,
# (which is something we cannot really test for but should debate theoretically) we have violated an assumption of the linear model, 
# which is the assumption of mean independence (zero conditional mean), stating that, conditional on the regressors included in the model, 
# the mean of the unobservable component (the error term) should be zero. 


# Interactions can be between dummies / categorical variables, dummies and continuous vaiables, or between two continuous variables.
# For example, we can add an interaction between the sex dummy and the continuous variable age: 
mod2 <- lm(statusquo ~ income + education + age + sex + sex:age, data = Chile)
mod2
# The key to interpreting interactions is to note that the effect of a variable now depends on the level of the variable it is interacted with. 
# The cleanest way to see this is by taking a derivative of the dependent variable w.r.t. the variable we are interested in.
# Consider a model y = ß0 + ß1 x1 + ß2 x2 + ß3 x3 + ß4 x2*x3 + e, then the effect on y of a unit change in x1 is simply dy/dx1 = ß1, 
# but the effect on y of a unit change in x2 is dy/dx2 = ß2 + ß4*x3. Thus the effect of x2 depends on the level of x3, and ß2 is the 
# marginal effect of x2 on y if and only if x3 = 0. 
summ(mod2, digits = 5)
# In the above example, we can see that the effect of age on support for the status quo is dstatusquo/dage = 0.00205 + 0.00453*sexM, 
# so for females the effect of an extra year is 0.00205, but for males the effect is 0.00205 + 0.00453 = 0.00658. Note however that 
# neither age nor the interaction effect are statistically significant at the 5% level, so we should not draw conclusions from this analysis. 

# Technically, we can interact two variables in the regression formula using the `:` operator e.g. x1:x2. 
# We can also specify a full interaction using `*`, which will also include each of the interacted variables:
lm(statusquo ~ income + education + age*sex, data = Chile) # Same as lm(statusquo ~ income + education + age + sex + age:sex, data = Chile)

# As a final interesting example here, let me estimate a full interaction between variables in my model
summ(lm(EDU_6_12_NAS_P ~ WEA_L2MAD_P*SDL_EDU_PRI_P*qF(Region), CENS), digits = 5) 
# So the effect of distance to primary school depends on both the region and the level of poverty as well as the combination of region and poverty level:
# dEDU_6_12_NAS_P/dSDL_EDU_PRI_P = 0.20726 - 0.62598*WEA_L2MAD_P - 0.05880*Eastern - 0.04452*Northern - 0.02424 Western + 0.00239 WEA_L2MAD_P*Eastern + 0.00810 WEA_L2MAD_P*Northern + 0.00099 WEA_L2MAD_P*Western
# So for the average poverty level given by 
fmean(CENS$WEA_L2MAD_P)
# the marginal effect of distance to primary school on absenteism in the Northern Region would be: 
# 0.20726 - 0.00158*11.18147 - 0.04452 + 0.00810*11.18147 = 0.2357
# Make sure you understand this calculation where I substituted WEA_L2MAD_P = 11.18147 and Centeral = 0, Western = 0, Eastern = 0, Northern = 1 in the 
# equation for dEDU_6_12_NAS_P/dSDL_EDU_PRI_P derived above. 

# In practice I think the 3-way interaction is a bit too complex, and many terms are statistically insignificant. We could try a model without the 
# 3-way interaction:
summ(lm(EDU_6_12_NAS_P ~ WEA_L2MAD_P*SDL_EDU_PRI_P*qF(Region) - WEA_L2MAD_P:SDL_EDU_PRI_P:qF(Region), CENS), digits = 5) 
# This is easier to interpret, it basically tells us that the effect of distance to school increases with poverty levels, 
# and is higher in the northern region compared to the central region, all other factors held constant. 

# A final important subject to discuss at this point are variable transformations, which can be used to deal with heteroskedasticity (log-transformation of dependent variable), 
# skewness (log or Box-Cox) or aid interpretation. The log transformation where we take the natural log of a variable is the most common transformation as it
# can deal with both issues of heteroskedasticity in the error (taking log(y)), skewness in either dependent or independent variables, and has a natural interpretation
# in terms of percentage chages. Lets consider the income variable in the Chilean election data. 
hist(Chile$income)        # Evidently it is skewed to the right. 
hist(log(Chile$income))   # Taking the natural log rectifies this. 
model2 <-lm(statusquo ~ log(income) + age + sex, data = Chile)
summ(model2, digits = 5)
# The coefficient can be interpreted as a one percent change in income leads to a 0.033/100 = 0.00033 unit change in support for the status quo. 
# This is because the derivative of the logarithm is dlog(x)/dx = 1/x or dlog(x) = dx/x, so that the coefficient ß = dy(x)/dlog(x) = dy/(dx/x) = dy/(dx/x / 100) / 100, 
# is the change in y resulting from a 100 percent change in x, and dividing the coefficient by 100 yields the response to a 1% change in x (dx/x / 100). 

# Using similar logic about derivatives, we can see that if both x and y are log transformed, then the coefficient ß = dln(y(x))/dln(x) = (dy/y)/(dx/x)
# is the elasticity of y w.r.t. x (the % change in y following a 1% change in x), and if only y is log transformed, then 
# ß = dln(y(x))/dx = (dy/y)/dx = (dy/y / 100)/dx * 100 is the percentage change in y (dy/y / 100) resulting from a 1 unit change in x, multiplied by 100. 
# Guidelines for interpretation with log-transformed variables are also found here: http://www.cazaar.com/ta/econ113/interpreting-beta
# Some references below: 

# Interactions:
# https://www.econometrics-with-r.org/8-3-interactions-between-independent-variables.html
# https://www.medicine.mcgill.ca/epidemiology/joseph/courses/EPIB-621/interaction.pdf
# https://www.youtube.com/watch?v=vZUtDJbzFRQ
# http://www.sthda.com/english/articles/40-regression-analysis/164-interaction-effect-in-multiple-regression-essentials/
# https://statisticsbyjim.com/regression/interaction-effects/

# Data transformations: 
# https://daviddalpiaz.github.io/appliedstats/transformations.html
# http://www.cazaar.com/ta/econ113/interpreting-beta
# https://darrendahly.github.io/post/loglog/
# https://kenbenoit.net/assets/courses/ME104/logmodels2.pdf


####################################################
# Excursus: Recoding factors
# Basic R
factor(Chile$sex, levels = c("M", "F"))
relevel(Chile$sex, ref = "F") # Set referene level
# More options in forcats package
forcats::fct_relevel(Chile$sex, "M", "F") # set order
forcats::fct_relevel(Chile$sex, "F", "M")
# Recode (rename) levels
forcats::fct_recode(Chile$sex, Male = "M", Female = "F")
# Or just reverse:
forcats::fct_rev(Chile$sex)
forcats::fct_count(Chile$sex)
forcats::fct_infreq(Chile$sex) # Order in order of frequency (largest first, usful to use as base category)
# To remove unused levels:
droplevels() # base R
collapse::fdroplevels() # Same thing but much faster, can apply to whole dataset, keeps variable labels
forcats::fct_drop() # Only applies to a single factor, but can specify which unused levels to drop
# more functions at 
?forcats
# See also cheatsheet. And vignette
vignette("forcats")


####################################################
# Excursus: Categorizing continuous variables. 
# Default: right endpoints are includive of the value
cut(CENS$POP, pretty(CENS$POP, 8)) # or scales::pretty_breaks(8)(CENS$POP)
# Note intervals are not overlapping (0, 10000] (10000, 20000] means the first interval goes from 0-10000 and the second from anything larger than 10000 (like 10000.001) to 20000. 
cut(1:2, breaks=c(0,1,2)) # The intervals defined by the cut() function are (by default) closed on the right.
# Custom breaks: 
cut(CENS$POP, c(0, 100, 1000, 10000, 20000, 50000, Inf))
cut(CENS$POP, c(0, 100, 1000, 10000, 20000, 50000, Inf), 
    labels = c("0-100", "101-1,000", "1,001-10,000", "10,001-20,000", "20,001-50,000", "50,000+"))
# Note that for any input value falling outside of the bins you define, cut() will return a value of NA. (so I use Inf as the last bin)
# See https://stackoverflow.com/questions/8233846/cut-function-in-r-exclusive-or-am-i-double-counting

# The other way around: left endpoints are inclusive of the value + other features
Hmisc::cut2(CENS$POP, scales::pretty_breaks(8)(CENS$POP))

# Also see
ggplot2::cut_interval(CENS$POP, 5) # makes 5 groups with equal range
ggplot2::cut_number(CENS$POP, 5)   # make 5 groups with equal number of observations
ggplot2::cut_width(CENS$POP, 1000) # make n groups with 1000 observations each

# Even more functionality see package cutr on github: https://github.com/moodymudskipper/cutr
# remotes::install_github("moodymudskipper/cutr")




# (1.1) Assumptions underlying the linear model and visual diagnostics ---------------------------------------------------------

# Gauss Markov assumptions are standard assumptions for the linear regression model:

# 1. Linearity in parameters: The response can be written as a linear combination of the predictors. (With noise about this true linear relationship.)
   # y = ß0 + ß1 x1 + ß2 x2 + ... + e # Linear
   # y = ß0 + ß1 x1^(ß2 x2 + e)       # Non linear 
   # y = ß0 + ß1 x1^x2 + e            # Linear
   # y = ß0 + ß1 x1 + ß2 x1^2 + e     # Linear
   plot(Sepal.Length ~ Petal.Width, iris)
   abline(lm(Sepal.Length ~ Petal.Width, iris), col = "blue")
   # Fitting a 4th order polynomial: Linear in parameters
   mod <- lm(Sepal.Length ~ poly(Petal.Width, 4), iris)
   summary(mod)
   x <- seq(min(iris$Petal.Width), max(iris$Petal.Width), 0.005)
   points(x, predict(mod, list(Petal.Width = x)), col = "red", pch = 20)
   
# 2. Random sampling: If we want our coefficient estimate to be an unbiased estimate of the population coeffieint, we need this assumption. For inference we assume the errors are independent which also necesitates this assumption. 
# 3. No perfect collinearity: To accurately estimate the coefficients on different variables in our model these variables should not be perfectly correlated or linear combinations of one-another
# 4. Zero conditional mean (exogeneity) – regressors are not correlated with the error term. Also implies no omitted variables correlated with other variables in our model.
   # Example from my_model: 
   my_model
   # school absenteism = poverty (2 meals per day) + distance to primary school + error
   # If we omit distance to school and estimate
   # school absenteism = poverty (2 meals per day) + error
   # then the coefficient on poverty will be biased (too large) if poverty is correlated with distance to primary school, which is likely the case. 
   # Thus we need to include this control variable. There are likely many other omitted factors correlated with both predictors and outcome. In practice,
   # the zero conditional mean assumption is seldomly satisfied, unless we have some kind of causal or experimental design. 
   
# 5. Homoscedasticity – variance of error term is constant over the scale of predictors and outcome. 
   # The opposite is called Heteroskedasticity, here is an example:
   par(mfrow = c(1, 2))
   x <- runif(500, 0, 1)
   y_homo <- x + rnorm(500)
   plot(x, resid(lm(y_homo ~ x)), col = "gray", pch = 19, main = "Homoskedasticity")
   y_hetero <- x + rnorm(500, sd = x)
   plot(x, resid(lm(y_hetero ~ x)), col = "gray", pch = 19, main = "Heteroskedasticity")
   par(mfrow = c(1, 1))

# 6. Normality: error term is normally distributed
   hist(resid(lm(y_homo ~ x)), breaks = 30) # Looks pretty much like a normal curve

# See also https://learningstatisticswithr.com/book/regression.html#regressionassumptions
# And: http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/

# The (approximate) validity of some of these assumtions can be investigated through a simple set of plots. 
# Four of these plots are computed automatically when calling plot(model). But we will compute them manually first and walk through them:

# (I) Linearity and Homoskedasticity 

# First, lets plot the residuals against the fitted values:
plot(fitted(my_model), resid(my_model), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model")
abline(h = 0, col = "darkorange", lwd = 2)

# We should look for two things in this plot.
# (i) At any fitted value, the mean of the residuals should be roughly 0. 
#     If this is the case, the linearity assumption is valid. (For this reason, we generally add a horizontal line at y = 0 to emphasize this point)
# (ii) At every fitted value, the spread of the residuals should be roughly the same. 
#      If this is the case, the constant variance assumption is valid.
#      Constant variance is often called homoscedasticity. Conversely, non-constant variance is called heteroscedasticity. 

# Example of Non-linearity
x <- rnorm(100)
y <- 2 + 1 * x + 0.5 * x^2 + rnorm(100, sd = 0.3)
plot(x, y)
mod <- lm(y ~ x)
add_regline(mod)
plot(fitted(mod), resid(mod), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model")
abline(h = 0, col = "darkorange", lwd = 2)

# Example of Heteroskedasticity
x <- rnorm(100) + 6
y <- 2 + 1 * x + rnorm(100, sd = abs(x))
plot(y, log(y))
plot(x, log(y))
mod <- lm(log(y) ~ x)
add_regline(mod)
plot(fitted(mod), resid(mod), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model")
abline(h = 0, col = "darkorange", lwd = 2)

# -> looks like the linearity assumption is violated
# Lets check further 
plot(fitted(my_model), resid(my_model), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model")
abline(h = 0, col = "darkorange", lwd = 2)
rfit <- round(fitted(my_model))
lines(funique(rfit), fmean(resid(my_model), rfit))

# In a lot of contexts, especially where you’re only interested in the pattern of the residuals and not their actual values, 
# it’s convenient to estimate the standardised residuals, which are normalised in such a way as to have standard deviation 1.
# To get the standardised residuals, the command you want is this
rstandard(my_model)
plot(fitted(my_model), rstandard(my_model), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model")
abline(h = 0, col = "darkorange", lwd = 2)
# There are also studentized residuals (also called “jackknifed residuals”), which are standardized with the variance used for standardization computed without the data point. 
rstudent(model)

# (II) Normality

# We have a number of tools for assessing the normality assumption. The most obvious would be to make a histogram of the residuals. 
# If it appears roughly normal, then we’ll believe the errors could truly be normal.
hist(resid(my_model), breaks = 40) # Too spiky (Leptokurtic)

# Another visual method for assessing the normality of errors, which is more powerful than a histogram, is a normal quantile-quantile plot, or Q-Q plot for short.
# In R these are very easy to make. The qqnorm() function plots the points, and the qqline() function adds the necessary line. We create a Q-Q plot for the residuals to check if the errors could truly be normally distributed.
qqnorm(resid(my_model), main = "Normal Q-Q Plot, fit_1", col = "darkgrey")
qqline(resid(my_model), col = "dodgerblue", lwd = 2)
# In short, if the points of the plot do not closely follow a straight line, this would suggest that the data do not come from a normal distribution.
# The calculations required to create the plot vary depending on the implementation, but essentially the y-axis is the sorted data (observed, or sample quantiles), 
# and the x-axis is the values we would expect if the data did come from a normal distribution (theoretical quantiles).
# The Wikipedia page for Normal probability plots gives details on how this is implemented in R if you are interested.

# (III) Unusual Observations

# See: https://daviddalpiaz.github.io/appliedstats/model-diagnostics.html#unusual-observations
# -> Look at the 3 graphs used to explain the concepts !!!
# Also:  https://learningstatisticswithr.com/book/regression.html#regressiondiagnostics

# (a) Leverage
  # A data point with high leverage, is a very unusual data point that could have a large influence when fitting the model.
  # This doesn't necessarily have to correspond to a large residual: if the observation happens to be unusual on all variables in precisely the same way, 
  # it can actually lie very close to the regression line. 
  # Use the hatvalues() function to return the leverages of each data point (hi).
  hatvalues(my_model)
  # hi is a measure of the extent to which the i-th observation is "in control" of where the regression line ends up going. 
  # Leverage sums to the umber of predictors + constant
  sum(hatvalues(my_model))
  
  # How is leverage calculated ? (in case you are interested)
    # Hat maker matrix
    X <- model.matrix(model)
    H <- X %*% solve(crossprod(X)) %*% t(X)
    drop(H %*% model$model$statusquo) # Generates fitted values
    fitted(model) # Same thing
    # Leverages (for each observation): Diagonal elements of hatvalues matrix
    diag(H)
  
  # What leverage would be considered large? There is no exact answer to this question. 
  # A common heuristic would be to compare each leverage to two times the average leverage. 
  sum(hatvalues(my_model) > 2 * mean(hatvalues(my_model)))  # Number of observations with high leverage...
  # Corresponding Plot:
  hist(hatvalues(my_model), breaks = 40)
  abline(v = 2 * mean(hatvalues(my_model)))
  
  # In general, high leverage points are also worth looking at in more detail, but they’re much less likely to be a cause for concern unless they are also outliers.
 
# (b) Outliers
  # Outliers are points which do not fit the model well. They may or may not have a large affect on the model. 
  # To identify outliers, we will look for observations with large residuals.
  hist(resid(my_model))
  # We can look at the stndardized or studentized residual for each observation
  rstudent(my_model) 
  # Let's look at residuals above 2 standard deviations from their mean
  rstudent(my_model)[abs(rstudent(my_model)) > 2] 
  

# (c) Influence
  # This brings us to our third measure of unusualness, the influence of an observation. 
  # A high influence observation is an outlier that has high leverage. 
  # That is, it is an observation that is very different to all the other ones in some respect, 
  # and also lies a long way from the regression line.
  # Observations with some combination of high leverage and large residual, we will call influential.
  # something that is an outlier and has high leverage has a big effect on the regression line.
  
  # A common measure of influence is Cook’s Distance, which is defined as
  rstandard(my_model)^2 / length(coef(my_model)) * hatvalues(my_model) / (1- hatvalues(my_model))
  # Notice that this is a function of both leverage and standardized residuals.
  # The Cook’s distance for each point of a regression can be calculated using cooks.distance() which is a default function in R. 
  cooks.distance(my_model)
  # A rule of thumb is that an observation has high influence if Cook’s distance exceeds 4/(n - p - 1), where n is the number of observations and p the number of predictor variables.: http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/

  influential <- which(cooks.distance(my_model) > 4 / my_model$df.residual)
  
  # We can also do this with plot: 
  ?plot.lm
  plot(my_model, which = 4)
  plot(my_model, which = 5)
  
  # Finally, the function dfbeta gives the change in the coefficients if the observation added to the model. 
  # If we have found influential data points using Cooks Distance, it is also good to examine their dfbetas.
  # https://blogs.sas.com/content/iml/2019/06/17/influence-regression-dfbeta.html
  # https://stats.stackexchange.com/questions/327562/what-exactly-does-the-dfbeta-function-do
  
  my_model
  View(round(dfbeta(my_model)[influential, ], 4))
  # Lets take ob 6501 and excluse it from the model  
  my_model_m6501 <- lm(formula(my_model), data = my_model$model[-6501, ])
  coef(my_model_m6501)
  coef(my_model) - coef(my_model_m6501)
  # Is the same as
  dfbeta(my_model)[6501, ]
  
  # Now we can view The value of the coefficients we could get when including each observation
  (-dfbeta(my_model) %r+% coef(my_model))[influential, ] # Note that %r+% is a function in the collapse package
  
  

  
#****************************
### In-Class Exercise 2 -----
#****************************

hist(CENS$EDU_6_12_NAS_P)  
# Re-estimating the model excluding some outliers
my_model <- lm(EDU_6_12_NAS_P ~ WEA_L2MAD_P + SDL_EDU_PRI_P, fsubset(CENS, EDU_6_12_NAS_P < 45))  
summ(my_model)
hist(my_model$model$EDU_6_12_NAS_P)

?plot.lm
# Run diagnostic plots on the model you specified in exercise 1
par(mfrow = c(2,2)) # To plot them all in one chart
plot(my_model, which = c(1,2,4,5))  
par(mfrow = c(1,1))

# Diagnostic plots generated one by one
plot(my_model, which = 1)
hist(resid(my_model), breaks = 100)
plot(my_model, which = 2)
plot(my_model, which = 4)
plot(my_model, which = 5)


most_influential <- c(1068, 5070, 6630)
View(-dfbeta(my_model)[most_influential, ])
(-dfbeta(my_model) %r+% coef(my_model))[most_influential, ]

influential <- which(cooks.distance(my_model) > 4 / my_model$df.residual)
infl <- influential[order(cooks.distance(my_model)[influential])]
View(round(dfbeta(my_model)[infl, ], 4))

(-dfbeta(my_model) %r+% coef(my_model))[infl, ]


# For more details or a referesher, do Swirl regression diagnostics (course 9)
library(swirl)
swirl() # Start Swirl
bye()   # Exit swirl (or press ESC)

# Also please look at multivariable regression swirl course..

# You can install further courses at: https://github.com/swirldev/swirl_courses
install_course("R Programming")
install_course("Statistical Inference")
install_course("Regression Models")
  
  

# (2) Testing linear models with 'lmtest' and 'car' and 'performance' ------
#***************************************************************************

# lmtest package: various tests for linear models
lmtest::coeftest(model)             # T-tests of coefficients (as in regression summary), can support different standard errors
lmtest::coefci(model)               # 95% confidence interval 
lmtest::coefci(model, level = 0.99) # 99% confidence interval

# Can also display confint in jtools::summ (also supports different standard errors as we will see)
jtools::summ(model, confint = TRUE)  

# Regarding visual model diagnostics, the 'performance' package produces a quick and comprehensive plot:
performance::check_model(model)
# Another option: 
library(ggfortify)
autoplot(model)

# We can also implement various diagnostic tests to formally test our linear model assumptions.
  
# (I) Homoskedasticity 

  # While a fitted versus residuals plot can give us an idea about homoscedasticity, sometimes we would prefer a more formal test. 
  # There are many tests for constant variance, but here we will present one, the Breusch-Pagan Test. 
  # The Breusch-Pagan test fits a linear regression model to the squared residuals of a linear regression model (by default the same explanatory variables are taken as in the main regression model) 
  # and rejects if too much of the variance is explained by the additional explanatory variables.
    
  # H0: Homoscedasticity. The errors have constant variance about the true model.
  # H1: Heteroscedasticity. The errors have non-constant variance about the true model.
  lmtest::bptest(my_model)
  # Another very similar test in the car package
  car::ncvTest(model)
  # Another option
  performance::check_heteroscedasticity(model)
  
  # This is how we can manually run a BP test
  lmtest::coeftest(lm(rstudent(model)^2 ~ fitted(model)))
  # Or using the full set of predictors (need to omit the intercept)
  summary(lm(rstudent(model)^2 ~ -1 + model.matrix(model)))


# (II) Normality

  # Histograms and Q-Q Plots give a nice visual representation of the residuals distribution, however if we are interested in formal testing, 
  # there are a number of options available. A commonly used test is the Shapiro–Wilk test, which is implemented in R.
  shapiro.test(resid(my_model))
  # This gives us the value of the test statistic and its p-value. The null hypothesis assumes the data were sampled from a normal distribution, thus a small p-value indicates we believe there is only a small probability the data could have been sampled from a normal distribution.
  # For details, see: Wikipedia: Shapiro–Wilk test.
  
  # Other options: 
  performance::check_normality(my_model)
  
  # car also provides furtehr plots: 
  car::qqPlot(model)


# (III) Unusual observations
  car::outlierTest(model)
  performance::check_outliers(model) # using a higher outlier threshold than 4/model$df.residual
  performance::check_outliers(model, method = "cook", threshold = list(cook = 4/model$df.residual))
  performance::check_outliers(model, method = "mahalanobis")
  
  # A number of nice additional plot functions are provided in the car package. 
  car::influenceIndexPlot(model)
  car::influencePlot(model)
  car::leveragePlot(model)
  car::leveragePlots(model)
  car::outlierTest(model)
  car::residualPlot(model)
  car::residualPlots(model) # pearsons = ordinary residuals (weighted if weighted regression)
  car::dfbetaPlots(model)

# (IV) Multicollinearity
  # https://daviddalpiaz.github.io/appliedstats/collinearity.html
  # collinearity occurs in multiple regression when predictor variables are highly correlated. 
  # Collinearity is often called multicollinearity, since it is a phenomenon that really only occurs during multiple regression.
  pairs(model.frame(model))  
  collapse::pwcor(model.frame(model))
  car::vif(my_model)
  jtools::summ(my_model, vif = TRUE)
  performance::check_collinearity(model)
  # VIF > 10 is a problem... 
  
  # Calculate VIF:
  1/(1-summary(lm(WEA_L2MAD_P ~ SDL_EDU_PRI_P, fsubset(CENS, EDU_6_12_NAS_P < 45)))$r.squared)


# See also: https://github.com/easystats/easystats
  
# Suggested wokflow: 
plot(my_model, which = c(1,2,4,5))
# If there is a problem: Calulcate dfbetas.. 
-dfbeta(my_model)[most_influential, ]
# Test for heteroskedasticity
lmtest::bptest(my_model)
# Check for multicollinearity 
car::vif(my_model)
jtools::summ(my_model, vif = TRUE)
# VIF > 10 means R^2 of regressing your variable oon the others is > 90% -> Problem


# Alternatively: (all plots in one window)
performance::check_model(my_model)





  
#****************************
### In-Class Exercise 2 -----
#****************************
  
# Walk through the first example using mtcars data:
# https://daviddalpiaz.github.io/appliedstats/model-diagnostics.html#data-analysis-examples
  



# (3) Measuring model performance (model validation) -----------------------
#***************************************************************************

# A general resource:
# http://www.sthda.com/english/articles/38-regression-model-validation/

x <- rnorm(10)
y <- 1 + x + rnorm(10, sd = 0.3)
plot(x, y)
abline(lm(y ~ x))
points(x, fitted(lm(y ~ poly(x, 5))), col = "red")


# Standard Performance metrics: http://www.sthda.com/english/articles/38-regression-model-validation/158-regression-model-accuracy-metrics-r-square-aic-bic-cp-and-more/
AIC(model) # minimum is best
BIC(model) # minimum is best
summary(model)$r.squared
summary(model)$adj.r.squared
performance::r2(model) # R-squared (R2), representing the squared correlation between the observed outcome values and the predicted values by the model. The higher the adjusted R2, the better the model
performance::model_performance(model) 
# Root Mean Squared Error (RMSE), which measures the average prediction error made by the model in predicting the outcome for an observation. That is, the average difference between the observed known outcome values and the values predicted by the model. The lower the RMSE, the better the model.
# Mean Absolute Error (MAE), an alternative to the RMSE that is less sensitive to outliers. It corresponds to the average absolute difference between observed and predicted outcomes. The lower the MAE, the better the model
jtools::glance(model)

summ(model)
performance::model_performance(model) 

model2 <- lm(EDU_6_12_NAS_P ~ WEA_L2MAD_P + SDL_EDU_PRI_P + factor(Region),  fsubset(CENS, EDU_6_12_NAS_P < 45))
performance::model_performance(model2)

model3 <- lm(EDU_6_12_NAS_P ~ WEA_L2MAD_P + SDL_EDU_PRI_P + factor(District),  fsubset(CENS, EDU_6_12_NAS_P < 45))
performance::model_performance(model3)


# Another approach: Training and tests sets, or Cross-Validation 
# Source: http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/

# Cross-validation refers to a set of methods for measuring the performance of a given predictive model on new test data sets.
# The basic idea, behind cross-validation techniques, consists of dividing the data into two sets:
# The training set, used to train (i.e. build) the model;
# and the testing set (or validation set), used to test (i.e. validate) the model by estimating the prediction error.

# Briefly, cross-validation algorithms can be summarized as follow:
#   
# Reserve a small sample of the data set
# Build (or train) the model using the remaining part of the data set
# Test the effectiveness of the model on the the reserved sample of the data set. If the model works well on the test data set, then it’s good.

# The different cross-validation methods for assessing model performance are:
# (1) Validation set approach (or data split)
# (2) Leave One Out Cross Validation
# (3) k-fold Cross Validation
# (4) Repeated k-fold Cross Validation

# Each of these methods has their advantages
# Generally, the (repeated) k-fold cross validation is recommended.

library(caret) # This package greatly facilitates cross validation and general machine learning in R. 
# R2, RMSE and MAE are used to measure the regression model performance during cross-validation.

  # (1) The Validation set Approach
     # The validation set approach consists of randomly splitting the data into two sets: 
     # one set is used to train the model and the remaining other set sis used to test the model.

     # Split the data into training and test set
     set.seed(123)
     training_samples <- model$model$EDU_6_12_NAS_P %>%
        createDataPartition(p = 0.8, list = FALSE)
      train_data <- model$model[training_samples, ]
      test_data <- model$model[-training_samples, ]
      # Build the model
      model_vs <- lm(formula(model), data = train_data)
      # Make predictions and compute the R2, RMSE and MAE
      predictions <- model_vs %>% predict(test_data) # predict(model_vs, test_data)
      data.frame(R2 = R2(predictions, test_data$EDU_6_12_NAS_P),
                 RMSE = RMSE(predictions, test_data$EDU_6_12_NAS_P),
                 MAE = MAE(predictions, test_data$EDU_6_12_NAS_P))
      
      # RMSE
      sqrt(mean((test_data$EDU_6_12_NAS_P-predictions)^2))
      mean(abs(test_data$EDU_6_12_NAS_P-predictions))
      
      c(2, 5)
      sqrt(mean(c(2, 5)^2))
      mean(abs(c(2, 5)))
      
      # Note that, the validation set method is only useful when you have a large data set that can be partitioned. 
      # A disadvantage is that we build a model on a fraction of the data set only, possibly leaving out some interesting 
      # information about data, leading to higher bias. Therefore, the test error rate can be highly variable, depending on 
      # which observations are included in the training set and which observations are included in the validation set.

  # (2) Leave one out Cross-Validation
      # This method works as follow:
        
      # Leave out one data point and build the model on the rest of the data set
      # Test the model against the data point that is left out at step 1 and record the test error associated with the prediction
      # Repeat the process for all data points
      # Compute the overall prediction error by taking the average of all these test error estimates recorded at step 2.
      
      # Define training control
      train_control <- trainControl(method = "LOOCV")
      # Train the model
      loocv_model <- train(formula(model), data = model$model, method = "lm",
                           trControl = train_control)
      # Summarize the results
      print(loocv_model)
      
      # The advantage of the LOOCV method is that we make use all data points reducing potential bias.
      # However, the process is repeated as many times as there are data points, resulting to a higher 
      # execution time when n is extremely large.
      # Additionally, we test the model performance against one data point at each iteration. 
      # This might result to higher variation in the prediction error, if some data points are outliers. 
      # So, we need a good ratio of testing data points, a solution provided by the k-fold cross-validation method.

  # (3) K-fold Cross-Validation
    
      # The k-fold cross-validation method evaluates the model performance on different subset of the training data and then calculate the average prediction error rate. 
      # The algorithm is as follow:
        
      # Randomly split the data set into k-subsets (or k-fold) (for example 5 subsets)
      # Reserve one subset and train the model on all other subsets
      # Test the model on the reserved subset and record the prediction error
      # Repeat this process until each of the k subsets has served as the test set.
      # Compute the average of the k recorded errors. This is called the cross-validation error serving as the performance metric for the model.
      
      # K-fold cross-validation (CV) is a robust method for estimating the accuracy of a model.
      # The most obvious advantage of k-fold CV compared to LOOCV is computational. A less obvious but potentially more important advantage of k-fold CV is that it often gives more accurate estimates of the test error rate than does LOOCV (James et al. 2014).
    
      # Typical question, is how to choose right value of k?
        
      # Lower value of K is more biased and hence undesirable. On the other hand, higher value of K is less biased, but can suffer from large variability. 
      # It is not hard to see that a smaller value of k (say k = 2) always takes us towards validation set approach, whereas a higher value of k (say k = number of data points) leads us to LOOCV approach.    
      
      # In practice, one typically performs k-fold cross-validation using k = 5 or k = 10, as these values have been shown empirically to yield test error rate estimates that suffer neither from excessively high bias nor from very high variance. 
      
      # Define training control
      train_control <- trainControl(method = "cv", number = 10)
      # Train the model
      cv_model <- train(formula(model), data = model$model, method = "lm",
                        trControl = train_control)
      # Summarize the results
      print(cv_model)
    
  # (4) Repeated K-fold Cross-Validation
      # The process of splitting the data into k-folds can be repeated a number of times, this is called repeated k-fold cross validation.
      # The final model error is taken as the mean error from the number of repeats.
      # The following example uses 10-fold cross validation with 3 repeats:
      
      # Define training control
      train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
      # Train the model
      set.seed(1)
      cv_model <- train(formula(model), data = model.frame(model), method = "lm",
                        trControl = train_control)
      # Summarize the results
      print(cv_model)
      
      
  # Generally recommend the (repeated) k-fold cross-validation to estimate the prediction error rate. It can be used in regression and classification settings.
      
  # Another alternative to cross-validation is the bootstrap resampling methods (Chapter @ref(bootstrap-resampling)), which consists of repeatedly and randomly selecting a sample of n observations from the original data set, and to evaluate the model performance on each copy:
  # http://www.sthda.com/english/articles/38-regression-model-validation/156-bootstrap-resampling-essentials-in-r/  
  # Reference: James, Gareth, Daniela Witten, Trevor Hastie, and Robert Tibshirani. 2014. An Introduction to Statistical Learning: With Applications in R. Springer Publishing Company, Incorporated.
    

#****************************
### In-Class Exercise 3 -----
#****************************    

# (a) Manually code 10-fold cross validation and compute the metrics
# use the sample() function.
cv_mod <- function(y, mod, data) {
  n <- nrow(data)
  nk_samp <- sample.int(10, n, replace = TRUE)
  folds <- split(1:n, nk_samp)
  
  r <- sapply(1:10, function(i) {  
    fi <- folds[[i]]
    res <- y[fi] - mod(data[-fi, ], data[fi, ])
    c(RMSE = sqrt(mean(res^2)), 
      MAE = mean(abs(res)), 
      R2 = 1 - var(res)/var(y[fi]))
  })
  r <- t(r)
  rownames(r) <- paste("Fold", 1:10)
  r
}
      
y <- model$model$EDU_6_12_NAS_P
data <- model$model
mod <- function(train_data, test_data) {
  predict(lm(EDU_6_12_NAS_P ~ WEA_L2MAD_P + SDL_EDU_PRI_P, train_data), 
          newdata = test_data)
}
set.seed(1)
colMeans(cv_mod(y, mod, data))
    
# (b) Verify your results against caret train.
    
    
    

# (4) Bootstrap and Robust Standard Errors ---------------------------------
#***************************************************************************

# If heteroskedastic errors (bptest rejects the null of homoskedasticity), need to adjust standard errors
lmtest::bptest(model) # We reject !
plot(model, which = 1)
      
library(sandwich)      
round(sqrt(diag(vcov(model))), 2)
summ(model)

vcovHC(model)  # Default is type = "HC3"
vcovHC(model, type = "HC1")  # STATA uses this

# This gives robust standard errors
sqrt(diag(vcovHC(model)))

# Robust coefficient tests:
lmtest::coeftest(model, vcov. = vcovHC(model))

# Again jtools makes things simple: 
jtools::summ(model, robust = TRUE)
jtools::summ(model, robust = "HC1")


# The bootstrap approach does not rely on any of these assumptions made by the linear model, and so it is likely giving a more accurate estimate of the coefficients standard errors than is the summary() function.

# look at easystats packages 
parameters::bootstrap_model(model) %>% qsu
parameters::bootstrap_parameters(model)

# car::Boot provides an efficient wrapper 
bobj <- car::Boot(lm(formula(model), data = model$model))
bobj
summary(bobj)
plot(bobj)
hist(bobj)
confint(bobj)

summary(model)

# manually doing it with the boot package: 
model_coef <- function(data, index, formula = statusquo ~ sex + age + income){
  coef(lm(formula, data = data, subset = index))
}
model_coef(model$model, 1:47)
boot::boot(model$model, model_coef, 500)


#****************************
### In-Class Exercise 4 -----
#**************************** 

# This is an Efficient bootstrap for linear models coded by hand
bootmod <- function(model, reps = 1000, return.reps = FALSE, chol = FALSE) {
  y <- model$model[[1L]]
  X <- model.matrix(model)
  n <- length(y)
  res <- t(replicate(reps, { 
    obs <- sample.int(n, replace = TRUE) # Note: Chol is more efficient but not recommended as default 
    if(chol) chol2inv(chol(crossprod(X[obs, ]))) %*% crossprod(X, y[obs]) else qr.coef(qr(X[obs, ]), y[obs]) 
  }))
  if(return.reps) return(res)
  SE <- apply(res, 2, sd) # Standard error is the standard deviation of the bootstrap sampling distribution 
  npos <- colSums(res > 0)
  pval <- ifelse(coef(model) > 0, 1-npos/reps,  npos/reps)
  cbind(Estimate = coef(model), `Bootstrap SE` = SE, `Bootstrap P-Value` = pval)
}

bootmod(model)
bootmod(model, return.reps = TRUE) %>% View

# Understand the code: What does sample.int(n, replace = TRUE) do? Why do I calculate the -value in this way. 





# (5) Reporting results from linear models in Latex, Excel, Word and Plots ----
#***************************************************************************


model1 <- lm(statusquo ~ log(income) + age + sex, data = Chile)
model2 <- lm(statusquo ~ log(income) + age*sex, data = Chile) # Interaction
model3 <- lm(statusquo ~ log(income) + education + age*sex, data = Chile) 
model4 <- lm(statusquo ~ log(income) + education*age + age*sex, data = Chile) # 2 Interactions

modlist <- list(Base = model1, 
                Interact = model2, 
                `Int. + Edu` = model3, 
                `Int. + Edu Int.` = model4)
names(modlist)
View(modlist)

# Native collapse solution: Create a long table for coeficient of different models
collapse::unlist2d(lapply(modlist, lmtest::coeftest), 
                   idcols = "Model", 
                   row.names = "Coefficient") %>% writexl::write_xlsx("models_long.xlsx")

# The broom package takes the messy output of built-in functions in R, such as lm, nls, or t.test, and turns them into tidy tibbles.
library(broom)
broom::tidy(model1)
broom::glance(model1)
broom::augment(model1)

# These are easy to export using i.e. writexl::write_xlsx()
modlist %>% 
  lapply(function(x) list(Coef = broom::tidy(x), 
                          Stats = broom::glance(x), 
                          Obs = broom::augment(x))) %>% 
  unlist(recursive = FALSE) %>% 
  writexl::write_xlsx("models.xlsx")


library(jtools)
vignette("summ", package = "jtools")
jtools::plot_coefs(modlist, robust = TRUE)
jtools::plot_summs(model1, model1, model1, 
                   robust = list(FALSE, "HC1", "HC3"),
                   model.names = c("OLS", "HC1", "HC3"))
jtools::plot_summs(modlist)
jtools::plot_summs(modlist, scale = TRUE)
jtools::plot_summs(modlist, scale = TRUE, plot.distributions = TRUE)

# install.packages("officer") # Editing Word and PPT tables etc. in R. 
# install.packages("flextable")
jtab <- jtools::export_summs(modlist, 
                     statistics = c(N = "nobs", R2 =  "r.squared", 
                                    `Adj. R2` = "adj.r.squared", 
                                    `F-Statistic` = "statistic", 
                                    `P(F)` = "p.value", 
                                    AIC = "AIC", BOC = "BIC"), # or specify statistics = "all" to see all
                     robust = TRUE)  # Or specify HC1 for STATA or HC3 (more advanced)

huxtable::quick_docx(jtab, file = "jtab.docx")
huxtable::quick_pptx(jtab, file = "jtab.pptx")
huxtable::quick_xlsx(jtab, file = "jtab.xlsx")
huxtable::quick_rtf(jtab, file = "jtab.rtf")
huxtable::quick_html(jtab, file = "jtab.html")
# huxtable::quick_latex(jtab, file = "jtab.tex")
# huxtable::quick_pdf(jtab, file = "jtab.pdf")

# Doing things manually with huxtable...
library(huxtable) # Easily Create and Style Tables for LaTeX, HTML and Other Formats
vignette("huxtable")
vignette("huxreg")
vignette("design-principles", package = "huxtable") # Compars different table creating packages (see web version)
# https://hughjonesd.github.io/huxtable/design-principles.html

# With robust standard errors
tab <- huxtable::huxreg(coeftest(model1, vcov = vcovHC, save = TRUE), 
                        coeftest(model2, vcov = vcovHC, save = TRUE), 
                        coeftest(model3, vcov = vcovHC, save = TRUE), 
                        coeftest(model4, vcov = vcovHC, save = TRUE), statistics = NULL)
openxlsx::saveWorkbook(huxtable::as_Workbook(tab), "tab.xlsx")


# Another option: Only html, picture (png or jpeg) or latex
library(modelsummary) # Summary Tables and Plots for Statistical Models and Data: Beautiful, Customizable, and Publication-Ready
modelsummary::modelplot(modlist)
modelsummary::modelsummary(modlist)
modelsummary::modelsummary(modlist, output = "mstab.html")
modelsummary::modelsummary(modlist, output = "mstab.png")
modelsummary::modelsummary(modlist, output = "mstab.tex")


# Other options: (Mainly Latex and HTML)
library(stargazer) 
library(flextable)
library(gtsummary)
library(kable)
library(kableExtra)


# (6) Binary response models (Logit and Probit) ----------------------------
#***************************************************************************

# See  https://sites.google.com/site/econometricsacademy/econometrics-models/probit-and-logit-models

# GLM's / Logit Example:
logit <- glm(vs ~ cyl + disp + drat, data = mtcars, family = binomial(link = "logit"))
summary(logit)
library(margins)
summary(margins(logit)) # Average marginal effects
marginal_effects(logit)
summary(marginal_effects(logit))

library(glm2) # More numerically stable glm fitting. 


#****************************
### In-Class Exercise 5 -----
#**************************** 

# (a) Using the following data, fit a linear model predicting titanic survival and interpret the results
head(carData::TitanicSurvival)

# (b) Now fit a logit model, compute the marginal effects at the mean

# (c) interact sex and passengerclass, how do you interpret the results?

# (d) Now also interact sex and age, how do you interpret the results?


# (7) Some special linear models for causal inference (IV, RDD, DID) -------
#***************************************************************************

# https://sites.google.com/site/econometricsacademy/econometrics-models/instrumental-variables

# Difference in Difference 
# https://ds4ps.org/PROG-EVAL-III/DiffInDiff.html (great theoretical exploration + graphics)
# https://www.princeton.edu/~otorres/DID101R.pdf (implement this)
# https://bookdown.org/ccolonescu/RPoE4/indvars.html#the-difference-in-differences-estimator
# https://en.wikipedia.org/wiki/Difference_in_differences

# RDD: library(rddtools)

# Also look at: Econometrics Academy on Matching. 

# Nice general resource on program evaluation (Apart from Econometrics Academy): https://ds4ps.org/PROG-EVAL-III/index.html

# (8) Additional useful packages and resources -----------------------------
#***************************************************************************

# Main Theoretical and Practical Resources for this Course: 

# Econometrics Academy (Introductions to all Econometric Models): https://sites.google.com/site/econometricsacademy
   # A similarly good resource is: https://www.econometrics-with-r.org
   # Also: https://bookdown.org/ccolonescu/RPoE4/
# Machine Learning Essentials in R (Various practical tutorial about Machine Learning in R): http://www.sthda.com/english/articles/11-machine-learning/
   # A similarly good resource is the introduction to statistical learning with applications in R: https://www.statlearning.com/
# Applied Statistics with R (Good, detailed introduction to statistics and regression in R): https://daviddalpiaz.github.io/appliedstats
   # A similar good resource is: https://learningstatisticswithr.com/book/

# A quick reference for implementing various statistical methods in R is: http://www.statmethods.net

# For a practical refresher on statistical inference see: https://www.coursera.org/learn/statistical-inference
# You can also do it using swirl: 
library(swirl)
install_course("Statistical Inference")
swirl()


# Another books to consider:
# Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.
# And: 
# Applied Econometrics with R Kleiber and Zeileis (2008)
# Nice book also..
# https://julianfaraway.github.io/faraway/LMR/


# R Packages: 

# See Cran Task Views for Econometrics
# https://cran.r-project.org/web/views/Econometrics.html
# And Machine Learning

# AER - Accompanies the Book Applied Econometrics with R Kleiber and Zeileis (2008) and provides useful functions and data sets.
# MASS - a collection of functions for applied statistics.

# R vs STATA Translations:  
# https://dss.princeton.edu/training/RStata.pdf
# -> See also how to implement various models in both softwares on Econometrics Academy. 


