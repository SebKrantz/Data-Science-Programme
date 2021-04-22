#############################################################
# *************** R BASED DATA SCIENCE TRAINING *************
# Course 3: Linear Modelling and Analysis + Extensions in R
# -----------------------------------------------------------
# Prepared by: Sebastian Krantz, ODI Fellow, MEPD, MoFPED
# Permanent Contact: sebastian.krantz@graduateinstitute.ch
#############################################################

# Course Aim: To familiarize participants with linear regression modelling 
#             in R, including specification, testing and various extensions to 
#             the classical linear model including panel-data, LASSO Regression etc. 

# Today:
# (0) Rstudio Projects, Git and Github
# (1) Linear models with lm() and some built in tools and diagnostics
# (2) Testing linear models with 'lmtest' and 'car' (and 'tseries')
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


# (1) Linear models with lm() and some built in tools and diagnostics ------
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


# Now on to linear models. Say we want to predict the statusquo variable 
qsu(Chile, statusquo ~ vote) # Summarising it by voting outcome: Clearly a strong relationship
barplot(fmean(Chile$statusquo, Chile$vote)) # People who votes yes favor the status quo

# R is an object oriented language, so when we fit a model, we can create and object to ssave the results
model <- lm(statusquo ~ sex + age + income, data = Chile)
class(model)
str(model)
names(model)
View(model)
print(model)        # Print method for lm() just shows the coefficients
summary(model)      # This is probably what you expect

# Alternative regression summaries. 
jtools::summ(model) # A more legible version # https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
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

# This could for example be used to manually estimate the coefficients using the available matrix algebra and the result that: coef = (X'X)^(-1)X'y where ' denotes the matrix transpose. 
X <- model.matrix(model) 
solve(crossprod(X), crossprod(X, model$model$statusquo)) # same as coef(model)

# More statistics can be extracted when we create a model summary object
model_smr <- summary(model)
names(model_smr)
model_smr$coefficients
model_smr$df        # degrees of freedom = number of observations - number of coefficients estimated (3 + constant)
model_smr$r.squared # R-squared: 1 means perfect fit, 0 means no fit

# compute R^2 manually
SSR <- sum(model$residuals^2)
SST <- sum(W(model$model$statusquo)^2) # collapse::W(x) is the same as x - mean(x) i.e. it centers data (takes a within-transformation)
R2 <- 1 - SSR/SST
# Another way: This shows you why it is called the r-squared
cor(model$model$statusquo, fitted(model))^2

model_smr$adj.r.squared # Adjusted R-squared
model_smr$fstatistic
vcov(model) # Variance-covariance matrix of the coefficients. 
# The standard error visible in the coefficients table (model_smr$coefficients) is computed like this
SE <- sqrt(diag(vcov(model)))
# The t-statistic is simply the coefficient divided by the standard error. 
# It measures how confident we are that the coefficient is different from 0. abs(tstat) > 2 typically indicates that this is true in 95% of samples drawn from the population. 
tstat <- coef(model) / SE
# the t-statistic follows a t-distribution, from which we can compute the probabilit of observing a t-statistic as large as this one if the true regression coefficient is 0. 
2 * pt(abs(tstat), model$df.residual, lower.tail = FALSE) # 2586 is degrees of freedom
# Compare to coefficients table 
model_smr$coefficients

# Other functions allow us to calculate: Confidence intervals
confint(model, level = 0.95)

# How do we make predictions?:
predict(model, newdata = Chile[1L, ])

# Plotting regression line
plot(statusquo ~ age, Chile)
abline(lm(statusquo ~ age, Chile), col = "red")

# Testing exclusion restrictions: H0: income is irrelevant 
restricted_model <- lm(statusquo ~ sex + age, data = na.omit(Chile))
full_model <- lm(statusquo ~ sex + age + income, data = na.omit(Chile))
anova(restricted_model, full_model)

# Here we see that the value of the F statistic is 0.553, and the p-value is very large, so we fail to reject the null hypothesis at any reasonable α
# 
# and say that none of cyl, disp, hp, and acc are significant with wt and year already in the model.
# 
# Again, we verify the sums of squares and degrees of freedom directly in R. You should match these to the table from R, and use this to match R’s output to the written table above.

# A faster function to do this without first estimation a model is collapse::fFtest: 
library(magrittr)
na_omit(Chile) %$% fFtest(statusquo, income, cbind(sex, age))


#****************************
### In-Class Exercise 1 -----
#****************************

# Run some regressions on the census and test them. 

CENS <- readRDS("Course 2 - Advanced Data Manipulation and Visualization in R/data/UBOS 2014 census.rds")
View(namlab(CENS))






# Also: Interactions, dummies, log variables etc.

hist(Chile$income)
hist(log(Chile$income))
# : denotes simple interaction
model2 <-lm(statusquo ~ log(income) + education + age + sex + sex:age, data = Chile) # Interactions and functional transformations are easy in R
# Same thing: * denotes full interaction
model2 <- lm(statusquo ~ log(income) + education + age*sex, data = Chile) # Interactions and functional transformations are easy in R

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




# NOW: Assumptions underlying the linear model and visual diagnostics: ---------------------------------------------------------

# Gauss Markov assumptions are standard assumptions for the linear regression model
# 1. Linearity in parameters: The response can be written as a linear combination of the predictors. (With noise about this true linear relationship.)
# 2. Random sampling: If we want our coefficient estimate to be an unbiased estimate of the population coeffieint, we need this assumption. For inference we assume the errors are independent which also necesitates this assumption. 
# 3. No perfect collinearity: To accurately estimate the coefficients on different variables in our model these variables should not be perfectly correlated or linear combinations of one-another
# 4. Zero conditional mean (exogeneity) – regressors are not correlated with the error term. Also implies no omitted variables correlated with other variables in our model.
# 5. Homoscedasticity – variance of error term is constant over the scale of predictors and outcome. 
# 6. Normality: error term is normally distributed

# See also https://learningstatisticswithr.com/book/regression.html#regressionassumptions
# And: http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/

# The (approximate) validity of some of these assumtions can be investigated through a simple set of plots. 
# These plots are computed automatically when calling plot(model). But we will compute them manually first and walk through them:

# (I) Linearity and Homoskedasticity 

# First, lets plot the residuals against the fitted values:
plot(fitted(model), resid(model), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model")
abline(h = 0, col = "darkorange", lwd = 2)

# We should look for two things in this plot.
# (i) At any fitted value, the mean of the residuals should be roughly 0. 
#     If this is the case, the linearity assumption is valid. (For this reason, we generally add a horizontal line at y = 0 to emphasize this point)
# (ii) At every fitted value, the spread of the residuals should be roughly the same. 
#      If this is the case, the constant variance assumption is valid.
#      Constant variance is often called homoscedasticity. Conversely, non-constant variance is called heteroscedasticity. 


# -> looks like the linearity assumption is violated
# Lets check further 
rfit <- round(fitted(model), 1)
lines(funique(rfit), fmean(resid(model), rfit))
# Seems ok still.. but the chart is weird

# In a lot of contexts, especially where you’re only interested in the pattern of the residuals and not their actual values, it’s convenient to estimate the standardised residuals, which are normalised in such a way as to have standard deviation 1.
# To get the standardised residuals, the command you want is this
rstandard(model)
plot(fitted(model), rstandard(model), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model")
abline(h = 0, col = "darkorange", lwd = 2)
# There are also studentized residuals (also called “jackknifed residuals”), which are standardized with the variance used for standardization computed without the data point. 
rstudent(model)

# (II) Normality

# We have a number of tools for assessing the normality assumption. The most obvious would be to make a histogram of the residuals. 
# If it appears roughly normal, then we’ll believe the errors could truly be normal.
hist(resid(model), breaks = 40)

# Another visual method for assessing the normality of errors, which is more powerful than a histogram, is a normal quantile-quantile plot, or Q-Q plot for short.
# In R these are very easy to make. The qqnorm() function plots the points, and the qqline() function adds the necessary line. We create a Q-Q plot for the residuals to check if the errors could truly be normally distributed.
qqnorm(resid(model), main = "Normal Q-Q Plot, fit_1", col = "darkgrey")
qqline(resid(model), col = "dodgerblue", lwd = 2)
# In short, if the points of the plot do not closely follow a straight line, this would suggest that the data do not come from a normal distribution.
# The calculations required to create the plot vary depending on the implementation, but essentially the y-axis is the sorted data (observed, or sample quantiles), 
# and the x-axis is the values we would expect if the data did come from a normal distribution (theoretical quantiles).
# The Wikipedia page for Normal probability plots gives details on how this is implemented in R if you are interested.

# (III) Unusual Observations

# See: https://daviddalpiaz.github.io/appliedstats/model-diagnostics.html#unusual-observations
# -> Look at the 3 graphs used to explain the concepts !!!
# Or:  https://learningstatisticswithr.com/book/regression.html#regressiondiagnostics

# (a) Leverage
  # A data point with high leverage, is a very unusual data point that could have a large influence when fitting the model.
  # This doesn't necessarily have to correspond to a large residual: if the observation happens to be unusual on all variables in precisely the same way, it can actually lie very close to the regression line. 
  # Use the hatvalues() function to return the leverages of each data point (hi).
  hatvalues(model)
  # hi is a measure of the extent to which the i-th observation is "in control" of where the regression line ends up going. 
  # Leverage sums to the umber of predictors + constant
  sum(hatvalues(model))
  
  # How is leverage calculated ? (in case you are interested)
    # Hat maker matrix
    H <- X %*% solve(crossprod(X)) %*% t(X)
    drop(H %*% model$model$statusquo) # Generates fitted values
    fitted(model) # Same thing
    # Leverages (for each observation): Diagonal elements of hatvalues matrix
    diag(H)
  
  # What leverage would be considered large? There is no exact answer to this question. 
  # A common heuristic would be to compare each leverage to two times the average leverage. 
  sum(hatvalues(model) > 2 * mean(hatvalues(model)))  # Number of observations with high leverage...
  # Corresponding Plot:
  hist(hatvalues(model), breaks = 40)
  abline(v = 2 * mean(hatvalues(model)))
  
  # In general, high leverage points are also worth looking at in more detail, but they’re much less likely to be a cause for concern unless they are also outliers.
 
# (b) Outliers
  # Outliers are points which do not fit the model well. They may or may not have a large affect on the model. 
  # To identify outliers, we will look for observations with large residuals.
  
  # We can look at the studentized residual for each observation
  rstudent(model) 
  # Let's look at residuals above 2 standard deviations from their mean
  rstudent(model)[abs(rstudent(model)) > 2] # None 
  
# (c) Influence
  # This brings us to our third measure of unusualness, the influence of an observation. 
  # A high influence observation is an outlier that has high leverage. 
  # That is, it is an observation that is very different to all the other ones in some respect, 
  # and also lies a long way from the regression line.
  # Observations with some combination of high leverage and large residual, we will call influential.
  # something that is an outlier and has high leverage has a big effect on the regression line.
  
  # A common measure of influence is Cook’s Distance, which is defined as
  rstandard(model)^2 / length(coef(model)) * hatvalues(model) / (1- hatvalues(model))
  # Notice that this is a function of both leverage and standardized residuals.
  # The Cook’s distance for each point of a regression can be calculated using cooks.distance() which is a default function in R. 
  cooks.distance(model)
  # A Cook’s Distance is often considered large if > 4 / (n - p - 1) with n the number of obs and p the number of independent variables
  # and an observation with a large Cook’s Distance is called influential. This is again simply a heuristic, and not an exact rule
  # We’ll now directly check if each of these is influential.
  
  # A rule of thumb is that an observation has high influence if Cook’s distance exceeds 4/(n - p - 1)(P. Bruce and Bruce 2017), where n is the number of observations and p the number of predictor variables.: http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/

  sum(cooks.distance(model) > 4 / model$df.residual)
  # We can also do this with plot: 
  plot(model, which = 4)
  plot(model, which = 5)
  
  # Also:
  dfbetas(model)
  influence.measures(model)
  
#****************************
### In-Class Exercise 2 -----
#****************************

# Swirl regression diagnostics  
library(swirl)
swirl()





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
  lmtest::bptest(model)
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
  shapiro.test(resid(model))
  # This gives us the value of the test statistic and its p-value. The null hypothesis assumes the data were sampled from a normal distribution, thus a small p-value indicates we believe there is only a small probability the data could have been sampled from a normal distribution.
  # For details, see: Wikipedia: Shapiro–Wilk test.
  
  # Other options: 
  performance::check_normality(model)
  
  # car also provides firtehr plots: 
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
  car::vif(model)
  jtools::summ(model, vif = TRUE)
  performance::check_collinearity(model)


# See also: https://github.com/easystats/easystats
  
  
#****************************
### In-Class Exercise 2 -----
#****************************
  
# Walk through the first example using mtcars data:
# https://daviddalpiaz.github.io/appliedstats/model-diagnostics.html#data-analysis-examples
  

# (3) Measuring model performance (model validation) -----------------------
#***************************************************************************

# A general resource:
# http://www.sthda.com/english/articles/38-regression-model-validation/


# Standard Performance metrics: http://www.sthda.com/english/articles/38-regression-model-validation/158-regression-model-accuracy-metrics-r-square-aic-bic-cp-and-more/
AIC(model)
BIC(model)
performance::r2(model) # R-squared (R2), representing the squared correlation between the observed outcome values and the predicted values by the model. The higher the adjusted R2, the better the model
performance::model_performance(model) 
# Root Mean Squared Error (RMSE), which measures the average prediction error made by the model in predicting the outcome for an observation. That is, the average difference between the observed known outcome values and the values predicted by the model. The lower the RMSE, the better the model.
# Mean Absolute Error (MAE), an alternative to the RMSE that is less sensitive to outliers. It corresponds to the average absolute difference between observed and predicted outcomes. The lower the MAE, the better the model
jtools::glance(model)

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
     training_samples <- model$model$statusquo %>%
        createDataPartition(p = 0.8, list = FALSE)
      train_data <- model$model[training_samples, ]
      test_data <- model$model[-training_samples, ]
      # Build the model
      model_vs <- lm(formula(model), data = train_data)
      # Make predictions and compute the R2, RMSE and MAE
      predictions <- model_vs %>% predict(test_data)
      data.frame(R2 = R2(predictions, test_data$statusquo),
                 RMSE = RMSE(predictions, test_data$statusquo),
                 MAE = MAE(predictions, test_data$statusquo))
      
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
      loocv_model <- train(formula(model), data = model.frame(model), method = "lm",
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
      cv_model <- train(formula(model), data = model.frame(model), method = "lm",
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
# use the sample function.
    
    
# (b) Verify your results against caret train.
    
    
    

# (4) Bootstrap and Robust Standard Errors ---------------------------------
#***************************************************************************

# If heteroskedastic errors (bptest rejects the null of homoskedasticity), need to adjust standard errors
lmtest::bptest(model) # We reject !
      
library(sandwich)      
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
bootmod <- function(model, reps = 1000, return.reps = FALSE) {
  y <- model$model[[1L]]
  X <- model.matrix(model)
  n <- length(y)
  res <- do.call(rbind, lapply(integer(reps), function(i) {
    obs <- sample.int(n, replace = TRUE)
    qr.coef(qr(X[obs, ]), y[obs]) # Or collapse::flm(y[obs], X[obs, ], method = "qr"), can put more efficient methods like "chol"
  })) 
  if(return.reps) return(res)
  SE <- apply(res, 2, sd) # Standard error is the standard deviation of the bootstrap sampling distribution 
  npos <- colSums(res > 0)
  pval <- ifelse(coef(model) > 0, 1-npos/reps,  npos/reps)
  cbind(Estimate = coef(model), `Bootstrap SE` = SE, `Bootstrap P-Value` = pval)
}

bootmod(model)
bootmod(model, return.reps = TRUE)

# Understand the code: What does sample.int(n, replace = TRUE) do? Why do I calculate the -value in this way. 





# (5) Reporting results from linear models in Latex, Excel, Word and Plots ----
#***************************************************************************
library(modelsummary)
library(jtools)
library(broom)
library(stargazer)
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


