# Multiple Regression Model

# Outline:
#   Multiple regression
#   Partialling out
#   Goodness of fit (R-squared and adjusted R-squared)
#   Perfect collinearity 
#   Multicollinearity using VIF	
#   Omitted variable bias
#   Variance in misspecified models	
#   Homoscedasticity and heteroscedasticity

# Data files:
#   wage1.csv
#   CEOSAL1.csv
#   HTV.csv
#   elemapi2.csv

# setup
rm(list = ls()) 
directory <- "C:/Econometrics/DataR/"

# Install packages
PackageNames <- c("tidyverse", "stargazer", "magrittr", "car")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}


# Multiple regression --------------------------------------------------

# Run a multiple regression analysis, interpret the coefficients
# See how the coefficients are different for simple vs multiple regression

# Wage Example
wage1 <- read.csv(paste0(directory, "wage1.csv"))
wage1 %>% 
  select(wage, educ, exper, tenure) %>% 
  head(10)
wage1 %>% 
  select(wage, educ, exper, tenure) %>%
  str
wage1 %>% 
  select(wage, educ, exper, tenure) %>%
  stargazer(type = "text")

# Compare results from simple regression and multiple regression:
# If education increases by 1 year, by how many dollars does wage increase? 

# Simple regression
model_simple <- lm(wage ~ educ, wage1)
summary(model_simple)

# Multiple regression
model_multiple1 <- lm(wage ~ educ + exper, wage1)
model_multiple2 <- lm(wage ~ educ + exper + tenure, wage1)
summary(model_multiple1)
summary(model_multiple2)

# Display the coefficients
coef(model_multiple2)
model_multiple2$coefficients

# Predicted values and residuals (same as simple regression)
wage1 %<>% mutate(wagehat = fitted(model_multiple2),
                  uhat = residuals(model_multiple2))

wage1 %>% 
  select(wage, wagehat, uhat) %>% 
  head(10)

wage1 %>% 
  select(wage, wagehat, uhat) %>%
  stargazer(type = "text")
# wage = wagehat + uhat
# mean(uhat) = 0 and mean(wage) = mean(wagehat)


# Partialing out -------------------------------------------------------

# Partialling out shows the coefficients beta1 are the same in first and 
# last regression.

# wage = beta0 + beta1*educ + beta2*exper + beta3*tenure + u
# Same as the model 'model_multiple2'
summary(model_multiple2)

# educ = alpha0 + alpha2*exper + alpha3*tenure + e
model_partial <- lm(educ ~ exper + tenure, wage1)
summary(model_partial)

# predict residuals ehat
wage1 %<>% mutate(ehat = resid(model_partial))

# wage = gamma0 + beta1*ehat + v
lm(wage ~ ehat, wage1) %>% summary


# Goodness of fit (R-squared and adjusted R-squared) -------------------

# Goodness of fit measures are R-squared and adjusted R-squared showing
# the proportion of variation explained by the regression

# Run a simple regression with 1 regressor
# same as the model 'model_simple'
summary(model_simple)

# Display stored R-squared
summary(model_simple)$r.squared

# Display stored adjusted R-squared
summary(model_simple)$adj.r.squared

# Run a multiple regression with 2 regressors
# same as the model 'model_multiple1'
summary(model_multiple1)
summary(model_multiple1)$r.squared
summary(model_multiple1)$adj.r.squared

# Run a multiple regression with 3 regressors
# same as the model 'model_multiple2'
summary(model_multiple2)
summary(model_multiple2)$r.squared
summary(model_multiple2)$adj.r.squared

# To calculate R-squared manually
y    <- wage1$wage
yhat <- fitted(model_multiple2)
ehat <- resid(model_multiple2)

SSE <- sum((yhat - mean(y))^2) # sum of squares explained or model
SSR <- sum(ehat^2) # sum of squares residual
SST <- SSE + SSR # sum of squares total

# number of observations
n <- nobs(model_multiple2)
n
# degrees of freedom for model = k = # regressors 
# rank gives the number of regressors including the constant
k <- model_multiple2$rank - 1
k

# degress of freedom for residual = n - k - 1
df_SSR <- n - k - 1
# degrees of freedom total = n-1 = number of observ - 1
df_SST <- n - 1

# Formula for R-squared
R_squared <- SSE/SST
# same as R-squared = 1 - SSR/SST
R_squared

# Formula for adjusted R-squared
adj_R_squared <- 1 - (SSR/df_SSR)/(SST/df_SST)
adj_R_squared


# CEO Salary Example 
CEOSAL1 <- read.csv(paste0(directory, "CEOSAL1.csv"))
CEOSAL1 %>% 
  select(salary, lsalary, roe, sales, lsales) %>%
  head(10)
CEOSAL1 %>% 
  select(salary, lsalary, roe, sales, lsales) %>%
  str
CEOSAL1 %>% 
  select(salary, lsalary, roe, sales, lsales) %>%
  stargazer(type = "text")

# Linear form
model_linear <- lm(salary ~ roe + sales, CEOSAL1)
summary(model_linear)
summary(model_linear)$r.squared
summary(model_linear)$adj.r.squared

# Linear-log model
model_linear_log <- lm(salary ~ roe + lsales, CEOSAL1)
summary(model_linear_log)
summary(model_linear_log)$r.squared
summary(model_linear_log)$adj.r.squared

# Log-linear form
model_log_linear <- lm(lsalary ~ roe + sales, CEOSAL1)
summary(model_log_linear)
summary(model_log_linear)$r.squared
summary(model_log_linear)$adj.r.squared

# Log-log form
model_log_log <- lm(lsalary ~ roe + lsales, CEOSAL1)
summary(model_log_log)
summary(model_log_log)$r.squared
summary(model_log_log)$adj.r.squared

# Compare R-squared and adjusted R-squared
# SST are different for salary vs lsalary
# Log-log model has the highest adjusted R-squared


# Perfect collinearity -------------------------------------------------

# Perfect collinearity is an exact linear relationship between the variables
# Perfect collinearity example is when male = 1-female

# Wage Example
# Keep using the data 'wage1'

# Model for wage with female
model_no_collinearity <- lm(wage ~ educ + female, wage1)
summary(model_no_collinearity)

# Male is an exact linear function of female (perfect collinearity)
wage1 %<>% mutate(male = 1 - female)

# Model for wage with male
model_no_collinearity1 <- lm(wage ~ educ + male, wage1)
summary(model_no_collinearity1)

# Try to run regression with both female and male  
model_collinearity <- lm(wage ~ educ + female + male, wage1)
summary(model_collinearity)
# This model cannot be estimated because of perfect collinearity
# R drops one variable, but it chooses which one to drop

# Run regression with "no constant" option
model_no_constant <- lm(wage ~ 0 + educ + female + male, wage1) 
summary(model_no_constant)


# Multicollinearity using VIF ------------------------------------------

# Multicollinearity is when regressors are highly correlated with each other.

# Test scores example
elemapi2 <- read.csv(paste0(directory, "elemapi2.csv"))

elemapi2 %<>% select(api00, avg_ed, grad_sch, col_grad)
str(elemapi2)
stargazer(elemapi2, type = "text")
head(elemapi2, 10)

# Multicollinearity: parents' average education is collinear with
# whether they completed grad school or college

# Correlation table
elemapi2 %>% 
  select(-api00) %>% 
  na.omit %>% # remove samples with NA
  cor

# Run regression, find VIF. If VIF>10 then drop the variable
(model_high_vif <- lm(api00 ~ avg_ed + grad_sch + col_grad, elemapi2))
vif(model_high_vif)

# Run regression without variable that has high VIF
(model_low_vif <- lm(api00 ~ grad_sch + col_grad, elemapi2)) 
vif(model_low_vif)

# Run regression without the other variables
(model_simple2 <- lm(api00 ~ avg_ed, elemapi2))

# VIF=1 for simple regression 
# The VIF function does not allow for a simple regression model


# Omitted variable bias ------------------------------------------------

# Omitted variable bias is when an omitted variable causes biased coefficients

# Wage2 example
HTV <- read.csv(paste0(directory, "HTV.csv"))
HTV %<>% select(wage, educ, abil)
str(HTV)
stargazer(HTV, type = "text")
head(HTV)

# True model with educ and ability
# wage = beta0 + beta1*educ + beta2*abil + u
model_true <- lm(wage ~ educ + abil, HTV)
summary(model_true)
beta1 <- coef(model_true)["educ"]
beta1
beta2 <- coef(model_true)["abil"]
beta2

# Model between ability and education
# abil = delta0 + delta1*educ + v
model_abil <- lm(abil ~ educ, HTV)
summary(model_abil)
delta1 <- coef(model_abil)["educ"]
delta1

# Model where ability is omitted variable, so coefficient on educ is biased
# wage = (beta0+beta2*delta0) + (beta1+beta2*delta1)*educ +(beta2*v+u)
model_omitted <- lm(wage ~ educ, HTV)
summary(model_omitted)
beta1_biased <- coef(model_omitted)["educ"]
beta1_biased

# Calculate bias and biased coefficient
bias <- beta2*delta1
bias

beta1_biased_calculated <- beta1 + beta2*delta1
beta1_biased_calculated

# Wage example
# Use the data 'wage1'

# True model with educ and exper
# Same as the model 'model_multiple1'
# model_multiple1 <- lm(wage ~ educ + exper, wage1)
summary(model_multiple1)

# Model between exper and education
model_exper <- lm(exper ~ educ, wage1)
summary(model_exper)

# Model where exper is omitted, so coefficient on educ is biased
# Same as the model 'model_simple'
# model_simple <- lm(wage ~ educ, wage1)
summary(model_simple)


# Variance in misspecified models --------------------------------------

# Variance in misspecified model (same as omitted variable bias example) 

# Wage 2 example
# Use the data 'HTV'

# True model with educ ability
# Same as the model 'model_true'
# model_true <- lm(wage ~ educ + abil, HTV)
summary(model_true)

# Model where ability is omitted variable
# model_omitted <- lm(wage ~ educ, HTV)
summary(model_omitted)
# The coefficient on educ is biased but has lower standard error


# Homoscedasticity and heteroscedasticity ------------------------------

# Homoscedasticy is when the variance of the error is constant for each x
# Heteroscedasticy is when the variance of the error is not constant for each x

# Wage example
# Use the data 'wage1'

# model_multiple2 <- lm(wage ~ educ + exper + tenure, wage1)
summary(model_multiple2)

# Plotting residuals with "geom_point"
ggplot(data = wage1, mapping = aes(x = educ)) +
  theme_bw() +
  geom_point(mapping = aes(y = uhat)) +
  geom_hline(yintercept = 0, col = 'red') + # add a horizontal line
  ylab(label = "Residuals") # change y-axis label

ggplot(data = wage1, mapping = aes(x = exper)) +
  theme_bw() +
  geom_point(mapping = aes(y = uhat)) +
  geom_hline(yintercept = 0, col = 'red') +
  ylab(label = "Residuals")

# Graphs show heteroscedasticity for educ and homoscedasticity for exper
