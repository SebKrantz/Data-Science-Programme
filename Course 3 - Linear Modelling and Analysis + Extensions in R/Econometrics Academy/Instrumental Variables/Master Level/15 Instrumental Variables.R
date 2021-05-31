# Instrumental Variables
  
# Outline:
#   IV estimation
#   2SLS (two stage least squares)
#   Testing for endogeneity

# Data files: 
#   MROZ.csv

# setup
rm(list = ls()) 
directory <- "C:/Econometrics/DataR/"

# Install packages
PackageNames <- c("tidyverse", "stargazer", "magrittr", "haven", "AER")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}   

MROZ <- read.csv(paste0(directory, 'MROZ.csv'))

# keep only working women
MROZ %<>% filter(inlf == 1)

MROZ %>% 
  select(lwage, educ, exper, expersq, fatheduc, motheduc) %>% 
  stargazer(type = "text")

MROZ %>% 
  select(lwage, educ, exper, expersq, fatheduc, motheduc) %>% 
  head(10)

# Regression model
model1 <- lm(lwage ~ educ, MROZ)
summary(model1)
coef(model1)["educ"]


# IV estimation -----------------------------------------------------------

# Dependent variable y, endogenous variable x, instrument z
# Coefficient_ols = sum((x-xbar)*(y-ybar))/sum((x-xbar)*(x-xbar))
# Coefficient_iv = sum((z-zbar)*(y-ybar))/sum((z-zbar)*(x-xbar))

# Calculating means
attach(MROZ) # attach MROZ so the variable names can be used directly
mean_fatheduc <- mean(fatheduc)
mean_educ <- mean(educ)
mean_lwage <- mean(lwage)

# OLS coefficient on educ
numerator_ols <- (educ - mean_educ) * (lwage-mean_lwage)
denominator_ols <- (educ - mean_educ) * (educ - mean_educ)
sum_numerator_ols <- sum(numerator_ols)
sum_denominator_ols <- sum(denominator_ols)

(coeff_ols <- sum_numerator_ols/sum_denominator_ols)

# IV coefficient on educ
numerator_iv <- (fatheduc - mean_fatheduc) * (lwage - mean_lwage)
denominator_iv <- (fatheduc - mean_fatheduc) * (educ - mean_educ)

sum_numerator_iv <- sum(numerator_iv)
sum_denominator_iv <- sum(denominator_iv)

(coeff_iv <- sum_numerator_iv/sum_denominator_iv)
detach(MROZ)


# 2SLS (two stage least squares) ------------------------------------------

# Simple regression model with one instrument

# OLS estimation
summary(model1)

# 2SLS
model2 <- ivreg(formula = lwage ~ educ | . - educ + fatheduc, data = MROZ)
summary(model2, diagnostics = TRUE)

# 2SLS - first stage 
# Regression of endogenous variable educ on instrument fatheduc
model3 <- lm(educ ~ fatheduc, MROZ)
summary(model3)

# Display R-squared of educ on fatheduc model
summary(model3)$r.squared

# Predicted values for educ_hat
MROZ %<>% mutate(educ_hat = fitted(model3))

# Compare educ with educ_hat
MROZ %>% select(educ, fatheduc, educ_hat) %>% head(10)

# 2SLS - second stage 
# Replace educ with predicted value educ_hat
model4 <- lm(lwage ~ educ_hat, MROZ)
summary(model4)
# Coefficients are correct but the standard errors are not correct


# 2SLS (two stage least squares) ------------------------------------------

# Multiple regression model with several independent variables and two instruments

# OLS estimation
model5 <- lm(lwage ~ educ + exper + expersq, MROZ)
summary(model5)

# 2SLS 
model6 <- ivreg(lwage ~ educ + exper + expersq | . - educ + fatheduc + motheduc, 
                data = MROZ)
summary(model6)

# 2SLS - first stage 
# Regression of endogenous variable educ on instruments fatheduc and motheduc
model7 <- lm(educ ~ exper + expersq + fatheduc + motheduc, MROZ)
summary(model7)

# Testing whether educ and fatheduc and motheduc are correlated
car::linearHypothesis(model7, c("fatheduc = 0", "motheduc = 0"))

# Predicted values for educ_hat1
MROZ %<>% mutate(educ_hat1 = fitted(model7))

# 2SLS - second stage 
# Replace endogenous variable educ with predicted value educ_hat1
model8 <- lm(lwage ~ educ_hat1 + exper + expersq, MROZ)
summary(model8)
# Coefficients are correct but the standard errors are not correct

# Testing for endogeneity -------------------------------------------------
# Testing for endogeneity of education in model for log wage

# Structural equation
summary(model5)

# Estimate reduced form model for education
model9 <- lm(educ ~ exper + expersq + fatheduc + motheduc, MROZ)
summary(model9)
# Predict the residuals vhat
MROZ %<>% mutate(vhat = resid(model9))

# Structural equation for log wage that includes residuals vhat
model10 <- update(model5, ~ . + vhat)
summary(model10)

# H0: coeff on vhat=0 (exogeneity) and H1: coeff on vhat ne 0 (endogeneity)
# The coefficient on vhat is significant so education is endogenous.
