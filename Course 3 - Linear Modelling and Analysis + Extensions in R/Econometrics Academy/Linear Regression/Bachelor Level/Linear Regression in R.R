# Linear Regression in R
# Copyright 2013 by Ani Katchova

mydata<- read.csv("C:/Econometrics/Data/regression_auto.csv")
attach(mydata)

# Define variables
Y <- cbind(mpg)
X1 <- cbind(weight1)
X <- cbind(weight1, price, foreign)

# Descriptive statistics
summary(Y)
summary(X)

# Correlation among variables
cor(Y, X)

# Plotting data on a scatter diagram
plot(Y ~ X1, data = mydata)

# Simple linear regression 
olsreg1 <- lm(Y ~ X1)
summary(olsreg1)
confint(olsreg1, level=0.95)
anova(olsreg1)

# Plotting regression line
abline(olsreg1)

# Predicted values for dependent variable
Y1hat <- fitted(olsreg1)
summary(Y1hat)
plot(Y1hat ~ X1)

# Regression residuals
e1hat <- resid(olsreg1)
summary(e1hat)
plot(e1hat ~ X1)

# Multiple linear regression
olsreg2 <- lm(Y ~ X)
summary(olsreg2)
confint(olsreg2, level=0.95)
anova(olsreg2)

# Predicted values for dependent variable
Yhat <- fitted(olsreg2)
summary(Yhat)

# Regression residuals
ehat <- resid(olsreg2)
summary(ehat)
