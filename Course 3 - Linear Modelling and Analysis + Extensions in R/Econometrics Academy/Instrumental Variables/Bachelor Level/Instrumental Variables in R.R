# Instrumental Variables in R
# Copyright 2013 by Ani Katchova

# install.packages("AER")
library(AER)
# install.packages("systemfit")
library(systemfit)

mydata <- read.csv("C:/Econometrics/Data/iv_health.csv")
attach(mydata)

# Defining variables (Y1 dependent variable, Y2 endogenous variable)
# (X1 exogenous variables, X2 instruments, X2 instruments, overidentified case)
Y1 <- cbind(logmedexpense)
Y2 <- cbind(healthinsu)
X1 <- cbind(illnesses, age, logincome)
X2 <- cbind(ssiratio)
X2alt <- cbind(ssiratio, firmlocation)

# Descriptive statistics
summary(Y1)
summary(Y2)
summary(X1)
summary(X2)

# OLS regression
olsreg <- lm(Y1 ~ Y2 + X1)
summary(olsreg)

# 2SLS estimation
ivreg <- ivreg(Y1 ~ Y2 + X1 | X1 + X2)
summary(ivreg)

# 2SLS estimation (details)
olsreg1 <- lm (Y2 ~ X1 + X2)
summary(olsreg1)
Y2hat <- fitted(olsreg1)

olsreg2 <- lm(Y1 ~ Y2hat + X1)
summary(olsreg2)

# 2SLS estimation, over-identified case
ivreg_o <- ivreg(Y1 ~ Y2 + X1 | X1 + X2alt)
summary(ivreg_o)

# Hausman test for endogeneity of regressors
cf_diff <- coef(ivreg) - coef(olsreg)
vc_diff <- vcov(ivreg) - vcov(olsreg)
x2_diff <- as.vector(t(cf_diff) %*% solve(vc_diff) %*% cf_diff)
pchisq(x2_diff, df = 2, lower.tail = FALSE)

# Systems of equations

# Defining equations for systems of equations (2SLS and 3SLS)
# (X12 exogenous variable for eq2, X22 instrument for eq2)
X12 <- cbind(illnesses)
X22 <- cbind(firmlocation)
eq1 <- Y1 ~ Y2 + X1 + X2
eq2 <- Y2 ~ Y1 + X12 + X22
inst <- ~ X1 + X2 + X22
system <- list(eq1 = eq1, eq2 = eq2)

# 2SLS estimation
reg2sls <- systemfit(system, "2SLS", inst = inst, data = mydata)
summary(reg2sls)

# 3SLS estimation
reg3sls <- systemfit(system, "3SLS", inst = inst, data = mydata)
summary(reg3sls)
