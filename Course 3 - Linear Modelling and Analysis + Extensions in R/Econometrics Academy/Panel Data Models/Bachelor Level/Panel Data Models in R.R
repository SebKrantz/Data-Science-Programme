# Panel Data Models in R


install.packages("plm")
library(plm)

mydata<- read.csv("C:/Econometrics/Data/panel_wage.csv")
attach(mydata)

Y <- cbind(lwage)
X <- cbind(exp, exp2, wks, ed)

# Set data as panel data
pdata <- pdata.frame(mydata, index=c("id","t"))

# Descriptive statistics
summary(Y)
summary(X)

# Pooled OLS estimator
pooling <- plm(Y ~ X, data=pdata, model= "pooling")
summary(pooling)

# Between estimator
between <- plm(Y ~ X, data=pdata, model= "between")
summary(between)

# First differences estimator
firstdiff <- plm(Y ~ X, data=pdata, model= "fd")
summary(firstdiff)

# Fixed effects or within estimator
fixed <- plm(Y ~ X, data=pdata, model= "within")
summary(fixed)

# Random effects estimator
random <- plm(Y ~ X, data=pdata, model= "random")
summary(random)

# LM test for random effects versus OLS
plmtest(pooling)

# LM test for fixed effects versus OLS
pFtest(fixed, pooling)

# Hausman test for fixed versus random effects model
phtest(random, fixed)
