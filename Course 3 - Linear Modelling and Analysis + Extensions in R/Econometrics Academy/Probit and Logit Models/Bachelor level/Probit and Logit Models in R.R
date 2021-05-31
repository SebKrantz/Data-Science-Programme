# Probit and Logit Models in R
# Copyright 2013 by Ani Katchova

mydata<- read.csv("C:/Econometrics/Data/probit_insurance.csv")
attach(mydata)

# Define variables
Y <- cbind(ins)
X <- cbind(retire, age, hstatusg, hhincome, educyear, married, hisp)

# Descriptive statistics
summary(Y)
summary(X)

table(Y)
table(Y)/sum(table(Y))

# Regression coefficients
olsreg <- lm(Y ~ X)
summary(olsreg)

# Logit model coefficients
logit<- glm(Y ~ X, family=binomial (link = "logit"))
summary(logit) 

# Logit model odds ratios
exp(logit$coefficients)

# Probit model coefficients
probit<- glm(Y ~ X, family=binomial (link="probit"))
summary(probit)


# Regression marginal effects
coef(olsreg)

# Logit model average marginal effects
LogitScalar <- mean(dlogis(predict(logit, type = "link")))
LogitScalar * coef(logit)

# Probit model average marginal effects
ProbitScalar <- mean(dnorm(predict(probit, type = "link")))
ProbitScalar * coef(probit)


# Regression predicted probabilities
polsreg<- predict(olsreg)
summary(polsreg)

# Logit model predicted probabilities
plogit<- predict(logit, type="response")
summary(plogit)

# Probit model predicted probabilities
pprobit<- predict(probit, type="response")
summary(pprobit)


# Percent correctly predicted values
table(true = Y, pred = round(fitted(probit)))
table(true = Y, pred = round(fitted(logit))) 

# McFadden's Pseudo R-squared
probit0<-update(probit, formula= Y ~ 1)
McFadden<- 1-as.vector(logLik(probit)/logLik(probit0))
McFadden
