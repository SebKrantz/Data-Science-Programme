# Probit and Logit Models
  
# Outline:
#   Probit and logit models
#   Predicted probabilities
#   Marginal effects
#   Pseudo R-squared
#   Percent correctly predicted

# Data files: 
#   MROZ.csv

# setup
rm(list = ls())
directory <- "C:/Econometrics/DataR/"

# Install packages
PackageNames <- c("tidyverse", "stargazer", "magrittr", "margins", "caret")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Probit and logit models  ------------------------------------------------

# Model of being in labor force for women
MROZ <- read.csv(paste0(directory, 'MROZ.csv'))

MROZ %>% select(inlf, nwifeinc, educ, exper, age, kidslt6) %>% str
MROZ %>% select(inlf, nwifeinc, educ, exper, age, kidslt6) %>% head(10)
MROZ %>% select(inlf, nwifeinc, educ, exper, age, kidslt6) %>% stargazer(type = "text")
count(MROZ, inlf)

# Linear probability model (LPM) 
LPM <- lm(inlf ~ nwifeinc + educ + exper + age + kidslt6, MROZ)
summary(LPM)

# Probit model 
Probit <- glm(formula = inlf ~ nwifeinc + educ + exper + age + kidslt6, 
              family = binomial(link = "probit"), 
              data = MROZ)
summary(Probit)

# Logit model 
Logit <- glm(formula = inlf ~ nwifeinc + educ + exper + age + kidslt6, 
              family = binomial(link = "logit"), 
              data = MROZ)
summary(Logit)

# Predicted probabilities -------------------------------------------------
# Predicted probabilities for LPM
MROZ %<>% mutate(inlfhat_lpm = fitted(LPM))

# Predicted probabilities for probit model
MROZ %<>% mutate(inlfhat_probit = fitted(Probit))

# Predicted probabilities for logit model
MROZ %<>% mutate(inlfhat_logit = fitted(Logit))

# Summarize predicted values for inlf
MROZ %>% select(inlf, inlfhat_lpm, inlfhat_probit, inlfhat_logit) %>% 
  stargazer(type = "text")
MROZ %>% select(inlf, inlfhat_lpm, inlfhat_probit, inlfhat_logit) %>% 
  head(5)
MROZ %>% select(inlf, inlfhat_lpm, inlfhat_probit, inlfhat_logit) %>% 
  .[601:605, ]


# Marginal effects --------------------------------------------------------
  
# Linear probability model (LPM)
summary(LPM)
coef(LPM)
# Coefficients are marginal effects in a linear model

# Probit model
summary(Probit)
Mean <- model.frame(Probit) %>% 
  map_df(mean) # mean of independent variables

# Probit - marginal effect at the mean
Probit.atmean <- margins(Probit, at = Mean)
summary(Probit.atmean)

# Probit - average marginal effect
Probit.AME <- margins(Probit)
summary(Probit.AME)

# Logit model
summary(Logit)

# Logit - marginal effect at the mean
Logit.atmean <- margins(Logit, at = Mean)
summary(Logit.atmean)

# Logit - average marginal effect
Logit.AME <- margins(Logit)
summary(Logit.AME)

# Pseudo R-squared --------------------------------------------------------

# Probit model - unrestricted model with all variables
summary(Probit)

# Probit model - unrestricted model with all variables
ur.model <- Probit 
summary(ur.model)

# Log-likelihood for unrestricted model
(LLur <- logLik(ur.model))

# Probit restricted model with only constant
r.model <- glm(inlf ~ 1, binomial(link = "probit"), data = MROZ) 
summary(r.model)

# Log-likelihood for model with only constant
(LL0 <- logLik(r.model))

# Calculate pseudo R-squared
(pseudo_r2 <- 1 - LLur/LL0)


# Percent correctly predicted ---------------------------------------------

# Percent correctly predicted for probit model
(Probit.pred <- (fitted(Probit) > 0.5) %>% as.numeric %>% as.factor) # Prediction
(actual <- MROZ$inlf %>% as.factor) # actual data
caret::confusionMatrix(Probit.pred, actual, positive = "1")

# Percent correctly predicted for logit model
(Logit.pred <- (fitted(Logit) > 0.5) %>% as.numeric %>% as.factor) # Prediction
confusionMatrix(Logit.pred, actual, positive = "1")
