# Panel Data Models
  
# Outline:
#   Pooled OLS estimator
#   Between estimator
#   First differences estimator
#   Fixed effects within estimator
#   Dummy variables regression
#   Random effects estimator
#   Hausman test for fixed versus random effects

# Data files: 
#   JTRAIN.csv

# setup
rm(list = ls()) 
directory <- "C:/Econometrics/DataR/"

# Install packages
PackageNames <- c("tidyverse", "stargazer", "magrittr", "haven", "plm")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
} 

# Example: Effect of training grants on firm scrap rate -------------------
JTRAIN <- read.csv(paste0(directory, 'JTRAIN.csv'))

# Drop missing observations for dependent variable
JTRAIN %<>% filter(!is.na(lscrap))

# Keep the variables needed
JTRAIN %<>% select(fcode, year, lscrap, tothrs, d88, d89, grant, grant_1)

# Describe and summarize
JTRAIN %>% str
JTRAIN %>% head(9)
JTRAIN %>% stargazer(type = "text")

# Panel data where fcode is the cross sectional dimension and year is the time series dimension

# Number of observations
JTRAIN %>% group_by(fcode) %>% summarize(n = n()) # number of obs by 'fcode'
JTRAIN %>% group_by(year) %>% summarize(n = n()) # number of obs by 'year'

# Overall variations
JTRAIN %>% 
  select(lscrap, tothrs, d88, d89, grant, grant_1) %>% 
  mutate_all(function(x) {x - mean(x, na.rm= T)}) %>% # variable - overall mean
  as.data.frame %>% 
  stargazer(type = "text", omit.summary.stat = "mean", digits = 2)

# Between variations
JTRAIN %>% group_by(fcode) %>%
  select(lscrap, tothrs, d88, d89, grant, grant_1) %>% 
  summarize_all(mean) %>% 
  as.data.frame %>% 
  select(-fcode) %>% 
  stargazer(type = "text", digits = 2)

# Within variations
JTRAIN %>% group_by(fcode) %>% 
  select(lscrap, tothrs, d88, d89, grant, grant_1) %>% 
  mutate_all(function(x) {x - mean(x)}) %>% # demean
  as.data.frame %>% 
  select(-fcode) %>%
  stargazer(type = "text", omit.summary.stat = "mean", digits =2)


# Pooled OLS, between, and first differences estimator --------------------

# Pooled OLS estimator
model_ols <- plm(formula = lscrap ~ tothrs + d88 + d89 + grant + grant_1, 
              data = JTRAIN, 
              index = c("fcode", "year"), # c(group index, time index)
              model = "pooling")  
summary(model_ols)
              
# Between estimator
model_be <- update(model_ols, model = "between")
summary(model_be)

# Taking first differences
diff <- function(x) {x - dplyr::lag(x)}
JTRAIN %<>% 
  group_by(fcode) %>%
  mutate(dlscrap = diff(lscrap),
         dtothrs = diff(tothrs),
         dgrant = diff(grant)) %>%
  ungroup()

# First differences estimator
model_fd <- lm(dlscrap ~ dtothrs + dgrant, JTRAIN)
summary(model_fd)


# Fixed effects within estimator ------------------------------------------
  
# Fixed effects within estimator
# model_fe <- update(model_ols, model = "within", effect = "individual")
model_fe <- plm(formula = lscrap ~ tothrs + d88 + d89 + grant + grant_1, 
              data = JTRAIN, 
              index = c("fcode", "year"), # c(group index, time index)
              model = "within", effect = "individual") 
summary(model_fe)

# Summarize the individual specific effects a_i
ai <- fixef(model_fe, type = "dmean") # extract fixed effects using 'fixef'
ai %>% head(10)
summary(ai) # 'summary' shows the p-value table
data.frame(ai) %>% stargazer(type = "text") # summarize


# Dummy variables regression ----------------------------------------------

# Dummy variables regression with fixed effects
# model_dv <- update(model_ols, ~ . + factor(fcode))
model_dv <- plm(formula = lscrap ~ tothrs + d88 + d89 + grant + grant_1 + factor(fcode),
              data = JTRAIN, 
              index = c("fcode", "year"), # c(group index, time index)
              model = "pooling")  
summary(model_dv)
# factor(fcode) creates one dummy variable for each fcode

# R-squared for fixed effects estimator and dummy variables regression
summary(model_fe)
yhat <- fitted(model_fe)
y <- pmodel.response(model_fe) # demeaned y
(mss <- sum((yhat - mean(y))^2))
(ess <- sum(resid(model_fe)^2))
(rsquared0 <- mss / (mss + ess))

summary(model_dv)
yhat <- fitted(model_dv)
y <- pmodel.response(model_dv)
(mss <- sum((yhat - mean(y))^2))
(ess <- sum(resid(model_fe)^2))
(rsquared <- mss / (mss + ess))


# Random effects estimator ------------------------------------------------

# Random effects estimator
# model_re <- update(model_ols, model = "random", random.method = "walhus")
model_re <- plm(formula = lscrap ~ tothrs + d88 + d89 + grant + grant_1, 
              data = JTRAIN, 
              index = c("fcode", "year"), # c(group index, time index)
              model = "random", random.method = "walhus")  
summary(model_re)
# The theta parameter is listed in the summary

# Calculate the random effects parameter theta
(sigma2_e <- model_re$ercomp$sigma2["idios"])
(sigma2_u <- model_re$ercomp$sigma2["id"])
(theta <- 1 - sqrt(sigma2_e / (sigma2_e + 3*sigma2_u)))


# Hausman test for fixed versus random effects ----------------------------
  
# The Hausman test is used to decide whether to use fixed effects or random effects.
# H0: FE coefficients are not significantly different from the RE coefficients
# Ha: FE coefficients are significantly different from the RE coefficients

# Fixed effects estimator
summary(model_fe)

# Random effects estimator
summary(model_re)

# Hausman test for fixed versus random effects
phtest(model_fe, model_re)
# If the Hausman test statistic is insignificant, use RE estimator because it is efficient
# If the Hausman test statistic is significant, use FE estimator because it is consistent
