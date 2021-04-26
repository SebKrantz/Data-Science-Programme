#############################################################
# *************** R BASED DATA SCIENCE TRAINING *************
# Course 3: Linear Modelling and Analysis + Extensions in R
# -----------------------------------------------------------
# Prepared by: Sebastian Krantz, ODI Fellow, MEPD, MoFPED
# Permanent Contact: sebastian.krantz@graduateinstitute.ch
#############################################################

# Session 2

# Today:
# (1) Panel Data Regression
# (2) Linear Model Selection
# (3) Shrinkage Methods (LASSO Regression)
# (4) Principal Components Analysis and Partial Least Squares Regression
# (5) Decision Trees and Random Forests

# (8) Additional Topics: K-Nearest Neighbours Imputation, Clustering, SVM


# (1) Panel Data Regression ------------------------------------------------
#***************************************************************************

library(collapse)
library(jtools) # I will be using jtools::summ instead of summary() to summarise regression models
View(wlddev)

qsu(wlddev, pid = ~ country)

# Lets say we are interested in measuring the average effect of GDP on Life Expectancy on this panel of countries
# We could be inclined to run a regression like this:

summary(lm(LIFEEX ~ PCGDP, wlddev))

# But this regression is problematic for a number of reasons.
# In particular 2 reasons come to mind that have to do with the zero conditional mean assumption: cor(X, e) = 0  (The model is y = ßX + e)
# (1) There are lots of factors that impact both income and life expectancy that are omitted from this regression 
#     -> Issue of Endogeneity of the independent variable (GDP) / Omitted variable bias: We would need to control for lots of things or find a valid
#        instrument for GDP to argue that cor(X, e) = 0 is satisfied, that is that GDP is uncorrelated with all other factors affecting Life Expectancy 
#        that we have omitted from the regression, so that we can argue we are truly capturing the effect of GDP on Life Expectancy and not also the effect 
#        of many things correlated with both GDP and Life Expectancy. 
# (2) A lot of these omitted factors are country specific, causing some countries to have high GDP and life expectancy and others to have low GDP and Life expectancy 
#     Such as history / colonialism, spread of technology, quality of institutions, missionary activity etc.

# To get a closer look at this, let's visualize the data
library(magrittr)
wlddev %>% psmat(PCGDP + LIFEEX ~ iso3c, ~ year) %>% plot(colour = TRUE)
# We see that GDP and life expectancy in most countries increase over time, which 
# may be due to a number of omitted factors. 

# Lets plot GDP per capita against life expectancy
wlddev %$% plot(LIFEEX, PCGDP, col = iso3c, pch = 20, 
                ylab = vlabels(PCGDP), xlab = vlabels(LIFEEX),
                ylim = c(0, 70000), xlim = c(30, 85), # setting bounds to remove outliers
                main = "GDP Per Capita and Life Expectancy, 1960-2018") 
# -> Each sequence of dots of a certain colour
# Lets run a corresponding regression
pooled_model <- lm(PCGDP ~ LIFEEX, wlddev)         # Pooled OLS estimator
summ(pooled_model)  

# This function adds a regression line + formula to the plot
add_regline <- function(x, pos = "topleft", col = "blue") {
  abline(x, col = col)
  cf <- coef(x)
  formula_text <- sprintf("y = %.1f + %.1f x, R2 = %.3f", cf[1L], cf[2L], summary(x)$r.squared)
  legend(pos, formula_text, col = col, lty = 1, bty = "n")
}

add_regline(pooled_model)

# We can visualize the cross-country aspect of this relationship by computing the mean for each country over time
wlddev_means <- wlddev %>% gby(iso3c) %>% slt(PCGDP, LIFEEX) %>% fmean
wlddev_means %$% plot(LIFEEX, PCGDP, col = iso3c, pch = 20, 
                      ylab = vlabels(PCGDP), xlab = vlabels(LIFEEX),
                      ylim = c(0, 70000), xlim = c(30, 85),
                      main = "GDP Per Capita and Life Expectancy, 1960-2018")
# -> Looks similar: Evidently a big part of this relationship is due to factors between countries. 
means_model <- lm(PCGDP ~ LIFEEX, wlddev_means)  # Country means estimator
summ(means_model)   
add_regline(means_model)

# Now we can get rid of the cross-country aspect by subtracting the mean GDP and life expectance of
# each country from all the data points for that country (centering or within-transformation), 
# thus giving each country the same average GDP per capita and life expectancy over time
wlddev_demeaned <- wlddev %>% slt(iso3c, PCGDP, LIFEEX) %>% na_omit %>% gby(iso3c) %>% fwithin 
wlddev_demeaned %$%  plot(LIFEEX, PCGDP, col = iso3c, pch = 20, 
                          ylab = vlabels(PCGDP), xlab = vlabels(LIFEEX),
                          #ylim = c(0, 70000), xlim = c(30, 85),
                          main = "GDP Per Capita and Life Expectancy, 1960-2018")
# -> we can see now more clearly the relationship between GDP and life expectancy over time:
#    Some countries increase faster on GDP, others faster on life expectancy. 
within_model <- lm(PCGDP ~ LIFEEX, wlddev_demeaned)  # Country means estimator
summ(within_model)   
add_regline(within_model)

# So we can see that demeaning the data by country gets rid of unobserved country specific effects 
# that cause countries to have different levels of GDP and life expectancy, and that also influence
# the relationship between GDP and life expectancy

# Note that instead of demeaning the data, we can also include a dummy for each country in the regression
summ(lm(PCGDP ~ LIFEEX + iso3c, wlddev)) # Least-Squares Dummy variable Estimator
# Coefficient is the same as in the within model, but this is inefficient for large data
View(model.matrix(PCGDP ~ LIFEEX + iso3c, wlddev)) # This is what happens internally: iso3c is a factor variable, and lm calls model.matrix which creates a dummy for each country / factor level

# A final possibility that we have that also gets rid of country specific effects is taking first differences of the data
wlddev_FD <- wlddev %>% slt(iso3c, year, PCGDP, LIFEEX) %>% gby(iso3c) %>% fdiff(t = year) 
wlddev_FD %$% plot(LIFEEX, PCGDP, col = iso3c, pch = 20, 
                   ylab = vlabels(PCGDP), xlab = vlabels(LIFEEX),
                   ylim = c(-5000, 5000), xlim = c(-2, 2),
                   main = "GDP Per Capita and Life Expectancy, 1960-2018")
FD_model <- lm(PCGDP ~ LIFEEX, wlddev_FD)  # First-difference estimator
summ(FD_model)   # Negative coefficient !
add_regline(FD_model)

# Another nice function in the jtools package
plot_coefs(pooled_model, means_model, within_model, FD_model, 
           model.names = c("Pooled", "Means", "Within", "FD"))
# 'modelsummary" also provides a set of related functions (also for exporting)
modelsummary::modelplot(list(Pooled = pooled_model, Means = means_model, Within = within_model, FD = FD_model))
modelsummary::modelsummary(list(Pooled = pooled_model, Means = means_model, Within = within_model, FD = FD_model))

# More formally, we can test the significance of country specific factors predicting LIFEEX and PCGDP
wlddev %$% fFtest(LIFEEX, iso3c) # fFtest efficiently deals with the dummies
wlddev %$% fFtest(PCGDP, iso3c)
# -> Both significant: At this point we know we have an omitted variable bias problem, there are country specific
# shifters that influence both the dependent and independent variable in the regression 
# We can also see that including country dimmues improves the fit of the regression a lot
wlddev %$% fFtest(LIFEEX, iso3c, PCGDP)


# Now Lecture: https://sites.google.com/site/econometricsacademy/econometrics-models/panel-data-models
# and work through example + code provided. 

library(plm)
phtest(PCGDP ~ LIFEEX, data = wlddev, index = c("iso3c", "year"))
summary(plm(PCGDP ~ LIFEEX, data = wlddev, model = "within", index = c("iso3c", "year")))
options(plm.fast = TRUE)
summary(plm(PCGDP ~ LIFEEX, data = wlddev, model = "within", 
            index = c("iso3c", "year"), effect = "twoway"))

# Higher-dimensional fixed effects. 
library(fixest)
summary(fixest::feols(PCGDP ~ LIFEEX, data = wlddev, panel.id = c("iso3c", "year"), 
              fixef = c("iso3c", "year")))


#****************************
### In-Class Exercise 1 -----
#****************************

# Lets get the census data: 
CENS <- readRDS("Course 2 - Advanced Data Manipulation and Visualization in R/data/UBOS 2014 census.rds")
View(namlab(CENS))

# Lets say we are interested in examining the effect of missing in school on poverty
CENS %$% plot(EDU_6_12_NAS_P, WEA_L2MAD_P, col = factor(District), 
              xlab = vlabels(EDU_6_12_NAS_P), ylab = vlabels(WEA_L2MAD_P), 
              main = "Schooling and Poverty")

# Apply the plm methodology to see if you should take district fixed effects in this regression
lm(WEA_L2MAD_P ~ EDU_6_12_NAS_P, CENS)

# Now control for distance to school: SDL_EDU_PRI_P,  SDL_EDU_SEC_P, repeat the exercise. 
lm(WEA_L2MAD_P ~ EDU_6_12_NAS_P + SDL_EDU_PRI_P + SDL_EDU_SEC_P, CENS)

lm(WEA_L2MAD_P ~ EDU_6_12_NAS_P + SDL_EDU_PRI_P + SDL_EDU_SEC_P, W(CENS, ~ District, stub = FALSE))

# If it can be argued that distance to school only affects poverty through its effect on schoool attendance 
# (which is doubtful as distance to school is probably related to a lot of other social and infrastructural factors)
# We can ise it as an IV. 
library(AER)
# First stage
summ(lm(EDU_6_12_NAS_P ~ SDL_EDU_PRI_P + SDL_EDU_SEC_P, W(CENS, ~ District, stub = FALSE)))
# Second stage
summary(ivreg(WEA_L2MAD_P ~ EDU_6_12_NAS_P | SDL_EDU_PRI_P + SDL_EDU_SEC_P, data = W(CENS, ~ District, stub = FALSE)))



# Now do using fixest... 
# Trade panels....
# IV...




# wlddev %>% slt(iso3c, PCGDP, LIFEEX) %>%  
#   na_omit %>% gby(iso3c) %>% fbetween %$% lm(PCGDP ~ LIFEEX) %>% summ  # Between-Country Estimator


# Elementary machine learning (with caret)...............

# Top Resource: For this second part : 
# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/

# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/

# http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/
