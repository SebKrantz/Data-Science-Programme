#############################################################
# *************** R BASED DATA SCIENCE TRAINING *************
# Course 4: Time Series Analysis and Forecasting with R
# -----------------------------------------------------------
# Prepared by: Sebastian Krantz, ODI Fellow, MEPD, MoFPED
# Permanent Contact: sebastian.krantz@graduateinstitute.ch
#############################################################

# Course Aim: To familiarize participants with time-series data, concepts and 
#             analysis methods in R, including standard procedures like differencing 
#             and seasonal adjustment, autocorrelation, univariate forecasting methods,
#             like ARIMA and Exponential Smoothing, and various methods to forecast
#             a variable based on covariates, such as the error correction model.
#             An emphasis is placed on forecast evaluation and model selection.


# This Course:
# (1) Basic time series concepts using real and simulated data (Stationarity, ACF)
# (2) Getting and visualizing Ugandan Time Series Data (UGATSDB API) + Time Series Regression
# (3) Seasonality, Unit Roots and Cointegration + Error Correction Model
# (4) Univariate Forecasting with Exponential Smoothing and ARIMA
# (5) Forecasting with Covariates (DLM, ARIMAX, ECM) + Harmonic Regression
# (6) Model selection (residual diagnostics, information criteria, and in-sample forecasting)
# (7) Forecasting with a large number of series (Ridge regression, Partial least squares)
# (8) Non-Linear Methods (Neural Networks, Boosted Trees)
# Additional topics: Ensemble Forecasting (possibly not covered). 


setwd("Course 4 - Time Series Analysis and Forecasting in R")

# (1) Basic time series concepts using simulated and real data -------------
#***************************************************************************

# In cross-sectional data, we often make the assumption of random sampling, resulting 
# in samples of observations that are independent and identically distributed (i.i.d).
# This is a convenient assumption for applying statistical techniques like regression and 
# making inferences to the population based on theorems such as Law of Large numbers (LLN)
# and the Central Limit Theorem (CLT). 

# Again the i.i.d setup stipulates that x is a random sample drawn from a population
x <- rnorm(100)
# ... and y is a function of x plus some random error. 
y <- 2 + 3 * x + rnorm(100)
plot(x, y)

# In time-series data observations are not randomly sampled at a point in time, but follow each other in time. 
t <- 1:100 # time variable
plot(t, y)
# If these observations are i.i.d with mean 0 and constant variance, we call this a white-noise series
ts.plot(y)
abline(h = mean(y))
# White noise means also that the time-series is uncorrelated with its lag. 
library(collapse) # flexible functions for lagging/leading and differencing a time-series
L(y) # Lag of y
View(cbind(y, lag_y = L(y)))
pwcor(y, L(y))
View(cbind(y, L(y, 1:10)))
pwcor(y, L(y, 0:10)) # Correlation of y with all its lags is close to 0. 

# A quick way to correlate a series with it's lags and visualiye the result is through the autocorrlation function
acf(y) # Default plots it
acf(y, plot = FALSE) # we can also print it
# -> None of the correlations of y with it's lags is outside the 95% confidence bounds, thus we are confident that y is a white-noise series.

# In practice, most time series are not white noise or i.i.d, but observations are correlated over time
plot(airmiles)
lines(L(airmiles))
acf(airmiles)
# -> This series has a positive autocorrelation, and it has a visible trend


### Autoregressive Process
# Time-series don't need to have a trend, but can be autocorrelated. A data generating process for a series that is 
# autocorrelated of order 1 (short AR(1)) could look like this:
n <- 1000L      # Length of process
e <- rnorm(n)   # White noise error term
z <- numeric(n) # Initializing series z
z[1L] <- e[1L]  # the first observation is just the error
# This generates the process (i denotes the time period, b = 0.9 is the autoregressive coefficient of the process).
# (Note: Time-recursion is one of the rare cases where we actually need to use for-loops in R)
for(i in 2:n) z[i] = 0.9 * z[i-1L] + e[i] # Each subsequent observation depends on b * the previous one + error
## The process equation is valid for all time periods i. We can take lags of process as follows:
# z[i-1] = 0.9 * z[i-2] + e[i-1]
# z[i-2] = 0.9 * z[i-3] + e[i-2]
## We can insert the lagged process equation for z[i-1] into the process, to yield
# z[i] = 0.9 * (0.9 * z[i-2L] + e[i-1]) + e[i]
# z[i] = 0.81 * z[i-2L] + 0.9 * e[i-1] + e[i]
## Doing this again for z[i-2] yields
# z[i] = 0.81 * (0.9 * z[i-3] + e[i-2]) + 0.9 * e[i-1] + e[i]
# z[i] = 0.729 * z[i-3] + 0.81 * e[i-2] + 0.9 * e[i-1] + e[i]
## Doing this again for z[i-3] yields
# z[i] = 0.6561 * z[i-4] + 0.729 * e[i-3] + 0.81 * e[i-2] + 0.9 * e[i-1] + e[i]
## ... we could continue to do this ad infinitum. The coefficient on z[t-h] will
## continue to shrink until it reaches 0. Thus we get:
## z[i] = \sum_{h = 0}^Inf 0.9^h * e[i-h] 
## -> z[i] is an infinite weighted sum of the lagged error terms, where the weights are the squares of b.
##    such weighted sums of lagged error terms are also called 'moving averages'. 
##    By the principle of lagging and inserting terms, any autoregressive process
##    can be rewritten as an infinite moving average in the error term.

# Plot of z against time
ts.plot(z)
# Note that the plot looks similar to the plot of the i.i.d series y
ts.plot(y)
# Yet the ACF of z shows significant autocorrellation, while the ACF of y does not
acf(z)
acf(y)
# So z is not i.i.d. Each observation depends in a systematic way on the previous one. 
# But when plotted against time, z looks almost like an i.i.d series. 
# We call series that look like i.i.d series when plotted against time, but still display some 
# Autocorrelation stationary series. Under some pretty loose assumptions, such series are 
# still useful for statistical analysis and inference. These assumptions are: 

# I. The series has a constant mean over time (no trend)
# II. The series has a constant and finite variance over time (also known as homoskedasticity)
# III. The covariance between z[i] and z[i-h] only depends on h, not on i (covariance between adjacent observations is constant
#      -> No change in the autocorrelation pattern in the series over time)
# We call a series that satisfies these assumptions weakly stationary. 

# So z is a weekly stationary series. Let's look at some series that don't satisfy these assumtions:
plot(AirPassengers)       # Trend + increasing variance

# To do statistical analysis on a series like this, we need to apply some transformations to make the series stationary
plot(log(AirPassengers))     # Log-transformation stabilizes variance
# After log-transforming, we see the series has constant variance but is still increasing with a slight curve
# We can make it stationary by regressing on a quadratic trend and taking the residuals
t <- seq_along(AirPassengers)
ts.plot(resid(lm(log(AirPassengers) ~ poly(t, 2))))
# If we are just interested in regression residuals, the simplest way to detrend is using collapse::HDW(), which will also preserve the time-series attributes
plot(HDW(log(AirPassengers), # Subtracting a quadratic trend to make the series stationary.
         poly(seq_along(AirPassengers), 2)))
# Instead of detrending after taking logs, another option is differencing after taking logs. This is simplified by the collapse::Dlog function
plot(Dlog(AirPassengers))    

## In general: If series does not satisfy assumptions I to III, need to transform it to make it weakly stationary. 

# In this case the series is also seasonal
AirPassengers
class(AirPassengers)
frequency(AirPassengers)
monthplot(AirPassengers)
plot(stl(AirPassengers, "periodic")) # Can do a seasonal decomposition, take the remainder
plot(Dlog(AirPassengers, 12))        # Another option is seasonal-differencing
# Still doesn't look quite stationary, so we could do difference in seasonal log-difference:
plot(D(Dlog(AirPassengers, 12)))

# What about a series like this?
rw <- cumsum(rnorm(10000))  # Each observation is equal to the previous one plus some error term. 
ts.plot(rw)                 # Can look like a trending series, but trend is not very systematic. 
acf(rw)                     # HUH!

# We call such a series a random walk or unit root series (due to the autocorrelation close 1). 
# such series can be made stationary by differencing them one or more times
ts.plot(D(rw))

# Stock-Market indices are generally said to exhibit random walk character
plot(EuStockMarkets)
frequency(EuStockMarkets)
acf(EuStockMarkets) # Autocorrelations on the diagonal, cross-correlations on the off-diagonal.
plot(D(EuStockMarkets))
# With stock markets we additionally have the issue of periods of increased volatility.
plot(D(EuStockMarkets)^2)
# Smoothing the volatility
plot(roll::roll_mean(D(EuStockMarkets)^2, 31))

# A final series to consider is the moving average contructed like this
ma <- numeric(n) 
ma[1L] <- e[1L]    # the first observation is just the error
# This generates an MA(2) process
for(i in 3:n) ma[i] = e[i] + 0.9 * e[i-1L] + 0.7 * e[i-2L] # Each subsequent observation is the error plus a weighted average of previous period error terms
ts.plot(ma) # Plot looks almost like white noise series
acf(ma)     # But ACF shows two significant spikes -> ma[t] is correlated with ma[t-1] and ma[t-2] through the lagged error terms, but not with ma[t-h] for h >= 3.
# But it is not, it depends on the previous period through the error term
# We can use what is called a partial-ACF or PACF to see this:
pacf(ma)
# Compare this to the ACF and PACF of the AR(1) process
acf(z)
pacf(z)
# What is partial ACF?: Easiest way to remember: We compute the ACF between x[t] and x[t-h], controlling for all the lagged terms in-between (x[t-1] to x[t-h+1]):
pacf(z, plot = FALSE)
lm(z ~ L(z) + L(z, 2))           # The partial ACF at lag 2 is the correlation between z and zt-2, controlling for zt-1
lm(z ~ L(z) + L(z, 2) + L(z, 3)) # The partial ACF at lag 3 is the correlation between z and zt-3, controlling for zt-1 and zt-2
# z is an AR(1) process, thus the PACF at lags 2 and higher is 0 -> there is no correlation between z[t] and z[t-2] after controlling for z[t-1].
## But the PACF of the MA(2) does not decay. Why? 
## To see this we can rewrite the MA(2) process and then start inserting the lags of teh process into itself
# e[i] = ma[i] - 0.9 * e[i-1] - 0.7 * e[i-2] 
# e[i-1] =  ma[i-1] - 0.9 * e[i-2] - 0.7 * e[i-3] 
# e[i-2] =  ma[i-2] - 0.9 * e[i-3] - 0.7 * e[i-4]  
## Inserting for e[i-1] and e[i-2] in the equations for e[i] yields:
# e[i] = ma[i] - 0.9 * (ma[i-1] - 0.9 * e[i-2] - 0.7 * e[i-3]) + 0.7 * (ma[i-2] - 0.9 * e[i-3] - 0.7 * e[i-4]  )
# e[i] = ma[i] - 0.9 * ma[i-1] - 0.81 * e[i-2] - 0.63 * e[i-3] + 0.7 * ma[i-2] - 0.63 * e[i-3] - 0.49 * e[i-4]  
# e[i] = ma[i] - 0.9 * ma[i-1] + 0.7 * ma[i-2] - 0.81 * e[i-2] - 1.26 e[i-3] - 0.49 * e[i-4]  
# ...
## We can iterate further and find that the MA can be expressed as an infinite autoregressive process (in ma[i]),
## Thus it's PACF does not die out. 


# Thus we have learned: An AR(p) process is a process of the form
# y[t] = ß1 y[t-1] + ß2 y[t-2] + ... + ßp y[t-p] + e[t]
# The ACF of this process decays slowly while the PACF goes to zero after lag p. 

# A MA(p) is a process of the form:
# y[t] = e[t] + ß1 e[t-1] + ß2 e[t-p] + ... + ßp e[t-p]
# The ACF of this process goes to zero after lag p and the PACF decays slowly. 

# More details are provided in the Free Book:
# ' Time Series Analysis and Its Applications With R Examples'
# Available here: https://www.stat.pitt.edu/stoffer/tsa4/


#****************************
### In-Class Exercise 1 -----
#****************************

# (1) This code generates an autoregressive process of order 1 short AR(1)
n <- 10000L
e <- rnorm(n)   # White noise error term
z <- numeric(n) # Initializing series z
z[1L] <- e[1L]    # the first observation is just the error
for(i in 2:n) z[i] = 0.9 * z[i-1L] + e[i] # Each subsequent observation depends on the previous one + error
ts.plot(z)
acf(z)
pacf(z)

# (a) write code to generate an AR(2)

# (b) examine your process by plotting it, computing the ACF and PACF

# (c) add a MA(1) term to the AR(2), generating a so-called ARMA(2, 1) process

# (d) also examine this process using plot, ACF and PACF


# (2) Examine there series, are they Stationary? Autocorrelated? AR, MA, ARMA?
plot(Nile)
plot(BJsales) 
plot(sunspots)
plot(UKgas) 

# See for various other time series built into R (in the 'datasets' package and other packages you have loaded)
data()


# (2) Getting and visualizing Ugandan Time Series Data (UGATSDB API) -------
#***************************************************************************

# In Course 2 Session 2, I showed you how you can download and process excel sheets (from BoU)
# to read them into R for time-series analysis. This can be a tedious task, and needless to say
# does not make for easy time-series workflows or reproducible research. 

# Fortunately, I have done most of the work for you, by creating a quite comprehensive time-series
# database describing the Ugandan economy. The database can be accessed through a preliminary data portal at
# mepd.shinyapps.io/Macro-Data-Portal/, or through an API package for R, which you can install using the following command:

remotes::install_github('https://github.com/SebKrantz/UGATSDB', 
                        auth_token = paste0('g', 'hp', '_PDwAszQ', 'N2Tn9nK', 'aPnQUyrZPt7L', '3iPw3YQ7wh')) 

# We can now download and explore some Ugandan time series data.
library(ugatsdb)     # package connects to database while loading
?ugatsdb
example("ugatsdb")   # Execute usage example
ugatsdb_reconnect()  # After a while the connection will expire, then we need to call

# See what data is there
datasets <- datasets()
View(datasets)
series <- series()
View(series)

View(series[DSID == "BOU_MMI"])  # All indicators in Bank of Uganda Monthly Macroeconomic Indicators Dataset
# We can also just download the series for a single dataset
View(series("BOU_MMI"))

# Getting monthly macroeconomic indicators from BOU
MMI <- get_data("BOU_MMI")
View(namlab(MMI))
qsu(MMI)
descr(MMI)
# The format the data comes in with a date variable in front makes it easy to coerce it to an xts time series
library(xts)
MMI_xts <- as.xts(MMI)
plot(MMI_xts)
# This plots the standardized annual difference of the data
plot(STD(D(MMI_xts, 12)))

# We can smooth the data and get rid of any seasonality by calculating rolling statistics
library(roll)
plot(roll_mean(MMI_xts, 12))
plot(roll_mean(STD(G(MMI_xts, 12)), 12))
plot(roll_sd(STD(D(MMI_xts)), 12))

# Let's just get a single series: the PMI from Stanbic Bank
PMI <- get_data("MOF_POE", "PMI")
PMI_xts <- as.xts(PMI)
plot(PMI_xts)
# Using the dygraphs library, we can also easily create an interactive chart of an xts time series
dygraphs::dygraph(PMI_xts)

# With this data it is very easy to do some exploration and analysis
E_CPI <- na.omit(MMI_xts[, c("E_PA", "CPI_HL_09")]) # Getting Exchange Rate and CPI
# Standardized raw data
plot(STD(E_CPI), 
     legend.loc = "topleft", 
     main = "Exhange Rate and CPI")
# Smoothed Standardized raw data
plot(roll_mean(STD(E_CPI), 12), 
     legend.loc = "topleft", 
     main = "Exhange Rate and CPI")
# MoM growth rates
plot(G(E_CPI, 12), 
     legend.loc = "topleft", 
     main = "Exhange Rate Depreciation and YoY Inflation")
# Taking differences in the log and standardizing those gives us an idea about volatility in the series
plot(STD(Dlog(E_CPI)), legend.loc = "topleft")
# An estimate of volatility: 12-month rolling standard deviations of Standardized monthly log-differences in the series
plot(roll_sd(STD(Dlog(E_CPI)), 12), legend.loc = "topleft")
# If we increase the interval for rolling standard deviations to 4 years, we see that volatility has been declining
plot(roll_sd(STD(Dlog(E_CPI)), 48), legend.loc = "topleft")

# How can we further examine the relationship between two time series?
# We can simply calculate the correlation coefficient between MoM growth rates of the series.
pwcor(G(E_CPI, 12), N = TRUE, P = TRUE)
# But this does not show us much about the dynamic relationship, better compute the ACF
acf(na.omit(G(E_CPI, 12)))
acf(E_CPI)
pacf(E_CPI)

# The multivariate ACF shows the Cross-correlation function CCF on the off-diagonal. 
# The CCF of two series x and y correlates x with the lags and leads of y. 
# We can also compute the CCF separately on the MoM growth rates 
ccf(as.numeric(G(E_CPI[, "CPI_HL_09"], 12)), 
    as.numeric(G(E_CPI[, "E_PA"], 12)), na.action = na.omit)
# -> Pretty clean evidence of exchange rate pass-through, where exchange rate depreciation translates to inflation

# We can estimate a linear model of exchange rate pass through

# first note that the database API package has some convenient functions
head(MMI)
head(expand_date(MMI)) # expand_date generates some additional identifiers from the date variable

MMI <- expand_date(MMI)
str(MMI, give.attr = FALSE)
# Model to estimate exchange rate pass-through

# This now fits an exchange rate pass through model, where we regress the log-differnce of CPI 
# on the log difference of the exchange rate, with 12 monthly lags, and we put monthly dummies to account for any seasonality
plot(Dlog(E_CPI), legend.loc = "topleft")
acf(na.omit(Dlog(E_CPI)))
EP_mod <- lm(Dlog(CPI_HL_09) ~ L(Dlog(CPI_HL_09), 1:6) + L(Dlog(E_PA), 0:12) + Month, data = MMI)
summary(EP_mod)
# Summarizing the model with heteroskedasticity and autocorrelation consistent standard errors
jtools::summ(EP_mod, digits = 5, vcov = sandwich::vcovHAC)
jtools::plot_coefs(EP_mod, vcov = sandwich::vcovHAC)
# What does this tell us?
acf(resid(EP_mod))

# Now the API is very flexible, it allows us to download multiple datasets in full:
MMI_GOV <- get_data(c("BOU_MMI", "MOF_TOT"))
View(namlab(MMI_GOV))
# Or we can send a very specific request
CIEA_REV <- get_data(c("BOU_MMI", "MOF_TOT"), 
                     series = c("CIEA", "REV"), 
                     from = 2006)

plot(STD(as.xts(CIEA_REV)), legend.loc = "topleft")
plot(log(as.xts(CIEA_REV)), legend.loc = "topleft")
with(CIEA_REV, ccf(G(CIEA), G(REV), na.action = na.omit))
# Regressing revenue on the CIEA and lags of revenue (to account for serial correlation)
CREV_mod <- lm(Dlog(REV) ~ L(Dlog(REV), 1:3) + Dlog(CIEA) + Month, expand_date(CIEA_REV))
jtools::summ(CREV_mod, digits = 5, vcov = sandwich::vcovHAC)
acf(resid(CREV_mod))
# We can eaily export this regression using jtools and huxtable:
huxtable::quick_xlsx(jtools::export_summs(CREV_mod, vcov = sandwich::vcovHAC), file = "CIEA_REV.xlsx")
huxtable::quick_docx(jtools::export_summs(CREV_mod, vcov = sandwich::vcovHAC), file = "CIEA_REV.docx")

# In the above regression we included lags of revenue to account for serial correlation in the residuals.
# Another option is Prais-Winsten estimation. If we have a model like this:
# yt = xt + et
# with serieal correlation of order 1 in the error term:
# et = p et-1 + ut
# Example: 
CREV_mod <- lm(Dlog(REV) ~ Dlog(CIEA) + Month, expand_date(CIEA_REV))
r <- resid(CREV_mod)
jtools::summ(lm(r ~ L(r)))
# we could transform the model by subtracting p times the model from itself. 
# yt - p yt-1 = xt - p xt-1 + et - p et-1
# The errors from this model are then 
# et - p et-1 = ut
# which is serially uncorrelated 
# This subtracts p times the model from itself and re-estimates it. 
CREV_mod <- lm(D(Dlog(REV), rho = -0.41) ~ D(Dlog(CIEA), rho = -0.41) + Month, expand_date(CIEA_REV))
r <- resid(CREV_mod)
jtools::summ(lm(r ~ L(r)))

# Fortunately you don't need to do this manually. the prais package automatically estimates p and then runs a regression on the transformed data.  
library(prais)
summary(prais_winsten(Dlog(REV) ~ Dlog(CIEA) + Month, expand_date(CIEA_REV)))
# Compare this to the model where we include lags of revenue
CREV_mod <- lm(Dlog(REV) ~ L(Dlog(REV), 1:3) + Dlog(CIEA) + Month, expand_date(CIEA_REV))

# Regarding the API, it further includes functions wide2long and long2wide to transform the data. 
# transform data to long format:
wide2long(MMI)

# This can be useful for plotting with ggplot2
library(ggplot2)
ggplot(wide2long(CIEA_REV), aes(x = Date, y = Value)) +
  geom_line() + facet_wrap( ~ Series, scales = "free_y")

ggplot(wide2long(MMI), aes(x = Date, y = Value)) +
  geom_line() + facet_wrap( ~ Series, scales = "free")

# Here also colouring series by month
ggplot(wide2long(MMI)[Series %in% c("CPI_HL_09", "CIEA", "BTI", "TB")], 
            aes(x = Date, y = Value, colour = Month)) +
  geom_line() +  facet_wrap( ~ Series, scales = "free") + 
  scale_colour_manual(values = rainbow(12)) +
  theme_dark() 

# Finally, it is very easy to export data to Excel:
wide2excel(MMI, "MMI.xlsx")
# Wide (row-based) Excel format:
wide2excel(MMI, "MMI_T.xlsx", transpose = TRUE)

#****************************
### In-Class Exercise 2 -----
#****************************

# Examine the relationship between one of these groups of variables:
# CIEA and CPI/Inflation (Phillips Curve) -> No significant relationship. 
# M0, M2, I_LR, CPI/Inflation, CIEA and CBR (Monetary Policy Transmission)
# CIEA, REER and TB (Trade, Economic Activity and The Real Exchange Rate)
# REER, Imports and Trade Tax Revenue (Real exchange rate, imports and trade tax revenue)
# NCG, TBILL, CIEA, DLG (Economic activity and credit to the government)
# NCG, TBILL, I_TBY_91, PSC, PMI and Government Expenditure (Government financing and expenditure)
# FIN_DOM from MOF_TOT, EXP from MOF_EXP
# CIEA and Tax Revenue (Economic activity and tax revenue)

# (a) Get the Series from 2000 onwards, in the datasets BOU_MMI, MOF_TOT, MOF_REV or MOF_EXP
# (b) Plot the series in levels and monthly growth rates. Is the level series stationary? Is the series in growth rates stationary?
# (c) Compute a simple correlation matrix between the growth rates
# (d) Compute a multivariate ACF of the growth rates
# (e) Specify an dynamic regression model (with monthly dummies to account for seasonality)



# Summary: The database API is an extremely powerful tool to get Ugandan data into R and do some quick analysis.
# If you want to get some Ugandan time series data into R, first check the series() table if it is not already there...
# The database has the majority of time series from BoU, MoFPED, IMF and World Bank. 
# Note that to use the API, you currently need a free internet connection. MoFPED internet is restricted so you cannot
# connect to a database outside your network. If this is the case, you need to use your own internet connection when in office. 


# (1) Seasonality, Unit Roots and Cointegration ----------------------------
#***************************************************************************

# Time series below Annual frequency often have seasonal patterns. 

# A stark example is the quarterly GDP series from UBOS:
GDP <- get_data("UBOS_GDP_KP", "GDP_KP")
plot(as.xts(GDP))

# In other series it is not that clear:
CPI_EFU <- get_data("BOU_CPI", "CPI_EFU_09")
plot(as.xts(CPI_EFU))
CIEA <- get_data("BOU_MMI", "CIEA")
plot(as.xts(CIEA))

# So how can we determine whether a series is seasonal. 
# -> Many different approaches, some simple, some more sophisticated.

# A simple and practical approach, is to regress a stationary series on a sequence of monthly dummies and 
# test whether the regression is significant. If the series is non-stationary, we need to make it stationary
# by differencing (or log-differencing to account for increasing variance)

steastest_GDP <- lm(Dlog(GDP_KP) ~ Quarter, expand_date(GDP))
summary(steastest_GDP) # Yup, significant
ts.plot(Dlog(GDP$GDP_KP))
lines(c(NA, fitted(steastest_GDP)), col = "red")

# A faster way to do this is with collapse::fFtest
fFtest(Dlog(GDP$GDP_KP), expand_date(GDP)$Quarter)

# For the others we can do
fFtest(Dlog(CPI_EFU$CPI_EFU_09), expand_date(CPI_EFU)$Month) # Seasonal
fFtest(Dlog(CIEA$CIEA), expand_date(CIEA)$Month)             # Non-Seasonal

# We should also do visual inspection...
GDP_ts <- ts(GDP$GDP_KP, start = c(2015, 1), frequency = 4)
plot(GDP_ts)
forecast::tsdisplay(GDP_ts)
monthplot(GDP_ts)
forecast::ggmonthplot(GDP_ts)
forecast::ggseasonplot(GDP_ts)
forecast::ggseasonplot(GDP_ts, polar = TRUE)

GDP_stl <- stl(GDP_ts, "periodic")
plot(GDP_stl)

# More sophisticated tests are in the seastest package:
library(seastests)
seastests::kw(GDP_ts)
seastests::qs(GDP_ts)
seastests::wo(GDP_ts)         # Combines the results of the two tests...
seastests::isSeasonal(GDP_ts) # This is a convenience function...

# Seasonally adjusted series
plot(GDP_stl$time.series[, "trend"] + GDP_stl$time.series[, "remainder"])
plot(forecast::seasadj(GDP_stl)) # Same thing
plot(decompose(GDP_ts)) # Another procedure (less sophisticated)
plot(forecast::seasadj(decompose(GDP_ts))) # Almost the same

# Now a really sophisticated method is the X-13 ARIMA SEATS Software developed by the US Bureu of Census, available in R through the seasonal package:
library(seasonal)
plot(seas(GDP_ts))
GDP_ajd <- final(seas(GDP_ts))
plot(GDP_ajd)
seastests::isSeasonal(GDP_ajd)


#****************************
### In-Class Exercise 3 -----
#****************************

# Take the series you analyzed for exercise 2 and check them for seasonality, 
# both visually, using a regression based test and the overall seasonality test in the seastest package
# If any series are seasonal, adjust them using both stl and seas from the seasonal package. 
# Chose the result you find more satisfactory

# Note: For regression analysis we need to decide whether seasonality is a problem or not. 
# It is a problem whenever there is seasonality in both x and y. 
# If the model is well-specified (i.i.d errors, no heteroskedasticity in residuals), and the seasonal
# pattern is constant through time, then we can use monthly or quarterly dummies in the regression
# to account for the seasonality.
# If seasonality is linearly increasing over time, we can interact seasonal dummies with a time variable to account
# for that, e.g. ~ Year*Month.
# For data with more complex seasonal patterns, or if the nature of seasonality in x and y is quite different, 
# it is better to adjust the data beforehand. 



# Unit Roots and Cointegration ---------------------------------------------------------------------------

# What do we make of a series like this?
plot(MMI_xts[, "E_PA"]) # Period-average monthly exchange rate...
acf(MMI_xts[, "E_PA"])
acf(MMI_xts[, "E_PA"], plot = FALSE)

# Remember this random walk series we generated?
rw <- cumsum(rnorm(1000))  # Each observation is equal to the previous one plus some random error term. 
ts.plot(rw)                 
acf(rw)   

# The ACF looks similar, hardly and decay, with autocorrelations close to 1
# This is also reflected in the regression coefficient on the first lag, which is close to 1 
lm(E_PA ~ L(E_PA), MMI)
# If this is the case (the first-order autocorrelation is very close to 1), we say 'the series has a unit root' 
# or that 'the series exhibits random walk behavior'. 
# In a random walk liek the one generated above, each observation is equal to the previous one plus some random error term, 
# so the progression of the series is totally unpredictable, which means there is nothing we can do to analyze the series 
# (we just sum up random noise.)

# An economic series like the exhange rate is not just random noise, but the unit root means that the series
# depends very strongly on the previous value, and there is nothing compelling the series to return to its average

# Recvall the autoregressive process with b = 0.5:
n <- 1000L
e <- rnorm(n)   # White noise error term
z <- numeric(n) # Initializing series z
z[1L] <- e[1L]    # the first observation is just the error
for(i in 2:n) z[i] = 0.5 * z[i-1L] + e[i] # 
ts.plot(z)
# We can see the series oscillates around 0 -> it is stationary with constant mean and variance. 
# Increasing b to 0.99, we see the series does not oscillate anymore around 0 but takes an arbitrary path that looks like a random walk.
# I said earlier that we need weak stationarity, with constant mean, constant variance and covariance only depending on the lag order
# to make inference in a time-series regression. We can not do this with a unit root series, it is non-stationary.

# So If we look at a time series and we see it is non-stationary, and not stationary around a very stable trend either, ..
plot(na.omit(MMI_xts[, "CIEA"])) # Trend-stationary series
# ... then we need to check if the series has a unit root. 
# Intuitively we could run the regression of the series on its lag
E_mod <- lm(E_PA ~ L(E_PA), MMI)
# and then test whether the lagged coefficient is equal to 1
car::linearHypothesis(E_mod, "L(E_PA) = 1") # Here P = 0.7 > 0.05, so we would accept the null hypothesis that b = 1.
# This is however tedious, since We need to run a regression and then run a test, 
# and it is also wrong because hypothesis testing can only be done with stationary data

# So his is not the way to go. But we can use a simple trick to transform our model: 
# If yt = 1 * yt-1 + et (b = 1), then we can subtract yt-1 from both sides and get yt - yt-1 = 0 * yt-1 + et, 
# which means if the series has a unit root, the regression of the first-difference of the series on its lagged level should give a 0 coefficient
plot(D(MMI_xts[, "E_PA"])) # If a series has one unit root (is integrated of order 1), the difference of the series is stationary
# We we can do inference when we run the regression
E_mod <- lm(D(E_PA) ~ L(E_PA), MMI)
summary(E_mod) # We fail to reject the Null hypothesis that the coefficient is different from 0, thus the series has a unit root. 
# This is called a Dickey-Fuller Unit-Root test. However the t-critical values are wrong since yt-1 is still non-stationary, 
# and if there is a trend or seral correlation in yt - yt-1, we also have a problem. 
# What is typically done therefore is to run the so-called Augmented Ducked Fuller test:
ADF_mod <- lm(D(E_PA) ~ L(E_PA) + seq(E_PA) + L(D(E_PA), 1:12), MMI) # Difference of the series regressed on lagged leve, a linear trend, and further lagged differences to curb anu autocorrelation (12 lags for monthly data)
summary(ADF_mod) # The critical values are still wrong...
car::Boot(lm(`D(E_PA)` ~ ., model.frame(ADF_mod))) # We can bootstrap the model (standard error is slightly larger)

# Now thankfully we don't need to do this ourselves, the tseries package provides several tests:
library(tseries)
adf.test(MMI_xts[, "E_PA"]) # We fail to reject the Null of a unit root
pp.test(MMI_xts[, "E_PA"])  # Another similar test: Phillips-Perron Unit Root Test -> Does not require normal erroes

# We can also run the opposite test for stationarity of a series
kpss.test(MMI_xts[, "E_PA"], null = "Trend") # We reject the null of stationarity!
kpss.test(MMI_xts[, "CIEA"], null = "Trend")

# -> So if the unit root tests fail to reject the null of a unit root and the stationarity test rejects the null of stationarity,
# we can be pretty confident that the series has a unit root.

# What do we do if a series has a unit root? -> We need to make it stationary by differencing it before including it in a regression !!

### There is one exception to this rule:
# Consider here the period-average and the end of period exchange rate
E <- na.omit(MMI_xts[, c("E_PA", "E_EP")])
plot(E, legend.loc = "topleft")
lapply(as.data.frame(E), adf.test) # Both series are similarly non stationary. 
# But we can see from the plot that they exhibit the same stochastic trend. 
# If we regress one on the other and compute the residuals
E_mod <- lm(E_EP ~ E_PA, E)
ts.plot(resid(E_mod))
# The residuals follow a stationary process
adf.test(resid(E_mod))  # Reject unit root
kpss.test(resid(E_mod)) # Fail to reject stationarity
# Quick note: A fast way to obtain the residuals from regressing on series on another is collapse::HDW
plot(HDW(E[, "E_PA"], E[, "E_EP"]))

# If this is the case, that two series are non-stationary but follow the same stochastic trend so that regressing 
# one on the other results in a stationary series, we say these series are cointegrated, and we can run that regression and conduct inference on the coefficient.

# Again to test whether two series are cointegrated, we regress one on the other and test the residuals for a unit root. 
# If both series have a unit root but the residuals are stationary, the series are cointegrated. 
# There is also a formal test for this:
po.test(E)
# In the urca package we have another cointegration test, which is better if we have more than 2 series and want to check if they are cointegrated. 
library(urca)
summary(ca.jo(E))

# Cointegration means two series are both integrated (= have a unit root) but follow the same stochastic trend, 
# so they never diverge too far from one another, and the residuals are stationary. 

# So we can run the regression of E_PA on E_EP and conduct inference. 
summary(E_mod)
# Now that in itself if not terrible interesting, the coefficient is close to 1 telling us that none is systematically greater than the other.
# This is the long-run relationship between these exchange rates. 

# We can still look at the short-run relationship by running the regression in first-differences
summary(lm(D(E_EP) ~ D(E_PA), E)) # About 85% of changes in E_PA are reflected in E_EP...

# Now the interesting thing about cointegration is that it can help us forecast the series using information about the long-run
# relationship. If 2 series have the same stochastic trend, they should never be too far from each other. 
# If there is a deviation between the two series, we would expect them to move closer again in the next period. 
# The deviation of the series in the current period is the residuals from the level-regression
E_LR_res <- resid(E_mod)
# We can use the deviation from the long-run relationship in the previous period as an additional regressor in the short-run model
summary(lm(D(E_EP) ~ D(E_PA) + L(E_LR_res), model.frame(E_mod)))
# This is what is called an error-correction model! The coefficient on E_LR_res can be interpreted as the speed of adjustment 
# following a deviation from equilibrium. 

# We write the error correction model (short ECM) as:
# D(yt) = b0 + b1 D(xt) + b2 (yt-1 - a xt-1) + et, where the termm yt - a yt-1 is the lagged residual
# from the levels regression and b2 is the speed of adjustment parameter. 

# We can rewrite this as D(yt) = b0 + b1 D(xt) + b2 yt-1 - b2 a xt-1 + et, 
# so we can also estimate the ECM in one equation:
ECM_mod <- lm(D(E_EP) ~ D(E_PA) + L(E_EP) + L(E_PA), E)
summary(ECM_mod)



#****************************
### In-Class Exercise 4 -----
#****************************

# Check your series from excercise 2 for a unit root, 
# If you have more than 2 series with a unit root, check if they are cointegrated
# If they are cointegrated, fit an error correction model. 


# Checking time-series regression models --------------------------------------------

# Same assumptions as before, we can plot diagnostic plots and do a heteroskedasticity test as discussed in the linear modelling course
plot(ECM_mod, which = 1:5)
car::infIndexPlot(ECM_mod)
car::dfbetaPlots(ECM_mod)
lmtest::bptest(ECM_mod) # Heteroskedasticity? -> Nope

# Now an additional assumption we make is that the residuals are uncorrelated over time...
# Why? Because if the residuals were correlated, we have failed to model some information in the data, 
# and we cannot do accurate inference on the coefficients

forecast::checkresiduals(ECM_mod) # There is some autocorrelation but strange. 
lmtest::dwtest(ECM_mod) # First-order autocorrelation? -> Nope
acf(resid(ECM_mod))

# What about autocorrelation at higher orders?
lmtest::bgtest(ECM_mod, order = 12)
# my corrgram function can be used to compute a correlogram of the residuals
source("resources/Macroeconomic Forecasting with ECM's and DLM's/ME_FC_ECM_DLM_PCK_FUN.R")

# This is nice to look at: 
corrgram(resid(ECM_mod), plot = TRUE)

#****************************
### In-Class Exercise 5 -----
#****************************

# Check your model from Exercise 2




# Concluding comments on time-series regression:

# We can fit linear models with time-series data as long as the series are stationary
# iF data are non-stationary, we need to difference the data to make it stationary, or, 
# if data are stationary around a trend, we can include a linear or quadratic trend in the regression.
# If both x and y exhibit seasonal variation that is unchanging over time, we need to account for it using 
# seasonal dummies. If seasonal variation is increasing or decreading over time, we can interact the seasonal dummies with 
# a time trend to account for that. 

# The usual assumptions for linear models (homoskedastic residuals, normality of residuals, no strong outliers etc.) carry through to time series regression.
# We additionally require residuals to be serially uncorrelated. If residuals are serially correlated, inference is invalied, and we possibly have an omitted variable bias problem. 
# So if residuals are serieally correlated, we need to add more variables or a lag of the dependent variable into the model. 

# Another option is estimating a generalized diffrerence equation where we parially difference the model to get rid of serial
# correlation in the error term: install.packages("prais")
summary(prais::prais_winsten(BTI ~ CIEA + Year*Month, MMI))

# Finally, if data have a unit root, but are cointegrated, we can estimate an error correction model. 





# (4) Univariate Forecasting with Naive Forecast, ETS and ARIMA ------------
#***************************************************************************

setwd("Course 4 - Time Series Analysis and Forecasting in R")

# Read the data from Excel into R
mydata <- readxl::read_excel("data/exercise1.xlsx")

# Look at the first few lines of mydata
head(mydata)

# Create a ts object called myts
myts <- ts(mydata[, 2:4], start = c(1981, 1), frequency = 4)

library(forecast)
# Time Plots ------------------------------------------------------------------------------
# Plot the data with facetting
autoplot(myts, facets = TRUE)
# Plot the data without facetting
autoplot(myts)
# Plot three datasets from the forecast package
autoplot(gold)      # containing gold prices in US dollars
autoplot(woolyrnq)  # containing information on the production of woollen yarn in Australia
autoplot(gas)       # containing Australian gas production
# Find the outlier in the gold series
goldoutlier <- which.max(gold)
# Look at the seasonal frequencies of the three series
frequency(gold)
frequency(woolyrnq)
frequency(gas)

# Seasonal Plots ------------------------------------------------------------------------------
library(fpp2) # Just some datasets...

# a10 contains monthly sales volumes for anti-diabetic drugs in Australia. 
# In the plots, can you see which month has the highest sales volume each year? 
# What is unusual about the results in March and April 2008?

# Create plots of the a10 data
autoplot(a10)
ggseasonplot(a10)
# Produce a polar coordinate season plot for the a10 data
ggseasonplot(a10, polar = TRUE)

# ausbeer which contains quarterly beer production for Australia. 
# What is happening to the beer production in Quarter 4?
# Restrict the ausbeer data to start in 1992
beer <- window(ausbeer, start = 1992)
# Make plots of the beer data
autoplot(beer)
ggsubseriesplot(beer)


# Autocorrelation of non-seasonal time series --------------------------------------------

# Another way to look at time series data is to plot each observation against another 
# observation that occurred some time previously by using gglagplot(). For example, you 
# could plot against. This is called a lag plot because you are plotting the time series 
# against lags of itself.

# The correlations associated with the lag plots form what is called the autocorrelation 
# function (ACF). The ggAcf() function produces ACF plots.

oil # annual oil production in Saudi Arabia from 1965-2013 (measured in millions of tons).
autoplot(oil)
gglagplot(oil)
ggAcf(oil)

# Autocorrelation of seasonal and cyclic time series --------------------------------------

# When data are either seasonal or cyclic, the ACF will peak around the seasonal lags 
# or at the average cycle length.

#****************************
### In-Class Exercise 6 -----
#****************************

# You will investigate this phenomenon by plotting the annual sunspot series (which follows the solar cycle of approximately 10-11 years) in sunspot.year and the daily traffic to the Hyndsight blog (which follows a 7-day weekly pattern) in hyndsight. 

# Plot the annual sunspot numbers
autoplot(sunspot.year)
ggAcf(sunspot.year)

# Plot the traffic on the Hyndsight blog
autoplot(hyndsight)
ggAcf(hyndsight)

# What is the seasonal period of these series?

# White Noise -------------------------------------------------------------------

# white noise is a term that describes purely random data. 
# You can conduct a Ljung-Box test using the function below to confirm the randomness of a series; 
# a p-value greater than 0.05 suggests that the data are not significantly different from white noise.

# There is a well-known result in economics called the "Efficient Market Hypothesis" that states that asset prices reflect all available information. A consequence of this is that the daily changes in stock prices should behave like white noise (ignoring dividends, interest rates and transaction costs). The consequence for forecasters is that the best forecast of the future price is the current price.
# You can test this hypothesis by looking at the goog series, which contains the closing stock price for Google over 1000 trading days ending on February 13, 2017. This data has been loaded into your workspace.

# Plot the original series
autoplot(goog)

# Plot the differenced series
autoplot(diff(goog))

# ACF of the differenced series
ggAcf(diff(goog))

# Ljung-Box test of the differenced series
Box.test(diff(goog), lag = 10, type = "Ljung")
corrgram(diff(goog))


# Forecasting and Potential Futures --------------------------------------------------------------

# Naive Forecast: 
# A forecast is the mean or median of simulated futures of a time series.
# The very simplest forecasting method is to use the most recent observation; 
# this is called a naive forecast and can be implemented in a namesake function. 
# This is the best that can be done for many time series including most stock price data, 
# and even if it is not a good forecasting method, it provides a useful benchmark for other forecasting methods.

# Use naive() to forecast the goog series
fcgoog <- naive(goog, h = 20)

# The resulting output is an object of class 'forecast'. 
# This is the core class of objects in the forecast package, and there are many functions for dealing with them including summary() and autoplot().

# Plot and summarize the forecasts
autoplot(fcgoog)
summary(fcgoog)

# For seasonal data, a related idea is to use the corresponding season from the last year of data. 
# For example, if you want to forecast the sales volume for next March, you would use the sales volume from the previous March. 
# This is implemented in the snaive() function, meaning, seasonal naive.

# Use snaive() to forecast the ausbeer series
fcbeer <- snaive(ausbeer, h = 16)

# Plot and summarize the forecasts
autoplot(fcbeer)
summary(fcbeer)

# Checking time series residuals ------------------------------------------------------------

# Then applying a forecasting method, it is important to always check that the residuals are well-behaved 
# (i.e., no outliers or patterns) and resemble white noise. The prediction intervals are computed assuming 
# that the residuals are also normally distributed. You can use the checkresiduals() function to verify 
# these characteristics; it will give the results of a Ljung-Box test.

#****************************
### In-Class Exercise 7 -----
#****************************

# Check the residuals from the naive forecasts applied to the goog series
goog %>% naive() %>% checkresiduals()
# Do they look like white noise?

# Check the residuals from the seasonal naive forecasts applied to the ausbeer series
ausbeer %>% snaive() %>% checkresiduals()
# Do they look like white noise?



# Training and test sets and forecast evaluation -------------------------------------------------------------

# In data science, a training set is a data set that is used to discover possible relationships. A test set is a data set that is used to verify the strength of these potential relationships. When you separate a data set into these parts, you generally allocate more of the data for training, and less for testing.
# One function that can be used to create training and test sets is subset.ts(), which returns a subset of a time series where the optional start and end arguments are specified using index values.

# Create the training data as train
train <- subset(gold, end = 1000)

# Compute naive forecasts and save to naive_fc
length(gold)
naive_fc <- naive(train, h = 108)
autoplot(naive_fc)

# Compute mean forecasts and save to mean_fc
mean_fc <- meanf(train, h = 108)
autoplot(mean_fc)

# Another function, accuracy(), computes various forecast accuracy statistics given the forecasts and the corresponding actual observations. It is smart enough to find the relevant observations if you give it more than the ones you are forecasting.
# The accuracy measures provided include root mean squared error (RMSE) which is the square root of the mean squared error (MSE). Minimizing RMSE, which corresponds with increasing accuracy, is the same as minimizing MSE.

# Use accuracy() to compute RMSE statistics
accuracy(naive_fc, gold)
accuracy(mean_fc, gold)

#****************************
### In-Class Exercise 8 -----
#****************************

# Which of the two forecasts is better?


# you will use the Melbourne quarterly visitor numbers (vn[, "Melbourne"]) to create three different training sets, omitting the last 1, 2 and 3 years, respectively. Inspect the pre-loaded vn data in your console before beginning the exercise; this will help you determine the correct value to use for the keyword h (which specifies the number of values you want to forecast) in your forecasting methods.

# Create three training series omitting the last 1, 2, and 3 years
train1 <- window(vn[, "Melbourne"], end = c(2014, 4))
train2 <- window(vn[, "Melbourne"], end = c(2013, 4))
train3 <- window(vn[, "Melbourne"], end = c(2012, 4))

# Produce forecasts using snaive()
fc1 <- snaive(train1, h = 4)
fc2 <- snaive(train2, h = 4)
fc3 <- snaive(train3, h = 4)

# for each training set, compute the next year of data, and finally compare the mean absolute percentage error (MAPE) of the forecasts using accuracy(). Why do you think that the MAPE vary so much?

# Use accuracy() to compare the MAPE of each series
accuracy(fc1, vn[, "Melbourne"])["Test set", "MAPE"]
accuracy(fc2, vn[, "Melbourne"])["Test set", "MAPE"]
accuracy(fc3, vn[, "Melbourne"])["Test set", "MAPE"]

#  A good model forecasts well (so has low RMSE on the test set) and uses all available information in the training data (so has white noise residuals).


# Using tsCV() for time series cross-validation ----------------------------------------------

# The tsCV() function computes time series cross-validation errors. It requires you to specify the time series, the forecast method, and the forecast horizon. Here is the example used in the video:

# Compute cross-validated errors for up to 8 steps ahead
e <- tsCV(goog, forecastfunction = naive, h = 8, initial = 900)

# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = TRUE)
sqrt(mse)

mae <- colMeans(abs(e), na.rm = TRUE)

# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()


# Simple exponential smoothing -------------------------------------------------------------------

# The ses() function produces forecasts obtained using simple exponential smoothing (SES). 
# The parameters are estimated using least squares estimation. All you need to specify is the time series and the forecast horizon; the default forecast time is h = 10 years

# Use ses() to forecast the next 10 years of winning times
plot(marathon)
fc <- ses(marathon, h = 10)

# Use summary() to see the model parameters
summary(fc)

# Use autoplot() to plot the forecasts
autoplot(fc)

# Add the one-step forecasts for the training data to the plot
autoplot(fc) + autolayer(fitted(fc))

# Comparing SES to naive on the test set: 

# Create a training set using subset()
train <- subset(marathon, end = length(marathon) - 20)

# Compute SES and naive forecasts, save to fcses and fcnaive
fcses <- ses(train, h = 20)
fcnaive <- naive(train, h = 20)
autoplot(fcnaive)
autoplot(fcses)

# Calculate forecast accuracy measures
accuracy(fcses, marathon)
accuracy(fcnaive, marathon)

# Naive is better -> More complex models are not always better. 


# Holt's trend methods (exponential smoothing with trend) --------------------------------------------------------------------

# Holt's local trend method is implemented in the holt() function:

# Produce 10 year forecasts of austa using holt()
fcholt <- holt(austa, h = 10)  # data on visitors to australia...

# Look at fitted model using summary()
summary(fcholt)

# Plot the forecasts
autoplot(fcholt)

# Check that the residuals look like white noise
checkresiduals(fcholt)

# Holt-Winters (Exponential Smoothing with Trend and Seasonality)  --------------------------------------------------------------------------------
library(fpp2)
fc1 <- hw(austourists, seasonal = "additive")
fc2 <- hw(austourists, seasonal = "multiplicative")
autoplot(forecast(fc1)) 
autoplot(forecast(fc2))

# Plot the data
autoplot(a10)

# Produce 3 year forecasts
fc <- hw(a10, seasonal = "multiplicative", h = 36)

# Check if residuals look like white noise
checkresiduals(fc)

# Plot forecasts
autoplot(fc)

# The Holt-Winters method can also be used for daily type of data, 
# where the seasonal pattern is of length 7, and the appropriate unit of time for h is in days.

# Here, you will compare an additive Holt-Winters method and a seasonal naive() method for 
# the hyndsight data, which contains the daily pageviews on the Hyndsight blog for one year 
# starting April 30, 2014. The data are available in your workspace.

# Create training data with subset()
train <- subset(hyndsight, end = length(hyndsight) - 4*7)

# Holt-Winters additive forecasts as fchw
fchw <- hw(train, seasonal = "additive", h = 4*7)
autoplot(fchw)

# Seasonal naive forecasts as fcsn
fcsn <- snaive(train, h = 4*7)
autoplot(fcsn)

# Find better forecasts with accuracy()
accuracy(fchw, hyndsight)
accuracy(fcsn, hyndsight)

# Plot the better forecasts
autoplot(fchw)

# Automatic forecasting with exponential smoothing ------------------------------------------------------

# The namesake function for finding errors, trend, and seasonality (ETS) provides a completely 
# automatic way of producing forecasts for a wide range of time series.

# You will now test it on two series, austa and hyndsight, that you have previously looked at 
# in this chapter. Both have been pre-loaded into your workspace.

# Fit ETS model to austa in fitaus
plot(austa)
fitaus  <- ets(austa)

# Check residuals
checkresiduals(fitaus)

# Plot forecasts
autoplot(forecast(fitaus))

# Repeat for hyndsight data in fiths
fiths <- ets(hyndsight)
checkresiduals(fiths)
autoplot(forecast(fiths))

# Which model(s) fails test?


# ETS vs seasonal naive ---------------------------------

# Here, you will compare ETS forecasts against seasonal naive forecasting for 20 years of cement, 
# which contains quarterly cement production using time series cross-validation for 4 steps ahead. 
# Because this takes a while to run, a shortened version of the cement series will be available in 
# your workspace.

# Function to return ETS forecasts
fets <- function(y, h) {
  forecast(ets(y), h = h)
}

# Apply tsCV() for both methods
args(tsCV)
e1 <- tsCV(cement, fets, h = 4)
e2 <- tsCV(cement, snaive, h = 4)

# Compute MSE of resulting errors (watch out for missing values)
mean(e1^2, na.rm = TRUE)
mean(e2^2, na.rm = TRUE)

# Copy the best forecast MSE


#****************************
### In-Class Exercise 9 -----
#****************************

# From the series you analyzed for exercise 2, pick one that you want to forecast, 
# and compare the ets to the naive fore cast for that series.. 



# When does ETS fail? -------------------------------------------------------
  
# Computing the ETS does not work well for all series.
# Here, you will observe why it does not work well for the annual Canadian 
# lynx population available in your workspace as lynx.

# Plot the lynx series
autoplot(lynx)

# Use ets() to model the lynx series
fit <- ets(lynx)

# Use summary() to look at model and parameters
summary(fit)

# Plot 20-year forecasts of the lynx series
fit %>% forecast(h = 20) %>% autoplot()

# Transformations for Stabilizing Variance: ----------------------------
# Not necessary for ETS because we can do multiplicative error and seasonality to account for increasing variance, 
# But we will need it for ARIMA and other methods that can't deal well with increasing variance. 

# Box-Cox transformations for time series ------------------------------

# Here, you will use a Box-Cox transformation to stabilize the variance of the pre-loaded a10 series, which contains monthly anti-diabetic drug sales in Australia from 1991-2008.
# In this exercise, you will need to experiment to see the effect of the lambda argument on the transformation. Notice that small changes in make little difference to the resulting series. 
# You want to find a value of lambda that makes the seasonal fluctuations of roughly the same size across the series.
# Recall from the video that the recommended range for lambda values is between -1 and 1.

# Plot the series
autoplot(a10)

# Try four values of lambda in Box-Cox transformations
a10 %>% BoxCox(lambda = 0.0) %>% autoplot()
a10 %>% BoxCox(lambda = 0.1) %>% autoplot()
a10 %>% BoxCox(lambda = 0.2) %>% autoplot()
a10 %>% BoxCox(lambda = 0.3) %>% autoplot()

# Compare with BoxCox.lambda() (automatic lambda selection)
BoxCox.lambda(a10)

a10 %>% BoxCox(lambda = BoxCox.lambda(a10)) %>% autoplot()


#****************************
### In-Class Exercise 10 -----
#****************************

# In the series you chose to forecast for exercise 9 exhibited increasing variance, 
# do a box-cox transformation and find the right lambda to stabilize the variance..


# Non-seasonal differencing for stationarity -------------------------------

# Differencing is a way of making a time series stationary; this means that you 
# remove any systematic patterns such as trend and seasonality from the data. 
# A white noise series is considered a special case of a stationary time series.

# Plot the US female murder rate
autoplot(wmurders)

# Plot the differenced murder rate
autoplot(diff(wmurders))

# Plot the ACF of the differenced murder rate
ggAcf(diff(wmurders))
# -> looks like white noise... 

# Seasonal differencing for stationarity ------------------------------------

# With seasonal data, differences are often taken between observations in the same season 
# of consecutive years, rather than in consecutive periods. For example, with quarterly data, 
# one would take the difference between Q1 in one year and Q1 in the previous year. 
# This is called seasonal differencing.

# Sometimes you need to apply both seasonal differences and lag-1 differences to the same series, 
# thus, calculating the differences in the differences.

# In this exercise, you will use differencing and transformations simultaneously to make a 
# time series look stationary. The data set here is h02, which contains 17 years of monthly 
# corticosteroid drug sales in Australia. It has been loaded into your workspace.

# Plot the data
autoplot(h02)

# Take logs and seasonal differences of h02
difflogh02 <- diff(log(h02), lag = 12)

# Plot difflogh02
autoplot(difflogh02)

# Take another difference and plot
ddifflogh02 <- diff(difflogh02)
autoplot(ddifflogh02)

# Plot ACF of ddifflogh02
ggAcf(ddifflogh02)

# Great! The data doesn't look like white noise after the transformation, but you could develop an ARIMA model for it. 


#****************************
### In-Class Exercise 11 -----
#****************************

# Test your series for seasonality, if it is seasonal and non-stationary apply seasonal differences to teh series. 


# Automatic ARIMA models for non-seasonal time series --------------------------------------------------

# the auto.arima() function will select an appropriate autoregressive integrated moving average (ARIMA) 
# model given a time series, just like the ets() function does for ETS models. 
# The summary() function can provide some additional insights:

# In this exercise, you will automatically choose an ARIMA model for the pre-loaded austa series, 
# which contains the annual number of international visitors to Australia from 1980-2015. 
# You will then check the residuals (recall that a p-value greater than 0.05 indicates that the 
# data resembles white noise) and produce some forecasts. Other than the modelling function, 
# this is identicial to what you did with ETS forecasting.

# Fit an automatic ARIMA model to the austa series
plot(austa)
fit <- auto.arima(austa)

# Check that the residuals look like white noise
checkresiduals(fit)

# Summarize the model
summary(fit)

# Find the AICc value and the number of differences used

# Plot forecasts of fit
fit %>% forecast(h = 10) %>% autoplot()
#  It looks like the ARIMA model created a pretty good forecast for you. 

plot(debitcards)
mod <- auto.arima(debitcards, lambda = 0)
mod %>% forecast(h = 36) %>% autoplot()

mod <- auto.arima(debitcards, lambda = 0, stepwise = FALSE)
mod %>% forecast(h = 36) %>% autoplot()


# Forecasting with ARIMA models ------------------------------------------------------------------------

# The automatic method in the previous exercise chose an ARIMA(0,1,1) with drift model for the austa data, 
# You will now experiment with various other ARIMA models for the data to see what difference it makes to the forecasts.

# The Arima() function can be used to select a specific ARIMA model. Its first argument, order, is set to a vector that 
# specifies the values of p, d and q. The second argument, include.constant, is a booolean that determines if the constant c, 
# or drift, should be included.

# In the examples here, watch for how the different models affect the forecasts and the prediction intervals

# Plot forecasts from an ARIMA(0,1,1) model with no drift
austa %>% Arima(order = c(0, 1, 1), include.constant = FALSE) %>% forecast() %>% autoplot()

# Plot forecasts from an ARIMA(2,1,3) model with drift
austa %>% Arima(order = c(2, 1, 3), include.constant = TRUE) %>% forecast() %>% autoplot()

# Plot forecasts from an ARIMA(0,0,1) model with a constant
austa %>% Arima(order = c(0, 0, 1), include.constant = TRUE) %>% forecast() %>% autoplot()

# Plot forecasts from an ARIMA(0,2,1) model with no constant
austa %>% Arima(order = c(0, 2, 1), include.constant = FALSE) %>% forecast() %>% autoplot()

# -> The model specification makes a big impact on the forecast! 

# Checking different models for 'austa' after visual inspection: 
?austa
plot(austa)
pp.test(austa)
kpss.test(austa, null = "Trend")
plot(diff(austa))
acf(austa)
pacf(austa)

austa %>% Arima(order = c(1, 1, 0), include.constant = TRUE) %>% forecast() %>% autoplot()

austa %>% auto.arima(stepwise = FALSE) %>% forecast() %>% autoplot()

austa %>% Arima(order = c(1, 1, 0), include.constant = TRUE) %>% BIC
austa %>% Arima(order = c(1, 1, 1), include.constant = TRUE) %>% BIC
austa %>% Arima(order = c(0, 1, 1), include.constant = TRUE) %>% BIC

mod <- austa %>% auto.arima(stepwise = FALSE)
checkresiduals(mod)
Acf(resid(mod))
Pacf(resid(mod))

fcmod <- function(x, h) {
  forecast(Arima(x, order = c(0, 1, 1), include.constant = TRUE), h = h)
}

austa %>% Arima(order = c(1, 1, 0), include.constant = TRUE)

fcmod2 <- function(x, h) {
  forecast(Arima(x, order = c(1, 1, 0), include.constant = TRUE), h = h)
}

fmean(abs(tsCV(austa, fcmod, h = 2)))
fmean(abs(tsCV(austa, fcmod2, h = 2)))



# We can find the Optimal ARIMA model for out data by applying the so called Box-Jenkins Methodology:

# 1. Plot: Look for tends, outliers, missing, structural breaks

# 2. Test for Stationarity, if non-stationary -> Difference or remove trend

# 3. Compute ACF and PACF: Select first p and q to try

# 4. Fit ARMA(p,q) and compute AIC and BIC
# information criteria (penalized goodness of fit measures):
#  AIC = T ln(SSR) + 2k
#  BIC = T ln(SSR) + k ln(T)
# In R functions AIC() and BIC()
# Lower IC means better model. Note: IC's increasin in T ! 
# T must be held constant since adding lags
# makes T and therefore the IC's smaller. SBIC has better
# large sample properties, AIC is inconsistent and favors overparameterized models.

# 5. Long time for convergence indicates specification problem.

# 6. Look if cofficeints AR and MA are significant (all should be
# significant). There should be no unit roots (coefficients close to 1), especially in the MA part.

# 7. Diagnostic Checking:
# - Plot residuals, look for outliers & periods of poor fit
# - Residuals should be serially uncorrelated (white noise: Necessary cond.)! correlogram and white noise test.

# 8. Evaluate Forecasting Properties (MSE, and do
# Diebold-Mariano test for significantly different forecasting performance (forecast::dm.test)).

# 9. Fit model for different time-slices. A good model should
# perform well on all parts of the series.

# Requirements for Box-Jenkins: Good forecasts generally require at
# least 50 observations (more with seasonality). Best for short term
# forecasts. Data must be detrended!. Note: Very different models
# can provide identical forecasts.


#*****************************
### In-Class Exercise 12 -----
#*****************************

# Find an optimal ARIMA forecasting model for your series applying the Box-Jenkins methodology
# Then use auto.arima and compare to your model to this one (in terms of forecasting performance)



# Comparing auto.arima() and ets() on non-seasonal data -------------------------------------------------

# The AICc statistic is useful for selecting between models in the same class. For example, you can use it to select an ETS model or to select an ARIMA model. However, you cannot use it to compare ETS and ARIMA models because they are in different model classes.
# Instead, you can use time series cross-validation to compare an ARIMA model and an ETS model on the austa data. Because tsCV() requires functions that return forecast objects, you will set up some simple functions that fit the models and return the forecasts.

# Set up forecast functions for ETS and ARIMA models
fets <- function(x, h) {
  forecast(ets(x), h = h)
}
farima <- function(x, h) {
  forecast(auto.arima(x), h = h)
}

# Compute CV errors for ETS on austa as e1
e1 <- tsCV(austa, fets, h = 1)

# Compute CV errors for ARIMA on austa as e2
e2 <- tsCV(austa, farima, h = 1)

# Find MSE of each model class
mean(e1^2, na.rm = TRUE)
mean(e2^2, na.rm = TRUE)

# Plot 10-year forecasts using the best model class
austa %>% farima(h = 10) %>% autoplot()


# Automatic ARIMA models for seasonal time series -----------------------------------------------------------

# the auto.arima() function also works with seasonal data. Note that setting lambda = 0 in the auto.arima() function 
# - applying a log transformation - means that the model will be fitted to the transformed data, and that the forecasts will be back-transformed onto the original scale.

# Check that the logged h02 data have stable variance
h02 %>% log() %>% autoplot()

# Fit a seasonal ARIMA model to h02 with lambda = 0
fit <- auto.arima(h02, lambda = 0)

# Summarize the fitted model
summary(fit)

# Record the amount of lag-1 differencing and seasonal differencing used
d <- 1
D <- 1

# Plot 2-year forecasts
fit %>% forecast(h = 24) %>% autoplot()

# Exploring auto.arima() options ------------------------------------------------------------------------------

# The auto.arima() function needs to estimate a lot of different models, 
# and various short-cuts are used to try to make the function as fast as possible. 
# This can cause a model to be returned which does not actually have the smallest AICc value. 
# To make auto.arima() work harder to find a good model, add the optional argument stepwise = FALSE to 
# look at a much larger collection of models.

# Here, you will try finding an ARIMA model for the pre-loaded euretail data, which contains quarterly retail trade 
# in the Euro area from 1996-2011. Inspect it in the console before beginning this exercise.

# Find an ARIMA model for euretail
fit1 <- auto.arima(euretail)
summary(fit1)
# Don't use a stepwise search
fit2 <- auto.arima(euretail, stepwise = FALSE)
summary(fit2)
# AICc of better model
AICc <- 68.39

# Compute 2-year forecasts from better model
fit2 %>% forecast(h = 8) %>% autoplot()

# Great! auto.arima() has a wealth of options that are worth exploring..

#*****************************
### In-Class Exercise 13 -----
#*****************************

# Find a model with auto.arima to forecast your series with option stepwise = FALSE


# Comparing auto.arima() and ets() on seasonal data ------------------------------------------------------------

# What happens when you want to create training and test sets for data that is more frequent than yearly? 
# If needed, you can use a vector in form c(year, period) for the start and/or end keywords in the window() function. 
# You must also ensure that you're using the appropriate values of h in forecasting functions. 
# Recall that h should be equal to the length of the data that makes up your test set.

# In the final exercise for this chapter, you will compare seasonal ARIMA and ETS models applied to the quarterly cement production data qcement. 
# Because the series is very long, you can afford to use a training and test set rather than time series cross-validation. This is much faster.

# Use 20 years of the qcement data beginning in 1988
train <- window(qcement, start = 1988, end = c(2007, 4))

# Fit an ARIMA and an ETS model to the training data
fit1 <- auto.arima(train)
fit2 <- ets(train)

# Check that both models have white noise residuals
checkresiduals(fit1)
checkresiduals(fit2)

# Produce forecasts for each model
h <- length(window(qcement, start = 1988)) - length(train)
fc1 <- forecast(fit1, h = h)
fc2 <- forecast(fit2, h = h)

# Use accuracy() to find better model based on RMSE
accuracy(fc1, qcement)
accuracy(fc2, qcement)

# Looks like the ETS model did better here. 




# (5) Forecasting with Covariates (DLM, ARIMAX, ECM) + Harmonic Regression ----
#******************************************************************************


# Dynamic Regression -------------------------------------------------------------------------------------------------

# The auto.arima() function will fit a dynamic regression model with ARIMA errors. 
# The only change to how you used it previously is that you will now use the xreg argument containing 
# a matrix of regression variables. 

# In this exercise, you will model sales data regressed against advertising expenditure, 
# with an ARMA error to account for any serial correlation in the regression errors. 
# The data are available in your workspace as advert and comprise 24 months of sales and 
# advertising expenditure for an automotive parts company.

# Time plot of both variables
autoplot(advert, facets = TRUE)

# Fit ARIMA model
fit <- auto.arima(advert[, "sales"], xreg = advert[, "advert"], stationary = TRUE)

# Check model. Increase in sales for each unit increase in advertising
salesincrease <- coef(fit)[3]

# Forecast fit as fc
fc <- forecast(fit, xreg = rep(10, 6))

# Plot fc with x and y labels
autoplot(fc) + xlab("Month") + ylab("Sales")

# Forecasting electricity demand ----------------------

# You can also model daily electricity demand as a function of temperature. 
# As you may have seen on your electric bill, more electricity is used on hot days 
# due to air conditioning and on cold days due to heating.

# In this exercise, you will fit a quadratic regression model with an ARMA error. 
# One year of daily data are stored as elec including total daily demand, an indicator variable for workdays 
# (a workday is represented with 1, and a non-workday is represented with 0), and daily maximum temperatures. 
# Because there is weekly seasonality, the frequency has been set to 7.

plot(unclass(elecdaily[, 1:2]))

# Time plots of demand and temperatures
autoplot(elecdaily[, c(1, 2)], facets = TRUE)

# Matrix of regressors
xreg <- cbind(MaxTemp = elecdaily[, "Temperature"], 
              MaxTempSq = elecdaily[, "Temperature"]^2, 
              Workday = elecdaily[, "WorkDay"])

# Fit model
fit <- auto.arima(elecdaily[, "Demand"], xreg = xreg)

# Forecast fit one day ahead
plot(forecast(fit, xreg = cbind(MaxTemp = 20, 
                                MaxTempSq = 20^2,
                                Workday = 1)))

# Now you've seen how multiple independent variables can be included using matrices. 


#*****************************
### In-Class Exercise 14 -----
#*****************************

# Fit a suitable ARIMAX model to your time series.


# Dynamic Harmonic regression: Predictors + Fourier terms to deal with seasonality + ARIMA errors -------------------------------------------------
# Fourier terms assume seasonal pattern does not change over time, whereas SARIMA allows seasonality to evolve.
# The benefit of fourier terms is that they can deal with very complex seasonality in weekly or daily data. 
# Need to chose number of sines an cosines (harmonic terms) K by minimizing the AICc.

# Forecasting weekly data --------------------------------------------------------

# With weekly data, it is difficult to handle seasonality using ETS or ARIMA models as the seasonal length is too large (approximately 52). 
# Instead, you can use harmonic regression which uses sines and cosines to model the seasonality.

# The fourier() function makes it easy to generate the required harmonics. 
# The higher the order (K), the more "wiggly" the seasonal pattern is allowed to be. With K = 1, 
# it is a simple sine curve. You can select the value of K by minimizing the AICc value. 
# fourier() takes in a required time series, required number of Fourier terms to generate, 
# and optional number of rows it needs to forecast: fourier(x, K, h = NULL)

# The 'gasoline' data comprises weekly data on US finished motor gasoline products. 
# In this exercise, you will fit a harmonic regression to this data set and forecast the next 3 years.

# Set up harmonic regressors of order 13 (chosen to minimize AICc)
harmonics <- fourier(gasoline, K = 13)

# Fit regression model with ARIMA errors
fit <- auto.arima(gasoline, xreg = harmonics, seasonal = FALSE)

# Forecasts next 3 years
newharmonics <- fourier(gasoline, K = 13, h = 52 * 3)
fc <- forecast(fit, xreg = newharmonics)

# Plot forecasts fc
autoplot(fc)

# Great. The point predictions look to be a bit low.



# Harmonic regression for multiple seasonality -----------------------------------------------------

# Harmonic regressions are also useful when time series have multiple seasonal patterns. 
# For example, 'taylor' contains half-hourly electricity demand in England and Wales over a few months in the year 2000. 
# The seasonal periods are 48 (daily seasonality) and 7 x 48 = 336 (weekly seasonality). 
# There is not enough data to consider annual seasonality.

# auto.arima() would take a long time to fit a long time series such as this one, so instead you will fit a 
# standard regression model with Fourier terms using the tslm() function. 
# This is very similar to lm() but is designed to handle time series. 
# With multiple seasonality, you need to specify the order for each of the seasonal periods.

# Fit a harmonic regression using order 10 for each type of seasonality
fit <- tslm(taylor ~ fourier(taylor, K = c(10, 10)))

# Forecast 20 working days ahead
fc <- forecast(fit, newdata = data.frame(fourier(taylor, K = c(10, 10), h = 20 * 48)))

# Plot the forecasts
autoplot(fc)

# Check the residuals of fit
checkresiduals(fit)
# As you can see, auto.arima() would have done a better job...
# The residuals from the fitted model fail the tests badly, yet the forecasts are quite good. 

# Forecasting call bookings -----------------------------------------------

# Another time series with multiple seasonal periods is calls, 
# which contains 20 consecutive days of 5-minute call volume data for a large North American bank. 
# There are 169 5-minute periods in a working day, and so the weekly seasonal frequency is 5 x 169 = 845. 
# The weekly seasonality is relatively weak, so here you will just model daily seasonality. 
# calls is pre-loaded into your workspace.

# The residuals in this case still fail the white noise tests, but their autocorrelations are tiny, 
# even though they are significant. This is because the series is so long. 
# It is often unrealistic to have residuals that pass the tests for such long series. 
# The effect of the remaining correlations on the forecasts will be negligible.

# Plot the calls data
autoplot(calls)

# Set up the xreg matrix
xreg <- fourier(calls, K = c(10, 0))

# Fit a dynamic regression model
fit <- auto.arima(calls, xreg = xreg, seasonal = FALSE, stationary = TRUE)

# Check the residuals
checkresiduals(fit)

# Plot forecasts for 10 working days ahead
fc <- forecast(fit, xreg = fourier(calls, c(10, 0), h = 169 * 10))
autoplot(fc)

# Great! Now you've gotten a lot of experience using complex forecasting techniques.

# TBATS Models -------------------------------------------------------------------------------
# Very general automated forecasting procedure, but can be slow and select wrong model.

# TBATS model is a special kind of time series model. It can be very slow to estimate, especially with multiple seasonal time series, so in this exercise you will try it on a simpler series to save time.

# Let's break down elements of a TBATS model in TBATS(1, {0,0}, -, {<51.18,14>}):
# 1 	Box-Cox transformation parameter
# {0,0} 	ARMA error
# - 	Damping parameter
# {\<51.18,14>} 	Seasonal period, Fourier terms (seasonal pattern can change over time)

# The gas data contains Australian monthly gas production. A plot of the data shows the variance has changed a lot over time, 
# so it needs a transformation. The seasonality has also changed shape over time, and there is a strong trend. 
# This makes it an ideal series to test the tbats() function which is designed to handle these features.

# Plot the gas data
autoplot(gas)

# Fit a TBATS model to the gas data
fit <- tbats(gas)

# Forecast the series for the next 5 years
fc <- forecast(fit, h = 5 * 12)

# Plot the forecasts
autoplot(fc)

# Record the Box-Cox parameter and the order of the Fourier terms
lambda <- 0.082
K <- 5
# Amazing! Just remember that completely automated solutions don't work every time. 


#*****************************
### Final Exercise 14 -----
#*****************************

# Fit a TBATS model to your time series.




# Books and Further Resources: ---------------------

# Forecasting Principles and Practice 2: https://otexts.com/fpp2/
# Forecasting Principles and Practice 3: https://otexts.com/fpp3/ (updated content, different packages, but mostly same materials)... 
# Time Series Analysis and Its Applications - With R Examples: https://www.stat.pitt.edu/stoffer/tsa4/

# See also Econometrics Academy on ARIMA models, and other resources suggested for the 
# linear modelling course which have sections on time-series analysis and forecasting. 

# Further Packages
asta
fable
autoTS
tsSelect 
tsensemble














  












#****************************
### In-Class Exercise 1 -----
#****************************


# Putting Everything Together
# 1. Check Stationarity: If non-stationary: Difference and test
# again, until stationary. (Or just remove trend if trend-stationary)

# 2. Taking log-transformation curbs variability (variance) and
# heteroskedasticity. It smoothes the time-series. Often FD in
# logs works well. Alternatively can find obtimal lambda for box-cox transformation

# 2. Apply Box-Jenkins Methodology to find an ARIMA Model
# Economic time-series are mostly I(0) or I(1). Workhorse
# models are ARMA(1,1) and ARIMA(1,1,1).



# Forecasting: Principles and Practice in R, Rob Hyndman: https://otexts.com/fpp2/bootstrap.html

