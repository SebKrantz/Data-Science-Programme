############################################
# Data Science Training
# Solution to the In-Class Exercises of
# Course 4 Session 1
############################################

setwd("Course 4 - Time Series Analysis and Forecasting in R")

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
for(i in 3:n) z[i] = 0.5 * z[i-1L] + 0.4 * z[i-2L] + e[i]

# (b) examine your process by plotting it, computing the ACF and PACF
ts.plot(z)
acf(z)
pacf(z) # See the second spike.. series is AR(2)

# (c) add a MA(1) term to the AR(2), generating a so-called ARMA(2, 1) process
for(i in 3:n) z[i] = 0.5 * z[i-1L] + 0.4 * z[i-2L] + e[i] + 0.7 * e[i-1]

# (d) also examine this process using plot, ACF and PACF
ts.plot(z)
acf(z)  # Decay
pacf(z) # Also Decay (Oscillating)
# -> Series is ARMA

# (2) Examine there series, are they Stationary? Autocorrelated? AR, MA, ARMA?
plot(Nile) # Looks non-stationary
acf(Nile)
pacf(Nile) # One significant spike, so series is AR(1)

plot(BJsales) # Clearly non-stationary
acf(BJsales)  # Autocorrelation coefficient 1 close to 1, so could be a randowm walk
pacf(BJsales) # One significant spike, so series is AR(1)

plot(sunspots) # Also non-stationary
acf(sunspots)
pacf(sunspots) # Decay -> ARMA process

plot(UKgas) # Non stationary and some seasonality
acf(UKgas)  # Seasonal pattern is also visible in the ACF
pacf(UKgas) # Some significant seasonal lags -> ARMA
plot(UKgas)
# Why does the ACF reflect the seasonal pattern?
plot(UKgas)
lines(L(UKgas), col = "red")
lines(L(UKgas, 2), col = "green")
lines(L(UKgas, 4), col = "blue")
# Seasonal differencing gives simpler ACF and PACF
acf(diff(UKgas, 4)) 
pacf(diff(UKgas, 4)) # Significant seasonal lags -> still ARMA


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

# Examples:

# M0, M2, I_LR, CPI/Inflation, CIEA and CBR (Monetary Policy Transmission)
library(ugatsdb)
View(datasets())
View(series("BOU_MMI"))
MP <- get_data("BOU_MMI", series = c("I_CBR", "I_LR", "CPI_HL_09", "CPI_C_09", "M0", "M2", "CIEA"), 
               expand.date = TRUE)
library(xts)
library(collapse)
MP_xts <- as.xts(MP)

mvars <- .c(I_CBR, M0, M2, I_LR, CPI_HL_09, CIEA)
plot(na.omit(MP_xts[, mvars]), multi.panel = TRUE, yaxis.same = FALSE)
plot(STD(na.omit(MP_xts[, mvars])), legend.loc = "topleft")
plot(STD(D(na.omit(MP_xts[, mvars]))), legend.loc = "topleft")
pwcor(D(MP_xts[, mvars]))
acf(Dlog(MP_xts[, mvars]), na.action = na.omit, plot = FALSE)

M0_mod <- lm(Dlog(M0) ~ L(Dlog(M0), 1:2) + L(Dlog(I_CBR), 0:6) + Month, MP)
summary(M0_mod)
acf(resid(M0_mod))

M2_mod <- lm(Dlog(M2) ~ L(Dlog(M2), 1:2) + L(Dlog(I_CBR), 0:6) + Month, MP)
summary(M2_mod)
acf(resid(M2_mod))

LR_mod <- lm(Dlog(I_LR) ~ L(Dlog(I_LR), 1:2) + L(Dlog(I_CBR), 0:6) + Month, MP)
summary(LR_mod)
acf(resid(LR_mod))

CPI_mod <- lm(Dlog(CPI_HL_09) ~ L(Dlog(I_CBR), 0:6) + Month, MP)
summary(CPI_mod)
acf(resid(CPI_mod))

CPI_mod2 <- lm(Dlog(CPI_HL_09) ~ L(Dlog(CPI_HL_09), 1:2) + L(Dlog(I_LR), 0:6) + Month, MP)
summary(CPI_mod2)
acf(resid(CPI_mod2))

CPI_mod3 <- lm(Dlog(CPI_HL_09) ~ L(Dlog(CPI_HL_09), 1:2) + L(Dlog(M2), 0:6) + Month, MP)
summary(CPI_mod3)
acf(resid(CPI_mod3))

EA_mod <- lm(Dlog(CIEA) ~ L(Dlog(CIEA), 1:3) + L(Dlog(I_CBR), 0:6) + Month, MP)
summary(EA_mod)
acf(resid(EA_mod))

EA_mod2 <- lm(Dlog(CIEA) ~ L(Dlog(CIEA), 1:3) + L(Dlog(I_LR), 0:6) + Month, MP)
summary(EA_mod2)
acf(resid(EA_mod2))


jtools::plot_coefs(TB_mod, omit.coefs = grep("Intercept|Month", names(coef(TB_mod)), value = TRUE)) 


# REER, Imports and Trade Tax Revenue (Real exchange rate, imports and trade tax revenue)
plot(na.omit(MMI_xts[, .c(CIEA, REER, TB)]), multi.panel = TRUE, yaxis.same = FALSE)
plot(STD(na.omit(MMI_xts[, .c(REER, TB)])), legend.loc = "topleft")
plot(STD(D(na.omit(MMI_xts[, .c(REER, TB)]))), legend.loc = "topleft")
pwcor(D(MMI_xts[, .c(REER, TB)]))
acf(D(MMI_xts[, .c(CIEA, REER, TB)]), na.action = na.omit)

settransform(MMI, TB_inv = -TB)

TB_mod <- lm(Dlog(TB_inv) ~ L(Dlog(TB_inv), 1:2) + L(Dlog(CIEA), 0:2) + L(Dlog(REER), 0:6) + Month, MMI)
jtools::summ(TB_mod)
jtools::plot_coefs(TB_mod, omit.coefs = grep("Intercept|Month", names(coef(TB_mod)), value = TRUE)) 
acf(resid(TB_mod))

# NCG, TBILL, I_TBY_91, PSC, PMI and Government Expenditure (Government financing and expenditure)
# FIN_DOM from MOF_TOT, EXP from MOF_EXP
GE <- get_data(c("BOU_MMI", "MOF_POE", "MOF_TOT", "MOF_EXP"), 
               series = c("NCG", "TBILL", "I_TBY_91", "PSC", "PMI", "FIN_DOM", "EXP"))

library(collapse)
pwcor(G(num_vars(GE)))


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

library(forecast)
CPI_ts <- na.omit(ts(MP$CPI_HL_09, start = c(1986, 1), frequency = 12))
plot(CPI_ts)
monthplot(CPI_ts)
ggseasonplot(CPI_ts)

summary(lm(Dlog(CPI_ts) ~ factor(cycle(CPI_ts))))
fFtest(Dlog(CPI_ts), factor(cycle(CPI_ts)))
seasdum(CPI_ts, autoarima = TRUE)
isSeasonal(CPI_ts)

CPI_stl <- stl(CPI_ts, "periodic")
plot(CPI_stl)
CPI_SA_stl <- seasadj(CPI_stl)
plot(CPI_SA_stl)
isSeasonal(CPI_SA_stl)

CPI_seas <- seas(CPI_ts)
plot(CPI_seas)
CPI_SA_seas <- seasadj(CPI_seas)
isSeasonal(CPI_SA_seas)

plot(cbind(CPI_SA_stl, CPI_SA_seas))
ts.plot(cbind(CPI_SA_stl, CPI_SA_seas), col = c("red", "blue"))
legend("topleft", c("STL", "X-13"), col = c("red", "blue"), lty = 1)


#****************************
### In-Class Exercise 4 -----
#****************************

# Check your series from excercise 2 for a unit root, 
# If you have more than 2 series with a unit root, check if they are cointegrated
# If they are cointegrated, fit an error correction model. 

plot(na.omit(MMI_xts[, "CPI_HL_09"]))
adf.test(na.omit(MMI_xts[, "CPI_HL_09"]))
pp.test(na.omit(MMI_xts[, "CPI_HL_09"]))
kpss.test(na.omit(MMI_xts[, "CPI_HL_09"]), null = "Trend")

plot(na.omit(MMI_xts[, c("CPI_HL_09", "CPI_C_09")]))
pp.test(na.omit(MMI_xts[, "CPI_C_09"]))

pp.test(resid(lm(CPI_HL_09 ~ CPI_C_09, MMI[, c("CPI_HL_09", "CPI_C_09")])))
po.test(MMI_xts[, c("CPI_HL_09", "CPI_C_09")])


#****************************
### In-Class Exercise 5 -----
#****************************

# Check your model from Exercise 2


#****************************
### In-Class Exercise 9 -----
#****************************

# From the series you analyzed for exercise 2, pick one that you want to forecast, 
# and compare the ets to the naive fore cast for that series.. 
plot(CPI_ts)
plot(forecast(ets(CPI_ts)))
plot(forecast(ets(CPI_SA_seas, damped = FALSE)))

plot(forecast(ets(GDP_ts)))

#****************************
### In-Class Exercise 10 -----
#****************************

# In the series you chose to forecast for exercise 9 exhibited increasing variance, 
# do a box-cox transformation and find the right lambda to stabilize the variance..
autoplot(GDP_ts)
BoxCox.lambda(GDP_ts)
GDP_ts %>% BoxCox(lambda = BoxCox.lambda(GDP_ts)) %>% autoplot()


#*****************************
### In-Class Exercise 12 -----
#*****************************

# Find an optimal ARIMA forecasting model for your series applying the Box-Jenkins methodology
# Then use auto.arima and compare to your model to this one (in terms of forecasting performance)

plot(CIEA_ts)
adf.test(CIEA_ts)
pp.test(CIEA_ts)
kpss.test(CIEA_ts, null = "Trend")
Acf(CIEA_ts)
Pacf(CIEA_ts)
seastests::isSeasonal(CIEA_ts)

mod <- Arima(CIEA_ts, order = c(1, 0, 0), include.drift = TRUE)
plot(forecast(mod))

mod <- auto.arima(CIEA_ts)
plot(forecast(mod))

REER_ts <- na.omit(ts(MMI$REER, start = c(1986, 1), frequency = 12))
plot(REER_ts)
pp.test(REER_ts)
kpss.test(REER_ts, null = "Trend")
plot(diff(REER_ts))
D_REER_ts <- diff(REER_ts)
seastests::isSeasonal(D_REER_ts)
Acf(D_REER_ts)
Pacf(D_REER_ts)

REER_mod <- Arima(REER_ts, order = c(0, 0, 1), seasonal = c(0, 1, 1))
autoplot(forecast(REER_mod, h = 24))

REER_mod <- auto.arima(REER_ts, stepwise = FALSE)
autoplot(forecast(REER_mod, h = 24))

fcmod <- function(x, h) {
  forecast(Arima(x, order = c(0, 0, 1), seasonal = c(0, 1, 1)), h = h)
}

fcmod2 <- function(x, h) {
  forecast(Arima(x, order = c(1, 1, 0), seasonal = c(2, 0, 0)), h = h)
}

e <- tsCV(REER_ts, fcmod, h = 12, initial = 300)
e2 <- tsCV(REER_ts, fcmod2, h = 12, initial = 300)

fmean(abs(e))
fmean(abs(e2))

