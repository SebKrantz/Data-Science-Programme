library(readxl)     # Import from Excel
library(collapse)   # Data transformation and time series operators
library(magrittr)   # Pipe operators %>%, %$% 
library(tseries)    # Time series tests
library(lmtest)     # Linear model tests
library(sandwich)   # Robust standard errors
library(jtools)     # Enhanced regression summary
library(xts)        # Extensible time series + pretty plots


# Function to compute a correlogram of a time series
corrgram <- function(x, lag.max = NULL, plot = FALSE,
                     na.action = function(x) if(anyNA(unclass(x))) 
                       x[!is.na(x)] else x) {
  if(plot) {
    oldpar <- par(mfrow = c(1L, 2L))
    on.exit(par(oldpar))
  }
  ac <- eval(substitute(acf(x, lag.max, plot = plot, na.action = na.action)))
  acf <- drop(ac[[1L]])[-1L]
  pacf <- drop(eval(substitute(pacf(x, lag.max, plot = plot, 
                                    na.action = na.action)))[[1L]])
  n <- ac$n.used
  qstat <- n * (n + 2) * cumsum(acf^2 / seq.int(n - 1, n - length(acf))) 
  lags <- seq_along(acf)
  res <- cbind(Lag = lags, AC = acf, PAC = pacf,
               Q = qstat, `Pr(>Q)` = pchisq(qstat, lags, lower.tail = FALSE))
  class(res) <- "corrgram"
  res
}

# Print method
print.corrgram <- function(x, digits = 3, ...) {
  xx <- cbind(format(x[, 1L, drop = FALSE], drop0trailing = TRUE),
              format(round(x[, -1L], digits)))
  print.default(`rownames<-`(xx, rep(" ", nrow(xx))), quote = FALSE, right = TRUE)
}

# Function to forecast n periods in-sample for forecast evaluation
forecast_is <- function(model, data, n = 1L) {
  data <- qDF(na_omit(data))
  N <- nrow(data)
  s <- (N-n):(N-1L)
  form <- terms(model)
  y <- attr(form, "variables")[1:2]
  # Dependent variable and forecast are returned as in the model (e.g. differenced)
  ydat <- qDF(eval(y, data, parent.frame()))
  names(ydat) <- deparse(y[[2L]])
  # Forecasting with expanding window
  fc <- numeric(0L)
  for(i in s) {
    # This re-estimates the model on the selected sample
    reest <- lm(form, ss(data, seq_len(i)))
    # This forecasts one period ahead
    fc <- c(fc, flast(predict(reest, newdata = ss(data, seq_len(i+1L))))) 
  }
  ydat[s + 1L, "fc"] <- unattrib(fc)
  return(ydat)
}

eval_forecasts <- function(y, fc, add.naive = TRUE, n.ahead = 1) {
  # eval substitute to get the name of the forecast if only a vector is passed
  mfc <- eval(substitute(qDF(fc))) 
  lagy <- flag(y, n.ahead)
  if (add.naive) mfc <- c(list(Naive = lagy), mfc)
  if (!all(length(y) == lengths(mfc))) 
    stop("All supplied quantities must be of equal length")
  res <- vapply(mfc, function(fcy) {
    # Preparation
    cc <- complete.cases(y, fcy)
    y <- y[cc]
    fcy <- fcy[cc]
    lagycc <- lagy[cc]
    n <- sum(cc)
    # Undo Bessel's correction: (n-1) instead of n in denominator
    nobessel <- sqrt((n - 1) / n) 
    sdy <- sd(y) * nobessel
    sdfcy <- sd(fcy) * nobessel
    diff <- fcy - y
    # Calculate Measures
    bias <- sum(diff) / n          # Bias
    MSE <- sum(diff^2) / n         # Mean Squared Error
    BP <- bias^2 / MSE             # Bias Proportion
    VP <- (sdy - sdfcy)^2 / MSE    # Variance Proportion
    CP <- 1 - BP - VP              # Covariance Proportion
    # CP <- 2 * (1 - cor(y, fcy)) * sdy * sdfcy / MSE
    RMSE <- sqrt(MSE)              # Root MSE
    R2 <- 1 - MSE / sdy^2          # R-Squared
    SE <- sd(diff) * nobessel      # Standard Forecast Error
    MAE <- sum(abs(diff)) / n      # Mean Absolute Error
    MPE <- sum(diff / y) / n * 100 # Mean Percentage Error
    MAPE <- sum(abs(diff / y)) / n * 100 # Mean Absolute Percentage Error
    U1 <- RMSE / (sqrt(sum(y^2) / n) + sqrt(sum(fcy^2) / n))   # Theils U1
    U2 <- sqrt(fmean((diff/lagycc)^2) / fmean((y/lagycc-1)^2)) # Theils U2 
    # Output
    return(c(Bias = bias, MSE = MSE, RMSE = RMSE, `R-Squared` = R2, SE = SE,
             MAE = MAE, MPE = MPE, MAPE = MAPE, U1 = U1, U2 = U2,
             `Bias Prop.` = BP, `Var. Prop.` = VP, `Cov. Prop.` = CP))
  }, numeric(13))
  attr(res, "naive.added") <- add.naive
  attr(res, "n.ahead") <- n.ahead
  attr(res, "call") <- match.call()
  class(res) <- "eval_forecasts"
  return(res)
}

# Print method
print.eval_forecasts <- function(x, digits = 3, ...) print.table(round(x, digits))

# Function to forecast dynamically from a dynamic model (with forecasted predictors)
forecast_dyn <- function(mod, data, y.diff = TRUE) {
  miss <- is.na(data)
  if(sum(colSums(miss) > 0L) > 1L) stop("Missing values should only be on the dependent variable")
  if(any(miss[1L, ])) stop("Missing values at beginning of sample")
  data[miss] <- 0
  # If the dependent variable is first-differenced in the model ... 
  if(y.diff) { # .. results at original scale
    for(i in which(rowSums(miss) > 0)) {
      last <- data[i-1L, miss[i, ]]
      data[i, miss[i, ]] <- last + flast(predict(mod, newdata = ss(data, seq_len(i))))
    }
  } else {
    for(i in which(rowSums(miss) > 0)) 
      data[i, miss[i, ]] <- flast(predict(mod, newdata = ss(data, seq_len(i))))
  }
  data
}