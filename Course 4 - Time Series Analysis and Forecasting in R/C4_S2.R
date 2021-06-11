#############################################################
# *************** R BASED DATA SCIENCE TRAINING *************
# Course 4: Time Series Analysis and Forecasting with R
# -----------------------------------------------------------
# Prepared by: Sebastian Krantz, ODI Fellow, MEPD, MoFPED
# Permanent Contact: sebastian.krantz@graduateinstitute.ch
#############################################################


# Today:
# (7) Automatic Forecast Method Selection
# (8) Non-Linear Methods (Neural Networks, Boosted Trees)
# (9) Forecasting with a large number of series (Ridge regression, Partial least squares)


# (7) Automatic Forecast Method Selection
library(autoTS)
ugatsdb_reconnect()
CPI <- ugatsdb::get_data("BOU_MMI", "CPI_HL_09")
best.algo <- getBestModel(CPI$Date, CPI$CPI_HL_09, "month", n_test = 48, metric.error = my.mae)
names(best.algo)
print(paste("The best algorithm is", best.algo$best))
best.algo$train.errors
final.pred <- my.predictions(bestmod = best.algo)

library(tsSelect)
CPI_ts <- ts(CPI$CPI_HL_09, start = c(2005, 7), frequency = 12)
bm <- run_models(CPI_ts, accuracy_measure = "MAE")

# (8) Non-Linear Methods (Neural Networks, Boosted Trees)
library(forecast)
nnetar_mod <- nnetar(CPI_ts)
plot(forecast(nnetar_mod, h = 10))

library(nnfor)
elm_mod <- elm(CPI_ts)
plot(elm_mod)
plot(forecast(elm_mod, h = 10))

mlp_mod <- mlp(CPI_ts)
plot(mlp_mod)
plot(forecast(mlp_mod, h = 10))

library(forecastxgb)
mod <- xgbar(CPI_ts, seas_method = "decompose")
plot(forecast(mod, h = 10))

library(iForecast)
ttsCPI <- ttsCaret(as.xts(CPI), method = c("svm","rf","rpart","gamboost","BstLm","bstSm","blackboost"),
                   train.end = "2018-01-01", type = "both")
P2 <- iForecast(Model=ttsCPI,newdata=CPI,type="recursive")


CPI_LSTM <- ttsLSTM(as.xts(CPI), train.end = "2018-01-01", type = "both")



# (9) Forecasting with a large number of series (Ridge regression, Partial least squares) 


library(glmnet) # Ridge Regression
library(pls)    # Partial Least Squares

