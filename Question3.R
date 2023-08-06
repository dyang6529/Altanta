library(Rmpfr)
library(forecast)

rm(list=ls())

source("genPiAppxDigits.R")
# i) ---------------------------------------------------------------------

n = 1050
pie <- genPiAppxDigits(n, 10000)

# convert to time series
ts_data <- ts(pie[1:1000,])

# Fit an AR(5) model
ar_model <- Arima(ts_data, order=c(5,0,0)) # Here we are using an AR(5) model

# Forecast the next 50 numbers
forecast_results <- forecast(ar_model, h=n-1000)

# Print the forecasted values
print(forecast_results$mean)
pred <- round(forecast_results$mean)
print(pred)

count <- sum(pred == pie[1001:n,])
print(count)


# ii) ---------------------------------------------------------------------
# Fix numdigits
pie1000 <- genPiAppxDigits(1000, 1000)
pie5000 <- genPiAppxDigits(1000, 5000)
pie10000 <- genPiAppxDigits(1000, 10000)
pie50000 <- genPiAppxDigits(1000, 50000)
pie100000 <- genPiAppxDigits(1000, 100000)
data_pie <- cbind(pie1000, pie5000, pie10000, pie50000, pie100000)
colnames(data_pie) <- c("1000", "5000", "10000", "50000", "100000")
cor(data_pie)
