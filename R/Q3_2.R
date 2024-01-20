library(here)
library(tidyr)
library(dplyr)
library(tseries)
library(forecast)
here()
setwd(here())
data11 <- read.csv("data/data11.csv")
data11 <- data11%>%
  select(date, name,y_items)

ts_data<-data11 %>% group_by(date) %>% summarise(prescriptions=sum(y_items))
ts_data$date<-as.Date(ts_data$date)
ts_data <- ts(ts_data$prescriptions, start = start(ts_data$date), frequency = 1)
ts_data_copy<-ts_data
plot(ts_data, main="Time Series Data")

adf_test_result <- adf.test(ts_data)
cat("ADF Test p-value:", adf_test_result$p.value, "\n")
# p-value is greater than the significance level the series is not stationary.
i<-0
if (adf_test_result$p.value > 0.05) {
  ts_data <- diff(ts_data)
  adf_test_result <- adf.test(ts_data)
  i<-i+1
}
cat("ADF Test p-value:", adf_test_result$p.value, "\n")
cat("Number of differencing steps (d):", i, "\n")


acf_pacf_plots <- function(ts_data, lag.max = 20) {
  par(mfrow=c(1,2))
  acf(ts_data, lag.max = lag.max, main="ACF")
  pacf(ts_data, lag.max = lag.max, main="PACF")
}

acf_pacf_plots(ts_data)


#notable p --> 1, 4,12 
#notable q--> 1,2,4,12

arima_model <- arima(ts_data_copy, order=c(p=4, d=1, q=12))

png("output/arima_model_residuals.png")
plot(residuals(arima_model), main="Residuals of ARIMA model", xlab ="months", ylab ="residuals")
dev.off()

Box.test(residuals(arima_model), lag = 20, type = "Ljung-Box")
n <- 12
forecast_values <- forecast(arima_model, h = n, level = c(95))


png("output/arima_model_prediction.png")
plot(forecast_values, main = "Bisphosphonates Forecast", ylab="total prescriptions", xlab="month")
dev.off()

#since that of 
