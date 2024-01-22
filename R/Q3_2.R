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

ts_data_training <- ts_data %>% filter(date < as.Date("2022-11-01"))
ts_data_testing<- ts_data %>% filter(date >= as.Date("2022-11-01"))

ts_data_training <- ts(ts_data_training$prescriptions, start = start(ts_data$date), frequency = 1)
ts_data_copy<-ts_data_training
plot(ts_data_training, main="Time Series Data")

adf_test_result <- adf.test(ts_data_training)
cat("ADF Test p-value:", adf_test_result$p.value, "\n")

i<-0
if (adf_test_result$p.value > 0.05) {
  ts_data_training <- diff(ts_data_training)
  adf_test_result <- adf.test(ts_data_training)
  i<-i+1
}
cat("ADF Test p-value:", adf_test_result$p.value, "\n")
cat("Number of differencing steps (d):", i, "\n")


acf_pacf_plots <- function(ts_data_training, lag.max = 48) {
  par(mfrow=c(1,2))
  acf(ts_data_training, lag.max = lag.max, main="ACF")
  pacf(ts_data_training, lag.max = lag.max, main="PACF")
}

acf_pacf_plots(ts_data_training)


#notable p -->0,1,2,4,
#notable q from ACF plot--> 1,3,19,12,13
best_mse <- Inf 
best_order <- c(0, 0)  
notable_p <- c(0, 1, 2, 4)
notable_q <- c(1, 3, 9, 12, 13)

for (p in notable_p) {
  for (q in notable_q) {
    arima_model <- Arima(ts_data_copy, order = c(p, 1, q), method = "ML")
    ts_data_testing_values <- as.numeric(ts_data_testing$prescriptions)
    forecast_values <- forecast(arima_model, h = length(ts_data_testing_values))
    
    forecast_mean_values <- as.numeric(forecast_values$mean)
    mse <- mean((ts_data_testing_values - forecast_values$mean)^2)
    
    cat("Order (p, q):", c(p, q), "  MSE:", mse, "\n")
    
    if (mse < best_mse) {
      best_mse <- mse
      best_order <- c(p, 1, q)
    }
  }
}
best_order





best_model<-Arima(ts_data_copy, order=best_order)
forecast_values <- forecast(best_model, h = length(ts_data_testing_values))
forecast_mean_values <- as.numeric(forecast_values$mean)
ts_data_testing_numeric <- as.numeric(ts_data_testing$prescriptions)
png("output/prediction_bisphosphates.png")
# Plot the actual values and the forecasted values
plot(ts_data_testing_numeric, type = "l", col = "blue", ylim = range(c(ts_data_testing_numeric, forecast_mean_values)), ylab = "Prescriptions", xlab = "months since cutoff")
lines(forecast_mean_values, col = "red")

legend("bottomright", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1)
dev.off()


forecast_horizon <- 9
ts_data <- ts(ts_data$prescriptions, start = start(ts_data$date), frequency = 1)
best_model<-Arima(ts_data, order=best_order)
forecast_values <- forecast(best_model, h = forecast_horizon)

png("output/prediction_bisphosphates_2.png")
plot(forecast_values, main = "Bisphosphonates Forecast", ylab="total prescriptions", xlab="month")

dev.off()


residuals <- residuals(best_model)
box_ljung_test <- Box.test(residuals,  type = "Ljung-Box")
box_ljung_test$p.value
