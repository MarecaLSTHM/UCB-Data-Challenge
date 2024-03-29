library(here)
library(tidyr)
library(dplyr)
library(tseries)
library(forecast)
library(extrafont)
library(Kendall)
library(ggplot2)
library(trend)
here()
setwd(here())
font_import()
loadfonts(device="win")


data11 <- read.csv("data/BISPHOSPHATES_NHS_REGIONS.csv")
data11 <- data11%>%
  select(date, name,y_items)

ts_data<-data11 %>% group_by(date) %>% summarise(prescriptions=sum(y_items))
ts_data_copy_2<-ts_data
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
best_order <- c(0, 0,0)  
notable_d<-c(1,2,3)
notable_p <- c(0:12)
notable_q <- c(0:13)
for (d in notable_d){
  for (p in notable_p) {
    for (q in notable_q) {
      arima_model <- Arima(ts_data_copy, order = c(p, d, q), method = "ML")
      ts_data_testing_values <- as.numeric(ts_data_testing$prescriptions)
      forecast_values <- forecast(arima_model, h = length(ts_data_testing_values))
      
      forecast_mean_values <- as.numeric(forecast_values$mean)
      mse <- mean((ts_data_testing_values - forecast_values$mean)^2)
      
      cat("Order (p,d, q):", c(p,d, q), "  MSE:", mse, "\n")
      
      if (mse < best_mse) {
        best_mse <- mse
        best_order <- c(p, d, q)
      }
    }
  }
}
best_order<-c(3,1,11)





best_model<-Arima(ts_data_copy, order=best_order)
forecast_values <- forecast(best_model, h =12)
forecast_mean_values <- as.numeric(forecast_values$mean)
ts_data_testing_numeric <- as.numeric(ts_data_testing$prescriptions)
plot_data <- data.frame(
  months_since_cutoff = 1:length(ts_data_testing_numeric),
  prescriptions = ts_data_testing_numeric,
  forecast_mean = forecast_mean_values
)
plot.new()
no_sci_format <- function(x) {
  format(x, scientific = FALSE)
}
png("output/prediction_bisphosphates.png")
ggplot(plot_data, aes(x = months_since_cutoff)) +
  geom_line(aes(y = prescriptions, color = "Observed"), size = 1.2) +
  geom_line(aes(y = forecast_mean, color = "Forecasted"), size = 1.2) +
  labs(
    y = "Prescriptions",
    x = "Months since cutoff"
  ) +
  theme_bw() +
  scale_color_manual(
    values = c("Observed" = "blue", "Forecasted" = "red")
  ) +
  theme(text = element_text(family = "Times New Roman", size = 12)) +
  scale_y_continuous(labels = scales::comma_format()) +
  coord_cartesian(ylim = c(0, 600000))






dev.off()

#  +legend("bottomright", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1, text.font = 2)






forecast_horizon <- 11
ts_data <- ts(ts_data$prescriptions, start = start(ts_data$date), frequency = 1)
best_model<-Arima(ts_data, order=best_order)
forecast_values <- forecast(best_model, h = forecast_horizon)


png("output/prediction_bisphosphates_2.png")
par(family = "serif", font = 2)
plot(forecast_values, ylab="total prescriptions", xlab="month",ylim=c(0,600000))

dev.off()






residuals <- residuals(best_model)
box_ljung_test <- Box.test(residuals,  type = "Ljung-Box")
box_ljung_test$p.value

best_order
ts_data<-ts_data_copy_2$prescriptions

Predict_with_all<-c(ts_data,forecast_mean_values)
MannKendall(Predict_with_all)

ts_data_copy$prescriptions
