# ------------------------ Load Required Libraries ------------------------

library(forecast)  # ARIMA modeling and forecasting
library(writexl)   # Export results to Excel

# ------------------------ Data Preparation ------------------------

# Read data
data <- read.table("111.txt", header = TRUE, sep = "\t", check.names = FALSE)

# Process column names to avoid special character issues
colnames(data) <- iconv(colnames(data), from = "UTF-8", to = "ASCII//TRANSLIT")
colnames(data) <- gsub("[^[:print:]]", "", colnames(data))
colnames(data) <- gsub("[^a-zA-Z0-9_ ]", "", colnames(data))

# Transpose data to set years as row indices
data_t <- as.data.frame(t(data[-1]))  # Remove the first column and transpose
colnames(data_t) <- data[, 1]         # Assign original variable names as column names
rownames(data_t) <- NULL              # Remove row names

# Convert years into a time series
years <- 2010:2021
data_ts <- ts(data_t, start = min(years), end = max(years), frequency = 1)  # Non-seasonal time series

# Select the target variable for forecasting
target_variable <- "Urbanization level"  
ts_data <- data_ts[, target_variable]

# ------------------------ Train ARIMA Model ------------------------

# Fit ARIMA model automatically
arima_model <- auto.arima(ts_data)
summary(arima_model)

# Generate future years (2022-2050)
future_years <- 2050 - max(years)
forecast_arima <- forecast(arima_model, h = future_years)

# Export forecast results to Excel
forecast_df_arima <- data.frame(
  Year = seq(max(years) + 1, max(years) + future_years), 
  Forecast = as.numeric(forecast_arima$mean),
  Model = "ARIMA"
)
write_xlsx(forecast_df_arima, "ARIMA_Forecast_Results.xlsx")

# ------------------------ Generate Forecast Plots ------------------------

pdf("ARIMA_Forecast.pdf")
plot(years, ts_data, type = "o", col = "blue", 
     main = "ARIMA Forecast", 
     xlab = "Year", ylab = "Urbanization Level", 
     xlim = c(min(years), 2050), ylim = c(min(ts_data), max(forecast_arima$mean)))
lines(seq(max(years) + 1, max(years) + future_years), forecast_arima$mean, col = "red", lwd = 2)
legend("topleft", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = c(1,1))
dev.off()

jpeg("ARIMA_Forecast.jpg")
plot(years, ts_data, type = "o", col = "blue", 
     main = "ARIMA Forecast", 
     xlab = "Year", ylab = "Urbanization Level", 
     xlim = c(min(years), 2050), ylim = c(min(ts_data), max(forecast_arima$mean)))
lines(seq(max(years) + 1, max(years) + future_years), forecast_arima$mean, col = "red", lwd = 2)
legend("topleft", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = c(1,1))
dev.off()

# ------------------------ Error Analysis ------------------------

# Compute errors (RMSE and MAPE)
actuals <- ts_data
predictions <- fitted(arima_model)
rmse_arima <- sqrt(mean((predictions - actuals)^2, na.rm = TRUE))
mape_arima <- mean(abs((predictions - actuals) / actuals), na.rm = TRUE) * 100

# Export error analysis results to Excel
error_results_arima <- data.frame(
  Model = "ARIMA",
  RMSE = round(rmse_arima, 4),
  MAPE = round(mape_arima, 2)
)
write_xlsx(error_results_arima, "Error_Analysis_ARIMA.xlsx")

# Print error results
print(error_results_arima)

# ------------------------ Residual Analysis ------------------------

# Compute residuals
residuals_arima <- residuals(arima_model)

# Generate residual analysis plots
pdf("Residual_Analysis_ARIMA.pdf")
par(mfrow = c(2, 2))

# Residual time series plot
plot(residuals_arima, type = "o", col = "blue", main = "Residuals Time Series", ylab = "Residuals", xlab = "Year")

# Residual histogram
hist(residuals_arima, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", border = "black", freq = FALSE)
lines(density(residuals_arima), col = "red")

# ACF and PACF plots of residuals
acf(residuals_arima, main = "ACF of Residuals")
pacf(residuals_arima, main = "PACF of Residuals")

dev.off()

jpeg("Residual_Analysis_ARIMA.jpg")
par(mfrow = c(2, 2))
plot(residuals_arima, type = "o", col = "blue", main = "Residuals Time Series", ylab = "Residuals", xlab = "Year")
hist(residuals_arima, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", border = "black", freq = FALSE)
lines(density(residuals_arima), col = "red")
acf(residuals_arima, main = "ACF of Residuals")
pacf(residuals_arima, main = "PACF of Residuals")
dev.off()

# ------------------------ Residual Diagnostics ------------------------

# Shapiro-Wilk test for normality
shapiro_test_arima <- shapiro.test(residuals_arima)

# Ljung-Box test for white noise
ljung_box_test_arima <- Box.test(residuals_arima, lag = 10, type = "Ljung-Box")

# Compute residual mean and standard deviation
residual_mean_arima <- mean(residuals_arima, na.rm = TRUE)
residual_sd_arima <- sd(residuals_arima, na.rm = TRUE)

# Organize residual analysis results
residual_analysis_results_arima <- data.frame(
  Test = c("Shapiro-Wilk p-value", "Ljung-Box p-value", "Residual Mean", "Residual SD"),
  Value = c(shapiro_test_arima$p.value, ljung_box_test_arima$p.value, residual_mean_arima, residual_sd_arima)
)
