# ------------------------ Load Required Libraries ------------------------

library(forecast)   # ETS modeling and forecasting
library(writexl)    # Export results to Excel

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
ts_data <- as.numeric(data_ts[, target_variable])

# ------------------------ ETS Model and Forecasting ------------------------

# Fit ETS model
ets_model <- ets(ts_data)
summary(ets_model)

# Forecast for the next 30 years (2022-2050)
future_years <- 2050 - max(years)
forecast_ets <- forecast(ets_model, h = future_years)

# Export forecast results to Excel
forecast_df_ets <- data.frame(
  Year = seq(max(years) + 1, max(years) + future_years),
  Forecast = as.numeric(forecast_ets$mean),
  Model = "ETS"
)
write_xlsx(forecast_df_ets, "ETS_Forecast_Results.xlsx")

# ------------------------ ETS Forecast Plot ------------------------

pdf("ETS_Forecast.pdf")
plot(years, ts_data, type = "o", col = "blue",
     main = "ETS Forecast", 
     xlab = "Year", ylab = "Urbanization Level",
     xlim = c(min(years), 2050), ylim = c(min(ts_data), max(forecast_ets$mean)))
lines(seq(max(years) + 1, max(years) + future_years), forecast_ets$mean, col = "red", lwd = 2)
legend("topleft", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = c(1, 1))
dev.off()

jpeg("ETS_Forecast.jpg")
plot(years, ts_data, type = "o", col = "blue",
     main = "ETS Forecast", 
     xlab = "Year", ylab = "Urbanization Level",
     xlim = c(min(years), 2050), ylim = c(min(ts_data), max(forecast_ets$mean)))
lines(seq(max(years) + 1, max(years) + future_years), forecast_ets$mean, col = "red", lwd = 2)
legend("topleft", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = c(1, 1))
dev.off()

# ------------------------ Error Analysis ------------------------

# Compute errors (RMSE and MAPE)
actuals <- ts_data
predictions <- fitted(ets_model)
rmse_ets <- sqrt(mean((predictions - actuals)^2, na.rm = TRUE))
mape_ets <- mean(abs((predictions - actuals) / actuals), na.rm = TRUE) * 100

# Export error analysis results to Excel
error_results_ets <- data.frame(
  Model = "ETS",
  RMSE = round(rmse_ets, 4),
  MAPE = round(mape_ets, 2)
)
write_xlsx(error_results_ets, "Error_Analysis_ETS.xlsx")

# Print error results
print(error_results_ets)

# ------------------------ Residual Analysis ------------------------

# Compute residuals
residuals_ets <- residuals(ets_model)

# Generate residual analysis plots
pdf("Residual_Analysis_ETS.pdf")
par(mfrow = c(2, 2))

# Residual time series plot
plot(residuals_ets, type = "o", col = "blue", main = "Residuals Time Series", ylab = "Residuals", xlab = "Year")

# Residual histogram
hist(residuals_ets, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", border = "black", freq = FALSE)
lines(density(residuals_ets), col = "red")

# ACF and PACF plots of residuals
acf(residuals_ets, main = "ACF of Residuals")
pacf(residuals_ets, main = "PACF of Residuals")

dev.off()

jpeg("Residual_Analysis_ETS.jpg")
par(mfrow = c(2, 2))
plot(residuals_ets, type = "o", col = "blue", main = "Residuals Time Series", ylab = "Residuals", xlab = "Year")
hist(residuals_ets, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", border = "black", freq = FALSE)
lines(density(residuals_ets), col = "red")
acf(residuals_ets, main = "ACF of Residuals")
pacf(residuals_ets, main = "PACF of Residuals")
dev.off()

# ------------------------ Residual Diagnostics ------------------------

# Shapiro-Wilk test for normality
shapiro_test_ets <- shapiro.test(residuals_ets)

# Ljung-Box test for white noise
ljung_box_test_ets <- Box.test(residuals_ets, lag = 10, type = "Ljung-Box")

# Compute residual mean and standard deviation
residual_mean_ets <- mean(residuals_ets, na.rm = TRUE)
residual_sd_ets <- sd(residuals_ets, na.rm = TRUE)

# Organize residual analysis results
residual_analysis_results_ets <- data.frame(
  Test = c("Shapiro-Wilk p-value", "Ljung-Box p-value", "Residual Mean", "Residual SD"),
  Value = c(shapiro_test_ets$p.value, ljung_box_test_ets$p.value, residual_mean_ets, residual_sd_ets)
)

# Export residual analysis results to Excel
write_xlsx(residual_analysis_results_ets, "Residual_Analysis_ETS.xlsx")

# Print residual analysis results
print(residual_analysis_results_ets)
