# ------------------------ Load Required Libraries ------------------------

library(randomForest)  # Random Forest regression
library(forecast)      # Forecasting functions
library(writexl)       # Export results to Excel

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

# Select the target variable for prediction
target_variable <- "Urbanization level"  
ts_data <- data_ts[, target_variable]

# Prepare dataset for Random Forest regression
df <- data.frame(Year = years, Urbanization_Level = as.numeric(ts_data))

# ------------------------ Train Random Forest Model ------------------------

set.seed(123)  # Set seed for reproducibility
rf_model <- randomForest(Urbanization_Level ~ Year, data = df, ntree = 500, importance = TRUE)

# Print model summary
print(rf_model)

# ------------------------ Forecast Future Values ------------------------

# Generate future years (2022-2050)
future_years <- 2050 - max(years)
future_df <- data.frame(Year = seq(max(years) + 1, max(years) + future_years))
forecast_rf <- predict(rf_model, newdata = future_df)

# Export forecast results to Excel
forecast_df_rf <- data.frame(
  Year = seq(max(years) + 1, max(years) + future_years), 
  Forecast = as.numeric(forecast_rf),
  Model = "Random Forest"
)
write_xlsx(forecast_df_rf, "Random_Forest_Forecast_Results.xlsx")

# ------------------------ Generate Forecast Plots ------------------------

pdf("Random_Forest_Forecast.pdf")
plot(years, ts_data, type = "o", col = "blue", 
     main = "Random Forest Forecast", 
     xlab = "Year", ylab = "Urbanization Level", 
     xlim = c(min(years), 2050), ylim = c(min(ts_data), max(forecast_rf)))
lines(seq(max(years) + 1, max(years) + future_years), forecast_rf, col = "red", lwd = 2)
legend("topleft", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = c(1,1))
dev.off()

jpeg("Random_Forest_Forecast.jpg")
plot(years, ts_data, type = "o", col = "blue", 
     main = "Random Forest Forecast", 
     xlab = "Year", ylab = "Urbanization Level", 
     xlim = c(min(years), 2050), ylim = c(min(ts_data), max(forecast_rf)))
lines(seq(max(years) + 1, max(years) + future_years), forecast_rf, col = "red", lwd = 2)
legend("topleft", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = c(1,1))
dev.off()

# ------------------------ Error Analysis ------------------------

# Compute errors (RMSE and MAPE)
predictions <- predict(rf_model, newdata = df)
actuals <- df$Urbanization_Level
rmse_rf <- sqrt(mean((predictions - actuals)^2, na.rm = TRUE))
mape_rf <- mean(abs((predictions - actuals) / actuals), na.rm = TRUE) * 100

# Export error analysis results to Excel
error_results_rf <- data.frame(
  Model = "Random Forest",
  RMSE = round(rmse_rf, 4),
  MAPE = round(mape_rf, 2)
)
write_xlsx(error_results_rf, "Error_Analysis_Random_Forest.xlsx")

# Print error results
print(error_results_rf)

# ------------------------ Residual Analysis ------------------------

# Compute residuals
residuals_rf <- actuals - predictions

# Generate residual analysis plots
pdf("Residual_Analysis_Random_Forest.pdf")
par(mfrow = c(2, 2))

# Residual time series plot
plot(residuals_rf, type = "o", col = "blue", main = "Residuals Time Series", ylab = "Residuals", xlab = "Year")

# Residual histogram
hist(residuals_rf, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", border = "black", freq = FALSE)
lines(density(residuals_rf), col = "red")

# ACF and PACF plots of residuals
acf(residuals_rf, main = "ACF of Residuals")
pacf(residuals_rf, main = "PACF of Residuals")

dev.off()

jpeg("Residual_Analysis_Random_Forest.jpg")
par(mfrow = c(2, 2))
plot(residuals_rf, type = "o", col = "blue", main = "Residuals Time Series", ylab = "Residuals", xlab = "Year")
hist(residuals_rf, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", border = "black", freq = FALSE)
lines(density(residuals_rf), col = "red")
acf(residuals_rf, main = "ACF of Residuals")
pacf(residuals_rf, main = "PACF of Residuals")
dev.off()

# ------------------------ Residual Diagnostics ------------------------

# Shapiro-Wilk test for normality
shapiro_test_rf <- shapiro.test(residuals_rf)

# Ljung-Box test for white noise
ljung_box_test_rf <- Box.test(residuals_rf, lag = 10, type = "Ljung-Box")

# Compute residual mean and standard deviation
residual_mean_rf <- mean(residuals_rf, na.rm = TRUE)
residual_sd_rf <- sd(residuals_rf, na.rm = TRUE)

# Organize residual analysis results
residual_analysis_results_rf <- data.frame(
  Test = c("Shapiro-Wilk p-value", "Ljung-Box p-value", "Residual Mean", "Residual SD"),
  Value = c(shapiro_test_rf$p.value, ljung_box_test_rf$p.value, residual_mean_rf, residual_sd_rf)
)
