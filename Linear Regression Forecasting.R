# ------------------------ Load Required Libraries ------------------------

library(forecast)  # Forecasting functions
library(writexl)   # Export results to Excel
library(ggplot2)   # Visualization

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

# Select the target variable for prediction
target_variable <- "Urbanization level"  
years <- 2010:2021

# Prepare dataset for Linear Regression
regression_data <- data.frame(
  Year = years,
  Target = as.numeric(data_t[, target_variable])
)

# ------------------------ Train Linear Regression Model ------------------------

lm_model <- lm(Target ~ Year, data = regression_data)

# Generate future years (2022-2050)
future_years <- 2050 - max(years)
future_data <- data.frame(Year = seq(max(years) + 1, max(years) + future_years))
lm_forecast <- predict(lm_model, newdata = future_data, interval = "confidence")

# Export forecast results to Excel
forecast_df_lm <- data.frame(
  Year = future_data$Year,
  Forecast = lm_forecast[, "fit"],
  Lower_95 = lm_forecast[, "lwr"],
  Upper_95 = lm_forecast[, "upr"],
  Model = "Linear Regression"
)
write_xlsx(forecast_df_lm, "Linear_Regression_Forecast_Results.xlsx")

# ------------------------ Generate Forecast Plots ------------------------

pdf("Linear_Regression_Forecast.pdf")
plot(regression_data$Year, regression_data$Target, type = "o", col = "blue", 
     main = "Linear Regression Forecast", xlab = "Year", ylab = "Urbanization Level", 
     xlim = c(min(years), 2050), ylim = c(min(regression_data$Target), max(lm_forecast[, "upr"])))
lines(future_data$Year, lm_forecast[, "fit"], col = "red", lwd = 2)
lines(future_data$Year, lm_forecast[, "lwr"], col = "green", lty = 2)
lines(future_data$Year, lm_forecast[, "upr"], col = "green", lty = 2)
legend("topleft", legend = c("Actual", "Forecast", "95% CI"), col = c("blue", "red", "green"), lty = c(1,1,2))
dev.off()

jpeg("Linear_Regression_Forecast.jpg")
plot(regression_data$Year, regression_data$Target, type = "o", col = "blue", 
     main = "Linear Regression Forecast", xlab = "Year", ylab = "Urbanization Level", 
     xlim = c(min(years), 2050), ylim = c(min(regression_data$Target), max(lm_forecast[, "upr"])))
lines(future_data$Year, lm_forecast[, "fit"], col = "red", lwd = 2)
lines(future_data$Year, lm_forecast[, "lwr"], col = "green", lty = 2)
lines(future_data$Year, lm_forecast[, "upr"], col = "green", lty = 2)
legend("topleft", legend = c("Actual", "Forecast", "95% CI"), col = c("blue", "red", "green"), lty = c(1,1,2))
dev.off()

# ------------------------ Error Analysis ------------------------

# Compute errors (RMSE and MAPE)
actuals <- regression_data$Target
predictions <- predict(lm_model, newdata = regression_data)
rmse_lm <- sqrt(mean((predictions - actuals)^2, na.rm = TRUE))
mape_lm <- mean(abs((predictions - actuals) / actuals), na.rm = TRUE) * 100

# Compute 95% confidence interval width
ci_width_lm <- mean(lm_forecast[, "upr"] - lm_forecast[, "lwr"])

# Export error analysis results to Excel
error_results_lm <- data.frame(
  Model = "Linear Regression",
  RMSE = round(rmse_lm, 4),
  MAPE = round(mape_lm, 2),
  Confidence_Interval_Width = round(ci_width_lm, 4)
)
write_xlsx(error_results_lm, "Error_Analysis_Linear_Regression.xlsx")

# Print error results
print(error_results_lm)

# ------------------------ Residual Analysis ------------------------

# Compute residuals
residuals_lm <- residuals(lm_model)

# Generate residual analysis plots
pdf("Residual_Analysis_Linear_Regression.pdf")
par(mfrow = c(2, 2))

# Residual time series plot
plot(residuals_lm, type = "o", col = "blue", main = "Residuals Time Series", ylab = "Residuals", xlab = "Year")

# Residual histogram
hist(residuals_lm, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", border = "black", freq = FALSE)
lines(density(residuals_lm), col = "red")

# ACF and PACF plots of residuals
acf(residuals_lm, main = "ACF of Residuals")
pacf(residuals_lm, main = "PACF of Residuals")

dev.off()

jpeg("Residual_Analysis_Linear_Regression.jpg")
par(mfrow = c(2, 2))
plot(residuals_lm, type = "o", col = "blue", main = "Residuals Time Series", ylab = "Residuals", xlab = "Year")
hist(residuals_lm, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", border = "black", freq = FALSE)
lines(density(residuals_lm), col = "red")
acf(residuals_lm, main = "ACF of Residuals")
pacf(residuals_lm, main = "PACF of Residuals")
dev.off()

# ------------------------ Residual Diagnostics ------------------------

# Shapiro-Wilk test for normality
shapiro_test_lm <- shapiro.test(residuals_lm)

# Ljung-Box test for white noise
ljung_box_test_lm <- Box.test(residuals_lm, lag = 10, type = "Ljung-Box")

# Compute residual mean and standard deviation
residual_mean_lm <- mean(residuals_lm, na.rm = TRUE)
residual_sd_lm <- sd(residuals_lm, na.rm = TRUE)

# Organize residual analysis results
residual_analysis_results_lm <- data.frame(
  Test = c("Shapiro-Wilk p-value", "Ljung-Box p-value", "Residual Mean", "Residual SD"),
  Value = c(shapiro_test_lm$p.value, ljung_box_test_lm$p.value, residual_mean_lm, residual_sd_lm)
)
