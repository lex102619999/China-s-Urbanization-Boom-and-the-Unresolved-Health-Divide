# ------------------------ Load Required Libraries ------------------------

install.packages(c("writexl", "ggplot2"), dependencies = TRUE)
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

# Select the target variable for forecasting
target_variable <- "Urbanization level"  
years <- 2010:2021

# Prepare dataset for polynomial regression
poly_data <- data.frame(
  Year = years,
  Target = as.numeric(data_t[, target_variable])
)

# Define polynomial degree (e.g., 2nd degree polynomial)
degree <- 2

# ------------------------ Train Polynomial Regression Model ------------------------

# Fit polynomial regression model
poly_model <- lm(Target ~ poly(Year, degree, raw = TRUE), data = poly_data)

# Generate future years (2022-2050)
future_years <- 2050 - max(years)
future_data <- data.frame(Year = seq(max(years) + 1, max(years) + future_years))
future_predictions <- predict(poly_model, newdata = data.frame(Year = future_data$Year))

# Export forecast results to Excel
forecast_df_poly <- data.frame(
  Year = future_data$Year,
  Forecast = future_predictions,
  Model = paste("Polynomial Regression (Degree", degree, ")")
)
write_xlsx(forecast_df_poly, "Polynomial_Regression_Forecast_Results.xlsx")

# ------------------------ Generate Forecast Plots ------------------------

pdf("Polynomial_Regression_Forecast.pdf")
plot(poly_data$Year, poly_data$Target, type = "o", col = "blue", 
     main = paste("Polynomial Regression Forecast (Degree", degree, ")"), 
     xlab = "Year", ylab = "Urbanization Level", 
     xlim = c(min(years), 2050), ylim = c(min(poly_data$Target), max(future_predictions)))
lines(future_data$Year, future_predictions, col = "red", lwd = 2)
legend("topleft", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = c(1,1))
dev.off()

jpeg("Polynomial_Regression_Forecast.jpg")
plot(poly_data$Year, poly_data$Target, type = "o", col = "blue", 
     main = paste("Polynomial Regression Forecast (Degree", degree, ")"), 
     xlab = "Year", ylab = "Urbanization Level", 
     xlim = c(min(years), 2050), ylim = c(min(poly_data$Target), max(future_predictions)))
lines(future_data$Year, future_predictions, col = "red", lwd = 2)
legend("topleft", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = c(1,1))
dev.off()

# ------------------------ Error Analysis ------------------------

# Compute errors (RMSE and MAPE)
actuals <- poly_data$Target
predictions <- predict(poly_model, newdata = poly_data)
rmse_poly <- sqrt(mean((predictions - actuals)^2, na.rm = TRUE))
mape_poly <- mean(abs((predictions - actuals) / actuals), na.rm = TRUE) * 100

# Export error analysis results to Excel
error_results_poly <- data.frame(
  Model = paste("Polynomial Regression (Degree", degree, ")"),
  RMSE = round(rmse_poly, 4),
  MAPE = round(mape_poly, 2)
)
write_xlsx(error_results_poly, "Error_Analysis_Polynomial_Regression.xlsx")

# Print error results
print(error_results_poly)

# ------------------------ Residual Analysis ------------------------

# Compute residuals
residuals_poly <- actuals - predictions

# Generate residual analysis plots
pdf("Residual_Analysis_Polynomial_Regression.pdf")
par(mfrow = c(2, 2))

# Residual time series plot
plot(residuals_poly, type = "o", col = "blue", main = "Residuals Time Series", ylab = "Residuals", xlab = "Year")

# Residual histogram
hist(residuals_poly, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", border = "black", freq = FALSE)
lines(density(residuals_poly), col = "red")

# ACF and PACF plots of residuals
acf(residuals_poly, main = "ACF of Residuals")
pacf(residuals_poly, main = "PACF of Residuals")

dev.off()

jpeg("Residual_Analysis_Polynomial_Regression.jpg")
par(mfrow = c(2, 2))
plot(residuals_poly, type = "o", col = "blue", main = "Residuals Time Series", ylab = "Residuals", xlab = "Year")
hist(residuals_poly, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", border = "black", freq = FALSE)
lines(density(residuals_poly), col = "red")
acf(residuals_poly, main = "ACF of Residuals")
pacf(residuals_poly, main = "PACF of Residuals")
dev.off()

# ------------------------ Residual Diagnostics ------------------------

# Shapiro-Wilk test for normality
shapiro_test_poly <- shapiro.test(residuals_poly)

# Ljung-Box test for white noise
ljung_box_test_poly <- Box.test(residuals_poly, lag = 10, type = "Ljung-Box")

# Compute residual mean and standard deviation
residual_mean_poly <- mean(residuals_poly, na.rm = TRUE)
residual_sd_poly <- sd(residuals_poly, na.rm = TRUE)

# Organize residual analysis results
residual_analysis_results_poly <- data.frame(
  Test = c("Shapiro-Wilk p-value", "Ljung-Box p-value", "Residual Mean", "Residual SD"),
  Value = c(shapiro_test_poly$p.value, ljung_box_test_poly$p.value, residual_mean_poly, residual_sd_poly)
)
