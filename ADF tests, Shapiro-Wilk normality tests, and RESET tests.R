
# Load necessary libraries
library(tseries)  # ADF test for stationarity
library(nortest)  # Shapiro-Wilk normality test
library(lmtest)   # RESET test for non-linearity
library(writexl)  # Export results to Excel

# Read the dataset
data <- read.table("111.txt", header = TRUE, sep = "\t", check.names = FALSE)

# Transpose the data, setting years as row indices
data_t <- as.data.frame(t(data[-1]))  # Remove the first column and transpose
colnames(data_t) <- data[, 1]  # Assign original variable names as column names
rownames(data_t) <- NULL  # Remove row names

# Convert years to a time series
years <- 2010:2021
data_ts <- ts(data_t, start = min(years), end = max(years), frequency = 1)

# Initialize data frames to store test results
adf_results <- data.frame()
shapiro_results <- data.frame()
reset_results <- data.frame()

# Perform ADF test, normality test, and RESET test for each variable
for (var in colnames(data_ts)) {
  # ADF test for stationarity
  adf_test <- adf.test(data_ts[, var])
  adf_temp <- data.frame(
    Variable = var, 
    ADF_Statistic = adf_test$statistic, 
    P_Value = adf_test$p.value
  )
  adf_results <- rbind(adf_results, adf_temp)
  
  # Shapiro-Wilk normality test (only applicable for sample size ¡Ü 5000)
  if (length(data_ts[, var]) <= 5000) {
    shapiro_test <- shapiro.test(data_ts[, var])
    shapiro_temp <- data.frame(
      Variable = var, 
      W_Statistic = shapiro_test$statistic, 
      P_Value = shapiro_test$p.value
    )
    shapiro_results <- rbind(shapiro_results, shapiro_temp)
  }
  
  # RESET test for non-linearity
  lm_model <- lm(data_ts[, var] ~ seq_along(data_ts[, var]))
  reset_test <- resettest(lm_model, power = 2, type = "fitted")
  reset_temp <- data.frame(
    Variable = var, 
    RESET_Statistic = reset_test$statistic, 
    P_Value = reset_test$p.value
  )
  reset_results <- rbind(reset_results, reset_temp)
}


