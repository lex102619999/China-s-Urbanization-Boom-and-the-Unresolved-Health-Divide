# Load required libraries
library(lmtest)
library(vars)
library(tseries)

# Load dataset
df <- read.csv("your_data.csv")

# Define variables
dependent_var <- df$?  # Health disparity outcome variable
independent_var <- df$?  # Urbanization level
control_vars <- df[, c("?", "?", "?", "?", "?", "?")]  # Control variables

# Merge into a single dataframe
data_granger <- cbind(dependent_var, independent_var, control_vars)

# Remove missing values
data_granger <- na.omit(data_granger)

# Convert to time series format
data_ts <- ts(data_granger, start = c(2010), frequency = 1)

# Perform ADF unit root test to check stationarity
adf_test_dependent <- adf.test(data_ts[, "dependent_var"])
adf_test_independent <- adf.test(data_ts[, "independent_var"])

# If non-stationary (p > 0.05), apply first-order differencing
if (adf_test_dependent$p.value > 0.05) {
    data_ts[, "dependent_var"] <- diff(data_ts[, "dependent_var"])
}
if (adf_test_independent$p.value > 0.05) {
    data_ts[, "independent_var"] <- diff(data_ts[, "independent_var"])
}

# Determine optimal lag order using AIC criterion
lag_selection <- VARselect(data_ts, lag.max = 5, type = "const")
best_lag <- lag_selection$selection["AIC(n)"]

# Conduct Granger causality test for lag = 1 and lag = 2
granger_1 <- grangertest(dependent_var ~ independent_var, order = 1, data = data_granger)
granger_2 <- grangertest(dependent_var ~ independent_var, order = 2, data = data_granger)

# Print results
print("Granger Causality Test (Lag = 1)")
print(granger_1)
print("Granger Causality Test (Lag = 2)")
print(granger_2)
