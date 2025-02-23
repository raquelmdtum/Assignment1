#install.packages("forecast")
#install.packages("tseries")
#install.packages("lmtest")

library(ggplot2)
library(lmtest)
library(forecast)
library(tseries)

load("Dtrain.RData")
load("Dtest.RData") 

# Open a new plot window
if (.Platform$OS.type == "windows") {
  x11()  # For Windows
} else if (Sys.info()["sysname"] == "Darwin") {
  quartz()  # For macOS
} else {
  X11()  # For Linux
}

# ----------------------------------------

# Exercise 1.1

# Year breaks for x-axis (whole years only)
year_breaks <- seq(floor(min(Dtrain$year)), ceiling(max(Dtrain$year)), by = 1)

p <- ggplot(Dtrain, aes(x = year, y = total)) +
  #geom_line(color = "blue") +  # Line plot
  geom_point(color = "black") +  # Points
  labs(title = "Number of Vehicles Registered in Denmark",
       x = "Year",
       y = "Total Vehicles (in millions)") +
  theme_minimal() +
  scale_x_continuous(breaks = year_breaks, labels = as.character(year_breaks)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

#ggsave("exer11.pdf", plot = p, width = 8, height = 5, dpi = 300)
print(p)

# Exercise 2.1

X_first3 <- Dtrain$year[1:3] # 2018.000 2018.083 2018.167
Y_first3 <- Dtrain$total[1:3] # 2.930483 2.934044 2.941422

# To check the first data points in both X and Y
print(X_first3)
print(Y_first3)

# Exercise 2.2

# Fit a linear model: total vehicles ~ year
model <- lm(total ~ year, data = Dtrain)

# Summary (parameter estimates & standard errors)
print(summary(model))

# Extract parameter estimates and standard errors
theta1 <- coef(model)[1]  # Intercept (θ1)
theta2 <- coef(model)[2]  # Slope (θ2)
se_theta1 <- summary(model)$coefficients[1, 2]  # SE of θ1
se_theta2 <- summary(model)$coefficients[2, 2]  # SE of θ2

# Print results
cat("θ1 (Intercept):", round(theta1, 3), "\n")
cat("θ2 (Slope):", round(theta2, 3), "\n")
cat("σ̂_θ1 (SE of θ1):", round(se_theta1, 3), "\n")
cat("σ̂_θ2 (SE of θ2):", round(se_theta2, 3), "\n")

# Create the plot
p <- ggplot(Dtrain, aes(x = year, y = total)) +
  geom_point(color = "black", size = 2) +  # Observed data
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
  labs(title = "Linear Trend Model: Vehicles Over Time",
       x = "Year",
       y = "Total Vehicles (in millions)") +
  theme_minimal()

# Print and save the plot
print(p)
#ggsave("linear_trend_plot.pdf", plot = p, width = 8, height = 5)

# Exercise 2.3

# Extract the first date from the test set to define the forecast period
test_start <- min(Dtest$year)  # Should be 2024.00
test_end <- max(Dtest$year)    # Should be 2024.92

# Generate a sequence for the 12-month forecast
future_years <- seq(test_start, test_end, by = 1/12)

# Create a data frame for predictions
future_data <- data.frame(year = future_years)

# Predict values with 95% confidence intervals
predictions <- predict(model, newdata = future_data, interval = "predict", level = 0.95)

# Combine results into a table
forecast_table <- data.frame(
  Year = round(future_years, 3),
  Predicted = round(predictions[, 1], 3),
  Lower_Bound = round(predictions[, 2], 3),
  Upper_Bound = round(predictions[, 3], 3)
)

# Print forecast table
print(forecast_table)

# Save the forecast as CSV for future comparison with test set
write.csv(forecast_table, "forecast_12months.csv", row.names = FALSE)

# Exercise 2.4

# Compare forecast with actual test data
comparison <- data.frame(
  Year = round(Dtest$year, 3),
  Actual = round(Dtest$total, 3),
  Predicted = round(predictions[, 1], 3),
  Lower_Bound = round(predictions[, 2], 3),
  Upper_Bound = round(predictions[, 3], 3)
)

print(comparison)

p <- ggplot() +
  # Plot Training Data
  geom_point(data = Dtrain, aes(x = year, y = total), color = "black", size = 2) +  # Training data points
  # Plot the Linear Model (Fitted Line)
  geom_smooth(data = Dtrain, aes(x = year, y = total), method = "lm", se = FALSE, color = "blue", linewidth = 1) +
  # Plot Forecasted Values (from model)
  geom_line(data = future_data, aes(x = year, y = predictions[, 1]), color = "green", linewidth = 1, linetype = "solid") +  # Forecast line
  # Plot Prediction Intervals (shaded area)
  geom_ribbon(data = future_data, aes(x = year, ymin = predictions[, 2], ymax = predictions[, 3]), 
              fill = "lightgreen", alpha = 0.5) +  # Prediction intervals (fill between the lines)
  # Plot Actual Test Data (in red)
  geom_point(data = Dtest, aes(x = year, y = total), color = "red", size = 2) +  # Actual test data points
  # Customize Labels and Theme
  labs(title = "Linear Model, Forecast, and Actual Data Comparison",
       x = "Year",
       y = "Total Vehicles (in millions)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis labels for better readability

# Print the plot
print(p)
ggsave("exer24.pdf", plot = p, width = 8, height = 5, dpi = 300)

# Exercise 2.6

# 1. Calculate the residuals
residuals <- model$residuals

# 2. Plot the residuals
# Residual vs Fitted plot (to check for homoscedasticity and linearity)
p1 <- ggplot(data = data.frame(fitted = model$fitted.values, residuals = residuals), 
             aes(x = fitted, y = residuals)) +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", 
       x = "Fitted Values", 
       y = "Residuals") +
  theme_minimal()
print(p1)

# Normal Q-Q plot (to check for normality of residuals)
p2 <- ggplot(data = data.frame(residuals = residuals), aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Normal Q-Q Plot of Residuals") +
  theme_minimal()
print(p2)

# 3. Conduct tests for independence using Durbin-Watson test from lmtest package
dw_test <- dwtest(model)  # Apply the Durbin-Watson test on the linear model

# Display test statistic and p-value
cat("Durbin-Watson test statistic:", round(dw_test$statistic, 3), "\n")
cat("p-value:", round(dw_test$p.value, 3), "\n")

# 4. Check for normality (Shapiro-Wilk test)
shapiro_test <- shapiro.test(residuals)
cat("Shapiro-Wilk normality test p-value:", shapiro_test$p.value, "\n")