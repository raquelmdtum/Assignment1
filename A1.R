#install.packages("forecast")
#install.packages("tseries")
#install.packages("lmtest")

library(MASS)
library(ggplot2)
library(lmtest)
library(forecast)
library(tseries)

load("Dtrain.RData")
load("Dtest.RData") 

# ----------------------------------------

cat("\n------------- Exercise 1.1 -------------\n")

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

ggsave("Figures/1.1_plot.pdf", plot = p, width = 8, height = 5, dpi = 300)
#print(p)

# ------- Exercise 2 ---------

cat("\n------------- Exercise 2.1 -------------\n")

X_first3 <- Dtrain$year[1:3] # 2018.000 2018.083 2018.167
Y_first3 <- Dtrain$total[1:3] # 2.930483 2.934044 2.941422

# To check the first data points in both X and Y
print(X_first3)
print(Y_first3)

# ------- Exercise 3 ---------

cat("\n------------- Exercise 3.1 -------------\n")

# Fit a linear model: total vehicles ~ year
model <- lm(total ~ year, data = Dtrain)

# Get the parameter estimates
theta1 <- coef(model)[1]  # Intercept (θ1)
theta2 <- coef(model)[2]  # Slope (θ2)

# Print results
cat("θ1 (Intercept):", round(theta1, 3), "\n")
cat("θ2 (Slope):", round(theta2, 3), "\n")

cat("\n------------- Exercise 3.2 -------------\n")

# Get estimated parameters (θ̂1 and θ̂2) - the same as before?? Not sure
theta_hat_1 <- coef(model)[1]  # Estimated intercept (θ̂1)
theta_hat_2 <- coef(model)[2]  # Estimated slope (θ̂2)

# Get standard errors
se_theta1 <- summary(model)$coefficients[1, 2]  # SE of θ1
se_theta2 <- summary(model)$coefficients[2, 2]  # SE of θ2

cat("θ̂1:", round(theta_hat_1, 3), "±", round(se_theta1, 3), "\n")
cat("θ̂2:", round(theta_hat_2, 3), "±", round(se_theta1, 3), "\n")
cat("σ̂_θ1 (SE of θ1):", round(se_theta1, 3), "\n")
cat("σ̂_θ2 (SE of θ2):", round(se_theta2, 3), "\n")

p <- ggplot(Dtrain, aes(x = year, y = total)) +
  # Observed data points (black)
  geom_point(aes(color = "Observed Data"), size = 2) +
  # Linear trend line (blue)
  geom_smooth(method = "lm", se = FALSE, aes(color = "Linear Trend"), linewidth = 1) +
  # Plot labels
  labs(title = "Linear Trend Model: Vehicles Over Time",
       x = "Year",
       y = "Total Vehicles (in millions)") +
  theme_minimal() +
  # Custom color scale for legend
  scale_color_manual(values = c("Observed Data" = "black", "Linear Trend" = "blue")) +
  theme(legend.position = "bottom")

#print(p)
ggsave("Figures/3.2_linear_trend_plot.pdf", plot = p, width = 8, height = 5)

cat("\n------------- Exercise 3.3 -------------\n")

# Define the starting and ending months for prediction
test_start <- min(Dtest$year)  # Should be 2024.000
test_end <- max(Dtest$year)    # Should be 2024.92

# Generate a sequence for the 12-month forecast (monthly data points)
future_months <- seq(test_start, test_end, by = 1/12)

# Create a data frame for predictions (one row per month)
future_data <- data.frame(year = future_months)

# Use the linear model to predict for each future month
predictions <- predict(model, newdata = future_data, interval = "predict", level = 0.95)

# Combine results into a table with predictions and confidence intervals
forecast_table <- data.frame(
  Year = round(future_months, 3),       # Round the year to 3 decimal places
  Predicted = round(predictions[, 1], 3),  # Predicted values
  Lower_Bound = round(predictions[, 2], 3),  # Lower bound of prediction interval
  Upper_Bound = round(predictions[, 3], 3)   # Upper bound of prediction interval
)

print(forecast_table)
write.csv(forecast_table, "3.3_forecast_12months.csv", row.names = FALSE)

cat("\n------------- Exercise 3.4 -------------\n")

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
  geom_point(data = Dtrain, aes(x = year, y = total, color = "Training Data"), size = 2) +  # Training data points
  
  # Plot the Linear Model (Fitted Line)
  geom_smooth(data = Dtrain, aes(x = year, y = total, color = "Linear Model (Fitted Line)"), method = "lm", se = FALSE, linewidth = 1) +
  
  # Plot Forecasted Values (from model)
  geom_point(data = future_data, aes(x = year, y = predictions[, 1], color = "Forecasted Values"), size = 2, shape = 16) +  # Forecast data points
  
  # Plot Prediction Intervals (shaded area)
  geom_ribbon(data = future_data, aes(x = year, ymin = predictions[, 2], ymax = predictions[, 3], fill = "Prediction Interval"), alpha = 0.5) +  # Prediction intervals (fill between the lines)
  
  # Plot Actual Test Data (in red)
  geom_point(data = Dtest, aes(x = year, y = total, color = "Actual Test Data"), size = 2) +  # Actual test data points
  
  # Customize Labels and Theme
  labs(title = "Linear Model, Forecast, and Actual Data Comparison",
       x = "Year",
       y = "Total Vehicles (in millions)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Adjust x-axis labels for better readability
  # Custom color and fill scales for legend
  scale_color_manual(values = c("Training Data" = "black", 
                                "Linear Model (Fitted Line)" = "blue", 
                                "Forecasted Values" = "green", 
                                "Actual Test Data" = "red")) +
  scale_fill_manual(values = c("Prediction Interval" = "lightgreen")) +
  theme(legend.position = "bottom")

#print(p)
ggsave("Figures/3.4_forecast_comparison_plot.pdf", plot = p, width = 9, height = 5, dpi = 300)

cat("\n------------- Exercise 3.6 -------------\n")

# Calculate residuals
residuals <- Dtrain$total - predict(model, newdata = Dtrain)

# Residual Plot
p <- ggplot(data = data.frame(year = Dtrain$year, residuals = residuals), aes(x = year, y = residuals)) +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot",
       x = "Year",
       y = "Residuals") +
  theme_minimal()

#print(p)
ggsave("Figures/3.6_residual_plot.pdf", plot = p, width = 8, height = 5, dpi = 300)

# Histogram of residuals
png("Figures/3.6_histogram.png", width=800, height=600)  # Open a PNG device
hist(residuals, main="Histogram of Residuals", xlab="Residuals", col="lightblue", breaks=10)
dev.off()  # Close the PNG device

# Q-Q Plot
png("Figures/3.6_qq_plot.png", width=800, height=600)  # Open a PNG device
qqnorm(residuals, main="Q-Q Plot of Residuals", pch=19, col="blue")
qqline(residuals, col="red", lwd=2)
legend("topleft", legend = c("Residuals", "Q-Q Line"), col = c("blue", "red"), pch = c(19, NA), lwd = c(NA, 2))
dev.off()  # Close the PNG device


# ------- Exercise 4 ---------

cat("\n------------- Exercise 4.2 -------------\n")

# Define range for x values (from 71 to 0)
x_values <- seq(71, 0, by = -1)  # x decreasing from 71 to 0

# Compute corresponding years (mapping 71 → 2018, 70 → 2018 + 1/12, ..., 0 → 2023)
year_values <- 2018 + (71 - x_values) / 12  # Linear transformation

# Compute y values using the exponential function y = 0.9^x
y_values <- (0.9)^x_values  # Keep y-values unchanged

# Create a dataframe for plotting
df <- data.frame(year = year_values, y = y_values)

# Generate the exponential decay plot and assign to variable 'p'
p <- ggplot(df, aes(x = year, y = y, color = "Exponential Decay")) +
  geom_line(linewidth = 1) +
  geom_point(aes(color = "Data Points")) +
  scale_color_manual(values = c("Exponential Decay" = "blue", "Data Points" = "red")) +
  scale_x_continuous(breaks = seq(2018, 2023, by = 1), labels = as.character(seq(2018, 2023, by = 1))) +
  labs(title = "Exponential Decay: y = 0.9^x",
       x = "Year",
       y = "y (0.9^x)",
       color = "Legend") + 
  theme_minimal()

#print(p)
ggsave("Figures/4.2_exponential_decay_plot.pdf", plot = p, width = 8, height = 5, dpi = 300)

# Display the dataframe for verification
print(head(df))
print(head(Dtrain$total))

cat("\n------------- Exercise 4.4 -------------\n")

# Ensure df and Dtrain have the same length
if (nrow(df) != nrow(Dtrain)) {
  stop("Error: df and Dtrain must have the same number of rows.")
}

# Define x (year from df) and y (total from Dtrain)
x <- df$year
y <- Dtrain$total

# Number of observations
N <- length(y)

# Define weights (exponential decay with lambda = 0.9)
lambda <- 0.9
weights <- lambda^(N - seq(1, N))  # Recent data gets higher weights

# Fit WLS model using lm() with weights
model_wls <- lm(y ~ x, weights = weights)

# Extract estimated parameters (θ̂1 and θ̂2)
theta_hat <- coef(model_wls)
theta_1 <- theta_hat[1]  # Intercept (θ̂1)
theta_2 <- theta_hat[2]  # Slope (θ̂2)

# Print estimated parameters
cat("Estimated Parameters:\n")
cat("θ̂1 (Intercept) =", theta_1, "\n")
cat("θ̂2 (Slope) =", theta_2, "\n")

# Predict y values using the fitted WLS model
y_pred <- predict(model_wls, newdata = data.frame(x = x))

# Plot actual vs. predicted values and assign to variable p
p <- ggplot() +
  geom_point(aes(x = x, y = y, color = "Actual"), size = 2) +  
  geom_line(aes(x = x, y = y_pred, color = "Predicted"), linewidth = 1) +  
  scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue")) +
  labs(title = "WLS Regression: Predicting Total Using Year",
       x = "Year",
       y = "Total",
       color = "Legend") +
  theme_minimal()

#print(p)
ggsave("Figures/4.4_WLS_Regression_Plot.pdf", plot = p, width = 8, height = 5, dpi = 300)

cat("\nFirst few predicted values:\n")
print(head(y_pred))

cat("\n------------- Exercise 4.5.1 -------------\n")

# Print actual test data
cat("\nActual Test Set Total Values:\n")
print(Dtest$total)

cat("\nTest Set Year Values:\n")
print(Dtest$year)

# get the predicted value
predictions <- predict(model_wls, 
                       newdata = data.frame(x = Dtest$year), 
                       interval = "prediction", 
                       level = 0.95)  # 95% confidence interval

cat("\nPredicted Values with 95% Confidence Intervals:\n")
print(predictions)

# Create a dataframe to plot the actual vs predicted values
pred_df <- data.frame(
  year = Dtest$year,
  actual = Dtest$total,
  fit = predictions[, "fit"],
  lower = predictions[, "lwr"],
  upper = predictions[, "upr"]
)

# Generate the plot and assign to 'p'
p <- ggplot(pred_df, aes(x = year)) +
  # Actual data points (red)
  geom_point(aes(y = actual, color = "Actual Data"), size = 2) +
  # Predicted values (blue line)
  geom_line(aes(y = fit, color = "Predicted Values"), linewidth = 1) +
  # Confidence intervals (light blue ribbon)
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "Confidence Interval"), alpha = 0.3) +
  # Plot labels
  labs(title = "WLS Regression: Test Set Predictions with Confidence Intervals",
       x = "Year",
       y = "Total Vehicles") +
  theme_minimal() +
  # Custom color and fill scales for legend
  scale_color_manual(values = c("Actual Data" = "red", "Predicted Values" = "blue")) +
  scale_fill_manual(values = c("Confidence Interval" = "lightblue")) +
  theme(legend.position = "bottom")

#print(p)
ggsave("Figures/4.5.1_WLS_TestSet_Predictions.pdf", plot = p, width = 8, height = 5, dpi = 300)

cat("\n------------- Exercise 4.5.2 -------------\n")

# Compute predicted values for the training set (without prediction interval)
train_pred <- predict(model_wls, 
                      newdata = data.frame(x = df$year))  # No interval argument

# Compute predicted values and prediction intervals for the test set
# Suppress the constant prediction variance warning
test_pred <- suppressWarnings(predict(model_wls, 
                                      newdata = data.frame(x = Dtest$year), 
                                      interval = "prediction", 
                                      level = 0.95))

# Convert training set data
train_df <- data.frame(year = df$year, 
                       total = Dtrain$total,  # Actual values of the training set
                       fit = train_pred,  # Only predicted values
                       set = "Train")  # Label as training set

# Convert test set data
test_df <- data.frame(year = Dtest$year, 
                      total = Dtest$total,  
                      fit = test_pred[, "fit"],  
                      lwr = test_pred[, "lwr"], 
                      upr = test_pred[, "upr"],
                      set = "Test")  

# Add lwr and upr columns to train_df with NA values
train_df$lwr <- NA
train_df$upr <- NA

# Reorder columns to match the test_df column order
train_df <- train_df[, c("year", "total", "fit", "lwr", "upr", "set")]

# Merge training and test set data
plot_df <- rbind(train_df, test_df)

# Assign the plot to variable p
p <- ggplot(plot_df, aes(x = year)) +
  # Training set actual values (red points)
  geom_point(data = train_df, aes(y = total, color = "Training Actual"), size = 2) +
  # Test set actual values (green points)
  geom_point(data = test_df, aes(y = total, color = "Test Actual"), size = 3) +
  # Test set predicted values (blue triangles)
  geom_point(data = test_df, aes(y = fit, color = "Test Predicted"), size = 3, shape = 17) +
  # Training set fitted curve
  geom_line(data = train_df, aes(y = fit, color = "Training Fitted"), linewidth = 1) +
  # Test set predicted curve (dashed green line)
  geom_line(data = test_df, aes(y = fit, color = "Test Fitted"), linewidth = 1, linetype = "dashed") +
  # Prediction intervals
  geom_ribbon(data = test_df, aes(ymin = lwr, ymax = upr, fill = "Prediction Interval"), alpha = 0.3) +
  # X-axis range to cover both train and test data
  scale_x_continuous(limits = range(c(df$year, Dtest$year))) +
  # Plot labels
  labs(title = "WLS Regression: Train & Test Predictions with Prediction Intervals",
       x = "Year", 
       y = "Total") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Training Actual" = "red",
                                "Test Actual" = "green",
                                "Test Predicted" = "blue",
                                "Training Fitted" = "blue",
                                "Test Fitted" = "green")) +
  scale_fill_manual(values = c("Prediction Interval" = "lightblue"))

#print(p)
ggsave("Figures/4.5.2_WLS_Train_Test_Predictions.pdf", plot = p, width = 9, height = 7, dpi = 300)
