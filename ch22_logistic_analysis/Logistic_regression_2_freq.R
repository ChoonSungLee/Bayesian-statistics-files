# Set seed for reproducibility
set.seed(123)

# Define sample size and generate predictors
n <- 200 # number of sample
age <- rnorm(n, mean = 50, sd = 10)
blood_pressure <- rnorm(n, mean = 120, sd = 15)

# --- Revised Data Generation ---
# 1. Define the 'true' model coefficients (beta values).
# These values determine the true relationship between predictors and the outcome.
true_intercept <- -12
true_beta_age <- 0.15
true_beta_bp <- 0.05

# 2. Calculate the log-odds (z) for each individual.
# This is the linear predictor part of the model.
log_odds <- true_intercept + (true_beta_age * age) + (true_beta_bp * blood_pressure)

# 3. Convert log-odds to probabilities (p) using the sigmoid function.
# This gives the probability of having the disease for each person.
prob_disease <- 1 / (1 + exp(-log_odds))

# 4. Simulate the binary outcome (disease) based on the probabilities.
# For each person, draw a 0 or 1 based on their specific probability.
# This process follows the y ~ Ber(p(x)) model.
disease <- as.factor(rbinom(n, size = 1, prob = prob_disease))

# Create the final dataframe
data <- data.frame(age, blood_pressure, disease)

# The rest of the analysis code (ggplot, glm, etc.) can be used as is.

# Check the data
head(data)

# Data Visualization
library(ggplot2)
ggplot(data, aes(x = age, y = blood_pressure, color = disease)) +
  geom_point() +
  labs(title = "CV Disease by Age & Blood Pressure", x = "Age", y = "Blood Pressure")

# Fit the Logistic Regression model
model <- glm(disease ~ age + blood_pressure, data = data, family = binomial)

# Model Summary
summary(model)

# Prediction (probability)
data$predicted_prob <- predict(model, type = "response")

# 평균값 구하기
mean_prob <- mean(data$predicted_prob)
print(paste("Predicted probabilities mean:", round(mean_prob, 3)))

# Prediction (0 or 1)
data$predicted_class <- ifelse(data$predicted_prob > 0.5, 1, 0)

# Create Confusion Matrix
table(Predicted = data$predicted_class, Actual = data$disease)

# Calculate Accuracy
accuracy <- mean(data$predicted_class == data$disease)
print(paste("Model Accuracy:", round(accuracy * 100, 2), "%"))

# Visualize Distribution of Predicted Probabilities
ggplot(data, aes(x = predicted_prob, fill = disease)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(title = "Distribution of Predicted Probabilities", x = "Predicted Probability", y = "Frequency")

# Code to plot a simple sigmoid curve
# Define Sigmoid function
sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

# Generate x values
x <- seq(-10, 10, by = 0.1)

# Calculate y values
y <- sigmoid(x)

# Visualize the Sigmoid curve
library(ggplot2)
ggplot(data = data.frame(x, y), aes(x = x, y = y)) +
  geom_line() +
  labs(title = "Sigmoid Curve", x = "x", y = "Sigmoid(x)") +
  theme_minimal()

# Calculate Odds Ratios
coefficients <- coef(model)
odds_ratio_age <- exp(coefficients["age"]) # Odds Ratio for age
odds_ratio_bp <- exp(coefficients["blood_pressure"]) # Odds Ratio for blood pressure

# Print Results
print(paste("Odds ratio for 1-year increase in age:", round(odds_ratio_age, 2)))
print(paste("Odds ratio for 1-unit increase in blood pressure:", round(odds_ratio_bp, 2)))












