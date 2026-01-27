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
# --- End of Revision ---

# Create the final dataframe
data <- data.frame(age, blood_pressure, disease)

# The rest of the analysis code (ggplot, glm, etc.) can be used as is.

# Check the data
head(data)


# Bayesian logistic regression
# Install brms package (if needed)
if (!require("brms")) install.packages("brms")

# Load the brms library
library(brms)

# Fit the Bayesian logistic regression model
bayes_model <- brm(disease ~ age + blood_pressure, data = data, family = bernoulli(), seed = 123)

# Summary of the model
summary(bayes_model)

# Check the posterior distribution samples
posterior_samples <- posterior_samples(bayes_model)
head(posterior_samples)

# Visualization of the posterior distribution
mcmc_plot(bayes_model, type = "trace")




