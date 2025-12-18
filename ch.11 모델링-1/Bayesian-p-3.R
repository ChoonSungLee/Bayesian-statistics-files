
# Load necessary libraries
library(rstan)

# --- 1. Data Preparation ---
# For consistency with the previous analysis, generate virtual raw data
# similar to the summary statistics (mean=75, SD=10).
# Use set.seed() to ensure reproducibility of the data generation.
set.seed(123)
n <- 30
y_raw <- rnorm(n = n, mean = 75, sd = 10)

# Check the summary statistics of the generated data (for reference)
# print(paste("Sample Mean:", mean(y_raw)))
# print(paste("Sample SD:", sd(y_raw)))


# --- 2. Stan Model Definition ---
# Modified to directly model the 30 raw data points (y_raw)
stan_code_corrected <- "
data {
  int<lower=0> N;      // Number of data points (30)
  vector[N] y;         // 30 raw score data points
}

parameters {
  real mu;             // Population mean
  real<lower=0> sigma; // Population standard deviation
}

model {
  // Priors
  mu ~ normal(70, 10);      // Prior knowledge: 'national average is around 70'
  sigma ~ cauchy(0, 5);     // A common weakly informative prior for standard deviation

  // Likelihood - modeling the data generation process
  y ~ normal(mu, sigma);    // Model the 30 scores as following a normal distribution
}

generated quantities {
  vector[N] y_rep;          // Replicated dataset
  real s_y_mean;            // Mean of original data (observed statistic)
  real s_y_rep_mean;        // Mean of replicated data (replicated statistic)
  
  // Generate y_rep (sampling from the posterior predictive distribution)
  for (n in 1:N) {
    y_rep[n] = normal_rng(mu, sigma);
  }
  
  // Calculate statistics
  s_y_mean = mean(y);
  s_y_rep_mean = mean(y_rep);
}
"


# --- 3. Stan Model Execution (Fitting) ---
# Prepare the data list to pass to Stan
data_list_corrected <- list(N = n, y = y_raw)

# Fit the model
fit_corrected <- stan(
  model_code = stan_code_corrected,
  data = data_list_corrected,
  iter = 2000,
  chains = 4,
  seed = 456  # Set seed for all randomness inside Stan for reproducibility
)


# --- 4. Result Analysis and Bayesian p-value Calculation ---
# Extract posterior samples
posterior_samples <- extract(fit_corrected)

# Extract observed and replicated statistics
s_obs_mean <- posterior_samples$s_y_mean[1] # Observed mean is always the same, so extract only one
s_rep_means <- posterior_samples$s_y_rep_mean  # 4000 replicated means

# Correct calculation of the two-sided Bayesian p-value
prob_greater <- mean(s_rep_means > s_obs_mean)
prob_less_equal <- mean(s_rep_means <= s_obs_mean)
bayesian_p_value <- 2 * min(prob_greater, prob_less_equal)

# Print results
print(fit_corrected, pars = c("mu", "sigma"))
print(paste("Bayesian p-value (for the mean):", round(bayesian_p_value, 3)))


# --- 5. Visual Model Check (PPC Plot) ---
hist(s_rep_means, breaks = 30, main = "Posterior Predictive Check (Mean)",
     xlab = "Replicated Sample Means", col = "lightblue", border = "white",
     xlim = range(c(s_rep_means, s_obs_mean))) # Adjust x-axis limits
abline(v = s_obs_mean, col = "red", lwd = 3, lty = 2)
legend("topright", legend = "Observed Mean", col = "red", lty = 2, lwd = 3)

s_obs_mean



















