library(rstan)

# Observed data
y <- c(10, 12, 9, 11, 8, 14, 10, 9, 10, 13)
n <- length(y)

# Stan model for Bayesian p-value
stan_code <- "
data {
  int<lower=0> N;
  int<lower=0> y[N];
}
parameters {
  real<lower=0> lambda;
}
model {
  lambda ~ exponential(0.2);  // Prior
  y ~ poisson(lambda);      // Likelihood
}
generated quantities {
  real s_y_rep;
  real s_y = mean(y);  // Observed statistic
  int y_rep[N];
  for (n in 1:N)
    y_rep[n] = poisson_rng(lambda);
  s_y_rep = mean(y_rep);  // Replicated statistic
}
"

# Fit Stan model
fit <- stan(model_code = stan_code, data = list(N = n, y = y), seed = 1234)

# Extract statistics
s_y <- extract(fit, pars = "s_y")$s_y[1]  # Observed mean
s_y_rep <- extract(fit, pars = "s_y_rep")$s_y_rep  # Posterior predictive means

# Calculate Bayesian p-value
bayesian_p_value <- mean(s_y_rep >= s_y)
print(paste("Bayesian p-value:", bayesian_p_value))


