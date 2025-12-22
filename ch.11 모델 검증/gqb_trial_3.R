library(rstan)

# Generate Poisson data 
set.seed(123) # Set seed for fair comparison
n <- 200
lambda <- 10
y <- rpois(n, lambda)

# Fit Stan model without generated quantities
stan_code <- "
data {
  int<lower=0> N;
  int<lower=0> y[N];
}
parameters {
  real<lower=0> lambda;
}
model {
  lambda ~ exponential(1);
  y ~ poisson(lambda);
}
"

fit <- stan(model_code = stan_code, data = list(N = n, y = y))

# Extract posterior samples for lambda
lambda_samples <- extract(fit, "lambda")$lambda

# Generate posterior predictive data in R
y_rep <- replicate(length(lambda_samples), rpois(n, lambda_samples))

# Plot PPC
library(bayesplot)
ppc_dens_overlay(y, y_rep[1:100, ])



