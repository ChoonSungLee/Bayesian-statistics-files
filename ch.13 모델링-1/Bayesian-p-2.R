# Step 1: Observed data
set.seed(123)
n <- 200
lambda <- 10
y <- rpois(n, lambda)

# Calculate the observed statistic (e.g., mean of y)
s_obs <- mean(y)

# Step 2: Fit Stan model with generated quantities for posterior predictive data
library(rstan)

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
generated quantities {
  real s_y_rep;
  int y_rep[N];
  for (n in 1:N) {
    y_rep[n] = poisson_rng(lambda);
  }
  s_y_rep = mean(y_rep);  // Calculate mean of replicated data
}
"

# Compile Stan model
fit <- stan(model_code = stan_code, data = list(N = n, y = y))

# Step 3: Extract posterior predictive statistics (s_y_rep)
s_y_rep <- extract(fit, pars = "s_y_rep")$s_y_rep

# Step 4: Calculate Bayesian p-value
p_value <- mean(s_y_rep >= s_obs)
print(paste("Bayesian p-value:", p_value))

















