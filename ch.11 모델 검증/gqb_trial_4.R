library(rstan)

# Generate Poisson data
set.seed(123) # 재현성을 위해 시드 설정
n <- 200
lambda <- 10
y <- rpois(n, lambda)


# Fit Stan model with generated quantities
stan_code_with_generated <- "
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
  int y_rep[N];
  for (n in 1:N)
    y_rep[n] = poisson_rng(lambda);
}
"

# Stan fitting (includes GQB execution)
fit_with_generated <- stan(model_code = stan_code_with_generated, data = list(N = n, y = y))

# Extract posterior predictive data (y_rep)
y_rep_with_generated <- extract(fit_with_generated, "y_rep")$y_rep

# Plot PPC
ppc_dens_overlay(y, y_rep_with_generated[1:100, ])



