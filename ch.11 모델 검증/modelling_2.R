library(tidyverse)
library(rstan)
library(bayesplot)
library(mltools)
library(gridExtra)
library(rstanarm)

n <- 200
y_nb <- rnbinom(n, 10, 1/2) # same mean as poisson

poisson_model <- "
data {
  int<lower=0> N;
  array[N] int<lower=0> y;
}
transformed data {
  real<lower=0> mean_y = mean(to_vector(y));
  real<lower=0> sd_y = sd(to_vector(y));
}
parameters {
  real<lower=0> lambda;
}
model {
  y ~ poisson(lambda);
  lambda ~ exponential(0.2);
}
generated quantities {
  array[N] int<lower=0> y_rep = poisson_rng(rep_array(lambda, N));
  // the below are for posterior p values.
  real<lower=0> mean_y_rep = mean(to_vector(y_rep));
  real<lower=0> sd_y_rep = sd(to_vector(y_rep));
  int<lower=0, upper=1> mean_gte = (mean_y_rep >= mean_y);
  int<lower=0, upper=1> sd_gte = (sd_y_rep >= sd_y);
}
"

pois_stan_model <- stan_model(model_code = poisson_model)

fit_nb <- sampling(pois_stan_model, data = list(N = n, y = y_nb), chains = 4, iter = 1000, warmup = 500, seed = 123)
#fit_poisson <- sampling(pois_stan_model, data = list(N = n, y = y), chains = 4, iter = 1000, warmup = 500, seed = 123)

#| fig.cap='Posterior predictive distribution'

print(fit_nb)

extract(fit_nb)

posterior_predictive <- extract(fit_nb, pars = "y_rep")[[1]]

g_nb <- data.frame(nb_sample = y_nb) %>%
  ggplot(aes(x = nb_sample)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Posterior predictive distribution", x = "Number of events", y = "density")
for (i in 1:100) {
  g_nb <- g_nb + 
    geom_density(data = data.frame(nb_sample = posterior_predictive[i, ]), 
                 colour = "blue", linewidth = 0.2)
}

g_nb <- g_nb +
  geom_density(aes(y = after_stat(density)),  
               colour = "red", linewidth = 2)

g_nb





