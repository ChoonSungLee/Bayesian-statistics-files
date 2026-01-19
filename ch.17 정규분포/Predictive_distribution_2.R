
# 1. Data Generation (Common for both)
set.seed(42)
data <- rnorm(30, mean = 10, sd = 2)
n <- length(data)

# 2. Bayesian Posterior Predictive Distribution
# Posterior parameter calculation (Analytical)
mu0 <- 0
sigma2 <- 4
tau2 <- 100
x_bar <- mean(data)

posterior_mean <- (mu0/tau2 + n*x_bar/sigma2) / (1/tau2 + n/sigma2)
posterior_sd <- sqrt(1 / (1/tau2 + n/sigma2))

# Sampling (Monte Carlo)
mu_samples <- rnorm(1000, posterior_mean, posterior_sd)
y_rep_bayes <- rnorm(1000, mean = mu_samples, sd = sqrt(sigma2))
hist(y_rep_bayes, breaks = 30, main = "Bayesian Predictive", col = "lightgreen")

# 3. Frequentist Predictive Distribution
sample_mean <- mean(data)
sample_sd <- sd(data)

# Prediction using fixed point estimates
y_rep_freq <- rnorm(1000, mean = sample_mean, sd = sample_sd)
hist(y_rep_freq, breaks = 30, main = "Frequentist Predictive", col = "skyblue")
