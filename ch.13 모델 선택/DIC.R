library(rstanarm)  
library(loo)       

# 데이터 정의
data <- c(1.1, 1.9, 2.3, 2.0, 1.8)  
n <- length(data)
data_frame <- data.frame(data=data)

# fitting
fit <- stan_glm(data ~ 1, 
                data = data_frame,   
                family = gaussian(), 
                prior = normal(0, 10), 
                prior_intercept = normal(0, 10), 
                chains = 4,
                iter=2000,
                warmup=1000,
                seed=123)

# DIC calculation
posterior_samples <- as.matrix(fit)  
log_lik <- log_lik(fit)              

# D_hat calculation (deviance at parameter mean)
# Note: Since the model estimates sigma, mean_sigma must be used.
mean_mu <- mean(posterior_samples[,"(Intercept)"])  
mean_sigma <- mean(posterior_samples[,"sigma"])

D_hat <- -2 * sum(dnorm(data, mean = mean_mu, 
                        sd = mean_sigma, log = TRUE))

# D_bar calculation (mean of deviances)
# Sum log-likelihoods across all data points for each MCMC sample first.
sum_log_lik_per_iter <- rowSums(log_lik) 
D_bar <- mean(-2 * sum_log_lik_per_iter)

# p_D, DIC calculate
p_D <- D_bar - D_hat
DIC <- D_bar + p_D

cat(sprintf("D_hat: %.4f\n", D_hat))
cat(sprintf("D_bar: %.4f\n", D_bar))
cat(sprintf("p_D:   %.4f\n", p_D))
cat(sprintf("DIC:   %.4f\n", DIC))









