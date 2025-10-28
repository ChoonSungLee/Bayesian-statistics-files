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

# D_hat calculation (deviance from parameter mean)
mean_mu <- mean(posterior_samples[, "(Intercept)"])  
D_hat <- -2 * sum(dnorm(data, mean = mean_mu, sd = 1, log = TRUE))

# D_bar calculation (deviance mean from posterior sampling)
D_bar <- mean(-2 * log_lik)

# p_D, DIC calculation
p_D <- D_bar - D_hat
DIC <- D_bar + p_D
cat(sprintf("D_hat: %.4f\n", D_hat))
cat(sprintf("DIC: %.4f\n", DIC))









