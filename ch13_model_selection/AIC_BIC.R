library(rstanarm)  
library(loo)       

# 데이터 정의
data <- c(1.1, 1.9, 2.3, 2.0, 1.8)  
n <- length(data)
data_frame <- data.frame(data=data)
n <- length(data)  # # Sample size (required for BIC calculation)

# AIC, BIC calculation
log_likelihood <- function(mu) {
  sum(dnorm(data, mean = mu, sd = 1, log = TRUE))
}

mle_mu <- optimize(log_likelihood, interval = c(0, 3), maximum = TRUE)$maximum
max_log_likelihood <- log_likelihood(mle_mu)

# AIC, BIC calculation
k <- 1  # parameter number
AIC <- -2 * max_log_likelihood + 2 * k
BIC <- -2 * max_log_likelihood + k * log(n)

cat(sprintf("AIC: %.4f\n", AIC))
cat(sprintf("BIC: %.4f\n", BIC))









