library(coda)

# 예: beta 분포에서 샘플 추출
samples <- rbeta(10000, 2, 5)

# 등확률구간
quantile(samples, probs = c(0.025, 0.975))

# HPD 구간
mcmc_samples <- as.mcmc(samples)
HPDinterval(mcmc_samples, prob = 0.95)

# 시각화
hist(samples, breaks = 50, col = "lightblue", probability = TRUE,
     main = "Posterior Distribution with 95% Intervals", xlab = "Parameter")
abline(v = quantile(samples, c(0.025, 0.975)), col = "red", lty = 2, lwd = 2)
abline(v = HPDinterval(mcmc_samples), col = "darkgreen", lty = 3, lwd = 2)
legend("topright", legend = c("Equal-tailed", "HPD"),
       col = c("red", "darkgreen"), lty = c(2,3), lwd = 2)
