### (1) Known standard deviation (sigma = 3)

# 가정
sigma <- 3

n <- 10
xbar <- 20
alpha <- 0.05

# z-임계값 (표준정규에서 양쪽 2.5% 꼬리)
z_crit <- qnorm(1 - alpha/2)   # 0.975 분위수, 약 1.96

# 신뢰구간 계산
se <- sigma / sqrt(n)
margin <- z_crit * se
ci_lower <- xbar - margin
ci_upper <- xbar + margin

cat("== Case (1): Known sigma ==\n")
cat("Sample mean:", xbar, "\n")
cat("Known sigma:", sigma, "\n")
cat("z-critical :", z_crit, "\n")
cat(sprintf("95%% CI for mu: [%.2f, %.2f]\n\n", ci_lower, ci_upper))
