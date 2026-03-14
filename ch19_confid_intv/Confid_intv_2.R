### (2) Unknown standard deviation (s = 4)

# 가정
s <- 4
n <- 10
xbar <- 20
alpha <- 0.05
df <- n - 1

# t-임계값
t_crit <- qt(1 - alpha/2, df=df)  # 약 2.262 (df=9에서)
se <- s / sqrt(n)                 # 표본표준편차를 이용한 표준오차
margin <- t_crit * se
ci_lower <- xbar - margin
ci_upper <- xbar + margin

cat("== Case (2): Unknown sigma, using s ==\n")
cat("Sample mean:", xbar, "\n")
cat("Sample sd:", s, "\n")
cat("t-critical:", t_crit, "(df=", df, ")\n")
cat(sprintf("95%% CI for mu: [%.2f, %.2f]\n", ci_lower, ci_upper))
