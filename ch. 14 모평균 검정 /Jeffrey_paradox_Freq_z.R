
# ✅ 데이터 설정
mu_0 <- 100         # 귀무가설 평균
x_bar <- 104        # 표본 평균
s <- 10             # 표본 표준편차
n <- 30             # 표본 크기

# ✅ 빈도주의 z-test  (정규분포 가정, 모분산 알고 있을 때)
z_value <- (x_bar - mu_0) / (s / sqrt(n))
p_value <- 2 * (1 - pnorm(abs(z_value)))

cat("[빈도주의 z-test]\n")
cat("z-value =", round(z_value, 3), "\n")
cat("p-value =", round(p_value, 4), "\n\n")












