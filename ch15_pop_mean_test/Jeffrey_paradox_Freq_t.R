
# ✅ 데이터 설정
mu_0 <- 100      # 귀무가설 평균
x_bar <- 104     # 표본 평균
s <- 10          # 표본 표준편차
n <- 30          # 표본 크기

# ✅ 올바른 접근: t-test
# t-통계량 계산 (공식의 형태는 z-통계량과 동일)
t_value <- (x_bar - mu_0) / (s / sqrt(n))

# 자유도(df) 계산
df <- n - 1

# p-값 계산 (t-분포 함수 'pt' 사용)
p_value_t <- 2 * pt(abs(t_value), df = df, lower.tail = FALSE)

cat("[올바른 t-test]\n")
cat("t-value =", round(t_value, 3), "\n")
cat("p-value =", round(p_value_t, 4), "\n\n")




























