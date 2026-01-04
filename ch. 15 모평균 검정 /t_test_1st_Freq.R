# 표본 데이터 설정
sample_mean <- 172      # 표본 평균
sample_sd <- 9          # 표본 표준편차
n <- 20                 # 표본 크기
mu_0 <- 170             # 귀무가설 하의 모집단 평균

# t-통계량 계산
t_stat <- (sample_mean - mu_0) / (sample_sd / sqrt(n))

# 자유도 계산
df <- n - 1

# p-값 계산 (양측 검정)
p_value <- 2 * pt(-abs(t_stat), df)

# 결과 출력
cat("t-통계량:", t_stat, "\n")
cat("자유도:", df, "\n")
cat("p-값:", p_value, "\n")

# 유의수준과 비교
if (p_value < 0.05) {
  cat("결론: p-값이 0.05보다 작으므로 귀무가설을 기각합니다.\n")
} else {
  cat("결론: p-값이 0.05보다 크므로 귀무가설을 기각하지 않습니다.\n")
}












