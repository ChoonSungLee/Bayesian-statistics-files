#예제 3; 빈도주의 p값
# 데이터
observed_mean <- 75  # 관측된 평균
expected_mean <- 70  # 귀무가설 평균
sample_std <- 10     # 표본 표준편차
n <- 30              # 표본 크기

# 검정 통계량 계산 (t-value)
t_value <- (observed_mean - expected_mean) / (sample_std / sqrt(n))

# 자유도
df <- n - 1

# p-value 계산 (양측 검정)
p_value <- 2 * (1 - pt(abs(t_value), df))

# 결과 출력
cat("t-value:", t_value, "\n")
cat("p-value:", p_value, "\n")
