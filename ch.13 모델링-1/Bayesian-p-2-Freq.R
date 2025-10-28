# Step 1: Observed data
set.seed(345)
n <- 200
lambda_true <- 10 # 실제 데이터 생성에 사용된 모수
y <- rpois(n, lambda_true)

# --- 올바른 빈도주의 p값 계산 ---

# 1. 데이터와 독립적인 귀무가설 설정
lambda_null <- 9.5 # 예: "이전 연구 결과가 9.5였다"

# 2. 관측 통계량 계산
s_obs <- mean(y)

# 3. 표준 오차 계산 (포아송 분포의 분산은 평균과 같으므로)
se <- sqrt(lambda_null / n)

# 4. z-점수 계산
z_score <- (s_obs - lambda_null) / se

# 5. 양측 검정(two-sided test) p-값 계산
p_value_freq <- 2 * pnorm(-abs(z_score))

# 결과 출력
print(paste("Frequentist p-value (z-test):", p_value_freq))


