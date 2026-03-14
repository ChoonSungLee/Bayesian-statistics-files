
# 1. Stan/MCMC를 돌려서 사후 표본(Posterior Samples)을 얻음
# fit <- stan(model_code = ..., data = ...)
# posterior_samples <- extract(fit)$theta  # 4000개의 theta 샘플

# 예: theta는 질병의 중증도 (0 ~ 1 사이의 연속 변수라고 가정)
theta_samples <- c(0.2, 0.8, 0.6, 0.9, 0.7, 0.5, 0.12) # MCMC 결과물

# 2. 효용 함수 정의 (예: 수술의 효용)
utility_surgery <- function(theta) {
  # 중증도(theta)가 높으면 수술 효용이 높고, 낮으면 효용이 낮음
  return(100 * theta - 30) 
}

# 3. 몬테카를로 적분으로 기대 효용(Expected Utility) 계산
# "적분 대신 평균(Mean)을 사용"
EU_surgery <- mean(utility_surgery(theta_samples))

print(paste("수술의 기대 효용:", EU_surgery))
