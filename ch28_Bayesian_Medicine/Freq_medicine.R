
# ==============================================================================
# 1. 데이터 생성 (Bayesian 예제와 동일한 조건)
# ==============================================================================
set.seed(2024) # 결과 재현을 위해 시드 고정

n_history <- 100
history_data <- data.frame(
  Age = rnorm(n_history, 50, 10),
  MRI_Grade = sample(1:4, n_history, replace = TRUE),
  SLR_Positive = rbinom(n_history, 1, 0.6)
)

# 로지스틱 회귀 관계 설정 (True Model)
logit_p <- -5 + 0.05 * history_data$Age + 1.2 * history_data$MRI_Grade + 1.5 * history_data$SLR_Positive
prob_failure <- 1 / (1 + exp(-logit_p))
history_data$Treatment_Failure <- rbinom(n_history, 1, prob_failure)

# 새로운 환자 데이터
new_patient <- data.frame(
  Age = 45,
  MRI_Grade = 3,
  SLR_Positive = 1
)

# ==============================================================================
# 2. 빈도주의 모델 적합 (Standard Frequentist Approach)
# ==============================================================================
# [수정] 변수명을 데이터프레임의 컬럼명(MRI_Grade, SLR_Positive)과 정확히 일치시켰습니다.
freq_model <- glm(Treatment_Failure ~ Age + MRI_Grade + SLR_Positive, 
                  data = history_data, 
                  family = binomial(link = "logit"))

print("--- 빈도주의 모델 요약 (MLE) ---")
print(summary(freq_model))

# ==============================================================================
# 3. 예측 및 신뢰구간 (Prediction & Confidence Interval)
# ==============================================================================
# 빈도주의는 분포가 아닌 '점 추정치(Point Estimate)'를 줍니다.
# type = "link"를 하면 logit scale의 예측값과 표준오차(SE)를 줍니다.

pred_link <- predict(freq_model, newdata = new_patient, type = "link", se.fit = TRUE)

# (1) 점 추정치 (확률 변환)
fit_prob <- plogis(pred_link$fit) # plogis는 inverse-logit 함수입니다.

# (2) 95% 신뢰 구간 계산 (Confidence Interval)
# Logit scale에서 1.96 * SE를 더하고 뺀 뒤, 다시 확률로 변환합니다.
lower_bound <- plogis(pred_link$fit - 1.96 * pred_link$se.fit)
upper_bound <- plogis(pred_link$fit + 1.96 * pred_link$se.fit)

print("--- 신규 환자 예측 결과 ---")
print(paste("수술 필요 확률 (점 추정):", round(fit_prob, 4)))
print(paste("95% 신뢰 구간:", round(lower_bound, 4), "~", round(upper_bound, 4)))

# (3) 빈도주의적 의사결정 (단순 Cut-off)
if (fit_prob > 0.5) {
  print("결정: 수술 (확률 > 0.5)")
} else {
  print("결정: 보존치료")
}

