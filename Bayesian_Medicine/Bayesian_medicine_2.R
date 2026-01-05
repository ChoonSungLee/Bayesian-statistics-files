# 필요한 라이브러리 로드
library(rstanarm)
library(dplyr)
library(ggplot2)
library(bayesplot)

# ==============================================================================
# 1. 데이터 생성 (Historical Data & New Patient)
# ==============================================================================
# 현실에서는 병원 EMR 데이터라고 가정합니다.
set.seed(2024)

# (1) 과거 환자 100명의 데이터 생성 (학습 데이터)
# Predictors: Age(나이), MRI_Grade(1~4점), SLR_Positive(하지직거상 양성여부 0/1)
# Outcome: Treatment_Failure (1=보존치료 실패/수술필요, 0=보존치료 성공)
n_history <- 100
history_data <- data.frame(
  Age = rnorm(n_history, 50, 10),
  MRI_Grade = sample(1:4, n_history, replace = TRUE),
  SLR_Positive = rbinom(n_history, 1, 0.6)
)

# 로지스틱 회귀 관계 설정 (MRI가 심할수록, SLR 양성일수록 실패 확률 증가)
logit_p <- -5 + 0.05 * history_data$Age + 1.2 * history_data$MRI_Grade + 1.5 * history_data$SLR_Positive
prob_failure <- 1 / (1 + exp(-logit_p))
history_data$Treatment_Failure <- rbinom(n_history, 1, prob_failure)

# (2) 현재 내원한 '한 명의 환자' (분석 대상)
# 45세, MRI Grade 3(심각), SLR 양성(1)인 환자
new_patient <- data.frame(
  Age = 45,
  MRI_Grade = 3,
  SLR_Positive = 1
)

print("--- 신규 환자 정보 ---")
print(new_patient)

# ==============================================================================
# 2. Stan 모델 적합 (rstanarm 이용)
# ==============================================================================
# 과거 데이터를 통해 "어떤 환자가 보존치료에 실패하는가?"에 대한 모델을 학습합니다.
# Prior: rstanarm의 기본 Weakly Informative Prior를 사용 (현실적임)

post_model <- stan_glm(
  Treatment_Failure ~ Age + MRI_Grade + SLR_Positive,
  data = history_data,
  family = binomial(link = "logit"),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 2.5),
  chains = 4, iter = 2000, seed = 1234,
  refresh = 0 # 출력 줄이기
)

print("--- 모델 적합 완료 ---")
print(summary(post_model))

# ==============================================================================
# 3. MCMC 결과 해석 및 신규 환자 예측 (Posterior Prediction)
# ==============================================================================
# 이 환자가 보존치료에 실패할 확률(p)의 분포를 뽑아냅니다.
# posterior_linpred(transform = TRUE)는 로짓을 확률(0~1)로 변환해줍니다.

# 4000개의 MCMC 샘플 추출 (Posterior Samples for the specific patient)

p_samples <- posterior_linpred(post_model, newdata = new_patient, transform = TRUE)

# 결과는 4000 x 1 행렬입니다. 벡터로 변환.
p_failure_dist <- as.vector(p_samples) 

# 시각화: 이 환자의 실패 확률 분포
mcmc_hist <- ggplot(data.frame(p = p_failure_dist), aes(x = p)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_vline(xintercept = mean(p_failure_dist), color = "red", linetype = "dashed") +
  labs(title = "Posterior Distribution of Treatment Failure Probability",
       subtitle = "For the specific New Patient (Age 45, MRI 3, SLR+)",
       x = "Probability of Failure (Need for Surgery)", y = "Count") +
  theme_minimal()

print(mcmc_hist)
print(paste("이 환자의 보존치료 실패 예상 확률(평균):", round(mean(p_failure_dist), 3)))

# ==============================================================================
# 4. 효용 함수(Utility Function) 정의 및 기대 효용 계산
# ==============================================================================
# 상태(State): theta_1 (수술 필요함/치료실패), theta_2 (자연치유 가능)
# 행동(Action): a_surg (수술), a_cons (보존치료)
# QALY 기반 효용 점수 설정 (앞선 대화의 예시 적용)

# 효용 행렬 정의
U_surg_needed    <- 0.95  # 수술함 & 실제 수술 필요했음 (성공)
U_surg_notneeded <- 0.80  # 수술함 & 굳이 필요 없었음 (과잉진료)
U_cons_needed    <- 0.60  # 보존치료 & 실제 수술 필요했음 (고통 지속, 상태 악화)
U_cons_notneeded <- 0.90  # 보존치료 & 자연치유됨 (성공)

# 몬테카를로 적분 (Monte Carlo Integration)
# MCMC 샘플 하나하나(p_i)에 대해 기대 효용을 계산합니다.

# p_i: 환자가 '수술이 필요한 상태'일 확률 샘플
# 1-p_i: 환자가 '자연치유 될 상태'일 확률 샘플

# (1) 수술 선택 시 기대 효용 분포
EU_surg_samples <- p_failure_dist * U_surg_needed + (1 - p_failure_dist) * U_surg_notneeded

# (2) 보존치료 선택 시 기대 효용 분포
EU_cons_samples <- p_failure_dist * U_cons_needed + (1 - p_failure_dist) * U_cons_notneeded

# ==============================================================================
# 5. 최종 결정 (Decision Making)
# ==============================================================================
# 두 행동의 평균 기대 효용 비교

mean_EU_surg <- mean(EU_surg_samples)
mean_EU_cons <- mean(EU_cons_samples)

print("--- 최종 결정 분석 결과 ---")
print(paste("수술 선택 시 기대 효용 (E[U|Surgery]):", round(mean_EU_surg, 4)))
print(paste("보존 선택 시 기대 효용 (E[U|Conservative]):", round(mean_EU_cons, 4)))

# Optimal Action Selection
if (mean_EU_surg > mean_EU_cons) {
  decision <- "결정: 수술 (Surgery)"
  reason <- "수술의 기대 효용이 더 높음"
} else {
  decision <- "결정: 보존적 치료 (Conservative)"
  reason <- "보존적 치료의 기대 효용이 더 높음"
}

print(paste(decision, "-", reason))

# (심화) 수술이 더 나을 확률 (Probabilistic Superiority)
prob_surg_better <- mean(EU_surg_samples > EU_cons_samples)
print(paste("베이지안 확신도: 수술이 보존치료보다 더 나은 선택일 확률은", round(prob_surg_better * 100, 1), "% 입니다."))



