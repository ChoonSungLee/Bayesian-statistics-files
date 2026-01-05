# 라이브러리 로드
library(rstan)
library(dplyr)

# 병렬 처리를 위한 옵션 (속도 향상)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# ==============================================================================
# 1. 데이터 준비 (Data Preparation)
# ==============================================================================
# rstan은 데이터프레임을 바로 받지 못하고, 'List' 형태로 줘야 합니다.

# (1) 과거 환자 데이터 (Training Data)
N <- 100
set.seed(2024)
age <- rnorm(N, 50, 10)
mri <- sample(1:4, N, replace = TRUE)
slr <- rbinom(N, 1, 0.6)
# 로지스틱 관계 생성
logit_p <- -5 + 0.05 * age + 1.2 * mri + 1.5 * slr
fail_prob <- 1 / (1 + exp(-logit_p))
y <- rbinom(N, 1, fail_prob) # 1=치료실패(수술필요), 0=성공

# (2) 새로운 환자 데이터 (New Patient for Prediction)
new_age <- 45
new_mri <- 3
new_slr <- 1

# (3) Stan에 넘겨줄 데이터 리스트
stan_data <- list(
  N = N,                # 환자 수
  age = age,            # 예측 변수들
  mri = mri,
  slr = slr,
  y = y,                # 결과 변수
  # -- 예측을 위한 단일 환자 데이터 --
  new_age = new_age,
  new_mri = new_mri,
  new_slr = new_slr
)

# ==============================================================================
# 2. Stan 모델 코드 작성 (The Engine)
# ==============================================================================
# 문자열(String) 형태로 작성합니다. 별도 .stan 파일로 저장해도 됩니다.

stan_code <- "
data {
  // 1. 입력 데이터 선언
  int<lower=0> N;          // 데이터 개수
  vector[N] age;           // 나이
  vector[N] mri;           // MRI 등급
  vector[N] slr;           // SLR 검사 결과
  int<lower=0, upper=1> y[N]; // 치료 실패 여부 (0 or 1)
  
  // 2. 예측할 환자 데이터
  real new_age;
  real new_mri;
  real new_slr;
}

parameters {
  // 3. 추정해야 할 미지수 (회귀 계수들)
  real alpha;       // 절편
  real beta_age;    // 나이 계수
  real beta_mri;    // MRI 계수
  real beta_slr;    // SLR 계수
}

model {
  // 4. 사전 확률 (Priors) - Weakly Informative
  alpha ~ normal(0, 5);
  beta_age ~ normal(0, 5);
  beta_mri ~ normal(0, 5);
  beta_slr ~ normal(0, 5);

  // 5. 우도 (Likelihood) - 로지스틱 회귀
  // bernoulli_logit 함수는 logit 변환과 베르누이 분포를 한 번에 처리함
  y ~ bernoulli_logit(alpha + beta_age * age + beta_mri * mri + beta_slr * slr);
}

generated quantities {
  // 6. 생성량 블록: 사후 표본을 이용해 2차 가공(예측, 효용 계산)을 하는 곳
  
  // (1) 이 환자의 실패 확률(p) 예측
  real p_fail;
  p_fail = inv_logit(alpha + beta_age * new_age + beta_mri * new_mri + beta_slr * new_slr);
  
  // (2) 효용(Utility) 계산
  // 변수 선언
  real u_surg; // 수술 선택 시 효용
  real u_cons; // 보존치료 선택 시 효용
  
  // 효용 상수 (QALY 등) - 코드 안에 하드코딩하거나 data 블록에서 받아와도 됨
  real U_s_fail = 0.95; // 수술 O, 병변 O (성공)
  real U_s_none = 0.80; // 수술 O, 병변 X (과잉)
  real U_c_fail = 0.60; // 보존 O, 병변 O (실패/고통)
  real U_c_none = 0.90; // 보존 O, 병변 X (성공)
  
  // 기대 효용 계산식 (p_fail은 병변이 있어 수술이 필요한 상태일 확률)
  u_surg = p_fail * U_s_fail + (1 - p_fail) * U_s_none;
  u_cons = p_fail * U_c_fail + (1 - p_fail) * U_c_none;
  
  // (3) 최적 결정 (Difference)
  real diff_utility;
  diff_utility = u_surg - u_cons; // 양수면 수술이 이득, 음수면 보존이 이득
}
"

# ==============================================================================
# 3. 모델 적합 (Fitting)
# ==============================================================================
fit <- stan(
  model_code = stan_code,
  data = stan_data,
  iter = 2000,
  chains = 4,
  seed = 1234,
  refresh = 0
)

# ==============================================================================
# 4. 결과 해석 (Interpretation)
# ==============================================================================
# generated quantities 블록에서 계산한 변수들만 뽑아봅니다.
results <- extract(fit, pars = c("p_fail", "u_surg", "u_cons", "diff_utility"))

# (1) 실패 확률 분포 확인
print(paste("환자의 수술 필요 확률(평균):", round(mean(results$p_fail), 3)))

# (2) 기대 효용 비교
mean_u_surg <- mean(results$u_surg)
mean_u_cons <- mean(results$u_cons)

print(paste("수술 기대 효용:", round(mean_u_surg, 4)))
print(paste("보존 기대 효용:", round(mean_u_cons, 4)))

# (3) 최종 결정
prob_surg_better <- mean(results$diff_utility > 0)
print(paste("수술이 더 나을 확률:", round(prob_surg_better * 100, 1), "%"))

# 시각화 (선택 사항)
hist(results$diff_utility, main="Difference in Utility (Surgery - Conservative)", 
     xlab="Difference (Positive = Surgery Preferred)", col="lightblue", breaks=30)
abline(v=0, col="red", lwd=2)