# -------------------------------------------------------------------
# 전립선암 치료법 3가지 비교 (Estimation + Testing + Prediction + Numerical Check)
# -------------------------------------------------------------------

# 1. 라이브러리 로드
library(rstan)
library(ggplot2)
library(dplyr)
library(bayesplot)

# 병렬 처리를 위한 옵션
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# -------------------------------------------------------------------
# 2. Stan 모델 정의 (통합 버전)
# -------------------------------------------------------------------
stan_code <- "
data {
  int<lower=1> K;          // 치료 그룹의 수 (3)
  int<lower=0> N[K];       // 각 그룹의 전체 환자 수
  int<lower=0> y[K];       // 각 그룹의 치료 성공 환자 수
}

parameters {
  vector<lower=0, upper=1>[K] theta; // 각 그룹의 성공 확률
}

model {
  // 사전 분포 (Prior)
  theta ~ beta(1, 1); 
  
  // 우도 함수 (Likelihood)
  y ~ binomial(N, theta);
}

generated quantities {
  // [PART 1] 파생 변수 계산 (그룹 간 비교)
  real diff_12 = theta[1] - theta[2]; // 수술 - IMRT
  real diff_13 = theta[1] - theta[3]; // 수술 - 양성자
  real diff_23 = theta[2] - theta[3]; // IMRT - 양성자
  
  real OR_12 = (theta[1] / (1 - theta[1])) / (theta[2] / (1 - theta[2]));
  
  // 수술이 가장 우수할 확률을 계산하기 위한 지시 변수
  int<lower=0, upper=1> is_best_1; 
  is_best_1 = (theta[1] > theta[2]) && (theta[1] > theta[3]); 

  // [PART 2] 사후 예측 (Posterior Predictive Check)
  // 현재 추정된 theta를 바탕으로 가상의 환자 데이터를 다시 생성
  int y_pred[K]; 
  for (k in 1:K) {
    y_pred[k] = binomial_rng(N[k], theta[k]);
  }
}
"

# -------------------------------------------------------------------
# 3. 데이터 준비
# -------------------------------------------------------------------
stan_data <- list(
  K = 3,
  N = c(50, 50, 50),     
  y = c(42, 40, 38)      
)

# -------------------------------------------------------------------
# 4. 모델 실행
# -------------------------------------------------------------------
fit <- stan(
  model_code = stan_code,
  data = stan_data,
  iter = 2000,
  chains = 4,
  seed = 12345
)

# -------------------------------------------------------------------
# 5. 결과 분석 및 시각화
# -------------------------------------------------------------------

# (1) 요약 통계량 출력 (모든 주요 변수 포함)
print(fit, 
      pars = c("theta", "diff_12", "diff_13", "diff_23", "OR_12", "is_best_1", "y_pred"), 
      probs = c(0.025, 0.5, 0.975))

# (2) 수술이 가장 우수할 확률 계산
posterior <- rstan::extract(fit)
prob_surgery_best <- mean(posterior$is_best_1)
cat(sprintf("\n수술이 세 가지 방법 중 성공률이 가장 높을 확률: %.1f%%\n", prob_surgery_best * 100))

# (3) [시각화 A] 치료법 간 차이 검증 (Density Plot)
plot_diff <- mcmc_areas(
  as.array(fit), 
  pars = c("diff_12", "diff_13", "diff_23"),
  prob = 0.95
) + ggtitle("Posterior Distributions of Treatment Differences")
print(plot_diff)

# (4) [시각화 B] 사후 예측 점검 (Graphical PPC) 
y_observed <- stan_data$y
y_pred_samples <- as.matrix(fit, pars = "y_pred")

# 수정: x에 문자열 대신 숫자(1:3)를 넣고 라벨을 붙임
plot_ppc <- ppc_intervals(
  y = y_observed, 
  yrep = y_pred_samples,
  x = 1:3 
) + 
  scale_x_continuous(
    breaks = 1:3,
    labels = c("Surgery", "IMRT", "Proton")
  ) +
  ggtitle("Observed vs. Predicted Outcomes (Graphical PPC)")
print(plot_ppc)

# (5) [수치적 검증] 사후 예측 점검 (Numerical PPC) - *새로 추가됨*
# 그래프로 본 예측 구간을 정확한 숫자로 확인

# y_pred의 요약 통계량 추출
fit_summary <- summary(fit, pars = "y_pred", probs = c(0.025, 0.5, 0.975))$summary

# 데이터 프레임 생성 (기존 stan_data 활용)

ppc_table <- data.frame(
  Treatment = c("Surgery", "IMRT", "Proton"),
  Observed_y = stan_data$y,                            # 실제 성공 수
  Predicted_Mean = round(fit_summary[, "mean"], 1),    # 예측 평균
  Lower_95 = round(fit_summary[, "2.5%"], 1),          # 예측 하한 (2.5%)
  Upper_95 = round(fit_summary[, "97.5%"], 1)          # 예측 상한 (97.5%)
)

cat("\n[Numerical PPC Table: 예측 범위 내에 실제값이 있는지 확인]\n")
print(ppc_table)

# Bayesian P-value 계산 (이상적 값 ~ 0.5)
# 실제값이 예측 분포의 어디쯤 위치하는지 확률로 계산
p_values <- colMeans(posterior$y_pred >= matrix(stan_data$y, nrow=4000, ncol=3, byrow=TRUE))
names(p_values) <- c("Surgery", "IMRT", "Proton")

cat("\n[Bayesian P-values (Ideal ~ 0.5, Extreme < 0.05 or > 0.95)]\n")
print(p_values)