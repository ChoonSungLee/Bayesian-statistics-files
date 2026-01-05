

# 1. 라이브러리 로드
library(rstan)
library(ggplot2)
library(dplyr)
library(bayesplot)

# 병렬 처리를 위한 옵션 (속도 향상)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# -------------------------------------------------------------------
# 2. Stan 모델 정의 (문자열로 저장)
# -------------------------------------------------------------------
stan_code <- "
data {
  int<lower=1> K;          // 치료 그룹의 수 (여기서는 3)
  int<lower=0> N[K];       // 각 그룹의 전체 환자 수 (배열)
  int<lower=0> y[K];       // 각 그룹의 치료 성공 환자 수 (배열)
}

parameters {
  vector<lower=0, upper=1>[K] theta; // 각 그룹의 성공 확률 (theta[1], theta[2], theta[3])
}

model {
  // 사전 분포 (Prior)
  // 정보가 없다고 가정하여 Uniform(Beta(1,1)) 분포 사용
  theta ~ beta(1, 1); 
  
  // 우도 함수 (Likelihood) - 벡터화된 코딩
  // 루프를 돌리지 않고 한 번에 기술 (Stan의 장점)
  y ~ binomial(N, theta);
}

generated quantities {
  // 사후 분포로부터 파생되는 값들을 계산하는 블록
  
  // 1. 그룹 간 성공률 차이 (Risk Difference)
  real diff_12; // 수술 vs IMRT
  real diff_13; // 수술 vs 양성자
  real diff_23; // IMRT vs 양성자
  
  // 2. 승산비 (Odds Ratio) - 로그 스케일 아님
  real OR_12; 
  
  // 3. 특정 그룹이 가장 우수할 확률을 계산하기 위한 지표
  int<lower=0, upper=1> is_best_1; // 수술이 1등인가?
  
  // 계산 로직
  diff_12 = theta[1] - theta[2];
  diff_13 = theta[1] - theta[3];
  diff_23 = theta[2] - theta[3];
  
  OR_12 = (theta[1] / (1 - theta[1])) / (theta[2] / (1 - theta[2]));
  
  // 수술(1)이 IMRT(2)보다 크고 AND 양성자(3)보다 클 경우 1, 아니면 0
  is_best_1 = (theta[1] > theta[2]) && (theta[1] > theta[3]); 
}
"

# -------------------------------------------------------------------
# 3. 데이터 준비 (R 리스트 형식)
# -------------------------------------------------------------------
# 그룹 인덱스: 1=수술, 2=IMRT, 3=양성자
stan_data <- list(
  K = 3,                 # 3개의 치료군
  N = c(50, 50, 50),     # 각 그룹 환자 수
  y = c(42, 40, 38)      # 각 그룹 성공 수
)

# -------------------------------------------------------------------
# 4. 모델 컴파일 및 MCMC 샘플링 실행
# -------------------------------------------------------------------
fit <- stan(
  model_code = stan_code,
  data = stan_data,
  iter = 2000,    # 총 반복 횟수
  warmup = 1000,  # 적응 기간 (버림)
  chains = 4,     # 체인 개수
  seed = 12345    # 재현성을 위한 시드
)

# -------------------------------------------------------------------
# 5. 결과 분석 및 시각화
# -------------------------------------------------------------------

# (1) 요약 통계량 출력
print(fit, pars = c("theta", "diff_12", "diff_13", "diff_23", "OR_12"), probs = c(0.025, 0.5, 0.975))

# (2) 사후 표본 추출 (Posterior Samples)
posterior <- rstan::extract(fit)

# (3) 수술(theta[1])이 다른 치료법보다 우수할 확률 계산
prob_surgery_best <- mean(posterior$is_best_1)
cat(sprintf(\"수술이 세 가지 방법 중 성공률이 가장 높을 확률: %.1f%%\\n\", prob_surgery_best * 100))

# (4) 수술 vs IMRT 차이 시각화 (Density Plot)
mcmc_areas(
  as.array(fit), 
  pars = c("diff_12", "diff_13", "diff_23"),
  prob = 0.95 # 95% 신뢰구간 표시
) + ggtitle("Posterior Distributions of Treatment Differences")


