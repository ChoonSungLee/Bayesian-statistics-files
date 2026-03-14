# 필요한 패키지 설치 및 로드
if (!require("rstan")) install.packages(c("rstan"))
if (!require("bridgesampling")) install.packages(c("bridgesampling"))
library(rstan)
library(bridgesampling)

# rstan 병렬 처리 설정
options(mc.cores = parallel::detectCores())

# 1. 데이터 준비
observed_counts <- c(95, 55, 50)
null_proportions <- c(0.5, 0.3, 0.2)
K <- length(observed_counts)

stan_data_H0 <- list(K = K, y = observed_counts, p0 = null_proportions)
stan_data_H1 <- list(K = K, y = observed_counts)

# 2. Stan 모델 컴파일 및 실행

# 파라미터가 없는 귀무모델(H0)은 샘플링 없이 로그 주변부 가능도 직접 계산. 
log_LML_H0 <- dmultinom(x = observed_counts, prob = null_proportions, log = TRUE)

# 귀무 모델 (H0) 피팅
#fit_H0 <- stan(
#  file = "model_H0.stan", # 귀무 모델 파일
#  data = stan_data_H0,
#  chains = 4,
#  iter = 2000
#)
#LML_H0 <- bridge_sampler(fit_H0, silent = TRUE)

# 3. 대립모델에서는 적합 후 브릿지 샘플링으로 주변부 가능도 계산
# 대립 모델 (H1) 피팅
fit_H1 <- stan(
  file = "model_H1.stan", # 대립 모델 파일
  data = stan_data_H1,
  chains = 4,
  iter = 2000
)

LML_H1 <- bridge_sampler(fit_H1, silent = TRUE)

# 4. 베이즈 팩터(Bayes Factor) 계산
# BF01 = P(D|H0) / P(D|H1)
# BF01 > 1 이면 귀무가설(H0) 지지
#BF01 <- bf(LML_H0, LML_H1)

log_BF01 <- log_LML_H0 - LML_H1$logml
BF01 <- exp(log_BF01)

# 5. 결과 출력
print("귀무 모델(H0)의 로그 주변부 가능도:")
print(log_LML_H0)

print("대립 모델(H1)의 로그 주변부 가능도:")
print(LML_H1)

print("베이즈 팩터 (BF01):")
print(BF01)

# 해석
# BF01의 `bf` 값을 확인합니다. 0.222... 로 계산됩니다.
# 이 값은 0.33보다 작으므로 "대립가설에 대한 중간 정도의 증거"로 해석할 수 있습니다.
# 이는 근사 공식의 결과(BF=0.205)와 매우 유사합니다.






