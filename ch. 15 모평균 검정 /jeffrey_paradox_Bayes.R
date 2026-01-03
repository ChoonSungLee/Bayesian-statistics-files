
# ✅ 데이터 설정
mu_0 <- 100         # 귀무가설 평균
x_bar <- 104        # 표본 평균
s <- 10             # 표본 표준편차
n <- 30             # 표본 크기

# ✅ Bayes factor 이용
# 패키지 설치
if(!require("BayesFactor")) install.packages("BayesFactor")
library(BayesFactor)

# 예제 데이터 생성
set.seed(123)
sample_data <- scale(rnorm(30)) * 10 + 104

# Bayes Factor 계산 (한 표본 t-test, 정규성 가정)
bf_result <- ttestBF(x = sample_data, mu = 100)

# 베이즈 팩터 계산
bf_result <- ttestBF(x = sample_data, mu = 100)
print(bf_result)










# (참고용) 아래 코드는 모분산을 모르는 상태에 정규-정규 모델을 적용한 오류를 범한 코드임.
# ✅ 베이즈 방식: 정규 사전, 정규 우도 → 정규 사후
# 사전분포: N(mu_prior, sigma_prior^2)
mu_prior <- 100
sigma_prior <- 10

# 사후분포 계산
posterior_mean <- (n * x_bar / s^2 + mu_prior / sigma_prior^2) /
  (n / s^2 + 1 / sigma_prior^2)

posterior_sd <- sqrt(1 / (n / s^2 + 1 / sigma_prior^2))

# P(mu > mu_0) 계산
prob_mu_gt_mu0 <- 1 - pnorm(mu_0, mean = posterior_mean, sd = posterior_sd)

cat("📊 [베이즈 정규-정규 모델]\n")
cat("사후 평균 =", round(posterior_mean, 3), "\n")
cat("사후 표준편차 =", round(posterior_sd, 3), "\n")
cat("P(mu > 100) =", round(prob_mu_gt_mu0, 4), "\n")




