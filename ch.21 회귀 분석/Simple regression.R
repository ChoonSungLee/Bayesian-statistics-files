# 회귀분석 예제

data <- data.frame(
  X = c(1, 2, 3, 4, 5),  # 독립변수
  Y = c(2, 4, 5, 4, 5)   # 종속변수
)

# 단순 선형 회귀 분석
model <- lm(Y ~ X, data = data)

# 회귀 결과 출력
summary(model)

# 필요한 패키지 설치
if (!require ("rstanarm")) install.packages("rstanarm") 
if (!require ("bayesplot")) install.packages("bayesplot") 

# 패키지 로드
library(rstanarm)
library(bayesplot)

# 베이지안 선형 회귀 모델
bayesian_model <- stan_glm(Y ~ X, data = data, family = gaussian,
                           prior = normal(0, 10),  
                           prior_intercept = normal(0, 10), 
                           chains = 4, iter = 2000, seed = 123)

# 모델 요약
summary(bayesian_model)

# 사후분포 시각화
plot(bayesian_model)

# 계수에 대한 사후분포 확인
posterior <- as.matrix(bayesian_model)
mcmc_areas(posterior, pars = c("(Intercept)", "X"))

# 사후 진단 플롯
pp_check(bayesian_model)  # 예측 점검
