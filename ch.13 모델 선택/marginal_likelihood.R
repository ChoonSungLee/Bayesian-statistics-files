library(rstanarm)
# 데이터 준비
data_frame <- data.frame(
  y = c(1.1, 1.9, 2.3, 2.0, 1.8),  # 종속 변수
  x1 = c(0, 1, 0, 1, 0),           # 독립 변수 1
  x2 = c(1, 0, 1, 0, 1)            # 독립 변수 2
)

library(bridgesampling)

# 모델 적합 (stan_glm 또는 rstan 모델)

fit <- stan_glm(
  y ~ x1 + x2,
  data = data_frame,
  family = gaussian(),
  prior = normal(0, 10),
  prior_intercept = normal(0, 10),
  chains = 4,
  iter = 2000,
  diagnostic_file = "/Users/ChoonSungLee/Dropbox/KyeongwonLee_folder/notebooks_2nd/BDA/by_topic/model_check/diagnostics.csv"  # 진단 파일 경로 지정
)

# Bridge Sampling으로 Marginal Likelihood 계산
bridge_result <- bridge_sampler(fit)
marginal_likelihood <- logml(bridge_result)

# 결과 출력
cat(sprintf("Marginal Likelihood (log scale): %.4f\n", marginal_likelihood))

