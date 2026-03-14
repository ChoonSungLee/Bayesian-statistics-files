
#Bayesian p value를 구하는 코드이다. 
# 데이터
n <- 100           # 총 실험 대상 수
successes <- 65    # 성공한 사례 수

library(rstan)

# Stan 모델 코드
stan_code <- "
data {
  int<lower=0> n;         // 총 실험 대상 수
  int<lower=0> successes; // 성공한 사례 수
}

parameters {
  real<lower=0, upper=1> p; // 성공률
}

model {
  // Prior
  p ~ beta(1, 1); // Uniform prior (Beta(1, 1))
  
  // Likelihood
  successes ~ binomial(n, p);
}

generated quantities {
  int predicted_successes; // 사후 예측 성공 사례 수
  predicted_successes = binomial_rng(n, p);
}
"

# 데이터 준비
data_list <- list(
  n = 100,
  successes = 65
)

# Stan 모델 실행
fit <- stan(model_code = stan_code, data = data_list, iter = 2000, chains = 4, seed = 1234)

# 사후 분포에서 추출
posterior <- extract(fit)
predicted_successes <- posterior$predicted_successes

# 관측된 성공 사례보다 극단적인 값 계산
observed_successes <- 65

# 올바른 방식의 양측 베이즈 p-값 계산
prob_greater_equal <- mean(predicted_successes >= observed_successes)
prob_less_equal <- mean(predicted_successes <= observed_successes)

# 더 작은 쪽 꼬리 확률에 2를 곱함
bayesian_p_value_corrected <- 2 * min(prob_greater_equal, prob_less_equal)

# 결과 출력
print(paste("Bayesian p-value:", bayesian_p_value_corrected))
