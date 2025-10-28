
# 평균과 분산이 모두 미지수인 정규분포 (이건 다루지 않았다)
# 필요한 라이브러리
library(rstan)
library(bayesplot)

# 데이터
data <- c(85, 87, 90, 88, 84, 89, 91, 86, 87, 88, 90, 85, 86, 89, 92)

# Stan 모델 코드
stan_code <- "
data {
  int<lower=0> N;
  real y[N];
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  mu ~ normal(88, 2); // 사전분포
  sigma ~ inv_gamma(2, 1); // 분산에 대한 사전분포
  y ~ normal(mu, sigma); // 우도함수
}
generated quantities {
  real sigma2;
  sigma2 = sigma * sigma; // 분산으로 변환
}
"

# 데이터 전달
stan_data <- list(
  N = length(data),
  y = data
)

# Stan 샘플링
fit <- stan(model_code = stan_code, data = stan_data, iter = 2000, chains = 4)

# 결과 요약
print(fit, pars = c("mu", "sigma", "sigma2"))

# 사후분포 시각화
posterior_samples <- extract(fit)
mu_samples <- posterior_samples$mu
sigma2_samples <- posterior_samples$sigma2

# 평균에 대한 신뢰구간
mu_ci <- quantile(mu_samples, probs = c(0.025, 0.975))
sigma2_ci <- quantile(sigma2_samples, probs = c(0.025, 0.975))

cat(sprintf("Posterior Mean of mu: %.2f\n", mean(mu_samples)))
cat(sprintf("95%% Credible Interval for mu: (%.2f, %.2f)\n", mu_ci[1], mu_ci[2]))
cat(sprintf("Posterior Mean of sigma²: %.2f\n", mean(sigma2_samples)))
cat(sprintf("95%% Credible Interval for sigma²: (%.2f, %.2f)\n", sigma2_ci[1], sigma2_ci[2]))

# 시각화
mcmc_trace(as.array(fit), pars = c("mu", "sigma2"))
mcmc_dens(as.array(fit), pars = c("mu", "sigma2"))


~~~~
