# 필요한 패키지 로드
library(rstan)

# 데이터 생성
set.seed(123)
n <- 100
true_mu <- 5
sigma <- 1
y <- rnorm(n, mean = true_mu, sd = sigma)

# Stan 모델 정의
stan_model_code <- "
data {
  int<lower=0> N;
  real y[N];
  real<lower=0> sigma;
}
parameters {
  real mu;
}
model {
  mu ~ normal(0, 10);     // 사전
  y ~ normal(mu, sigma);  // 가능도
}
"

# 데이터 준비
stan_data <- list(N = n, y = y, sigma = sigma)

# MCMC로 샘플링
fit_mcmc <- stan(model_code = stan_model_code, data = stan_data,
                 iter = 2000, chains = 4, seed = 123)

# 변분 추론 (VI)
fit_vi <- vb(stan_model(model_code = stan_model_code), data = stan_data,
             output_samples = 1000, seed = 123)

# 결과 비교
print(fit_mcmc, pars = "mu")
print(fit_vi, pars = "mu")

# 시각화 비교
posterior_mcmc <- extract(fit_mcmc)$mu
posterior_vi <- extract(fit_vi)$mu

hist(posterior_mcmc, breaks = 30, probability = TRUE, col = rgb(0, 0, 1, 0.3),
     main = "Posterior of mu: MCMC (blue) vs VI (red)", xlab = "mu")
hist(posterior_vi, breaks = 30, probability = TRUE, col = rgb(1, 0, 0, 0.3), add = TRUE)
legend("topright", legend = c("MCMC", "VI"), fill = c(rgb(0,0,1,0.3), rgb(1,0,0,0.3)))
