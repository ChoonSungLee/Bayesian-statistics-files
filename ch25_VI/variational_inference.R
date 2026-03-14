library(rstan)

# 관측 데이터
x_data <- c(1, 0, 1, 1, 0)
N <- length(x_data)
data_list <- list(N = N, x = x_data)

# 모델 컴파일
stan_model <- stan_model("bernoulli_beta.stan")

# 변분 추론 (vb = variational bayes)
fit_vb <- vb(
  stan_model,
  data = data_list,
  output_samples = 1000
)

# 결과 요약
print(fit_vb, pars = "p")

#시각화
posterior_samples <- extract(fit_vb)$p
hist(posterior_samples, breaks = 30, col = "skyblue", main = "Posterior of p (VI)", xlab = "p")
abline(v = mean(posterior_samples), col = "red", lwd = 2)

