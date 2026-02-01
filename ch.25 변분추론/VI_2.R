# 필요한 패키지
library(ggplot2)

# 고정된 관측 데이터
x_obs <- 1.5

# p(z) ~ N(0,1), p(x|z) ~ N(z,1)
# q(z) ~ N(mu, sigma^2)

# ELBO 계산 함수
elbo <- function(mu, sigma, x) {
  # log p(x|z) term (E_q)
  recon_term <- -0.5 * ( (x - mu)^2 + sigma^2 )  # E_q[log p(x|z)]
  
  # KL(q || p): KL between N(mu, sigma^2) and N(0,1)
  kl <- log(1 / sigma) + (sigma^2 + mu^2 - 1) / 2
  
  return(recon_term - kl)
}

# (좌표상승 방식) 최적화 
optimize_vi <- function(x, mu_init=0, sigma_init=1, lr=0.1, steps=50) {
  mu <- mu_init
  sigma <- sigma_init
  trace <- data.frame(step = 0, mu = mu, sigma = sigma, elbo = elbo(mu, sigma, x))
  
  for (i in 1:steps) {
    # 좌표상승 업데이트 방식: mu 고정하고 sigma, sigma 고정하고 mu, 번갈아 업데이트
    grad_mu <- (x - mu) - mu  # ∂ELBO/∂mu (근사도 고려)
    grad_sigma <- -1 / sigma + sigma - sigma  # ∂ELBO/∂sigma 근사
    
    mu <- mu + lr * grad_mu
    sigma <- max(0.1, sigma + lr * grad_sigma)  # sigma는 0보다 커야 함
    
    trace <- rbind(trace, data.frame(step = i, mu = mu, sigma = sigma, elbo = elbo(mu, sigma, x)))
  }
  return(trace)
}

# 최적화 실행
result <- optimize_vi(x_obs, steps=50)

# 결과 시각화

ggplot(result, aes(x = step)) +
  geom_line(aes(y = elbo), color = "blue", size = 1.2) +
  labs(title="ELBO Optimization Process (Coordinate Ascent Variational Inference)", y = "ELBO", x = "Step") +
  # labs(title = "ELBO 최적화 과정 (좌표상승 변분 추론)", y = "ELBO", x = "Step") +
  theme_minimal()
