# 필요한 패키지
library(ggplot2)


# 고정된 관측 데이터
x_obs <- 1.5


# ELBO 계산 함수 (원본 파일에서 가져옴)
elbo <- function(mu, sigma, x) {
  # log p(x|z) term (E_q)
  recon_term <- -0.5 * ( (x - mu)^2 + sigma^2 )
  
  # KL(q || p): KL between N(mu, sigma^2) and N(0,1)
  kl <- log(1 / sigma) + (sigma^2 + mu^2 - 1) / 2
  
  return(recon_term - kl)
}


# Softplus 함수와 그 역함수 정의
softplus <- function(x) {
  log(1 + exp(x))
}
inverse_softplus <- function(y) {
  log(exp(y) - 1)
}


# Softplus를 사용하도록 수정한 최적화 함수
optimize_vi_softplus <- function(x, mu_init = 0, sigma_init = 1, lr = 0.01, steps = 50) {
  mu <- mu_init
  
  # 1. sigma 대신 범위 제약이 없는 'sigma_raw' 파라미터를 초기화합니다.
  # sigma_init=1에 해당하는 raw 값을 역함수로 계산합니다.
  sigma_raw <- inverse_softplus(sigma_init)
  
  # 초기 값을 기록하기 위해 sigma를 계산합니다.
  sigma <- softplus(sigma_raw)
  trace <- data.frame(step = 0, mu = mu, sigma = sigma, elbo = elbo(mu, sigma, x))
  
  for (i in 1:steps) {
    # mu 업데이트는 이전과 동일
    grad_mu <- (x - mu) - mu
    mu <- mu + lr * grad_mu
    
    # --- sigma 업데이트 과정 ---
    # a. 현재 sigma_raw로부터 sigma 값을 계산
    sigma <- softplus(sigma_raw)
    
    # b. ELBO를 sigma에 대해 미분 (기존 코드의 그래디언트를 올바르게 수정)
    grad_sigma <- (1 / sigma) - 2 * sigma 
    
    # c. softplus 함수의 도함수(sigmoid 함수)를 계산
    grad_softplus <- 1 / (1 + exp(-sigma_raw)) # Sigmoid(sigma_raw)
    
    # d. 연쇄 법칙(chain rule)을 이용해 sigma_raw에 대한 그래디언트 계산
    # (d_ELBO / d_sigma_raw) = (d_ELBO / d_sigma) * (d_sigma / d_sigma_raw)
    grad_sigma_raw <- grad_sigma * grad_softplus
    
    # e. 제약 없는 파라미터 sigma_raw를 업데이트
    sigma_raw <- sigma_raw + lr * grad_sigma_raw
    # --- sigma 업데이트 종료 ---
    
    # 기록을 위해 최종 sigma 값을 다시 계산
    sigma <- softplus(sigma_raw)
    
    trace <- rbind(trace, data.frame(step = i, mu = mu, sigma = sigma, elbo = elbo(mu, sigma, x)))
  }
  return(trace)
}

# 수정한 함수 실행
# softplus는 더 안정적이므로 lr=0.1을 다시 시도해볼 수 있으나, 0.01로 시작하겠습니다.
result_softplus <- optimize_vi_softplus(x_obs, lr = 0.01, steps = 50)


# 시각화
ggplot(result_softplus, aes(x = step, y = elbo)) +
  geom_line(aes(y = elbo), color = "green", size = 1.2) +
  labs(title="ELBO Optimization with Softplus", y = "ELBO", x = "Step") +
  theme_minimal()
