if(!require("ggplot2")) install.packages("ggplot2")
if(!require("sysfonts")) install.packages("sysfonts")
if(!require("showtextdb")) install.packages("showtextdb")

# SVI를 시연하기 위한 샘플 데이터 생성 (1000개)
set.seed(123)
y_data <- rnorm(1000, mean = 3.5, sd = 1)

# 1. elbo 함수 (변경 없음)
elbo <- function(mu, sigma, x) {
  recon_term <- -0.5 * ( (x - mu)^2 + sigma^2 )
  kl <- log(1 / sigma) + (sigma^2 + mu^2 - 1) / 2
  return(recon_term - kl)
}


# 2. SVI 최적화 함수
# y: 전체 데이터셋, batch_size: 미니배치 크기
optimize_svi <- function(y, batch_size = 10, mu_init = 0, sigma_init = 1, lr = 0.01, steps = 500) {
  mu <- mu_init
  sigma <- sigma_init
  # 전체 데이터 개수
  N <- length(y)
  
  # trace 객체 초기화 (전체 데이터셋의 평균 ELBO를 기록)
  mean_elbo <- mean(sapply(y, function(val) elbo(mu, sigma, val)))
  trace <- data.frame(step = 0, mu = mu, sigma = sigma, elbo = mean_elbo)
  
  for (i in 1:steps) {
    # 전체 데이터에서 미니배치 랜덤 샘플링
    mini_batch <- sample(y, batch_size)
    
    # 미니배치에 대한 경사를 계산
    # 데이터에 의존하는 항(recon_term)의 경사만 계산 후 합산
    grad_mu_data_part <- sum(mini_batch - mu)
    
    # KL항의 경사와 데이터 항의 경사를 합산
    # 데이터 항의 경사는 전체 데이터에 대한 추정을 위해 스케일링 (N / batch_size)
    grad_mu <- (N / batch_size) * grad_mu_data_part - mu
    
    # sigma에 대한 경사 (이전의 불안정성 문제를 해결하기 위해 올바른 경사로 수정)
    # sigma 경사는 데이터에 의존하지 않으므로 스케일링이 필요 없음
    grad_sigma <- (1 / sigma) - 2 * sigma 
    
    # 파라미터 업데이트 (전체 경사가 아닌 미니배치 경사의 평균을 사용하므로 lr 조정)
    mu <- mu + (lr / batch_size) * grad_mu
    sigma <- max(0.1, sigma + lr * grad_sigma)
    
    # 50 스텝마다 현재 상태 기록 (전체 데이터에 대한 평균 ELBO)
    if (i %% 50 == 0) {
      mean_elbo <- mean(sapply(y, function(val) elbo(mu, sigma, val)))
      trace <- rbind(trace, data.frame(step = i, mu = mu, sigma = sigma, elbo = mean_elbo))
    }
  }
  return(trace)
}

# 3. SVI 실행 및 결과 확인
#svi_results <- optimize_svi(y_data, steps = 1000, lr=0.1): NaN 발생
svi_results <- optimize_svi(y_data, steps = 1000, lr=0.01)
tail(svi_results)

# 4. SVI 결과 시각화
library(ggplot2)
library(showtext)
font_add_google("Nanum Gothic", "nanumgothic")
showtext_auto()

ggplot(svi_results, aes(x = step, y = elbo)) +
  geom_line(color = "green", linewidth = 1.2) +
  geom_point(color = "darkgreen") +
  labs(
    title = "ELBO Optimization Process (Stochastic VI)",
    y = "Mean ELBO",
    x = "Step"
  ) +
  theme_minimal(base_family = "nanumgothic")

