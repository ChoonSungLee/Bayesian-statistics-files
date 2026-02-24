# 관측값
x <- 2.0

# 초기값 설정
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1

# ELBO 계산 함수
elbo <- function(mu1, mu2, sigma1, sigma2, x) {
  # recon term: E[log p(x | z1+z2)]  ~ N(x; z1+z2, 1)
  mean_z <- mu1 + mu2
  var_z <- sigma1^2 + sigma2^2
  recon <- -0.5 * ( (x - mean_z)^2 + var_z )
  
  # prior term: E[log p(z1)] + E[log p(z2)] (표준 정규분포 가정)
  prior <- -0.5 * (mu1^2 + sigma1^2 - log(sigma1^2)) - 0.5 * (mu2^2 + sigma2^2 - log(sigma2^2))
  
  return(recon + prior)
}

# 최적화 반복
results <- data.frame(step=0, mu1=mu1, mu2=mu2, elbo=elbo(mu1, mu2, sigma1, sigma2, x))

for (step in 1:30) {
  # 좌표상승 업데이트 방식 (간단한 경사 업데이트 근사)
  grad_mu1 <- x - mu1 - mu2 - mu1  # d/dmu1
  grad_mu2 <- x - mu1 - mu2 - mu2  # d/dmu2
  
  mu1 <- mu1 + 0.1 * grad_mu1
  mu2 <- mu2 + 0.1 * grad_mu2
  
  e <- elbo(mu1, mu2, sigma1, sigma2, x)
  results <- rbind(results, data.frame(step=step, mu1=mu1, mu2=mu2, elbo=e))
}

# 시각화
library(ggplot2)
ggplot(results, aes(x=step)) +
  geom_line(aes(y=elbo), color="blue", linewidth=1.2) +
  labs(title="평균장 변분추론: ELBO 최적화", y="ELBO", x="반복 스텝") +
  theme_minimal()
