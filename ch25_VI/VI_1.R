# 필요한 패키지
library(ggplot2)

# KL(q || p)를 계산하는 함수 (q: N(mu, sigma^2), p: N(0, 1))
kl_divergence <- function(mu, sigma) {
  # analytic KL divergence between two Gaussians
  kl <- log(1 / sigma) + (sigma^2 + mu^2 - 1) / 2
  return(kl)
}

# ELBO는 -KL
elbo <- function(mu, sigma) {
  return(-kl_divergence(mu, sigma))
}

# 파라미터 조합을 위한 그리드 생성
mu_vals <- seq(-2, 2, length.out = 100)
sigma_vals <- seq(0.5, 2, length.out = 100)

grid <- expand.grid(mu = mu_vals, sigma = sigma_vals)
grid$elbo <- mapply(elbo, grid$mu, grid$sigma)

# 시각화
ggplot(grid, aes(x = mu, y = sigma, fill = elbo)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "ELBO(mu, sigma): -KL divergence between q(z) and p(z)",
    x = expression(mu),
    y = expression(sigma),
    fill = "ELBO"
  ) +
  theme_minimal()
