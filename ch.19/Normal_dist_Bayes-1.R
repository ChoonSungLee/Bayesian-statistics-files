# 평균이 미지수인 정규분포 (분산은 알려진 경우)

# 문제 설정:
# •	데이터:  x = [9.5, 10.1, 10.3, 9.8, 10.0, 9.7, 10.2, 9.9, 9.6, 10.4] 
# •	알려진 분산:  \sigma^2 = 4  (표준편차:  \sigma = 2 )
# •	사전분포:  \mu \sim N(10, 1) 

# install.packages(c("ggplot2", "rstan", "bayesplot"))

library(ggplot2)

# 데이터
data <- c(9.5, 10.1, 10.3, 9.8, 10.0, 9.7, 10.2, 9.9, 9.6, 10.4)
n <- length(data)
known_variance <- 4  # 분산 (σ²)
known_std <- sqrt(known_variance)  # 표준편차 (σ)

# 사전분포 모수
prior_mean <- 10  # μ ~ N(10, 1)
prior_variance <- 1  # 사전분포 분산

# 데이터 평균
sample_mean <- mean(data)

# 사후분포 모수 계산
posterior_mean <- (prior_mean / prior_variance + n * sample_mean / known_variance) / 
  (1 / prior_variance + n / known_variance)
posterior_variance <- 1 / (1 / prior_variance + n / known_variance)

# 95% 신뢰구간
lower_bound <- posterior_mean - 1.96 * sqrt(posterior_variance)
upper_bound <- posterior_mean + 1.96 * sqrt(posterior_variance)

cat(sprintf("Posterior Mean: %.2f\n", posterior_mean))
cat(sprintf("95%% Credible Interval: (%.2f, %.2f)\n", lower_bound, upper_bound))

# 시각화
x <- seq(9, 11, length.out = 1000)
prior <- dnorm(x, mean = prior_mean, sd = sqrt(prior_variance))
likelihood <- dnorm(x, mean = sample_mean, sd = known_std / sqrt(n))
posterior <- dnorm(x, mean = posterior_mean, sd = sqrt(posterior_variance))

plot_data <- data.frame(
  x = rep(x, 3),
  density = c(prior, likelihood, posterior),
  Distribution = rep(c("Prior", "Likelihood", "Posterior"), each = length(x))
)

ggplot(plot_data, aes(x = x, y = density, color = Distribution)) +
  geom_line(size = 1) +
  geom_vline(xintercept = posterior_mean, color = "red", linetype = "dashed") +
  labs(title = "Bayesian Inference for Normal Mean (Known Variance)",
       x = "Mean (μ)",
       y = "Density") +
  theme_minimal()


# CI 추가 코드
ggplot(plot_data, aes(x = x, y = density, color = Distribution)) +
  geom_line(size = 1) +
  geom_vline(xintercept = posterior_mean, color = "red", linetype = "dashed") +
  geom_vline(xintercept = lower_bound, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = upper_bound, color = "blue", linetype = "dashed") +
  labs(title = "Bayesian Inference for Normal Mean (Known Variance)",
       x = "Mean (μ)",
       y = "Density") +
  theme_minimal()








