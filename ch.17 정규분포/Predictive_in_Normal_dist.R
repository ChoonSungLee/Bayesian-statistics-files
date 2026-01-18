# 베이즈 예측분포

# 설정
# •	평균(μ)과 분산(σ²)에 대한 사전분포를 설정.
# •	데이터와 사전분포를 통해 사후분포를 구함.
# •	새로운 데이터 포인트  \tilde{x} 의 예측분포를 유도.


# 베이즈 예측분포
if (!require("ggplot2")) install.packages(c("ggplot2"))

library(ggplot2)

# 데이터
data <- c(9.5, 10.1, 10.3, 9.8, 10.0, 9.7, 10.2, 9.9, 9.6, 10.4)
n <- length(data)
known_variance <- 4  # 알려진 분산 (σ²)
known_std <- sqrt(known_variance)

# 사전분포
prior_mean <- 10

prior_variance <- 1

# 사후분포 모수
sample_mean <- mean(data)
posterior_mean <- (prior_mean / prior_variance + n * sample_mean / known_variance) / 
  (1 / prior_variance + n / known_variance)
posterior_variance <- 1 / (1 / prior_variance + n / known_variance)

# 예측분포의 평균과 분산
predictive_mean <- posterior_mean
predictive_variance <- known_variance + posterior_variance


# 예측분포 시각화

# 베이지안 예측분포 x 값 정의
x_pred <- seq(8, 12, length.out = 1000)

# 베이지안 예측분포의 밀도 계산
predictive_density <- dnorm(x_pred, mean = predictive_mean, sd = sqrt(predictive_variance))

ggplot(data.frame(x = x_pred, density = predictive_density), aes(x = x, y = density)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Bayesian Predictive Distribution",
       x = "Predicted Value",
       y = "Density") +
  theme_minimal()

cat(sprintf("Bayesian Predictive Mean: %.2f\n", predictive_mean))
cat(sprintf("Bayesian Predictive Variance: %.2f\n", predictive_variance)) 





# 빈도주의 예측분포

# 설정
# •	표본 평균과 표본 분산을 사용해 새로운 데이터 포인트를 예측.
# •	예측값은  \bar{x} 를 중심으로, 추가적인 오차 항을 포함함.
# •	예측 분산은  \sigma^2 (1 + \frac{1}{n}) 로 표현됨.


# 빈도주의 예측분포
# 표본 평균과 표준편차
sample_mean <- mean(data)
n <- length(data)
known_variance <- 4  # 알려진 분산 (σ²)
known_std <- sqrt(known_variance)

# 예측 분산: σ²(1 + 1/n)
predictive_variance_freq <- known_variance * (1 + 1 / n)
predictive_std_freq <- sqrt(predictive_variance_freq)

# 예측분포 시각화
x_pred_freq <- seq(8, 12, length.out = 1000)
predictive_density_freq <- dnorm(x_pred_freq, mean = sample_mean, sd = predictive_std_freq)

ggplot(data.frame(x = x_pred_freq, density = predictive_density_freq), aes(x = x, y = density)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Frequentist Predictive Distribution",
       x = "Predicted Value",
       y = "Density") +
  theme_minimal()

cat(sprintf("Frequentist Predictive Mean: %.2f\n", sample_mean))
cat(sprintf("Frequentist Predictive Variance: %.2f\n", predictive_variance_freq))



# 시각화 비교

# 베이즈와 빈도주의 예측분포 비교
df <- data.frame(
  x = c(x_pred, x_pred_freq),
  density = c(predictive_density, predictive_density_freq),
  Method = rep(c("Bayesian", "Frequentist"), each = length(x_pred))
)

ggplot(df, aes(x = x, y = density, color = Method)) +
  geom_line(size = 1) +
  labs(title = "Comparison of Bayesian and Frequentist Predictive Distributions",
       x = "Predicted Value",
       y = "Density") +
  theme_minimal()




