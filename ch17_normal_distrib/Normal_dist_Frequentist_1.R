# 평균이 미지수인 정규분포 (분산은 알려진 경우)

# 문제 설정:
# •	데이터:  x = [9.5, 10.1, 10.3, 9.8, 10.0, 9.7, 10.2, 9.9, 9.6, 10.4] 
# •	알려진 분산:  \sigma^2 = 4  (표준편차:  \sigma = 2 )

# 빈도주의 접근법:
# •	평균에 대한 점 추정: 표본 평균
# •	평균에 대한 신뢰구간: 표준오차(SE)를 사용한 95% 신뢰구간


# 평균이 미지수인 정규분포 (분산은 알려진 경우 - 빈도주의)
# 데이터
if (!require("ggplot2")) install.packages(c("ggplot2"))

data <- c(9.5, 10.1, 10.3, 9.8, 10.0, 9.7, 10.2, 9.9, 9.6, 10.4)
n <- length(data)
known_variance <- 4  # 분산 (σ²)
known_std <- sqrt(known_variance)  # 표준편차 (σ)

# 표본 평균과 표준오차 계산
sample_mean <- mean(data)
se <- known_std / sqrt(n)  # 표준오차

# 95% 신뢰구간
lower_bound <- sample_mean - 1.96 * se
upper_bound <- sample_mean + 1.96 * se

cat(sprintf("Sample Mean: %.2f\n", sample_mean))
cat(sprintf("95%% Confidence Interval for Mean: (%.2f, %.2f)\n", lower_bound, upper_bound))

# 시각화
library(ggplot2)

x <- seq(9, 11, length.out = 1000)
likelihood <- dnorm(x, mean = sample_mean, sd = se)

ggplot(data.frame(x = x, density = likelihood), aes(x = x, y = density)) +
  geom_line(size = 1, color = "blue") +
  geom_vline(xintercept = sample_mean, color = "red", linetype = "dashed") +
  geom_vline(xintercept = lower_bound, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = upper_bound, color = "blue", linetype = "dashed") +
  labs(title = "Frequentist Inference for Normal Mean (Known Variance)",
       x = "Mean (μ)",
       y = "Likelihood") +
  theme_minimal()




