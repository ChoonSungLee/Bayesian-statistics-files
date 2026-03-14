
# 평균과 분산이 모두 미지수인 정규분포

# 문제 설정:
#  •	데이터:  x = [85, 87, 90, 88, 84, 89, 91, 86, 87, 88, 90, 85, 86, 89, 92] 

# 빈도주의 접근법:
# •	평균과 분산에 대한 점 추정: 표본 평균과 표본 분산
# •	평균과 분산에 대한 신뢰구간:
# •	평균:  \bar{x} \pm t_{\alpha/2, n-1} \cdot \frac{s}{\sqrt{n}} 
# •	분산: 카이제곱 분포를 이용한 신뢰구간

# 평균과 분산이 모두 미지수인 정규분포 (빈도주의)

# 데이터 생성
data <- c(85, 87, 90, 88, 84, 89, 91, 86, 87, 88, 90, 85, 86, 89, 92)
n <- length(data)

# 표본 평균과 표본 분산
sample_mean <- mean(data)
sample_var <- var(data)
sample_std <- sqrt(sample_var)
cat("Sample mean:", sample_mean, "\n")
cat("Sample var :", sample_var,  "\n")

# 모수 추정
# - 정규분포에서 µ, σ 미지일 때 MLE:
#   mu_hat = sample_mean
#   sigma2_hat = (1/n)*sum((x_i - sample_mean)^2)
#   => R에서 var()*(n-1)/n 를 쓰면 동일
mu_hat     <- sample_mean
sigma2_hat <- (n - 1)/n * sample_var  # MLE 분모가 n임

cat("\n[MLE 추정치]\n")
cat("MLE for mu       =", mu_hat, "\n")
cat("MLE for sigma^2  =", sigma2_hat, "\n")
cat("MLE for sigma    =", sqrt(sigma2_hat), "\n")

# 평균에 대한 95% 신뢰구간 (t-분포 사용)
t_value <- qt(0.975, df = n - 1)  # t-분포 임계값
mean_se <- sample_std / sqrt(n)  # 평균의 표준오차

mean_lower_bound <- sample_mean - t_value * mean_se
mean_upper_bound <- sample_mean + t_value * mean_se

cat(sprintf("Sample Mean: %.2f\n", sample_mean))
cat(sprintf("95%% Confidence Interval for Mean: (%.2f, %.2f)\n", mean_lower_bound, mean_upper_bound))

# 분산에 대한 95% 신뢰구간 (Chi-squared 분포 사용)
chi2_lower <- qchisq(0.975, df = n - 1)
chi2_upper <- qchisq(0.025, df = n - 1)

variance_lower_bound <- (n - 1) * sample_var / chi2_lower
variance_upper_bound <- (n - 1) * sample_var / chi2_upper

cat(sprintf("Sample Variance: %.2f\n", sample_var))
cat(sprintf("95%% Confidence Interval for Variance: (%.2f, %.2f)\n", variance_lower_bound, variance_upper_bound))

# 시각화: 평균과 분산의 신뢰구간
library(ggplot2)

# 평균 신뢰구간 시각화
ggplot(data.frame(x = data), aes(x = x)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  geom_vline(xintercept = sample_mean, color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean_lower_bound, color = "blue", linetype = "dotted") +
  geom_vline(xintercept = mean_upper_bound, color = "blue", linetype = "dotted") +
  labs(title = "Frequentist Inference for Normal Mean and Variance",
       x = "Data",
       y = "Count") +
  theme_minimal()

