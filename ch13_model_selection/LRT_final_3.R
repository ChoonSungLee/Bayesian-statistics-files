# 패키지 설치 및 로드
if (!require("rstanarm")) install.packages("rstanarm", dependencies = TRUE)
if (!require("loo")) install.packages("loo", dependencies = TRUE)
library(rstanarm)
library(loo)


# 데이터 준비
data_frame <- data.frame(
  y = c(1.1, 1.9, 2.3, 2.0, 1.8, 3.5), 
  x1 = c(0, 1, 0, 1, 0, 1),            
  x2 = c(1, 0, 1, 0, 1, 1)
)

# Reduced Model 정의
reduced_model <- stan_glm(
  y ~ x1,
  data = data_frame,
  family = gaussian(),
  prior = normal(0, 10),
  prior_intercept = normal(0, 10),
  chains = 4,
  iter = 2000
)

# Full Model 정의 (예: 상호작용 포함)
full_model <- stan_glm(
  y ~ x1 * x2,
  data = data_frame,
  family = gaussian(),
  prior = normal(0, 10),
  prior_intercept = normal(0, 10),
  chains = 4,
  iter = 2000
)

# 로그 가능도 계산
log_lik_reduced_mean <- sum(colMeans(log_lik(reduced_model)))
log_lik_full_mean <- sum(colMeans(log_lik(full_model)))

# LRT와 p-value 계산
lrt <- -2 * (log_lik_reduced_mean - log_lik_full_mean)
k <- length(full_model$coefficients) - length(reduced_model$coefficients)  # 자유도 계산
p_value <- ifelse(lrt >= 0, pchisq(lrt, df = k, lower.tail = FALSE), NA)

# 결과 출력
cat("Reduced Model vs Full Model:\n")
cat(sprintf("LRT: %.4f\n", lrt))
cat(sprintf("p-value: %.4f\n", p_value))

