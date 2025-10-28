# 데이터 (1: 성공, 0: 실패)
data <- c(1, 1, 0, 1, 0)

# Likelihood 함수 정의
likelihood <- function(theta) {
  prod(theta^data * (1 - theta)^(1 - data))  # 베르누이 분포의 likelihood
}

# 가능한 theta 값
theta_values <- seq(0, 1, by = 0.01)

# 각 theta에서의 likelihood 값 계산
likelihood_values <- sapply(theta_values, likelihood)

# MLE: likelihood가 최대인 theta 찾기
mle_theta <- theta_values[which.max(likelihood_values)]
cat("MLE for theta:", mle_theta, "\n")

# 그래프 출력
plot(theta_values, likelihood_values, type = "l", main = "Likelihood Curve",
     xlab = "Theta", ylab = "Likelihood")
abline(v = mle_theta, col = "red", lty = 2)  # MLE 위치 표시



#lst <- list(a = 1:5, b = 6:10)
#lapply(lst, mean)



#lst <- list(a = 1:5, b = 6:10)
#sapply(lst, mean)
