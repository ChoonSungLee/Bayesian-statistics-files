# MLE를 구할 때 L-BFGS-B와 같은 최적화 알고리즘을 사용하지 않고, 그냥 편미분하여 구하는 간단한 예제  
# 데이터 설정
n <- 10  # 총 동전 던진 횟수
x <- 7   # 앞면이 나온 횟수

# theta 값 범위
theta <- seq(0, 1, by = 0.01)

# 가능도 (Likelihood) 계산 및 정규화
likelihood <- dbinom(x, size = n, prob = theta)
likelihood <- likelihood / max(likelihood)  # Likelihood 정규화

# 사전 분포 (Prior): Beta(1,1)
alpha <- 1
beta <- 1

# 사후 분포 (Posterior): Beta(alpha + x, beta + (n - x)) 및 정규화
posterior <- dbeta(theta, alpha + x, beta + (n - x))
posterior <- posterior / max(posterior)  # Posterior 정규화

# MLE 추정치
theta_mle <- x / n

# 그래프 그리기
plot(theta, likelihood, type = "l", col = "blue", lwd = 3,
     main = "MLE vs Bayes posterior",
     xlab = "theta", ylab = "Density (Normalized)",
     ylim = c(0, 1))

# 사후 분포 추가 (Posterior)
lines(theta, posterior, col = "red", lwd = 2, lty = 2)

# MLE 점선 추가
abline(v = theta_mle, col = "blue", lty = 2, lwd = 2)

# 범례 추가
legend("topright", legend = c("Likelihood (Freq)", "Posterior (Bayes)"),
       col = c("blue", "red"), lwd = c(3, 2), lty = c(1, 2))




######################
### 이하는 보충코드 (원본 가능도와 정규화된 가능도의 비교)
## 1. 데이터 설정
n <- 10
x <- 7   # 관측된 성공(앞면) 횟수

## 2. theta(모수) 범위 설정
theta <- seq(0, 1, length.out = 101)  # 0부터 1 사이를 101개 구간으로

## 3. 이항분포를 이용하여 likelihood(가능도) 계산
likelihood <- dbinom(x, size = n, prob = theta)  
# 'dbinom'은 PMF를 계산하는데, 데이터 x=7이 주어졌을 때 
# 모수 theta일 때의 확률 p(X=7|theta)를 각각 구해 벡터로 반환

## 4. 가능도 곡선 시각화 (base R plot)
plot(
  theta, 
  likelihood, 
  type = "l",          # 선 형태(line)로 그리기
  lwd = 2,            # 선 두께
  col = "blue",       
  main = "Likelihood Curve for Binomial Model", 
  xlab = expression(theta),
  ylab = "Likelihood"
)

## (추가) 필요하다면 정규화된 곡선도 같이 그려볼 수 있음
likelihood_norm <- likelihood / max(likelihood)   # 최대값을 1로 맞춤
lines(theta, likelihood_norm, col = "red", lwd = 2, lty = 2)
legend(
  "topright", 
  legend = c("Raw Likelihood", "Normalized Likelihood"), 
  col = c("blue", "red"), lwd = 2, lty = c(1, 2)
)











