set.seed(123)
sigma <- 2
mu <- 10
n <- 10

# 모수 기반 정규분포로부터 표본 생성

x <- rnorm(n, mean = mu, sd = sigma)

# 불편추정량

sample_var <- var(x)

# MLE

mle_var <- (n - 1)/n * sample_var

cat(sprintf("불편추정량: %.4f\n", sample_var))
cat(sprintf("MLE: %.4f\n", mle_var))

diff <- sigma^2 - mean(mle_var)
cat(sprintf("MLE의 평균적 편향: %.4f\n", diff))
