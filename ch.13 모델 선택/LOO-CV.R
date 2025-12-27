# rstanarm 모델 적합
library(rstanarm)
data <- data.frame(x = rnorm(100), y = rnorm(100))
fit <- stan_glm(y ~ x, data = data, family = gaussian())

# LOO-CV 계산
library(loo)
loo_result <- loo(fit)

# LPD와 관련된 정보 출력
print(loo_result)
