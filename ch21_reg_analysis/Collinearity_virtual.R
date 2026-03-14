# 가상의 예제
# 데이터 생성 (다중공선성 유도)
if(!require("car")) install.packages("car")
library(car)

set.seed(123)
n <- 100
x1 <- rnorm(n, mean = 10, sd = 2)  # 독립변수 1
x2 <- 2 * x1 + rnorm(n, mean = 0, sd = 0.1)  # 독립변수 2 (x1에 종속적)
y <- 3 + 2 * x1 - 1.5 * x2 + rnorm(n, mean = 0, sd = 3)  # 종속변수

# 다중 회귀 모델
model <- lm(y ~ x1 + x2)

# 상관계수 확인
cor(x1, x2)

# VIF 확인
vif_values <- vif(model)
print(vif_values)
