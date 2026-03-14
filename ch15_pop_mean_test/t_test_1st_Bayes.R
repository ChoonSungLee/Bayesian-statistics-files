
# 필요한 패키지 설치 및 로드
if (!require("brms")) install.packages("brms")
library(brms)

# 표본 데이터 (실제 평균=172, 표준편차=9)
# 데이터 생성 과정을 보여주는 대신, 확정된 데이터를 직접 제공
height_data <- c(172.00, 178.65, 177.30, 169.65, 161.01, 158.43, 176.65, 
                 172.54, 164.88, 174.45, 175.64, 165.75, 185.79, 166.41, 
                 179.99, 169.89, 163.78, 179.47, 181.67, 166.05)
data <- data.frame(y = height_data)

# 베이지안 모델 설정 (이하 동일)
fit <- brm(
  formula = y ~ 1,
  data = data,
  prior = prior(normal(170, 5), class = "Intercept"),
  family = gaussian(),
  iter = 4000, chains = 4, cores = 4
)

# 결과 확인 (이하 동일)
print(fit)
plot(fit)
hypothesis(fit, "Intercept = 170")






