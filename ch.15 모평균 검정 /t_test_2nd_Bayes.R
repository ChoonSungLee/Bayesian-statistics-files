# This file comes from regression_variety.R
# 2 t-test for mean (Bayesian)
# 패키지 설치 및 로드
if (!require("BayesFactor")) install.packages("BayesFactor")
library(BayesFactor)

# 데이터 예제
scores <- c(72, 68, 74, 65, 70, 78, 66, 72, 71, 69)

# 베이지안 t-test 수행
bf <- ttestBF(x = scores, mu = 70)
print(bf)
