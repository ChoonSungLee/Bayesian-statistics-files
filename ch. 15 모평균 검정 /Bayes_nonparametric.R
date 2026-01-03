if (!require("BayesFactor")) install.packages("BayesFactor")  # 베이즈 t-test를 위한 대표 패키지
library(BayesFactor)

set.seed(123)
group_A <- rnorm(30, mean = 70, sd = 10)
group_B <- c(rnorm(25, mean = 75, sd = 10), 100, 100, 100, 100, 100)

# Bayes t-test (독립 표본)
bf_result <- ttestBF(x = group_A, y = group_B, paired = FALSE)

# 결과 출력
print(bf_result)

plot(bf_result)
