# BayesFactor 패키지 설치 및 로드
# install.packages("BayesFactor")
library(BayesFactor)

# 데이터는 위 빈도주의 예제와 동일
set.seed(42)
group_A <- rnorm(10, mean = 5, sd = 3)
group_B <- rnorm(10, mean = 12, sd = 3)

# --- 베이지안 t-test 수행 ---
bf_result <- ttestBF(x = group_A, y = group_B)

# 결과 출력
print(bf_result)
