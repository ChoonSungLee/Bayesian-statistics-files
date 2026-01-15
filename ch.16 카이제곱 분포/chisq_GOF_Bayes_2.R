# 코드 3; Goodness of Fit Test; 작동 함
# 패키지 설치
if (!require("arm")) install.packages("arm")  
library(arm)

# 데이터 정의
observed <- c(95, 55, 50) # 관측값
expected_proportions <- c(0.5, 0.3, 0.2) # 예상 비율
total <- sum(observed) # 전체 관측값 합
expected <- total * expected_proportions # 기대값 계산

# Goodness of Fit Test를 위한 베이즈 접근
# 모수로 관측값을 모델링 (ex: 다항 분포 기반)
bayes_model <- bayesglm(observed ~ expected, family = poisson)

# 결과 출력
summary(bayes_model)

# 사후 분포 샘플링
posterior_samples <- sim(bayes_model, n.sims = 1000)
print(posterior_samples)
