# chisq test (Goodness of fit test)

# 관측된 데이터
observed <- c(95, 55, 50)

# 예상 비율 (빨강: 50%, 파랑: 30%, 초록: 20%)
expected_proportions <- c(0.5, 0.3, 0.2)

# 기대값 계산
total <- sum(observed)
expected <- total * expected_proportions

# 카이제곱 검정 수행
chisq_test <- chisq.test(x = observed, p = expected_proportions)

# 결과 출력
chisq_test
