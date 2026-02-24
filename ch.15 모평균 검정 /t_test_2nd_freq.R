# this file comes from regression_variety.R
# 1 t-test for mean
# 데이터 예제
scores <- c(72, 68, 74, 65, 70, 78, 66, 72, 71, 69)

# 평균이 70점과 다른지 검정 (양측 검정)
t.test(scores, mu = 70, alternative = "two.sided")


