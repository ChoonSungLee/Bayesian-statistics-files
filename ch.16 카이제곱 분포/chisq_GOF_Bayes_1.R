# 코드 2; Goodness of Fit Test; 

# 관측값과 기대값
observed <- c(95, 55, 50)
expected_proportions <- c(0.5, 0.3, 0.2)
total <- sum(observed)
expected <- total * expected_proportions

# 베이즈 팩터 계산 (근사적 방법)
bayes_factor <- exp(-0.5 * chi_sq_stat)  # 간단한 변환

print(bayes_factor)
