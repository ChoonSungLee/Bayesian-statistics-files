# chisq test (Independence test)
# 관측 데이터 (성별과 색상 선호도)
data_matrix <- matrix(c(50, 30, 20, 45, 25, 30), nrow = 2, byrow = TRUE,
                      dimnames = list(Gender = c("Male", "Female"),
                                      Color = c("Red", "Blue", "Green")))

# 카이제곱 독립성 검정
chisq_independence <- chisq.test(data_matrix)

# 결과 출력
chisq_independence
