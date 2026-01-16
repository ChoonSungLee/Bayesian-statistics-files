#코드 4; independence_test

library(BayesFactor)

# 관측 데이터: 성별과 색상 선호도
data_matrix <- matrix(c(50, 30, 20, 45, 25, 30), nrow = 2, byrow = TRUE,
                      dimnames = list(Gender = c("Male", "Female"),
                                      Color = c("Red", "Blue", "Green")))

# 베이즈 독립성 검정
independence_test <- contingencyTableBF(as.table(data_matrix),
                                        sampleType = "jointMulti")
print(independence_test)
