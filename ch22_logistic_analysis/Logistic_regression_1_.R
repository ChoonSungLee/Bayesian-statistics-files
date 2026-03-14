# 데이터 생성(Data Generation)
set.seed(123) # 재현 가능성 확보(To ensure reproducibility)
n <- 200 # 샘플 수 # Number of samples
age <- rnorm(n, mean = 50, sd = 10) # 나이: 평균 50세, 표준편차 10
blood_pressure <- rnorm(n, mean = 120, sd = 15) # 혈압: 평균 120, 표준편차 15

# 질병 유무: 나이와 혈압에 따라 질병 여부를 결정(Disease status: Determined based on a threshold rule for age and blood pressure)
disease <- as.factor(ifelse(0.2*age + 0.05*blood_pressure + rnorm(n, 0, 2) > 20, 1, 0))

# 데이터프레임 생성(Create dataframe)
data <- data.frame(age, blood_pressure, disease)

# 데이터 확인(Check the data)
head(data)














