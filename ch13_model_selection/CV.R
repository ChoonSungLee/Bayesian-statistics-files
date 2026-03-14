# Bayesian Modeling Script - Cleaned and Optimized
if (!require("rstanarm")) install.packages("rstanarm")
if (!require("loo")) install.packages("loo")
library(rstanarm)  # 베이지안 분석 및 WAIC 계산
library(loo)       # WAIC, LOO 계산
library(caret)

# 데이터 준비
data <- data.frame(y = c(1.1, 1.9, 2.3, 2.2, 1.8))

# rstanarm 모델링
#fit <- stan_glm(data ~ 1, 
#                data = data_frame,         # 데이터 프레임 지정
#                family = gaussian(),       # 정규 분포 가정
#                prior = normal(0, 10),     # 사전 분포 설정
#                prior_intercept = normal(0, 10), 
#                chains = 4,                # 체인 수
#                iter = 2000)               # 반복 수


set.seed(123)
folds <- createFolds(data$y, k = 5)

print(folds)

cv_scores <- numeric()

formula <- y ~ 1  # 단순 모델: y의 평균만 사용

for (fold in folds) {
  train_data <- data[-fold, , drop = FALSE]  # 학습 데이터
  test_data <- data[fold, , drop = FALSE]    # 테스트 데이터
  
  model <- stan_glm(formula, data = train_data, family = gaussian())
  
  predictions <- posterior_predict(model, newdata = test_data)
  
  mse <- mean((test_data$y - rowMeans(predictions))^2)
  cv_scores <- c(cv_scores, mse)
}

mean_cv_score <- mean(cv_scores)
cat(sprintf("Cross-Validation MSE: %.4f\n", mean_cv_score))







df <- data.frame(x = 1:3, y = 4:6)
df[1, ]         # 이 경우는 길이가 1인 벡터 형태로 바뀔 수 있음
df[1, , drop=FALSE]  # 항상 데이터프레임 구조(2차원)로 유지

