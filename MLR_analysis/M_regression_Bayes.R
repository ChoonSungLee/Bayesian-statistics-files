# 1. 데이터 및 패키지 준비 

# Bayesian solution
# 패키지 필요하면 설치 
if (!require("rstanarm")) install.packages("rstanarm")
if (!require("here")) install.packages("here")
if (!require("bayesplot")) install.packages("bayesplot")

library(rstanarm)
library(here)
library(bayesplot)

# here() 함수를 이용해 'data' 폴더에 있는 데이터 파일을 불러옵니다.
# 이 코드는 'mlr_data'라는 이름의 데이터프레임을 불러옵니다.
load(here("data", "mlr_data.Rdata"))

# 불러온 데이터의 구조와 앞부분을 확인합니다.
print("--- 불러온 데이터 확인 ---")
head(mlr_data)
dim(mlr_data)
print("--------------------------")


# 2. 베이지안 다중 회귀 모델 (사전 분포 설정 가능)
# stan_glm() 함수를 사용하여 베이지안 모델을 적합합니다.
bayesian_model <- stan_glm(y ~ x1 + x2, data = mlr_data, family = gaussian(), seed = 123, refresh = 0)

# 모델 요약
print("--- 베이지안 회귀분석 결과 ---")
summary(bayesian_model)
print("--------------------------")

# 3. 모델 진단 및 시각화 

# 사후 분포 시각화 (각 파라미터의 불확실성 확인)
plot(bayesian_model, pars = c("(Intercept)", "x1", "x2"))

# 사후 예측점검(PPC): 모델이 데이터를 잘 설명하는지 확인
pp_check(bayesian_model)


# 4. 사후 예측 분포를 이용한 추가 분석 (Bayesian p-value) 
# Bayesian p-value 계산 예제 (PPC 포함)

# 모델로부터 가상 데이터셋(y_rep) 생성
y_rep <- posterior_predict(bayesian_model)

# 테스트 통계량 (평균)을 기준으로 Bayesian p-value 계산
# T(y) : 실제 데이터 y의 평균
# T(y_rep) : 예측 데이터 y_rep의 평균

T_y <- mean(mlr_data$y)  # 실제 데이터 평균
T_y_rep <- apply(y_rep, 1, mean)  # 예측 데이터 평균 (각 시뮬레이션별 평균)

# Bayesian p-value 계산 및 출력
bayesian_p_value <- mean(T_y_rep > T_y)
print(paste("Bayesian p-value (mean):", round(bayesian_p_value, 3)))

