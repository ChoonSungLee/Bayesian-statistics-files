# ==========================================================
# 회귀분석의 t-검정 (메인 분석 스크립트)
# 역할: 저장된 데이터를 불러와 분석을 수행
# ==========================================================

# --- 1. 데이터 및 패키지 준비 ---

# 필요한 패키지 로드 (최초 1회 설치 필요)
if (!require("here")) install.packages("here")
if (!require("rstanarm")) install.packages("rstanarm")
if (!require("bayesplot")) install.packages("bayesplot")
library(here)
library(rstanarm)
library(bayesplot)

# here() 함수를 이용해 'data' 폴더에 있는 데이터 파일의 경로를 찾아 불러옵니다.
load(here("data", "lm_data.Rdata"))

# 불러온 데이터의 구조와 앞부분을 확인합니다.
print("--- 불러온 데이터 확인 ---")
head(lm_data)
dim(lm_data)
print("--------------------------")


# --- 2. 베이지안 선형 회귀 모델 ---

# stan_glm() 함수를 사용하여 베이지안 회귀 모델을 적합합니다.
# seed: 재현성을 위해 난수 시드 설정
# refresh = 0: MCMC 샘플링 진행 과정 메시지를 생략하여 출력을 깔끔하게 함
bayesian_model <- stan_glm(weight ~ height, 
                           data = lm_data, 
                           seed = 123, 
                           refresh = 0)

# 베이지안 모델의 요약 결과를 확인합니다.
print("--- 베이지안 회귀분석 결과 ---")
summary(bayesian_model)
print("--------------------------")

# 베이지안 분석 결과 시각화
# 계수(Intercept, height)의 사후분포 시각화
mcmc_areas(bayesian_model, 
           pars = c("(Intercept)", "height"), 
           prob = 0.95) # 95% 신뢰구간 표시

# 사후 예측 점검(PPC): 모델이 데이터를 잘 설명하는지 확인
pp_check(bayesian_model)
